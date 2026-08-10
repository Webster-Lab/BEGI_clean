#### READ ME ####

# The purpose of this script is to construct a set of exploratory models to test whether the mean and variance of groundwater depth (depth to water, DTW), water temperature, or fDOM in the 2 days preceding a DO event is related to the size or rate of that event's accrual and respiration phases, or to the total size of the DO event. 

# Note that these models are exploratory (hypothesis generating) and not for inference (strict hypothesis testing) in nature, a la Tredennick et al. 2021. We limit the analysis to this scope due to the low sample size across sites and wells and because of the "wide net" nature of fitting so many models. 
# We add three checks to help validate interpretation of these potentially numerically fragile models:
#   1. A leave-one-out (LOO) sensitivity test in the model diagnosis: this refits each model N times, each time dropping one observation, so we can see whether the estimated slope (and its sign) is stable or is being carried by one or two points.
#   2. A model comparison with contrasting random effect structures: nested (~1|siteID/wellID) vs site-level (~1|wellID). The nested structure is more theoretically correct for the sample design, but is estimating variance components from just 2 site-level groups and 4 well-level groups, both below what's usually recommended for stable random-effect variance estimation (rule-of-thumb guidance is at least 5 groups per level (Harrison et al. 2018)). Comparing to a site-level structure flags a sketchy result if the two disagree in the trends they show; in principle, one might provide a better fit, but the signs of the slopes should be the same. 
#   3. An independent look at simple linear regressions in each well: these are statistically weak compared to the pooled mixed effects models, but provide a  visual/qualitative check that the mixed models are estimating slopes that are driven by a consistent pattern across wells (not by just one or two of them). 

# Response variables tested (from ER_calc_all_events.csv):
#   1. gross_total_areal_gO2_m2   -- total event size (accrual + |ER|), ALL events
#   2. ER_total_areal_gO2_m2      -- total ER, EXCLUDING fDOM-rebound events
#   3. accrual_total_areal_gO2_m2 -- total accrual, ALL events
#   4. ER_rate_hourly_areal_gO2_m2_hr      -- ER rate, EXCLUDING fDOM-rebound events
#   5. accrual_rate_hourly_areal_gO2_m2_hr -- accrual rate, ALL events

# Predictors tested:
#   1. DTW_mean_2d   -- mean depth to water (meters), 2 days before the event
#   2. DTW_sd_2d     -- SD of depth to water (meters), 2 days before the event. 
#   3. temp_mean_2d  -- mean water temperature, 2 days before the event
#   4. fDOM_mean_2d  -- mean fDOM (QSU), 2 days before the event
# We combine testing for correlation across wells and events with within-subject centering (van de Pol & Wright 2009): for each model, each predictor is split into two pieces:
#   a) pred_between = the well-level average of the predictor (mean value repeated over events)
#   b) pred_within  = the event-level value of the predictor, normalized to the wells average (predictor - pred_between)
# this approach separates pooled slopes that can in principle be driven entirely by between-well differences (e.g. one well just generally sits deeper AND generally respires more, with no real event-to-event relationship) from within-well trends and prevents one from confounding the other. 


#### Libraries ####
library(tidyverse)
library(lubridate)
library(nlme)
library(visreg)
library(car)
library(MuMIn)
library(forecast)
library(patchwork)
library(psych)

#### Check/make file structure ####

# make sure output folders exist before anything tries to write to them
dir.create("results", recursive = TRUE, showWarnings = FALSE)

#### Load and join data ####

ER_events_raw <- read_csv("EXO_compiled/ER_calc_all_events.csv") %>%
  mutate(event_id = as.character(event_id))

# dtw
dtw_events_raw <- read_csv("DTW_compiled/DO_mv_2days.csv") %>%
  mutate(event_id = as.character(event_id)) %>%
  select(site, event_id, Eventdate_dtw = Eventdate, DTW_mean_2d, DTW_var_2d, DTW_sd_2d, DTW_cv_2d, DTW_relvar_2d)

events <- left_join(ER_events_raw, dtw_events_raw, by = c("site", "event_id"))
events <- events %>% select(-Eventdate_dtw)

# temperature
temp_events_raw <- read_csv("DTW_compiled/temp_mv_2days.csv") %>%
  mutate(event_id = as.character(event_id)) %>%
  select(site, event_id, Eventdate_temp = Eventdate, temp_mean_2d, temp_sd_2d)

events <- left_join(events, temp_events_raw, by = c("site", "event_id"))
events <- events %>% select(-Eventdate_temp)

# fDOM
fDOM_events_raw <- read_csv("DTW_compiled/fDOM_mv_2days.csv") %>%
  mutate(event_id = as.character(event_id)) %>%
  select(site, event_id, Eventdate_temp = Eventdate, fDOM_mean_2d, fDOM_sd_2d)

events <- left_join(events, fDOM_events_raw, by = c("site", "event_id"))
events <- events %>% select(-Eventdate_temp)

#### Define derived variables ####

# siteID = site pair (SLO wells vs VDO wells), wellID = individual well.
# Used for the nested random effect structure ~1|siteID/wellID.
events <- events %>%
  mutate(
    siteID = substr(site, 1, 3),
    wellID = site,
    # Flip sign on ER so larger magnitude = more respiration (matches the
    # legacy script's posER convention) -- needed for log-transformation too,
    # since raw ER values are <= 0.
    ER_total_mag = -ER_total_areal_gO2_m2,
    ER_rate_mag = -ER_rate_hourly_areal_gO2_m2_hr
  )

events$siteID <- as.factor(events$siteID)
events$wellID <- as.factor(events$wellID)

# Within-subject centering (within/between well variables):
wb_predictors <- c("DTW_mean_2d", "DTW_sd_2d", "temp_mean_2d",
                   "accrual_total_areal_gO2_m2", "accrual_rate_hourly_areal_gO2_m2_hr",
                   "fDOM_mean_2d")
for (p in wb_predictors) {
  well_mean <- ave(events[[p]], events$wellID, FUN = function(x) mean(x, na.rm = TRUE))
  events[[paste0(p, "_between")]] <- well_mean
  events[[paste0(p, "_within")]] <- events[[p]] - well_mean
}

#### Define function for model fitting and diagnosis ####

# random_form: ~1|wellID or ~1|siteID/wellID
# log_transform: if TRUE, models log(response)
# label: short string used in printed headers and saved plot filenames.

fit_and_diagnose <- function(data, response, predictor, random_form,
                             log_transform = FALSE, label = NULL) {
  
  within_col <- paste0(predictor, "_within")
  between_col <- paste0(predictor, "_between")
  if (!all(c(within_col, between_col) %in% names(data))) {
    stop("Predictor '", predictor, "' has no precomputed '", within_col, "'/'", between_col,
         "' columns -- add it to wb_predictors in the Derived variables section above.")
  }
  
  d <- as.data.frame(data)
  d <- d[stats::complete.cases(d[, c(response, within_col, between_col, "wellID")]), ]
  
  resp_expr <- response
  if (log_transform) {
    n_dropped <- sum(d[[response]] <= 0, na.rm = TRUE)
    if (n_dropped > 0) {
      cat("Dropping", n_dropped, "event(s) with", response, "<= 0 to allow log-transform.\n")
      d <- d[d[[response]] > 0, ]
    }
    resp_expr <- paste0("log(", response, ")")
  }
  
  d$pred_within <- d[[within_col]]
  d$pred_between <- d[[between_col]]
  
  n <- nrow(d)
  n_groups_well <- length(unique(d$wellID))
  cat("n =", n, "| wells represented:", n_groups_well, "\n")
  if (n < 10 || n_groups_well < 2) {
    cat("Too few observations/groups to fit -- skipping.\n")
    return(invisible(NULL))
  }
  
  fit_one <- function(fixed_rhs) {
    tryCatch(
      nlme::lme(as.formula(paste(resp_expr, "~", fixed_rhs)),
                data = d, random = random_form, method = "ML",
                control = nlme::lmeControl(opt = "optim")),
      error = function(e) { cat("MODEL FAILED TO FIT:", conditionMessage(e), "\n"); NULL }
    )
  }
  
  m.null <- fit_one("1")
  m.wb <- fit_one("pred_within + pred_between")
  if (is.null(m.null) || is.null(m.wb)) return(invisible(NULL))
  
  aicc_tab <- MuMIn::AICc(m.null, m.wb)
  print(aicc_tab)
  delta_aicc <- aicc_tab$AICc[1] - aicc_tab$AICc[2]
  
  plot(m.wb, main = paste(label, "- residuals vs fitted"))
  qqnorm(residuals(m.wb), main = paste(label, "- QQ plot")); qqline(residuals(m.wb))
  forecast::Acf(residuals(m.wb), main = paste(label, "- residual ACF"))
  
  anova_tab <- anova.lme(m.wb, type = "marginal", adjustSigma = FALSE)
  print(anova_tab)
  ci <- tryCatch(nlme::intervals(m.wb, level = 0.95, which = "fixed"), error = function(e) NULL)
  if (!is.null(ci)) print(ci)
  print(summary(m.wb))
  r2 <- tryCatch(MuMIn::r.squaredGLMM(m.wb), error = function(e) NULL)
  if (!is.null(r2)) { cat("Pseudo-R2 (marginal, conditional):\n"); print(r2) }
  
  # LOO
  full_within <- tryCatch(nlme::fixef(m.wb)[["pred_within"]], error = function(e) NA)
  full_between <- tryCatch(nlme::fixef(m.wb)[["pred_between"]], error = function(e) NA)
  loo_within <- loo_between <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    m_loo <- tryCatch(
      nlme::lme(as.formula(paste(resp_expr, "~ pred_within + pred_between")),
                data = d[-i, ], random = random_form, method = "ML",
                control = nlme::lmeControl(opt = "optim", msMaxIter = 200)),
      error = function(e) NULL
    )
    if (!is.null(m_loo)) {
      loo_within[i] <- tryCatch(nlme::fixef(m_loo)[["pred_within"]], error = function(e) NA)
      loo_between[i] <- tryCatch(nlme::fixef(m_loo)[["pred_between"]], error = function(e) NA)
    }
  }
  flag_one <- function(loo, full) {
    pct <- 100 * (loo - full) / abs(full)
    which(abs(pct) > 50 | (sign(loo) != sign(full)))
  }
  flagged_within <- flag_one(loo_within, full_within)
  flagged_between <- flag_one(loo_between, full_between)
  cat("\nLOO on pred_within: full =", round(full_within, 4),
      "| flagged event(s):", length(flagged_within), "\n")
  cat("LOO on pred_between: full =", round(full_between, 4),
      "| flagged event(s):", length(flagged_between), "\n")
  
  get_pval <- function(term) {
    tryCatch(anova_tab$"p-value"[rownames(anova_tab) == term], error = function(e) NA)
  }
  
  list(
    label = label, n = n, m.null = m.null, m.wb = m.wb, delta_aicc = delta_aicc,
    r2 = r2,
    coef_within = full_within, coef_between = full_between,
    p_within = get_pval("pred_within"), p_between = get_pval("pred_between"),
    n_loo_flagged_within = length(flagged_within),
    n_loo_flagged_between = length(flagged_between)
  )
}

#### Fit models ####

hypotheses <- tibble::tribble(
  ~response,                                  ~data_subset,
  "gross_total_areal_gO2_m2",                 "all",
  "ER_total_mag",                             "no_rebound",
  "accrual_total_areal_gO2_m2",               "all",
  "ER_rate_mag",                              "no_rebound",
  "accrual_rate_hourly_areal_gO2_m2_hr",      "all"
)

predictors <- c("DTW_mean_2d", "DTW_sd_2d", "temp_mean_2d", "fDOM_mean_2d")

extra_pairs <- tibble::tribble(
  ~response,       ~predictor,                              ~data_subset,
  "ER_total_mag",  "accrual_total_areal_gO2_m2",             "no_rebound",
  "ER_rate_mag",   "accrual_rate_hourly_areal_gO2_m2_hr",    "no_rebound"
)

model_specs <- bind_rows(
  tidyr::crossing(hypotheses, predictor = predictors),
  extra_pairs
)

random_structures <- list(nested = ~1 | siteID/wellID, well_only = ~1 | wellID)

all_results <- list()

for (i in seq_len(nrow(model_specs))) {
  resp <- model_specs$response[i]
  pred <- model_specs$predictor[i]
  subset_data <- if (model_specs$data_subset[i] == "no_rebound") {
    events %>% filter(!fDOM_rebound)
  } else {
    events
  }
  
  for (rs_name in names(random_structures)) {
    key <- paste(resp, pred, rs_name, sep = " | ")
    all_results[[key]] <- fit_and_diagnose(
      data = subset_data, response = resp, predictor = pred,
      random_form = random_structures[[rs_name]],
      log_transform = TRUE,
      label = paste0(resp, " ~ ", pred, " (within/between) [", rs_name, ", log]")
    )
  }
}

#### Write summary table across all fitted models ####

summary_rows <- purrr::imap_dfr(all_results, function(res, key) {
  if (is.null(res)) return(NULL)
  parts <- strsplit(key, " \\| ")[[1]]
  bind_rows(
    tibble(
      response = parts[1], predictor = paste0(parts[2], "_within"), random_structure = parts[3],
      model_type = "within/between", n = res$n, delta_AICc = res$delta_aicc,
      coef = res$coef_within, p_value = res$p_within,
      R2_marginal = tryCatch(res$r2[1, "R2m"], error = function(e) NA),
      R2_conditional = tryCatch(res$r2[1, "R2c"], error = function(e) NA),
      n_loo_flagged = res$n_loo_flagged_within
    ),
    tibble(
      response = parts[1], predictor = paste0(parts[2], "_between"), random_structure = parts[3],
      model_type = "within/between", n = res$n, delta_AICc = res$delta_aicc,
      coef = res$coef_between, p_value = res$p_between,
      R2_marginal = tryCatch(res$r2[1, "R2m"], error = function(e) NA),
      R2_conditional = tryCatch(res$r2[1, "R2c"], error = function(e) NA),
      n_loo_flagged = res$n_loo_flagged_between
    )
  )
})

write_csv(summary_rows, "results/EReventmodels_summary.csv")


#### Per-well simple linear regressions ####


dir.create("plots", showWarnings = FALSE)

per_well_list <- list()

for (i in seq_len(nrow(model_specs))) {
  resp <- model_specs$response[i]
  pred <- model_specs$predictor[i]
  subset_data <- if (model_specs$data_subset[i] == "no_rebound") {
    events %>% filter(!fDOM_rebound)
  } else {
    events
  }
  
  # one combined plot per response x predictor, faceted by well
  d_plot <- subset_data[stats::complete.cases(subset_data[, c(resp, pred)]), ]
  d_plot <- d_plot[d_plot[[resp]] > 0, ]  # log scale on y
  
  p <- ggplot(d_plot, aes(x = .data[[pred]], y = .data[[resp]])) +
    geom_point() +
    scale_y_log10() +
    geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
    facet_wrap(~wellID, scales = "free_x") +
    labs(title = paste(resp, "~", pred, "by well (unpooled lm, log y-axis)"),
         x = pred, y = paste0(resp, " (log scale)")) +
    theme_classic()
  print(p)
  ggsave(
    filename = file.path("plots", paste0("perwell_", resp, "_", pred, ".png")),
    plot = p, width = 8, height = 6, dpi = 150
  )
  
  for (well in levels(events$wellID)) {
    d_well <- d_plot[d_plot$wellID == well, ]
    key <- paste(resp, pred, well, sep = " | ")
    
    if (nrow(d_well) < 5) {
      cat("Skipping", key, "-- only", nrow(d_well), "event(s) with complete data.\n")
      next
    }
    
    m <- tryCatch(
      lm(as.formula(paste0("log(", resp, ") ~ ", pred)), data = d_well),
      error = function(e) NULL
    )
    if (is.null(m)) next
    s <- summary(m)
    
    per_well_list[[key]] <- tibble(
      response = resp, predictor = pred, well = well, n = nrow(d_well),
      slope = coef(m)[[pred]],
      p_value = s$coefficients[pred, "Pr(>|t|)"],
      r_squared = s$r.squared
    )
  }
}

per_well_summary <- bind_rows(per_well_list)

write_csv(per_well_summary, "DTW_compiled/EReventmodels_perwell_summary.csv")
