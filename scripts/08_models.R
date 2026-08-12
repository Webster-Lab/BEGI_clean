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
library(gt)

#### Check/make file structure ####

# make sure output folders exist before anything tries to write to them
dir.create("results", recursive = TRUE, showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)

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

model_specs <- bind_rows(
  tidyr::crossing(hypotheses, predictor = predictors)
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

#### Pub-ready model results ####

#### Make model summary tables
response_unit_labels <- c(
  "gross_total_areal_gO2_m2"            = "Total event size (g O₂ m⁻²)",
  "ER_total_mag"                        = "ER magnitude (g O₂ m⁻²)",
  "accrual_total_areal_gO2_m2"          = "Accrual total (g O₂ m⁻²)",
  "ER_rate_mag"                         = "ER rate magnitude (g O₂ m⁻² hr⁻¹)",
  "accrual_rate_hourly_areal_gO2_m2_hr" = "Accrual rate (g O₂ m⁻² hr⁻¹)"
)

predictor_unit_labels <- c(
  "DTW_mean_2d"  = "Mean depth to water, 2d pre-event (m)",
  "DTW_sd_2d"    = "SD of depth to water, 2d pre-event (m)",
  "temp_mean_2d" = "Mean water temperature, 2d pre-event (°C)",
  "fDOM_mean_2d" = "Mean fDOM, 2d pre-event (QSU)"
)

term_unit_labels <- c("within" = "Within-well", "between" = "Between-well")

group_unit_labels <- c(
  "DTW_sd_2d_within"     = "SD of depth to water, 2d pre-event — within-well",
  "fDOM_mean_2d_between" = "Mean fDOM, 2d pre-event — between-well"
)

# Falls back to the raw value (rather than NA) for anything not in the lookups
humanize <- function(x, lookup) dplyr::if_else(x %in% names(lookup), unname(lookup[x]), x)

summary_all <- read_csv("results/EReventmodels_summary.csv")

summary_nested <- summary_all %>%
  filter(random_structure == "nested") %>%
  mutate(
    term = str_extract(predictor, "(within|between)$"),
    predictor_base = str_remove(predictor, "_(within|between)$")
  ) 

make_summary_table <- function(data, title, subtitle, out_file) {
  prepped <- data %>%
    arrange(predictor_base, response, term) %>%
    mutate(
      predictor_base = humanize(predictor_base, predictor_unit_labels),
      response = humanize(response, response_unit_labels),
      term = humanize(term, term_unit_labels)
    ) %>%
    select(predictor_base, response, term, n, coef, p_value, delta_AICc,
           R2_marginal, R2_conditional, n_loo_flagged)
  
  tab <- gt(prepped, groupname_col = "predictor_base")
  tab <- tab_header(tab, title = title, subtitle = subtitle)
  tab <- fmt_number(tab, columns = c(coef, delta_AICc, R2_marginal, R2_conditional), n_sigfig = 2)
  tab <- fmt_number(tab, columns = p_value, n_sigfig = 2)
  tab <- cols_label(
    tab,
    response = "Response", term = "Term", n = "n", coef = "Coefficient",
    p_value = "p", delta_AICc = "ΔAICc", R2_marginal = "R2 (marginal)",
    R2_conditional = "R2 (conditional)", n_loo_flagged = "LOO flags"
  )
  
  
  gtsave(tab, file.path("tables", out_file))
  print(tab)
  tab
}

gt_dtw <- make_summary_table(
  summary_nested %>% filter(predictor_base %in% c("DTW_mean_2d", "DTW_sd_2d")),
  title = "Depth-to-water (DTW) predictors",
  subtitle = "Response ~ Mean DTW or SD DTW (m), 2 days pre-event",
  out_file = "summary_DTW.html"
)

gt_temp <- make_summary_table(
  summary_nested %>% filter(predictor_base == "temp_mean_2d"),
  title = "Temperature predictor",
  subtitle = "Response ~ Mean Temperature (C), 2 days pre-event",
  out_file = "summary_temperature.html"
)

gt_fdom <- make_summary_table(
  summary_nested %>% filter(predictor_base == "fDOM_mean_2d"),
  title = "fDOM predictor",
  subtitle = "Response ~ Mean fDOM (QSU), 2 days pre-event",
  out_file = "summary_fDOM.html"
)




####
#### Make figures of most "interesting" models

ER_events_raw <- read_csv("EXO_compiled/ER_calc_all_events.csv") %>%
  mutate(event_id = as.character(event_id))

dtw_events_raw <- read_csv("DTW_compiled/DO_mv_2days.csv") %>%
  mutate(event_id = as.character(event_id)) %>%
  select(site, event_id, DTW_sd_2d)

fdom_events_raw <- read_csv("DTW_compiled/fdom_mv_2days.csv") %>%
  mutate(event_id = as.character(event_id)) %>%
  select(site, event_id, fDOM_mean_2d)

events <- ER_events_raw %>%
  left_join(dtw_events_raw, by = c("site", "event_id")) %>%
  left_join(fdom_events_raw, by = c("site", "event_id")) %>%
  mutate(
    siteID = as.factor(substr(site, 1, 3)),
    wellID = as.factor(site),
    ER_total_mag = -ER_total_areal_gO2_m2
  )

# Within/between variables
wb_predictors <- c("DTW_sd_2d", "fDOM_mean_2d")
for (p in wb_predictors) {
  well_mean <- ave(events[[p]], events$wellID, FUN = function(x) mean(x, na.rm = TRUE))
  events[[paste0(p, "_between")]] <- well_mean
  events[[paste0(p, "_within")]] <- events[[p]] - well_mean
}

### The highlighted models:
highlighted_models <- tibble::tribble(
  ~group,                   ~response,                      ~predictor,     ~data_subset,
  "DTW_sd_2d_within",       "accrual_total_areal_gO2_m2",   "DTW_sd_2d",    "all",
  "DTW_sd_2d_within",       "ER_total_mag",                  "DTW_sd_2d",    "no_rebound",
  "DTW_sd_2d_within",       "gross_total_areal_gO2_m2",      "DTW_sd_2d",    "all"
)

nested_random <- ~1 | siteID/wellID

### Fit function: nested random effects, log(response), with full stats + leave-one-out extracted
fit_highlighted <- function(data, response, predictor, label) {
  
  within_col <- paste0(predictor, "_within")
  between_col <- paste0(predictor, "_between")
  
  d <- as.data.frame(data)
  d <- d[stats::complete.cases(d[, c(response, within_col, between_col, "wellID", "siteID")]), ]
  d <- d[d[[response]] > 0, ]  # required for log()
  d$pred_within <- d[[within_col]]
  d$pred_between <- d[[between_col]]
  
  d$y <- log(d[[response]])
  null_formula <- as.formula("y ~ 1")
  wb_formula <- as.formula("y ~ pred_within + pred_between")
  
  m.null <- nlme::lme(null_formula, data = d, random = nested_random, method = "ML",
                      control = nlme::lmeControl(opt = "optim"))
  m.wb <- nlme::lme(wb_formula, data = d, random = nested_random, method = "ML",
                    control = nlme::lmeControl(opt = "optim"))
  m.null$call$fixed <- null_formula
  m.wb$call$fixed <- wb_formula
  
  aicc_tab <- MuMIn::AICc(m.null, m.wb)
  delta_aicc <- aicc_tab$AICc[1] - aicc_tab$AICc[2]
  r2 <- tryCatch(MuMIn::r.squaredGLMM(m.wb), error = function(e) c(NA, NA))
  ci <- tryCatch(as.data.frame(nlme::intervals(m.wb, level = 0.95, which = "fixed")$fixed),
                 error = function(e) NULL)
  
  tt <- summary(m.wb)$tTable
  coef_tab <- tibble::tibble(
    term = rownames(tt), estimate = tt[, "Value"], se = tt[, "Std.Error"],
    df = tt[, "DF"], t_value = tt[, "t-value"], p_value = tt[, "p-value"]
  )
  if (!is.null(ci)) {
    coef_tab$ci_lower <- ci[coef_tab$term, "lower"]
    coef_tab$ci_upper <- ci[coef_tab$term, "upper"]
  } else {
    coef_tab$ci_lower <- NA_real_
    coef_tab$ci_upper <- NA_real_
  }
  
  # LOO
  n <- nrow(d)
  full_within <- nlme::fixef(m.wb)[["pred_within"]]
  full_between <- nlme::fixef(m.wb)[["pred_between"]]
  loo_within <- loo_between <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    m_loo <- tryCatch(
      nlme::lme(wb_formula, data = d[-i, ], random = nested_random, method = "ML",
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
    sum(abs(pct) > 50 | (sign(loo) != sign(full)), na.rm = TRUE)
  }
  loo_flags <- c(pred_within = flag_one(loo_within, full_within),
                 pred_between = flag_one(loo_between, full_between))
  coef_tab$n_loo_flagged <- ifelse(coef_tab$term %in% names(loo_flags),
                                   loo_flags[coef_tab$term], NA_integer_)
  
  coef_tab <- coef_tab %>%
    mutate(
      label = label, n = n, n_wells = length(unique(d$wellID)),
      R2_marginal = r2[1], R2_conditional = r2[2], delta_AICc = delta_aicc,
      .before = 1
    )
  
  list(coef_tab = coef_tab, model = m.wb, data = d)
}

### Fit all 
fit_list <- list()
for (i in seq_len(nrow(highlighted_models))) {
  grp <- highlighted_models$group[i]
  resp <- highlighted_models$response[i]
  pred <- highlighted_models$predictor[i]
  subset_data <- if (highlighted_models$data_subset[i] == "no_rebound") {
    events %>% filter(!fDOM_rebound)
  } else {
    events
  }
  key <- paste(grp, resp, sep = " | ")
  label <- paste0(resp, " ~ ", pred, " (", grp, ")")
  cat("\nFitting", label, "...\n")
  fit_list[[key]] <- fit_highlighted(subset_data, response = resp, predictor = pred, label = label)
}

### Highlighted models: full fixed-effects statistics table
highlighted_full_stats <- purrr::map_dfr(fit_list, "coef_tab", .id = "key") %>%
  separate(key, into = c("group", "response"), sep = " \\| ") %>%
  left_join(highlighted_models %>% select(group, response, predictor), by = c("group", "response")) %>%
  relocate(group, response, predictor)

write_csv(highlighted_full_stats, "results/highlightedmodels_fullstats.csv")

prepped_highlighted <- highlighted_full_stats %>%
  filter(term != "(Intercept)") %>%
  mutate(
    group = humanize(group, group_unit_labels),
    response = humanize(response, response_unit_labels),
    predictor = humanize(predictor, predictor_unit_labels),
    term = dplyr::case_match(term, "pred_within" ~ "Within-well", "pred_between" ~ "Between-well",
                             .default = term)
  ) %>%
  select(group, response, predictor, term, estimate, se, p_value, ci_lower, ci_upper,
         n, n_wells, R2_marginal, R2_conditional, delta_AICc, n_loo_flagged)

gt_highlighted <- gt(prepped_highlighted, groupname_col = "group")
gt_highlighted <- tab_header(
  gt_highlighted,
  title = "Highlighted models: full fixed-effects statistics"
)
# n_sigfig, not fixed decimals -- see note in make_summary_table above.
gt_highlighted <- fmt_number(
  gt_highlighted, columns = c(estimate, se, ci_lower, ci_upper, R2_marginal, R2_conditional, delta_AICc),
  n_sigfig = 2
)
gt_highlighted <- fmt_number(gt_highlighted, columns = p_value, n_sigfig = 2)
gt_highlighted <- cols_label(
  gt_highlighted,
  response = "Response", predictor = "Predictor", term = "Term", estimate = "Coefficient",
  se = "SE", p_value = "p", ci_lower = "95% CI low", ci_upper = "95% CI high",
  n = "n", n_wells = "Wells", R2_marginal = "R2 (marginal)", R2_conditional = "R2 (conditional)",
  delta_AICc = "ΔAICc", n_loo_flagged = "LOO flags"
)

gtsave(gt_highlighted, "tables/highlightedmodels_fullstats.html")
print(gt_highlighted)


### Fit-over-data figures
# Two different plot designs, matching the two different kinds of relationship being shown:
#  - DTW_sd_2d_within: a within-well relationship, so it's shown faceted by well (matches how many individual events per well there are to fit a line through), fitted line/CI from the fixed effects.
#  - fDOM_mean_2d_between: a between-well relationship. Each well only contributes ONE value (average fDOM), so this plots one point per well (well-mean response vs. well-mean fDOM) with the fitted between-well line/CI overlaid.
# Both fitted lines/CIs are computed on the log scale using fixed-effects parameter uncertainty only and back-transformed with exp().

response_label <- function(resp) {
  switch(resp,
         "accrual_total_areal_gO2_m2" = expression(paste("Accrual total (g ", O[2], " ", m^-2, ")")),
         "ER_total_mag"                = expression(paste("ER magnitude (g ", O[2], " ", m^-2, ")")),
         "gross_total_areal_gO2_m2"    = expression(paste("Total event size (g ", O[2], " ", m^-2, ")")),
         resp
  )
}

well_colors <- c(
  SLOC = "#440154FF", SLOW = "#31688EFF", VDOS = "#35B779FF", VDOW = "#FDE725FF"
)

make_fit_plot_within <- function(fit, response, predictor, x_label, y_label) {
  d <- fit$data
  m <- fit$model
  
  well_ranges <- d %>%
    group_by(wellID) %>%
    summarise(
      x_min = min(.data[[predictor]]), x_max = max(.data[[predictor]]),
      well_mean = mean(.data[[predictor]]), .groups = "drop"
    )
  
  pred_grid <- purrr::pmap_dfr(well_ranges, function(wellID, x_min, x_max, well_mean) {
    x <- seq(x_min, x_max, length.out = 50)
    tibble::tibble(wellID = wellID, x = x, pred_within = x - well_mean, pred_between = well_mean)
  })
  
  term_order <- c("(Intercept)", "pred_within", "pred_between")
  beta <- nlme::fixef(m)[term_order]
  V <- vcov(m)[term_order, term_order]
  X <- cbind(1, pred_grid$pred_within, pred_grid$pred_between)
  
  fitted_log <- as.vector(X %*% beta)
  se_log <- sqrt(rowSums((X %*% V) * X))
  
  pred_grid$fitted <- exp(fitted_log)
  pred_grid$lower <- exp(fitted_log - qnorm(0.975) * se_log)
  pred_grid$upper <- exp(fitted_log + qnorm(0.975) * se_log)
  
  ggplot() +
    geom_ribbon(data = pred_grid, aes(x = x, ymin = lower, ymax = upper),
                fill = "grey", alpha = 0.15) +
    geom_point(data = d, aes(x = .data[[predictor]], y = .data[[response]], color = wellID),
               size = 1.8, alpha = 0.8) +
    geom_line(data = pred_grid, aes(x = x, y = fitted), color = "grey", linewidth = 1) +
    facet_wrap(~wellID, scales = "free_x", nrow = 1) +
    scale_color_manual(values = well_colors) +
    scale_y_log10() +
    labs(x = x_label, y = y_label) +
    theme_classic() +
    theme(legend.position = "none")
}
make_fit_plot_between <- function(fit, response, title, x_label, y_label) {
  d <- fit$data
  m <- fit$model
  
  well_pts <- d %>%
    group_by(wellID) %>%
    summarise(x_between = dplyr::first(pred_between), y_mean = mean(.data[[response]]), .groups = "drop")
  
  x_seq <- seq(min(well_pts$x_between), max(well_pts$x_between), length.out = 50)
  term_order <- c("(Intercept)", "pred_between")
  beta <- nlme::fixef(m)[term_order]
  V <- vcov(m)[term_order, term_order]
  X <- cbind(1, x_seq)
  fitted_log <- as.vector(X %*% beta)
  se_log <- sqrt(rowSums((X %*% V) * X))
  
  line_df <- tibble::tibble(
    x = x_seq, fitted = exp(fitted_log),
    lower = exp(fitted_log - qnorm(0.975) * se_log),
    upper = exp(fitted_log + qnorm(0.975) * se_log)
  )
  
  ggplot() +
    geom_ribbon(data = line_df, aes(x = x, ymin = lower, ymax = upper),
                fill = "grey", alpha = 0.15) +
    geom_line(data = line_df, aes(x = x, y = fitted), color = "grey", linewidth = 1) +
    geom_point(data = well_pts, aes(x = x_between, y = y_mean, color = wellID), size = 3) +
    geom_text(data = well_pts, aes(x = x_between, y = y_mean, label = wellID, color = wellID),
              size = 3, vjust = -1, show.legend = FALSE) +
    scale_color_manual(values = well_colors) +
    scale_y_log10() +
    labs(title = title, x = x_label, y = y_label) +
    theme_classic() +
    theme(plot.title = element_text(size = 10), legend.position = "none")
}


dtw_group <- highlighted_models %>% filter(group == "DTW_sd_2d_within")
dtw_fit_plots <- purrr::pmap(dtw_group, function(group, response, predictor, data_subset) {
  make_fit_plot_within(
    fit_list[[paste(group, response, sep = " | ")]],
    response = response, predictor = predictor,
    x_label = "SD of depth to water, 2 days pre-event (m)",
    y_label = response_label(response)
  )
})
p_dtw_within_fit <- wrap_plots(dtw_fit_plots, ncol = 1) +
  plot_annotation(
    title = "Accrual, ER, and total event size vs. within-well groundwater variability (DTW SD, 2d pre-event)"
  )
ggsave("plots/highlighted_DTW_sd_2d_within_fit.png", p_dtw_within_fit, width = 10, height = 10.5, dpi = 150)


#### Model diagnostics figures
# residuals vs. fitted, QQ plot, residual ACF
make_diagnostic_row <- function(fit, label) {
  m <- fit$model
  resid_df <- tibble::tibble(fitted = fitted(m), resid = residuals(m))
  
  p_resid <- ggplot(resid_df, aes(fitted, resid)) +
    geom_point(size = 1.5, alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    labs(title = label, x = "Fitted (log scale)", y = "Residual") +
    theme_classic() +
    theme(plot.title = element_text(size = 9))
  
  p_qq <- ggplot(resid_df, aes(sample = resid)) +
    stat_qq(size = 1.5, alpha = 0.7) +
    stat_qq_line(color = "#1b6ca8") +
    labs(title = NULL, x = "Theoretical quantiles", y = "Sample quantiles") +
    theme_classic()
  
  acf_obj <- acf(residuals(m), plot = FALSE)
  acf_df <- tibble::tibble(lag = as.vector(acf_obj$lag), acf = as.vector(acf_obj$acf))
  ci_line <- qnorm(0.975) / sqrt(acf_obj$n.used)
  p_acf <- ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_col(width = 0.4, fill = "grey30") +
    geom_hline(yintercept = c(-ci_line, ci_line), linetype = "dashed", color = "#1b6ca8") +
    labs(title = NULL, x = "Lag", y = "ACF") +
    theme_classic()
  
  p_resid + p_qq + p_acf + patchwork::plot_layout(nrow = 1)
}

dtw_diag_rows <- purrr::pmap(dtw_group, function(group, response, predictor, data_subset) {
  make_diagnostic_row(fit_list[[paste(group, response, sep = " | ")]], label = response)
})
p_dtw_diag <- wrap_plots(dtw_diag_rows, ncol = 1) +
  plot_annotation(
    title = "Model diagnostics: response ~ DTW_sd_2d_within (nested random effects)"
  )
ggsave("plots/highlighted_DTW_sd_2d_within_diagnostics.png", p_dtw_diag, width = 10, height = 9, dpi = 150)

