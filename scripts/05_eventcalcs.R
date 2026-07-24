#### READ ME ####

# The purpose of this script is to calculate total event size, ecosystem respiration (ER), and accrual metrics for every DO event, every well, in BEGI_events.rds.
# ER and accrual are calculated using Odum's (1956) / Hall & Hotchkiss's (2017) direct-calculation approach, adapted for our shallow groundwater setting (no light -> mean GPP = 0; no air-water interface or turbulence -> gas exchange (K) assumed negligible).
#
# Because DO events are sporadic and don't align with a diel cycle, a "per day" rate isn't a natural unit to report. Each event instead gets TWO distinct quantities:
#   - an average hourly rate (an "intensity", comparable across events/wells). Units g O2/m2/hr.
#   - an event total (a total amount, g O2/m2, NOT a rate - the full integrated accrual or respiration for that one event).
#
# Accrual/ER classification is done per-interval, by the sign (+/-) of each 15-min DO change (rising = accrual, declining = ER). This is sensitive to sensor noise, so a noise threshold (noise_threshold_mgL) excludes any per-interval change smaller than the EXO's stated resolution.

### BIG NOTE: this script analyzes ALL DO events as accrual + aerobic respiration in order to get total (gross) event sizes and do the conversion to C mass for all as part of the workflow. The following script contains a cluster analysis used to identify and remove suspected chemical oxidation events from the total. SO, the final C numbers are including suspected chemical oxidation and therefore are overestimating C respired when summed. The code producing the "carbon_summary_by_well.csv" table  (line 158-end) should be re-run after chemical oxidation events are removed.  

# Requirements: rds files from previous scripts:
# 1. BEGI_events.rds

# Outputs for downstream use:
# 1. ER_calc_all_events.csv
# 2. carbon_summary_by_well.csv


#### Libraries ####
library(tidyverse)
library(dplyr)
library(patchwork)

#### Import all events ####
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")



#### Run the ER/accrual calculation on one event dataframe ####

#### Define time step constants for calculation function
interval_min   = 15                       # sampling interval, minutes
steps_per_hour = 60 / interval_min        # sampling intervals per hour
interval_hr    = interval_min / 60        # width of one interval, in hours

noise_threshold_mgL = 0.01   # EXO manual resolution
z_m = 1   # meters; sensor depth below water table

#### Pull an event's category (metabolism / lateral transfer / other) 
get_event_category = function(dz) {
  vals = dz$event[!is.na(dz$event)]
  if (length(vals) == 0) return("unclassified")
  names(sort(table(vals), decreasing = TRUE))[1]
}

# define function
calc_ER_event = function(event_df, maxgap = 4*3) {
  
  # gap-fill short stretches of missing ODO.mg.L.mn via spline interpolation before computing rate of change, so a few missing 15-min readings don't break accrual/decline classification around them. maxgap is the longest run of consecutive missing readings that gets filled, in # of 15-min intervals - longer gaps are left as NA rather than interpolated across, since assuming smooth change over a long gap isn't well justified for these short, punctuated events. 
  event_df$ODO.mg.L.mn_filled = as.numeric(
    zoo::na.spline(zoo::zoo(event_df$ODO.mg.L.mn, order.by = event_df$datetimeMT),
                   maxgap = maxgap, na.rm = FALSE)
  )
  # flag which readings were actually filled, for QA/transparency
  event_df$ODO_was_filled = is.na(event_df$ODO.mg.L.mn) & !is.na(event_df$ODO.mg.L.mn_filled)
  
  event_df = event_df %>%
    mutate(
      rate_of_change_per_interval = c(NA, diff(ODO.mg.L.mn_filled)),        # mg/L per 15-min step
      rate_of_change_hourly = rate_of_change_per_interval * steps_per_hour  # mg/L per hour
    )
  
  accrual_idx = which(event_df$rate_of_change_per_interval > noise_threshold_mgL)
  decline_idx = which(event_df$rate_of_change_per_interval < -noise_threshold_mgL)
  
  accrual_duration_hr = length(accrual_idx) * interval_hr
  decline_duration_hr = length(decline_idx) * interval_hr
  
  # NaN (not NA) is expected here if an event has zero accrual (or zero
  # decline) intervals - i.e. an event that moved only one direction
  accrual_rate_hourly = mean(event_df$rate_of_change_hourly[accrual_idx], na.rm = TRUE)
  ER_rate_hourly       = mean(event_df$rate_of_change_hourly[decline_idx], na.rm = TRUE)
  
  accrual_total = sum(event_df$rate_of_change_per_interval[accrual_idx], na.rm = TRUE)
  ER_total       = sum(event_df$rate_of_change_per_interval[decline_idx], na.rm = TRUE)
  
  # total gross activity over the whole event, regardless of direction
  gross_total = accrual_total + abs(ER_total)
  
  accrual_total_areal = accrual_total * z_m
  ER_total_areal       = ER_total * z_m
  gross_total_areal     = accrual_total_areal + abs(ER_total_areal)
  
  data.frame(
    accrual_rate_hourly_mgL_hr          = accrual_rate_hourly,
    accrual_duration_hr                 = accrual_duration_hr,
    accrual_total_mgL                   = accrual_total,
    ER_rate_hourly_mgL_hr               = ER_rate_hourly,
    ER_duration_hr                      = decline_duration_hr,
    ER_total_mgL                        = ER_total,
    gross_total_mgL                     = gross_total,
    accrual_rate_hourly_areal_gO2_m2_hr = accrual_rate_hourly * z_m,
    accrual_total_areal_gO2_m2          = accrual_total_areal,
    ER_rate_hourly_areal_gO2_m2_hr      = ER_rate_hourly * z_m,
    ER_total_areal_gO2_m2               = ER_total_areal,
    gross_total_areal_gO2_m2            = gross_total_areal,
    n_gaps_filled                       = sum(event_df$ODO_was_filled, na.rm = TRUE)
  )
}

#### Run for every DO event, every site ####
DO_events = BEGI_events[["DO_events"]]

ER_results = do.call(rbind, lapply(names(DO_events), function(site_key) {
  site_events = DO_events[[site_key]]
  site_name = sub("_DO$", "", site_key)   # "SLOC_DO" -> "SLOC"
  
  do.call(rbind, lapply(seq_along(site_events), function(i) {
    result = calc_ER_event(site_events[[i]])
    result$site     = site_name
    result$event_id = i
    result$category = get_event_category(site_events[[i]])
    result
  }))
}))

ER_results = ER_results[, c("site", "event_id", "category",
                             "accrual_rate_hourly_mgL_hr", "accrual_duration_hr", "accrual_total_mgL",
                             "ER_rate_hourly_mgL_hr", "ER_duration_hr", "ER_total_mgL",
                             "gross_total_mgL",
                             "accrual_rate_hourly_areal_gO2_m2_hr", "accrual_total_areal_gO2_m2",
                             "ER_rate_hourly_areal_gO2_m2_hr", "ER_total_areal_gO2_m2",
                             "gross_total_areal_gO2_m2")]


#### Convert ER estimates to CO2 and carbon mass via respiratory quotients (RQ) ####
# RQ = mol CO2 produced / mol O2 consumed. Point estimate RQ = 1.2; the full theoretical range (0.5-4.0, Berggren et al. 2012) gives lower/upper bounds reflecting uncertainty in substrate.
MW_O2  = 32   # g/mol
MW_CO2 = 44   # g/mol
MW_C   = 12   # g/mol - mass of C is the same whether expressed as CO2 or elemental C
RQ_mid  = 1.2
RQ_low  = 0.5
RQ_high = 4.0

# Converts an O2 quantity (any units - carries through unchanged, only the chemical species/mass changes) to CO2 mass at a given RQ. Sign is flipped so CO2 PRODUCTION is reported as positive (ER quantities are negative, since they represent declining DO)
O2_to_CO2 = function(O2_value, RQ) -O2_value * RQ * (MW_CO2 / MW_O2)
O2_to_C   = function(O2_value, RQ) -O2_value * RQ * (MW_C / MW_O2)

# Applies both conversions (CO2 and C) at all three RQ bounds to one ER column, returning 6 new columns named with the source column as a prefix.
# NOTE: the magnitude (mg vs g, per L vs per m2, rate vs total) carries over unchanged from the source column - only the chemical species changes, from O2 to CO2 or C. e.g. ER_total_mgL_as_C_mid is in mg C/L, not mg O2/L.
add_RQ_conversions = function(er_col, prefix) {
  out = data.frame(
    as_CO2_mid  = O2_to_CO2(er_col, RQ_mid),
    as_CO2_low  = O2_to_CO2(er_col, RQ_low),
    as_CO2_high = O2_to_CO2(er_col, RQ_high),
    as_C_mid    = O2_to_C(er_col, RQ_mid),
    as_C_low    = O2_to_C(er_col, RQ_low),
    as_C_high   = O2_to_C(er_col, RQ_high)
  )
  names(out) = paste0(prefix, "_", names(out))
  out
}

ER_results = cbind(
  ER_results,
  add_RQ_conversions(ER_results$ER_rate_hourly_mgL_hr,         "ER_rate_hourly_mgL_hr"),
  add_RQ_conversions(ER_results$ER_total_mgL,                   "ER_total_mgL"),
  add_RQ_conversions(ER_results$ER_rate_hourly_areal_gO2_m2_hr, "ER_rate_hourly_areal_gO2_m2_hr"),
  add_RQ_conversions(ER_results$ER_total_areal_gO2_m2,          "ER_total_areal_gO2_m2")
)

#### Save results
write.csv(ER_results, "EXO_compiled/ER_calc_all_events.csv", row.names = FALSE)


#### Summarize carbon respiration by well, across all events for annual rates & total ####

carbon_summary = ER_results %>%
  group_by(site) %>%
  summarise(
    n_events = n(),
    
    # total carbon mass respired, summed across all events (g C/m2)
    total_C_respired_gC_m2_mid  = sum(ER_total_areal_gO2_m2_as_C_mid,  na.rm = TRUE),
    total_C_respired_gC_m2_low  = sum(ER_total_areal_gO2_m2_as_C_low,  na.rm = TRUE),
    total_C_respired_gC_m2_high = sum(ER_total_areal_gO2_m2_as_C_high, na.rm = TRUE),
    
    # average rate of carbon consumption across all events (g C/m2/hr)
    mean_C_rate_gC_m2_hr_mid  = mean(ER_rate_hourly_areal_gO2_m2_hr_as_C_mid,  na.rm = TRUE),
    mean_C_rate_gC_m2_hr_low  = mean(ER_rate_hourly_areal_gO2_m2_hr_as_C_low,  na.rm = TRUE),
    mean_C_rate_gC_m2_hr_high = mean(ER_rate_hourly_areal_gO2_m2_hr_as_C_high, na.rm = TRUE),
    
    .groups = "drop"
  )

write.csv(carbon_summary, "EXO_compiled/carbon_summary_by_well.csv", row.names = FALSE)

# mean annual C respired:
mean(carbon_summary$total_C_respired_gC_m2_mid)
# 6.0 g C/m2/y
