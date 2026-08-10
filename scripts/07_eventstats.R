#### read me ####
# The purpose of this script is to calculate for each event delineated in 04_eventdelineation.R, 
# the mean and variance of depth to groundwater and temperature, to be used in later script of linear mixed models

# Output:
# 1. dataframe of groundwater mean and variance for each well
# 2. dataframe of groundwater mean and variance for each event

#### Libraries and functions####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)
library(DescTools)

cv <- function (x){
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
}

#### Import compiled data ####
EXOz.dtw = readRDS("DTW_compiled/BEGI_EXOz_dtw.rds")

#### Groundwater depth whole well mean/var ####
wells<-c("SLOC","SLOW","VDOS","VDOW")

# calc mean
gwmean_well<-c(mean(EXOz.dtw[["SLOC"]]$DTW_m, na.rm = TRUE),
               mean(EXOz.dtw[["SLOW"]]$DTW_m, na.rm = TRUE),
               mean(EXOz.dtw[["VDOS"]]$DTW_m, na.rm = TRUE),
               mean(EXOz.dtw[["VDOW"]]$DTW_m, na.rm = TRUE))

# calc variance as sample variance
gwtruevar_well <- c(var(EXOz.dtw[["SLOC"]]$DTW_m, na.rm = TRUE),
                    var(EXOz.dtw[["SLOW"]]$DTW_m, na.rm = TRUE),
                    var(EXOz.dtw[["VDOS"]]$DTW_m, na.rm = TRUE),
                    var(EXOz.dtw[["VDOW"]]$DTW_m, na.rm = TRUE))
names(gwtruevar_well) <- wells

# calc variance as cv (normalized to mean)
gwvar_well<-c(cv(EXOz.dtw[["SLOC"]]$DTW_m),
              cv(EXOz.dtw[["SLOW"]]$DTW_m),
              cv(EXOz.dtw[["VDOS"]]$DTW_m),
              cv(EXOz.dtw[["VDOW"]]$DTW_m))

gwmv_well<-data.frame(wells,gwmean_well,gwvar_well,gwtruevar_well)
gwmv_well

#### Export for use in other scripts ####
write.csv(gwmv_well, "DTW_compiled/gwmv_well.csv")

#### Import list of event dates per well ####
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")

#Turns lists into vectors
SLOC_dates <- c(BEGI_events[["Eventdate"]][["SLOC_dates"]])
SLOW_dates <- c(BEGI_events[["Eventdate"]][["SLOW_dates"]])
VDOS_dates <- c(BEGI_events[["Eventdate"]][["VDOS_dates"]])
VDOW_dates <- c(BEGI_events[["Eventdate"]][["VDOW_dates"]])


#### DO event dtw mean, variance, SD, and CV (2 days before each event) ####

wells <- c("SLOC", "SLOW", "VDOS", "VDOW")

dtw_event_stats_list <- list()

for (site in wells) {
  site_key <- paste0(site, "_DO")
  site_events <- BEGI_events[["DO_events"]][[site_key]]
  
  for (event_name in names(site_events)) {
    event_time <- site_events[[event_name]]$datetimeMT[1]
    start_time <- event_time - (60 * 60 * 48)
    
    window_vals <- EXOz.dtw[[site]] %>%
      filter(datetimeMT >= start_time & datetimeMT < event_time) %>%
      pull(DTW_m)
    
    event_var <- var(window_vals, na.rm = TRUE)
    
    dtw_event_stats_list[[length(dtw_event_stats_list) + 1]] <- data.frame(
      site = site,
      event_id = event_name,
      Eventdate = event_time,
      DTW_mean_2d = mean(window_vals, na.rm = TRUE),
      DTW_var_2d = event_var,
      DTW_sd_2d = sd(window_vals, na.rm = TRUE),
      DTW_cv_2d = cv(window_vals),
      DTW_relvar_2d = event_var / gwtruevar_well[[site]] # relative variance: this event's 2-day variance as a multiple of that well's own overall variance (>1 = more variable than usual for this well; <1 = calmer than usual for this well). 
    )
  }
}

DO_event_mv <- bind_rows(dtw_event_stats_list)

write_csv(DO_event_mv, "DTW_compiled/DO_mv_2days.csv")


#### Temp whole Well Mean/Var ####
wells <- c("SLOC", "SLOW", "VDOS", "VDOW")

tempmean_well <- sapply(wells, function(w) mean(EXOz.dtw[[w]][["Temp..C.mn"]], na.rm = TRUE))
tempvar_well <- sapply(wells, function(w) cv(EXOz.dtw[[w]][["Temp..C.mn"]]))
temptruevar_well <- sapply(wells, function(w) var(EXOz.dtw[[w]][["Temp..C.mn"]], na.rm = TRUE))
names(temptruevar_well) <- wells

tempmv_well <- data.frame(wells, tempmean_well, tempvar_well, temptruevar_well)
tempmv_well

#### export for use in other scripts
write.csv(tempmv_well, "DTW_compiled/tempmv_well.csv")

#### DO event water temp mean, variance, SD, and CV (2 days before each event) ####

temp_event_stats_list <- list()

for (site in wells) {
  site_key <- paste0(site, "_DO")
  site_events <- BEGI_events[["DO_events"]][[site_key]]
  
  for (event_name in names(site_events)) {
    event_time <- site_events[[event_name]]$datetimeMT[1]
    start_time <- event_time - (60 * 60 * 48)
    
    window_vals <- EXOz.dtw[[site]] %>%
      filter(datetimeMT >= start_time & datetimeMT < event_time) %>%
      pull(Temp..C.mn)
    
    event_var <- var(window_vals, na.rm = TRUE)
    
    temp_event_stats_list[[length(temp_event_stats_list) + 1]] <- data.frame(
      site = site,
      event_id = event_name,
      Eventdate = event_time,
      temp_mean_2d = mean(window_vals, na.rm = TRUE),
      temp_var_2d = event_var,
      temp_sd_2d = sd(window_vals, na.rm = TRUE),
      temp_cv_2d = cv(window_vals),
      temp_relvar_2d = event_var / temptruevar_well[[site]]
    )
  }
}

temp_event_mv <- bind_rows(temp_event_stats_list)

write_csv(temp_event_mv, "DTW_compiled/temp_mv_2days.csv")


#### fDOM whole Well Mean/Var ####
wells <- c("SLOC", "SLOW", "VDOS", "VDOW")

fDOMmean_well <- sapply(wells, function(w) mean(EXOz.dtw[[w]][["fDOM.QSU.mn.Tc"]], na.rm = TRUE))
fDOMvar_well <- sapply(wells, function(w) cv(EXOz.dtw[[w]][["fDOM.QSU.mn.Tc"]]))
fDOMtruevar_well <- sapply(wells, function(w) var(EXOz.dtw[[w]][["fDOM.QSU.mn.Tc"]], na.rm = TRUE))
names(fDOMtruevar_well) <- wells

fDOMmv_well <- data.frame(wells, fDOMmean_well, fDOMvar_well, fDOMtruevar_well)
fDOMmv_well

#### export for use in other scripts
write.csv(fDOMmv_well, "DTW_compiled/fDOMmv_well.csv")

#### DO event water fDOM mean, variance, SD, and CV (2 days before each event) ####

fDOM_event_stats_list <- list()

for (site in wells) {
  site_key <- paste0(site, "_DO")
  site_events <- BEGI_events[["DO_events"]][[site_key]]
  
  for (event_name in names(site_events)) {
    event_time <- site_events[[event_name]]$datetimeMT[1]
    start_time <- event_time - (60 * 60 * 48)
    
    window_vals <- EXOz.dtw[[site]] %>%
      filter(datetimeMT >= start_time & datetimeMT < event_time) %>%
      pull(fDOM.QSU.mn.Tc)
    
    event_var <- var(window_vals, na.rm = TRUE)
    
    fDOM_event_stats_list[[length(fDOM_event_stats_list) + 1]] <- data.frame(
      site = site,
      event_id = event_name,
      Eventdate = event_time,
      fDOM_mean_2d = mean(window_vals, na.rm = TRUE),
      fDOM_var_2d = event_var,
      fDOM_sd_2d = sd(window_vals, na.rm = TRUE),
      fDOM_cv_2d = cv(window_vals),
      fDOM_relvar_2d = event_var / fDOMtruevar_well[[site]]
    )
  }
}

fDOM_event_mv <- bind_rows(fDOM_event_stats_list)

write_csv(fDOM_event_mv, "DTW_compiled/fDOM_mv_2days.csv")


