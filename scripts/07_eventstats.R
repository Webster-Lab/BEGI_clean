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
wells<-c("SLOC","SLOW","VDOS","VDOW")

tempmean_well<-c(mean(EXOz.dtw[["SLOC"]][["Temp..C.mn"]], na.rm = TRUE),
                 mean(EXOz.dtw[["SLOW"]][["Temp..C.mn"]], na.rm = TRUE),
                 mean(EXOz.dtw[["VDOS"]][["Temp..C.mn"]], na.rm = TRUE),
                 mean(EXOz.dtw[["VDOW"]][["Temp..C.mn"]], na.rm = TRUE))

cv <- function (x){
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
}
tempvar_well<-c(cv(EXOz.dtw[["SLOC"]][["Temp..C.mn"]]),
                cv(EXOz.dtw[["SLOW"]][["Temp..C.mn"]]),
                cv(EXOz.dtw[["VDOS"]][["Temp..C.mn"]]),
                cv(EXOz.dtw[["VDOW"]][["Temp..C.mn"]]))

tempmv_well<-data.frame(wells,tempmean_well,tempvar_well)
tempmv_well 

#### export for use in other scripts ####
write.csv(tempmv_well, "DTW_compiled/tempmv_well.csv")

#### DO event water temp mean (2 days) ####
# note from AJW: I need the events labeled by date (or something more sophisticated if they span multiple dates) so that I can match them to the AUC etc. results for modeling!

#SLOC
#mean calculated 2 days before each event
SLOC_event_mean<-numeric()

for (i in seq_along(BEGI_events[["DO_events"]][["SLOC_DO"]])) {
  event_time <- BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1]
  start_time <- event_time - (60 * 60 * 48)
  end_time <- event_time
  #specify 2 days preceding event
  SLOC_mean <- EXOz.dtw[["SLOC"]] %>%
    filter(datetimeMT >= start_time & datetimeMT < end_time) %>%
    summarise(mean_temp = mean(Temp..C.mn, na.rm = TRUE)) %>%
    pull(mean_temp)
  #filter temp data to 2 days preceding event, calc mean
  SLOC_event_mean <- c(SLOC_event_mean, SLOC_mean)
}


#SLOW
#mean calculated 2 days before each event
SLOW_event_mean<-numeric()

for (i in seq_along(BEGI_events[["DO_events"]][["SLOW_DO"]])) {
  event_time <- BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1]
  start_time <- event_time - (60 * 60 * 48)
  end_time <- event_time
  #specify 2 days preceding event
  SLOW_mean <- EXOz.dtw[["SLOW"]] %>%
    filter(datetimeMT >= start_time & datetimeMT < end_time) %>%
    summarise(mean_temp = mean(Temp..C.mn, na.rm = TRUE)) %>%
    pull(mean_temp)
  #filter temp data to 2 days preceding event, calc mean
  SLOW_event_mean <- c(SLOW_event_mean, SLOW_mean)
}


#VDOW
#mean calculated 2 days before each event
VDOW_event_mean<-numeric()

for (i in seq_along(BEGI_events[["DO_events"]][["VDOW_DO"]])) {
  event_time <- BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1]
  start_time <- event_time - (60 * 60 * 48)
  end_time <- event_time
  #specify 2 days preceding event
  VDOW_mean <- EXOz.dtw[["VDOW"]] %>%
    filter(datetimeMT >= start_time & datetimeMT < end_time) %>%
    summarise(mean_temp = mean(Temp..C.mn, na.rm = TRUE)) %>%
    pull(mean_temp)
  #filter temp data to 2 days preceding event, calc mean
  VDOW_event_mean <- c(VDOW_event_mean, VDOW_mean)
}


#VDOS
#mean calculated 2 days before each event
VDOS_event_mean<-numeric()

for (i in seq_along(BEGI_events[["DO_events"]][["VDOS_DO"]])) {
  event_time <- BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1]
  start_time <- event_time - (60 * 60 * 48)
  end_time <- event_time
  #specify 2 days preceding event
  VDOS_mean <- EXOz.dtw[["VDOS"]] %>%
    filter(datetimeMT >= start_time & datetimeMT < end_time) %>%
    summarise(mean_temp = mean(Temp..C.mn, na.rm = TRUE)) %>%
    pull(mean_temp)
  #filter temp data to 2 days preceding event, calc mean
  VDOS_event_mean <- c(VDOS_event_mean, VDOS_mean)
}

#### DO event water temp CV (2 days) ####
# note from AJW: I need the events labeled by date (or something more sophisticated if they span multiple dates) so that I can match them to the AUC etc. results for modeling!

#SLOC
#CV calculated 2 days before each event
SLOC_event_cv<-numeric()

for (i in seq_along(BEGI_events[["DO_events"]][["SLOC_DO"]])) {
  event_time <- BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1]
  start_time <- event_time - (60 * 60 * 48)
  end_time <- event_time
  #specify 2 days preceding event
  SLOC_cv <- EXOz.dtw[["SLOC"]] %>%
    filter(datetimeMT >= start_time & datetimeMT < end_time) %>%
    summarise(cv_temp = cv(Temp..C.mn)) %>%
    pull(cv_temp)
  #filter temp data to 2 days preceding event, calc cv
  SLOC_event_cv <- c(SLOC_event_cv, SLOC_cv)
}


#SLOW
SLOW_event_cv<-numeric()

for (i in seq_along(BEGI_events[["DO_events"]][["SLOW_DO"]])) {
  event_time <- BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1]
  start_time <- event_time - (60 * 60 * 48)
  end_time <- event_time
  #specify 2 days preceding event
  SLOW_cv <- EXOz.dtw[["SLOW"]] %>%
    filter(datetimeMT >= start_time & datetimeMT < end_time) %>%
    summarise(cv_temp = cv(Temp..C.mn)) %>%
    pull(cv_temp)
  #filter temp data to 2 days preceding event, calc cv
  SLOW_event_cv <- c(SLOW_event_cv, SLOW_cv)
}


#VDOW
VDOW_event_cv<-numeric()

for (i in seq_along(BEGI_events[["DO_events"]][["VDOW_DO"]])) {
  event_time <- BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1]
  start_time <- event_time - (60 * 60 * 48)
  end_time <- event_time
  #specify 2 days preceding event
  VDOW_cv <- EXOz.dtw[["VDOW"]] %>%
    filter(datetimeMT >= start_time & datetimeMT < end_time) %>%
    summarise(cv_temp = cv(Temp..C.mn)) %>%
    pull(cv_temp)
  #filter temp data to 2 days preceding event, calc cv
  VDOW_event_cv <- c(VDOW_event_cv, VDOW_cv)
}


#VDOS
VDOS_event_cv<-numeric()

for (i in seq_along(BEGI_events[["DO_events"]][["VDOS_DO"]])) {
  event_time <- BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1]
  start_time <- event_time - (60 * 60 * 48)
  end_time <- event_time
  #specify 2 days preceding event
  VDOS_cv <- EXOz.dtw[["VDOS"]] %>%
    filter(datetimeMT >= start_time & datetimeMT < end_time) %>%
    summarise(cv_temp = cv(Temp..C.mn)) %>%
    pull(cv_temp)
  #filter temp data to 2 days preceding event, calc cv
  VDOS_event_cv <- c(VDOS_event_cv, VDOS_cv)
}

#### Dataframe of event mean/var for 2 days ####
#BEGI_events[["Eventdate"]][["SLOC_DO"]]
temp_event_mean <- c(SLOC_event_mean,SLOW_event_mean,VDOS_event_mean,VDOW_event_mean)
temp_event_cv <- c(SLOC_event_cv,SLOW_event_cv,VDOS_event_cv,VDOW_event_cv)
Eventdates<-c(SLOC_dates,SLOW_dates,VDOS_dates,VDOW_dates)
WellID<-c(rep(c("SLOC","SLOW","VDOS","VDOW"),
              times=c(length(SLOC_event_mean),length(SLOW_event_mean),length(VDOS_event_mean),length(VDOW_event_mean))))
temp_event_mv <- data.frame(WellID,Eventdates,temp_event_mean,temp_event_cv)
View(temp_event_mv)

write.csv(temp_event_mv %>%
            mutate(Eventdates = format(Eventdates, "%Y-%m-%d %H:%M:%S")),
          "DTW_compiled/temp_mv_2days.csv", row.names = FALSE)
#could re-do to test effects of different days, but 2 days proves to make the most sense considering interference of close events/diel variation
