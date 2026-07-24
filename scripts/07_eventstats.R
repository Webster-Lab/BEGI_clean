#### read me ####
# The purpose of this script is to calculate for each event delineated in 04_eventdelineation.R, 
# the mean and variance of depth to groundwater and temperature, as well as the rate of change
# of DO and the amount of fDOM lost for each DO event

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

#### Whole well mean/var ####
wells<-c("SLOC","SLOW","VDOS","VDOW")

gwmean_well<-c(mean(EXOz.dtw[["SLOC"]]$DTW_m, na.rm = TRUE),
               mean(EXOz.dtw[["SLOW"]]$DTW_m, na.rm = TRUE),
               mean(EXOz.dtw[["VDOS"]]$DTW_m, na.rm = TRUE),
               mean(EXOz.dtw[["VDOW"]]$DTW_m, na.rm = TRUE))

cv <- function (x){
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
}
gwvar_well<-c(cv(EXOz.dtw[["SLOC"]]$DTW_m),
              cv(EXOz.dtw[["SLOW"]]$DTW_m),
              cv(EXOz.dtw[["VDOS"]]$DTW_m),
              cv(EXOz.dtw[["VDOW"]]$DTW_m))

gwmv_well<-data.frame(wells,gwmean_well,gwvar_well)
gwmv_well 

#### export for use in other scripts ####
write.csv(gwmv_well, "DTW_compiled/gwmv_well.csv")

####import list of event dates per well ####
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")

#Turns lists into vectors
SLOC_dates <- c(BEGI_events[["Eventdate"]][["SLOC_dates"]])
SLOW_dates <- c(BEGI_events[["Eventdate"]][["SLOW_dates"]])
VDOS_dates <- c(BEGI_events[["Eventdate"]][["VDOS_dates"]])
VDOW_dates <- c(BEGI_events[["Eventdate"]][["VDOW_dates"]])

#Turn lists into vectors for fDOM event dates
#SLOC#
SLOC_eventdate <- POSIXct()
for (i in seq_along(BEGI_events[["fDOM_events"]][["SLOC_fDOM"]])) {
  SLOC_eventdate[i] <- BEGI_events[["fDOM_events"]][["SLOC_fDOM"]][[i]]$datetimeMT[1]
}

#SLOW#
SLOW_eventdate <- POSIXct()
for (i in seq_along(BEGI_events[["fDOM_events"]][["SLOW_fDOM"]])) {
  SLOW_eventdate[i] <- BEGI_events[["fDOM_events"]][["SLOW_fDOM"]][[i]]$datetimeMT[1]
}

#VDOW#
VDOW_eventdate <- POSIXct()
for (i in seq_along(BEGI_events[["fDOM_events"]][["VDOW_fDOM"]])) {
  VDOW_eventdate[i] <- BEGI_events[["fDOM_events"]][["VDOW_fDOM"]][[i]]$datetimeMT[1]
}

#VDOS#
VDOS_eventdate <- POSIXct()
for (i in seq_along(BEGI_events[["fDOM_events"]][["VDOS_fDOM"]])) {
  VDOS_eventdate[i] <- BEGI_events[["fDOM_events"]][["VDOS_fDOM"]][[i]]$datetimeMT[1]
}

#### DO event dtw mean (2 days) ####
#SLOC
#mean calculated 2 days before each event
SLOC_event_mean<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["SLOC_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOC_mean <- EXOz.dtw[["SLOC"]] %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOC_mean = mean(DTW_m, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to SLOC_event_mean
  SLOC_event_mean <- c(SLOC_event_mean,SLOC_mean)
}

SLOC_event_mean = unlist(SLOC_event_mean,use.names = F)

#SLOW
#mean calculated 2 days before each event
SLOW_event_mean<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["SLOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOW_mean <- EXOz.dtw[["SLOW"]] %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOW_mean = mean(DTW_m, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to SLOW_event_mean
  SLOW_event_mean <- c(SLOW_event_mean,SLOW_mean)
}

SLOW_event_mean = unlist(SLOW_event_mean,use.names = F)


#VDOS
#mean calculated 2 days before each event
VDOS_event_mean<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOS_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOS_mean <- EXOz.dtw[["VDOS"]] %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOS_mean = mean(DTW_m, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to VDOS_event_mean
  VDOS_event_mean <- c(VDOS_event_mean,VDOS_mean)
}

VDOS_event_mean = unlist(VDOS_event_mean,use.names = F)


#VDOW
#mean calculated 2 days before each event
VDOW_event_mean<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOW_mean <- EXOz.dtw[["VDOW"]] %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOW_mean = mean(DTW_m, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to VDOW_event_mean
  VDOW_event_mean <- c(VDOW_event_mean,VDOW_mean)
}

VDOW_event_mean = unlist(VDOW_event_mean,use.names = F)


#### fDOM event dtw mean (2 days) ####
#SLOC
#mean calculated 2 days before each event
SLOC_fDOM_mean<-numeric()

for (i in c(1:length(BEGI_events[["fDOM_events"]][["SLOC_fDOM"]]))){
  temptimes = seq(from=BEGI_events[["fDOM_events"]][["SLOC_fDOM"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["fDOM_events"]][["SLOC_fDOM"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOC_mean <- EXOz.dtw[["SLOC"]] %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOC_mean = mean(DTW_m, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to SLOC_event_mean
  SLOC_fDOM_mean <- c(SLOC_fDOM_mean,SLOC_mean)
}

SLOC_fDOM_mean = unlist(SLOC_fDOM_mean,use.names = F)

#SLOW
#mean calculated 2 days before each event
SLOW_fDOM_mean<-numeric()

for (i in c(1:length(BEGI_events[["fDOM_events"]][["SLOW_fDOM"]]))){
  temptimes = seq(from=BEGI_events[["fDOM_events"]][["SLOW_fDOM"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["fDOM_events"]][["SLOW_fDOM"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOW_mean <- EXOz.dtw[["SLOW"]] %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOW_mean = mean(DTW_m, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to SLOW_event_mean
  SLOW_fDOM_mean <- c(SLOW_fDOM_mean,SLOW_mean)
}

SLOW_fDOM_mean = unlist(SLOW_fDOM_mean,use.names = F)


#VDOW
#mean calculated 2 days before each event
VDOW_fDOM_mean<-numeric()

for (i in c(1:length(BEGI_events[["fDOM_events"]][["VDOW_fDOM"]]))){
  temptimes = seq(from=BEGI_events[["fDOM_events"]][["VDOW_fDOM"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["fDOM_events"]][["VDOW_fDOM"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOW_mean <- EXOz.dtw[["VDOW"]] %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOW_mean = mean(DTW_m, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to VDOW_event_mean
  VDOW_fDOM_mean <- c(VDOW_fDOM_mean,VDOW_mean)
}

VDOW_fDOM_mean = unlist(VDOW_fDOM_mean,use.names = F)


#VDOS
#mean calculated 2 days before each event
VDOS_fDOM_mean<-numeric()

for (i in c(1:length(BEGI_events[["fDOM_events"]][["VDOS_fDOM"]]))){
  temptimes = seq(from=BEGI_events[["fDOM_events"]][["VDOS_fDOM"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["fDOM_events"]][["VDOS_fDOM"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOS_mean <- EXOz.dtw[["VDOS"]] %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOS_mean = mean(DTW_m, na.rm = TRUE))
  #mean of DTW_m over period of temptimes
  #add mean to VDOS_event_mean
  VDOS_fDOM_mean <- c(VDOS_fDOM_mean,VDOS_mean)
}

VDOS_fDOM_mean = unlist(VDOS_fDOM_mean,use.names = F)


#### DO event dtw CV (2 days) ####
# note from AJW: I need the events labeled by date (or something more sophisticated if they span multiple dates) so that I can match them to the AUC etc. results for modeling!

#SLOC
#CV calculated 2 days before each event
SLOC_event_cv<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["SLOC_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOC_cv <- EXOz.dtw[["SLOC"]] %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOC_cv = cv(DTW_m))
  #cv of DTW_m over period of temptimes
  #add cv to SLOC_event_cv
  SLOC_event_cv <- c(SLOC_event_cv,SLOC_cv)
}

SLOC_event_cv = unlist(SLOC_event_cv,use.names = F)


#SLOW
#CV calculated 2 days before each event
SLOW_event_cv<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["SLOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOW_cv <- EXOz.dtw[["SLOW"]] %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOW_cv = cv(DTW_m))
  #cv of DTW_m over period of temptimes
  #add cv to SLOW_event_mean
  SLOW_event_cv <- c(SLOW_event_cv,SLOW_cv)
}

SLOW_event_cv = unlist(SLOW_event_cv,use.names = F)


#VDOS
#CV calculated 2 days before each event
VDOS_event_cv<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOS_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOS_cv <- EXOz.dtw[["VDOS"]] %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOS_cv = cv(DTW_m))
  #cv of DTW_m over period of temptimes
  #add cv to VDOS_event_mean
  VDOS_event_cv <- c(VDOS_event_cv,VDOS_cv)
}

VDOS_event_cv = unlist(VDOS_event_cv,use.names = F)


#VDOW
#CV calculated 2 days before each event
VDOW_event_cv<-numeric()

for (i in c(1:length(BEGI_events[["DO_events"]][["VDOW_DO"]]))){
  temptimes = seq(from=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOW_cv <- EXOz.dtw[["VDOW"]] %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOW_cv = cv(DTW_m))
  #cv of DTW_m over period of temptimes
  #add cv to VDOW_event_mean
  VDOW_event_cv <- c(VDOW_event_cv,VDOW_cv)
}

VDOW_event_cv = unlist(VDOW_event_cv,use.names = F)


#### fDOM event dtw CV (2days) ####
#SLOC
#CV calculated 2 days before each event
SLOC_fDOM_cv<-numeric()

for (i in c(1:length(BEGI_events[["fDOM_events"]][["SLOC_fDOM"]]))){
  temptimes = seq(from=BEGI_events[["fDOM_events"]][["SLOC_fDOM"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["fDOM_events"]][["SLOC_fDOM"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOC_cv <- EXOz.dtw[["SLOC"]] %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOC_cv = cv(DTW_m))
  #cv of DTW_m over period of temptimes
  #add cv to SLOC_event_cv
  SLOC_fDOM_cv <- c(SLOC_fDOM_cv,SLOC_cv)
}

SLOC_fDOM_cv = unlist(SLOC_fDOM_cv,use.names = F)

#SLOW
#CV calculated 2 days before each event
SLOW_fDOM_cv<-numeric()

for (i in c(1:length(BEGI_events[["fDOM_events"]][["SLOW_fDOM"]]))){
  temptimes = seq(from=BEGI_events[["fDOM_events"]][["SLOW_fDOM"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["fDOM_events"]][["SLOW_fDOM"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  SLOW_cv <- EXOz.dtw[["SLOW"]] %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(SLOW_cv = cv(DTW_m))
  #cv of DTW_m over period of temptimes
  #add cv to SLOW_event_cv
  SLOW_fDOM_cv <- c(SLOW_fDOM_cv,SLOW_cv)
}

SLOW_fDOM_cv = unlist(SLOW_fDOM_cv,use.names = F)

#VDOW
#CV calculated 2 days before each event
VDOW_fDOM_cv<-numeric()

for (i in c(1:length(BEGI_events[["fDOM_events"]][["VDOW_fDOM"]]))){
  temptimes = seq(from=BEGI_events[["fDOM_events"]][["VDOW_fDOM"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["fDOM_events"]][["VDOW_fDOM"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOW_cv <- EXOz.dtw[["VDOW"]] %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOW_cv = cv(DTW_m))
  #cv of DTW_m over period of temptimes
  #add cv to VDOW_event_cv
  VDOW_fDOM_cv <- c(VDOW_fDOM_cv,VDOW_cv)
}

VDOW_fDOM_cv = unlist(VDOW_fDOM_cv,use.names = F)

#VDOS
#CV calculated 2 days before each event
VDOS_fDOM_cv<-numeric()

for (i in c(1:length(BEGI_events[["fDOM_events"]][["VDOS_fDOM"]]))){
  temptimes = seq(from=BEGI_events[["fDOM_events"]][["VDOS_fDOM"]][[i]]$datetimeMT[1]-(60*60*48),
                  to=BEGI_events[["fDOM_events"]][["VDOS_fDOM"]][[i]]$datetimeMT[1], 
                  by = '15 mins')
  VDOS_cv <- EXOz.dtw[["VDOS"]] %>%
    filter(between(datetimeMT,temptimes[1],temptimes[length(temptimes)])) %>%
    summarise(VDOS_cv = cv(DTW_m))
  #cv of DTW_m over period of temptimes
  #add cv to VDOS_event_cv
  VDOS_fDOM_cv <- c(VDOS_fDOM_cv,VDOS_cv)
}

VDOS_fDOM_cv = unlist(VDOS_fDOM_cv,use.names = F)


#### Dataframe of event mean/var for 2 days ####
#BEGI_events[["Eventdate"]][["SLOC_DO"]]
DO_event_mean <- c(SLOC_event_mean,SLOW_event_mean,VDOS_event_mean,VDOW_event_mean)
DO_event_cv <- c(SLOC_event_cv,SLOW_event_cv,VDOS_event_cv,VDOW_event_cv)
Eventdates<-c(SLOC_dates,SLOW_dates,VDOS_dates,VDOW_dates)
WellID<-c(rep(c("SLOC","SLOW","VDOS","VDOW"),
              times=c(length(SLOC_event_mean),length(SLOW_event_mean),length(VDOS_event_mean),length(VDOW_event_mean))))
DO_event_mv <- data.frame(WellID,Eventdates,DO_event_mean,DO_event_cv)
View(DO_event_mv)

write_csv(DO_event_mv,"DTW_compiled/DO_mv_2days.csv")

#fdom event mean/var for 2 days
fdom_event_mean <-c(SLOC_fDOM_mean,SLOW_fDOM_mean,VDOW_fDOM_mean,VDOS_fDOM_mean)
fdom_event_cv <-c(SLOC_fDOM_cv,SLOW_fDOM_cv,VDOW_fDOM_cv,VDOS_fDOM_cv)
Eventdates<-c(SLOC_eventdate,SLOW_eventdate,VDOW_eventdate,VDOS_eventdate)
WellID<-c(rep(c("SLOC","SLOW","VDOW","VDOS"),
              times=c(length(SLOC_fDOM_mean),length(SLOW_fDOM_mean),length(VDOW_fDOM_mean),length(VDOS_fDOM_mean))))
fdom_event_mv <- data.frame(WellID,Eventdates,fdom_event_mean,fdom_event_cv)
write_csv(fdom_event_mv,"DTW_compiled/fdom_mv_2days.csv")

