#### READ ME ####

# The purpose of this script is to delineate accrual + respiration events from the compiled EXO1 RDS files in the Webster BEGI project

# Requirements: rds files from previous scripts:
# 1. BEGI_EXOz_dtw.rds
# 2. service times for each well

# Outputs for downstream use:
# 1. lists of all DO and fDOM events in each well: BEGI_events.rds
# 2. "megaplots" of each event showing all sonde data + dtw data for each eventS

#### Libraries ####
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)
library(DescTools)
library(readxl)
library(ggplot2)
library(patchwork)
library(scales)

#### Check/make file structure ####

# make sure output folders exist before anything tries to write to them
dir.create("EXO_compiled", recursive = TRUE, showWarnings = FALSE)

siteIDz <- c("VDOW", "VDOS", "SLOW", "SLOC")
for (i in siteIDz) {
  dir.create(paste0("plots/delineations/", i, "/events"), recursive = TRUE, showWarnings = FALSE)
}

#### Import compiled EXO1 RDS file ####
EXOz.dtw = readRDS("DTW_compiled/BEGI_EXOz_dtw.rds")

#### Get sunrise/sunset times ####

suntimes = getSunlightTimes(date = seq.Date(
  from = as.Date("2023-09-14"), 
  to = as.Date("2024-09-5"), by = 1),
  keep = c("sunrise", "sunset"),
  lat = 34.9, lon = -106.7, tz = "US/Mountain")
pm.pts = suntimes$sunset[-(nrow(suntimes))]
am.pts = suntimes$sunrise[-1]

#Define shaded am/pm
shade_df <- data.frame(
  xmin = pm.pts,
  xmax = am.pts,
  ymin = -Inf,
  ymax = Inf
)




#### Read in .csv files of service dates and times ####

service.VDOW = read.csv("EXO_compiled/service.VDOW.csv", row.names = 1)
names(service.VDOW) = "datetimeMT"
service.VDOW$datetimeMT = as.POSIXct(service.VDOW$datetimeMT, tz="US/Mountain")

service.VDOS = read.csv("EXO_compiled/service.VDOS.csv", row.names = 1)
names(service.VDOS) = "datetimeMT"
service.VDOS$datetimeMT = as.POSIXct(service.VDOS$datetimeMT, tz="US/Mountain")

service.SLOC = read.csv("EXO_compiled/service.SLOC.csv", row.names = 1)
names(service.SLOC) = "datetimeMT"
service.SLOC$datetimeMT = as.POSIXct(service.SLOC$datetimeMT, tz="US/Mountain")

service.SLOW = read.csv("EXO_compiled/service.SLOW.csv", row.names = 1)
names(service.SLOW) = "datetimeMT"
service.SLOW$datetimeMT = as.POSIXct(service.SLOW$datetimeMT, tz="US/Mountain")

#### Make vector of dates ####

date <- (
  datetimeMT = seq.POSIXt(
    from = ISOdatetime(2023,09,15,0,0,0, tz = "US/Mountain"),
    to = ISOdatetime(2024,09,04,0,0,0, tz= "US/Mountain"),
    by = "24 h" ))

date = as.Date(date)

#### Plot 24 hr periods for visual inspection ####
# NOTE: the code below generates a plot for DO and fDOM for every 24 hours so that events can be identified by visual inspection. It is not necessary to run this code to reproduce the rest of the analysis. 

## SLOC 24 h ##
for (i in c(1:length(date))) {
  dz = date[i]
  day_start <- as.POSIXct(dz, tz = "US/Mountain") + 1
  tempdat <- EXOz.dtw[["SLOC"]][
    EXOz.dtw[["SLOC"]]$datetimeMT > day_start &
      EXOz.dtw[["SLOC"]]$datetimeMT < day_start + 60*60*24,
  ]
  
  #save plot 
  file_name = paste("plots/delineations/SLOC/SLOC_", dz, ".pdf", sep="")
  pdf(file_name)
  
  par(mfrow=c(2,1))
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,10))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
  abline(v=as.POSIXct(service.SLOC$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  title(main="Dissolved Oxygen (mg/L)")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,80))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
  abline(v=as.POSIXct(service.SLOC$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  title(main="fDOM (QSU)")
  
  dev.off()
}

## SLOW 24 h ##
for (i in c(1:length(date))) {
  dz = date[i]
  day_start <- as.POSIXct(dz, tz = "US/Mountain") + 1
  tempdat <- EXOz.dtw[["SLOW"]][
    EXOz.dtw[["SLOW"]]$datetimeMT > day_start &
      EXOz.dtw[["SLOW"]]$datetimeMT < day_start + 60*60*24,
  ]
  
  #save plot 
  file_name = paste("plots/delineations/SLOW/SLOW_", dz, ".pdf", sep="")
  pdf(file_name)
  
  par(mfrow=c(2,1))
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,10))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
  abline(v=as.POSIXct(service.SLOW$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  title(main="Dissolved Oxygen (mg/L)")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,80))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
  abline(v=as.POSIXct(service.SLOW$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  title(main="fDOM (QSU)")
  
  dev.off()
}

## VDOW 24 h ##
for (i in c(1:length(date))) {
  dz = date[i]
  day_start <- as.POSIXct(dz, tz = "US/Mountain") + 1
  tempdat <- EXOz.dtw[["VDOW"]][
    EXOz.dtw[["VDOW"]]$datetimeMT > day_start &
      EXOz.dtw[["VDOW"]]$datetimeMT < day_start + 60*60*24,
  ]
  
  #save plot 
  file_name = paste("plots/delineations/VDOW/VDOW_", dz, ".pdf", sep="")
  pdf(file_name)
  
  par(mfrow=c(2,1))
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,10))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
  abline(v=as.POSIXct(service.VDOW$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  title(main="Dissolved Oxygen (mg/L)")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,100))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
  abline(v=as.POSIXct(service.VDOW$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  title(main="fDOM (QSU)")
  
  dev.off()
}

## VDOS 24 h ##
for (i in c(1:length(date))) {
  dz = date[i]
  day_start <- as.POSIXct(dz, tz = "US/Mountain") + 1
  tempdat <- EXOz.dtw[["VDOS"]][
    EXOz.dtw[["VDOS"]]$datetimeMT > day_start &
      EXOz.dtw[["VDOS"]]$datetimeMT < day_start + 60*60*24,
  ]
  
  #save plot 
  file_name = paste("plots/delineations/VDOS/VDOS_", dz, ".pdf", sep="")
  pdf(file_name)
  
  par(mfrow=c(2,1))
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,10))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
  abline(v=as.POSIXct(service.VDOS$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  title(main="Dissolved Oxygen (mg/L)")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,140))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
  abline(v=as.POSIXct(service.VDOS$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  title(main="fDOM (QSU)")
  
  dev.off()
}

#### Identify event dates ####

SLOC_events <- c("2023-10-09","2023-10-10","2023-10-11","2023-10-16","2023-10-17","2023-10-18","2023-11-18","2023-11-21","2023-11-26","2023-12-06","2023-12-18","2023-12-23","2023-12-24","2023-12-25","2023-12-29","2024-01-11","2024-01-12","2024-01-19","2024-01-21","2024-01-22","2024-01-23","2024-01-24","2024-02-10","2024-02-21","2024-03-09","2024-03-17","2024-03-20","2024-04-01","2024-04-02","2024-04-14","2024-04-15","2024-05-23","2024-07-04","2024-07-05","2024-07-13","2024-07-17","2024-08-19","2024-08-26","2024-08-27","2024-08-29","2024-08-30")
SLOC_events <- as.Date(SLOC_events)

SLOW_events <- c("2023-09-20","2023-10-16","2023-11-21","2023-12-18","2023-12-25","2023-12-26","2023-12-27","2023-12-28","2023-12-29","2024-01-19","2024-02-10","2024-02-11","2024-02-21","2024-03-20","2024-04-24","2024-04-25","2024-04-26","2024-04-27","2024-04-28","2024-05-14","2024-06-30","2024-07-13","2024-07-17","2024-07-22","2024-08-19")
SLOW_events <- as.Date(SLOW_events)

VDOS_events <- c("2023-09-21","2023-09-22","2023-10-10","2023-10-16","2023-10-25","2023-10-26","2023-10-28","2023-10-29","2023-11-03","2023-11-04","2023-11-04","2023-11-11","2023-11-12","2023-11-13","2023-11-14","2023-11-15","2023-11-16","2023-11-17","2023-11-25","2023-11-27","2023-11-29","2023-11-30","2024-02-21","2024-04-17","2024-05-23","2024-06-12","2024-07-10","2024-07-11","2024-07-12","2024-07-13","2024-07-14","2024-07-16","2024-07-17","2024-07-18","2024-08-07","2024-08-08","2024-08-09","2024-08-10","2024-08-11","2024-08-12","2024-08-19")
VDOS_events <- as.Date(VDOS_events)

VDOW_events <- c("2023-09-17","2023-09-18","2023-09-19","2023-09-30","2023-10-01","2023-10-02","2023-10-03","2023-10-04","2023-10-05","2023-10-06","2023-10-07","2023-10-11","2023-10-16","2023-10-17","2023-10-17","2023-11-02","2023-11-03","2023-11-04","2023-11-05","2023-11-06","2023-11-18","2023-11-19","2023-11-20","2023-11-21","2023-11-22","2023-11-27","2023-11-28","2023-11-29","2023-12-04","2023-12-05","2023-12-18","2023-12-19","2024-02-21","2024-02-22","2024-02-23","2024-02-24","2024-02-25","2024-02-26","2024-02-27","2024-02-28","2024-02-29","2024-02-21","2024-03-06","2024-03-07","2024-03-08","2024-03-09","2024-03-10","2024-03-20","2024-03-21","2024-03-22","2024-04-17","2024-05-14","2024-05-15","2024-05-23","2024-05-24","2024-06-17","2024-06-18","2024-06-19","2024-07-17","2024-07-18","2024-08-19","2024-08-20")
VDOW_events <- as.Date(VDOW_events)

#### Plot just the events for visual inspection ####
# NOTE: the code below generates a plot for DO and fDOM for every suspected event for visual inspection. It is not necessary to run this code to reproduce the rest of the analysis. 


#SLOC#
for (i in c(1:length(SLOC_events))) {
  dz = SLOC_events[i]
  day_start <- as.POSIXct(dz, tz = "US/Mountain") + 1
  tempdat <- EXOz.dtw[["SLOC"]][
    EXOz.dtw[["SLOC"]]$datetimeMT > day_start &
      EXOz.dtw[["SLOC"]]$datetimeMT < day_start + 60*60*24,
  ]
  
  #save plot 
  file_name = paste("plots/delineations/SLOC/events/SLOC_", dz, ".pdf", sep="")
  pdf(file_name)
  
  par(mfrow=c(2,1))
  
  # Define the range of datetime values
  start_time <- min(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"), na.rm = TRUE)
  end_time <- max(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"), na.rm = TRUE)
  
  # Create a sequence of 15-minute intervals
  intervals_15min <- seq(from = start_time, to = end_time, by = "15 min")
  
  # Create a sequence of hourly intervals
  hour_intervals <- seq(from = start_time, to = end_time, by = "1 hour")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,2.5))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
  abline(v = intervals_15min, col = "blue")
  abline(v=as.POSIXct(service.SLOC$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="Dissolved Oxygen (mg/L)")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,140))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
  abline(v = intervals_15min, col = "blue")
  abline(v=as.POSIXct(service.SLOC$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="fDOM (QSU)")
  
  dev.off()
}

#SLOW#
for (i in c(1:length(SLOW_events))) {
  dz = SLOW_events[i]
  day_start <- as.POSIXct(dz, tz = "US/Mountain") + 1
  tempdat <- EXOz.dtw[["SLOW"]][
    EXOz.dtw[["SLOW"]]$datetimeMT > day_start &
      EXOz.dtw[["SLOW"]]$datetimeMT < day_start + 60*60*24,
  ]
  
  #save plot 
  file_name = paste("plots/delineations/SLOW/events/SLOW_", dz, ".pdf", sep="")
  pdf(file_name)
  
  par(mfrow=c(2,1))
  
  # Define the range of datetime values
  start_time <- min(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"), na.rm = TRUE)
  end_time <- max(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"), na.rm = TRUE)
  
  # Create a sequence of 15-minute intervals
  intervals_15min <- seq(from = start_time, to = end_time, by = "15 min")
  
  # Create a sequence of hourly intervals
  hour_intervals <- seq(from = start_time, to = end_time, by = "1 hour")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,2.5))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
  abline(v = intervals_15min, col = "blue")
  abline(v=as.POSIXct(service.SLOW$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="Dissolved Oxygen (mg/L)")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,140))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
  abline(v = intervals_15min, col = "blue")
  abline(v=as.POSIXct(service.SLOW$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="fDOM (QSU)")
  
  dev.off()
}

#VDOW#
for (i in c(1:length(VDOW_events))) {
  dz = VDOW_events[i]
  day_start <- as.POSIXct(dz, tz = "US/Mountain") + 1
  tempdat <- EXOz.dtw[["VDOW"]][
    EXOz.dtw[["VDOW"]]$datetimeMT > day_start &
      EXOz.dtw[["VDOW"]]$datetimeMT < day_start + 60*60*24,
  ]
  
  #save plot 
  file_name = paste("plots/delineations/VDOW/events/VDOW_", dz, ".pdf", sep="")
  pdf(file_name)
  
  par(mfrow=c(2,1))
  
  # Define the range of datetime values
  start_time <- min(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"), na.rm = TRUE)
  end_time <- max(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"), na.rm = TRUE)
  
  # Create a sequence of 15-minute intervals
  intervals_15min <- seq(from = start_time, to = end_time, by = "15 min")
  
  # Create a sequence of hourly intervals
  hour_intervals <- seq(from = start_time, to = end_time, by = "1 hour")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,2.5))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
  abline(v = intervals_15min, col = "blue")
  abline(v=as.POSIXct(service.VDOW$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="Dissolved Oxygen (mg/L)")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,140))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
  abline(v = intervals_15min, col = "blue")
  abline(v=as.POSIXct(service.VDOW$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="fDOM (QSU)")
  
  dev.off()
}

#VDOS#
for (i in c(1:length(VDOS_events))) {
  dz = VDOS_events[i]
  day_start <- as.POSIXct(dz, tz = "US/Mountain") + 1
  tempdat <- EXOz.dtw[["VDOS"]][
    EXOz.dtw[["VDOS"]]$datetimeMT > day_start &
      EXOz.dtw[["VDOS"]]$datetimeMT < day_start + 60*60*24,
  ]
  
  #save plot 
  file_name = paste("plots/delineations/VDOS/events/VDOS_", dz, ".pdf", sep="")
  pdf(file_name)
  
  par(mfrow=c(2,1))
  
  # Define the range of datetime values
  start_time <- min(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"), na.rm = TRUE)
  end_time <- max(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"), na.rm = TRUE)
  
  # Create a sequence of 15-minute intervals
  intervals_15min <- seq(from = start_time, to = end_time, by = "15 min")
  
  # Create a sequence of hourly intervals
  hour_intervals <- seq(from = start_time, to = end_time, by = "1 hour")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="",ylim=c(-0.2,2.5))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(-0.2,10)
  abline(v = intervals_15min, col = "blue")
  abline(v=as.POSIXct(service.VDOS$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="Dissolved Oxygen (mg/L)")
  
  plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
       pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n",ylim=c(22.5,140))
  rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
  lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
        pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5)
  abline(v = intervals_15min, col = "blue")
  abline(v=as.POSIXct(service.VDOS$datetimeMT), col="red")
  axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
  axis.POSIXct(side = 1, at = hour_intervals, format = "%H:%M", las = 2)
  title(main="fDOM (QSU)")
  
  dev.off()
}



#### Create dataframes of just the DO events ####

#SLOC#

#1st event
SLOC_DO1= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2023-10-08 22:45:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2023-10-09 02:30:00",tz= "US/Mountain"),]

#2nd event
SLOC_DO2= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2023-10-09 22:00:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2023-10-10 06:30:00",tz= "US/Mountain"),]

#3rd event
SLOC_DO3= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2023-10-10 22:15:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2023-10-11 05:45:00",tz= "US/Mountain"),]

#4th event
SLOC_DO4= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2023-11-17 19:15:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2023-11-17 22:15:00",tz= "US/Mountain"),]

#5th event
SLOC_DO5= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2023-11-21 16:15:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2023-11-21 17:00:00",tz= "US/Mountain"),]

#6th event
SLOC_DO6= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2023-12-05 17:15:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2023-12-05 18:00:00",tz= "US/Mountain"),]

#7th event
SLOC_DO7= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2023-12-18 11:00:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2023-12-18 12:00:00",tz= "US/Mountain"),]

#8th event
SLOC_DO8= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2024-01-19 12:30:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2024-01-19 14:00:00",tz= "US/Mountain"),]

#9th event
SLOC_DO9= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2024-01-20 18:00:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2024-01-21 03:15:00",tz= "US/Mountain"),]

#10th event
SLOC_DO10= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2024-01-21 07:45:00",tz= "US/Mountain")
                              &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2024-01-24 15:00:00",tz= "US/Mountain"),]

#11th event
SLOC_DO11= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2024-02-10 15:00:00",tz= "US/Mountain")
                              &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2024-02-10 18:00:00",tz= "US/Mountain"),]

#12th event
SLOC_DO12= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2024-02-21 10:45:00",tz= "US/Mountain")
                              &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2024-02-21 13:00:00",tz= "US/Mountain"),]

#13th event
SLOC_DO13= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2024-3-20 10:00:00",tz= "US/Mountain")
                              &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2024-03-20 15:45:00",tz= "US/Mountain"),]

#14th event
SLOC_DO14= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2024-04-14 06:30:00",tz= "US/Mountain")
                              &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2024-04-14 12:45:00",tz= "US/Mountain"),]

#15th event
SLOC_DO15= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2024-04-15 03:30:00",tz= "US/Mountain")
                              &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2024-04-15 13:30:00",tz= "US/Mountain"),]

#16th event
SLOC_DO16= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2024-05-23 10:45:00",tz= "US/Mountain")
                              &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2024-05-23 12:45:00",tz= "US/Mountain"),]

#17th event
SLOC_DO17= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2024-07-17 14:00:00",tz= "US/Mountain")
                              &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2024-07-17 16:15:00",tz= "US/Mountain"),]

#18st event
SLOC_DO18= EXOz.dtw[["SLOC"]][EXOz.dtw[["SLOC"]]$datetimeMT >= as.POSIXct("2024-08-19 10:15:00",tz= "US/Mountain")
                              &EXOz.dtw[["SLOC"]]$datetimeMT <= as.POSIXct("2024-08-19 12:00:00",tz= "US/Mountain"),]

SLOC_DO <-list(SLOC_DO1,SLOC_DO2,SLOC_DO3,SLOC_DO4,SLOC_DO5,SLOC_DO6,SLOC_DO7,SLOC_DO8,SLOC_DO9,SLOC_DO10,
               SLOC_DO11,SLOC_DO12,SLOC_DO13,SLOC_DO14,SLOC_DO15,SLOC_DO16,SLOC_DO17,SLOC_DO18)
names(SLOC_DO)<-c('SLOC_DO1','SLOC_DO2','SLOC_DO3','SLOC_DO4','SLOC_DO5','SLOC_DO6','SLOC_DO7','SLOC_DO8','SLOC_DO9','SLOC_DO10',
                  'SLOC_DO11','SLOC_DO12','SLOC_DO13','SLOC_DO14','SLOC_DO15','SLOC_DO16','SLOC_DO17','SLOC_DO18')

SLOC_dates <- as.POSIXct(character(0))
for (i in seq_along(SLOC_DO)){
  temptime <- as.POSIXct(SLOC_DO[[i]]$datetimeMT[1])
  SLOC_dates<-c(SLOC_dates,temptime)
}


#SLOW#

#1st event
SLOW_DO1= EXOz.dtw[["SLOW"]][EXOz.dtw[["SLOW"]]$datetimeMT >= as.POSIXct("2023-10-16 14:30:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOW"]]$datetimeMT <= as.POSIXct("2023-10-16 15:15:00",tz= "US/Mountain"),]

#2nd event
SLOW_DO2= EXOz.dtw[["SLOW"]][EXOz.dtw[["SLOW"]]$datetimeMT >= as.POSIXct("2023-11-21 16:30:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOW"]]$datetimeMT <= as.POSIXct("2023-11-21 17:45:00",tz= "US/Mountain"),]

#3rd event
SLOW_DO3= EXOz.dtw[["SLOW"]][EXOz.dtw[["SLOW"]]$datetimeMT >= as.POSIXct("2023-12-18 10:45:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOW"]]$datetimeMT <= as.POSIXct("2023-12-18 12:15:00",tz= "US/Mountain"),]

#4th event
SLOW_DO4= EXOz.dtw[["SLOW"]][EXOz.dtw[["SLOW"]]$datetimeMT >= as.POSIXct("2024-01-19 12:45:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOW"]]$datetimeMT <= as.POSIXct("2024-01-19 14:45:00",tz= "US/Mountain"),]

#5th event
SLOW_DO5= EXOz.dtw[["SLOW"]][EXOz.dtw[["SLOW"]]$datetimeMT >= as.POSIXct("2024-02-21 10:15:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOW"]]$datetimeMT <= as.POSIXct("2024-02-21 12:15:00",tz= "US/Mountain"),]

#6th event
SLOW_DO6= EXOz.dtw[["SLOW"]][EXOz.dtw[["SLOW"]]$datetimeMT >= as.POSIXct("2024-03-20 10:00:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOW"]]$datetimeMT <= as.POSIXct("2024-03-20 12:00:00",tz= "US/Mountain"),]

#7th event
SLOW_DO7= EXOz.dtw[["SLOW"]][EXOz.dtw[["SLOW"]]$datetimeMT >= as.POSIXct("2024-05-14 14:15:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOW"]]$datetimeMT <= as.POSIXct("2024-05-14 17:45:00",tz= "US/Mountain"),]

#8th event
SLOW_DO8= EXOz.dtw[["SLOW"]][EXOz.dtw[["SLOW"]]$datetimeMT >= as.POSIXct("2024-07-17 14:00:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOW"]]$datetimeMT <= as.POSIXct("2024-07-17 15:15:00",tz= "US/Mountain"),]

#9th event
SLOW_DO9= EXOz.dtw[["SLOW"]][EXOz.dtw[["SLOW"]]$datetimeMT >= as.POSIXct("2024-08-19 10:30:00",tz= "US/Mountain")
                             &EXOz.dtw[["SLOW"]]$datetimeMT <= as.POSIXct("2024-08-19 18:15:00",tz= "US/Mountain"),]

SLOW_DO<-list(SLOW_DO1,SLOW_DO2,SLOW_DO3,SLOW_DO4,SLOW_DO5,SLOW_DO6,SLOW_DO7,SLOW_DO8,SLOW_DO9)
names(SLOW_DO)<-c('SLOW_DO1','SLOW_DO2','SLOW_DO3','SLOW_DO4','SLOW_DO5','SLOW_DO6','SLOW_DO7','SLOW_DO8','SLOW_DO9')

SLOW_dates <- as.POSIXct(character(0))
for (i in seq_along(SLOW_DO)){
  temptime <- as.POSIXct(SLOW_DO[[i]]$datetimeMT[1])
  SLOW_dates<-c(SLOW_dates,temptime)
}


#VDOW#

#1st event
VDOW_DO1= EXOz.dtw[["VDOW"]][EXOz.dtw[["VDOW"]]$datetimeMT >= as.POSIXct("2023-10-03 11:30:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOW"]]$datetimeMT <= as.POSIXct("2023-10-03 13:15:00",tz= "US/Mountain"),]

#2nd event
VDOW_DO2= EXOz.dtw[["VDOW"]][EXOz.dtw[["VDOW"]]$datetimeMT >= as.POSIXct("2023-10-16 14:00:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOW"]]$datetimeMT <= as.POSIXct("2023-10-16 15:30:00",tz= "US/Mountain"),]

#3rd event
VDOW_DO3= EXOz.dtw[["VDOW"]][EXOz.dtw[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-20 14:30:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-20 19:00:00",tz= "US/Mountain"),]

#4th event
VDOW_DO4= EXOz.dtw[["VDOW"]][EXOz.dtw[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-27 09:30:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-27 16:15:00",tz= "US/Mountain"),]

#5th event
VDOW_DO5= EXOz.dtw[["VDOW"]][EXOz.dtw[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-28 10:00:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-28 15:15:00",tz= "US/Mountain"),]

#6th event
VDOW_DO6= EXOz.dtw[["VDOW"]][EXOz.dtw[["VDOW"]]$datetimeMT >= as.POSIXct("2023-11-29 10:15:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOW"]]$datetimeMT <= as.POSIXct("2023-11-29 13:15:00",tz= "US/Mountain"),]

#7th event
VDOW_DO7= EXOz.dtw[["VDOW"]][EXOz.dtw[["VDOW"]]$datetimeMT >= as.POSIXct("2023-12-18 09:45:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOW"]]$datetimeMT <= as.POSIXct("2023-12-18 12:00:00",tz= "US/Mountain"),]

#8th event
VDOW_DO8= EXOz.dtw[["VDOW"]][EXOz.dtw[["VDOW"]]$datetimeMT >= as.POSIXct("2024-02-21 13:45:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOW"]]$datetimeMT <= as.POSIXct("2024-02-21 18:15:00",tz= "US/Mountain"),]

#9th event
VDOW_DO9= EXOz.dtw[["VDOW"]][EXOz.dtw[["VDOW"]]$datetimeMT >= as.POSIXct("2024-03-20 09:45:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOW"]]$datetimeMT <= as.POSIXct("2024-03-20 14:15:00",tz= "US/Mountain"),]

#10th event
VDOW_DO10= EXOz.dtw[["VDOW"]][EXOz.dtw[["VDOW"]]$datetimeMT >= as.POSIXct("2024-04-17 10:15:00",tz= "US/Mountain")
                              &EXOz.dtw[["VDOW"]]$datetimeMT <= as.POSIXct("2024-04-17 20:15:00",tz= "US/Mountain"),]

#11th event
VDOW_DO11= EXOz.dtw[["VDOW"]][EXOz.dtw[["VDOW"]]$datetimeMT >= as.POSIXct("2024-05-14 13:15:00",tz= "US/Mountain")
                              &EXOz.dtw[["VDOW"]]$datetimeMT <= as.POSIXct("2024-05-15 01:30:00",tz= "US/Mountain"),]

#12th event
VDOW_DO12= EXOz.dtw[["VDOW"]][EXOz.dtw[["VDOW"]]$datetimeMT >= as.POSIXct("2024-05-23 10:00:00",tz= "US/Mountain")
                              &EXOz.dtw[["VDOW"]]$datetimeMT <= as.POSIXct("2024-05-23 13:15:00",tz= "US/Mountain"),]

#13th event
VDOW_DO13= EXOz.dtw[["VDOW"]][EXOz.dtw[["VDOW"]]$datetimeMT >= as.POSIXct("2024-06-17 12:45:00",tz= "US/Mountain")
                              &EXOz.dtw[["VDOW"]]$datetimeMT <= as.POSIXct("2024-06-17 22:15:00",tz= "US/Mountain"),]

#14th event
VDOW_DO14= EXOz.dtw[["VDOW"]][EXOz.dtw[["VDOW"]]$datetimeMT >= as.POSIXct("2024-07-17 13:15:00",tz= "US/Mountain")
                              &EXOz.dtw[["VDOW"]]$datetimeMT <= as.POSIXct("2024-07-17 22:45:00",tz= "US/Mountain"),]

#15th event
VDOW_DO15= EXOz.dtw[["VDOW"]][EXOz.dtw[["VDOW"]]$datetimeMT >= as.POSIXct("2024-08-19 09:30:00",tz= "US/Mountain")
                              &EXOz.dtw[["VDOW"]]$datetimeMT <= as.POSIXct("2024-08-19 11:15:00",tz= "US/Mountain"),]

VDOW_DO<-list(VDOW_DO1,VDOW_DO2,VDOW_DO3,VDOW_DO4,VDOW_DO5,VDOW_DO6,VDOW_DO7,VDOW_DO8,VDOW_DO9,VDOW_DO10,
              VDOW_DO11,VDOW_DO12,VDOW_DO13,VDOW_DO14,VDOW_DO15)
names(VDOW_DO)<-c('VDOW_DO1','VDOW_DO2','VDOW_DO3','VDOW_DO4','VDOW_DO5','VDOW_DO6','VDOW_DO7','VDOW_DO8','VDOW_DO9','VDOW_DO10',
                  'VDOW_DO11','VDOW_DO12','VDOW_DO13','VDOW_DO14','VDOW_DO15')

VDOW_dates <- as.POSIXct(character(0))
for (i in seq_along(VDOW_DO)){
  temptime <- as.POSIXct(VDOW_DO[[i]]$datetimeMT[1])
  VDOW_dates<-c(VDOW_dates,temptime)
}


#VDOS#

#1st event
VDOS_DO1= EXOz.dtw[["VDOS"]][EXOz.dtw[["VDOS"]]$datetimeMT >= as.POSIXct("2023-10-15 19:00:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOS"]]$datetimeMT <= as.POSIXct("2023-10-16 19:15:00",tz= "US/Mountain"),]

#2nd event
VDOS_DO2= EXOz.dtw[["VDOS"]][EXOz.dtw[["VDOS"]]$datetimeMT >= as.POSIXct("2023-11-27 09:45:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOS"]]$datetimeMT <= as.POSIXct("2023-11-27 16:30:00",tz= "US/Mountain"),]

#3rd event
VDOS_DO3= EXOz.dtw[["VDOS"]][EXOz.dtw[["VDOS"]]$datetimeMT >= as.POSIXct("2023-11-29 10:15:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOS"]]$datetimeMT <= as.POSIXct("2023-11-29 14:30:00",tz= "US/Mountain"),]

#4th event
VDOS_DO4= EXOz.dtw[["VDOS"]][EXOz.dtw[["VDOS"]]$datetimeMT >= as.POSIXct("2023-11-30 10:30:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOS"]]$datetimeMT <= as.POSIXct("2023-11-30 12:15:00",tz= "US/Mountain"),]

#5th event
VDOS_DO5= EXOz.dtw[["VDOS"]][EXOz.dtw[["VDOS"]]$datetimeMT >= as.POSIXct("2024-02-21 13:30:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOS"]]$datetimeMT <= as.POSIXct("2024-02-21 18:15:00",tz= "US/Mountain"),]

#6th event
VDOS_DO6= EXOz.dtw[["VDOS"]][EXOz.dtw[["VDOS"]]$datetimeMT >= as.POSIXct("2024-04-17 10:00:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOS"]]$datetimeMT <= as.POSIXct("2024-04-17 14:15:00",tz= "US/Mountain"),]

#7th event
VDOS_DO7= EXOz.dtw[["VDOS"]][EXOz.dtw[["VDOS"]]$datetimeMT >= as.POSIXct("2024-05-23 10:15:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOS"]]$datetimeMT <= as.POSIXct("2024-05-23 19:15:00",tz= "US/Mountain"),]

#8th event
VDOS_DO8= EXOz.dtw[["VDOS"]][EXOz.dtw[["VDOS"]]$datetimeMT >= as.POSIXct("2024-07-16 10:00:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOS"]]$datetimeMT <= as.POSIXct("2024-07-16 15:15:00",tz= "US/Mountain"),]

#9th event
VDOS_DO9= EXOz.dtw[["VDOS"]][EXOz.dtw[["VDOS"]]$datetimeMT >= as.POSIXct("2024-07-17 13:15:00",tz= "US/Mountain")
                             &EXOz.dtw[["VDOS"]]$datetimeMT <= as.POSIXct("2024-07-17 16:00:00",tz= "US/Mountain"),]

#10th event
VDOS_DO10= EXOz.dtw[["VDOS"]][EXOz.dtw[["VDOS"]]$datetimeMT >= as.POSIXct("2024-08-19 09:45:00",tz= "US/Mountain")
                              &EXOz.dtw[["VDOS"]]$datetimeMT <= as.POSIXct("2024-08-19 12:45:00",tz= "US/Mountain"),]

VDOS_DO<-list(VDOS_DO1,VDOS_DO2,VDOS_DO3,VDOS_DO4,VDOS_DO5,VDOS_DO6,VDOS_DO7,VDOS_DO8,VDOS_DO9,VDOS_DO10)
names(VDOS_DO)<-c('VDOS_DO1','VDOS_DO2','VDOS_DO3','VDOS_DO4','VDOS_DO5','VDOS_DO6','VDOS_DO7','VDOS_DO8','VDOS_DO9','VDOS_DO10')

VDOS_dates <- as.POSIXct(character(0))
for (i in seq_along(VDOS_DO)){
  temptime <- as.POSIXct(VDOS_DO[[i]]$datetimeMT[1])
  VDOS_dates<-c(VDOS_dates,temptime)
}

#### Save lists of event dataframes ####
#DO
Eventdate <-list(SLOC_dates,SLOW_dates,VDOW_dates,VDOS_dates)  
names(Eventdate)<-c('SLOC_dates','SLOW_dates','VDOW_dates','VDOS_dates')

DO_events<-list(SLOC_DO,SLOW_DO,VDOW_DO,VDOS_DO)
names(DO_events)<-c('SLOC_DO','SLOW_DO','VDOW_DO','VDOS_DO')

#compile all events
BEGI_events<-list(DO_events,Eventdate)
names(BEGI_events)<-c('DO_events','Eventdate')

#save list
saveRDS(BEGI_events,"EXO_compiled/BEGI_events.rds")

#### Plot all DO events with complementary data (megaplots) ####

#import BEGI events (with tc data)
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")

#import compiled DTW and EXOz.dtw
EXOz.dtw = readRDS("DTW_compiled/BEGI_EXOz_dtw.rds")


# generate plot for each event in SLOC
for (i in seq_along(BEGI_events[["DO_events"]][["SLOC_DO"]])) {
  
  dz <- BEGI_events[["DO_events"]][["SLOC_DO"]][[i]]
  
  #Time window: 6 hours before event to 6 hours after event end. Adding and subtracting time objects occurs in seconds. There are 3600 seconds in an hour and 86400 seconds in 24 hours
  start_time <- min(dz$datetimeMT, na.rm = TRUE) - 3600*3
  end_time <- max(dz$datetimeMT, na.rm = TRUE) + 3600*3
  
  start_time_event <- min(dz$datetimeMT, na.rm = TRUE)
  end_time_event <- max(dz$datetimeMT, na.rm = TRUE)
  
  #Subset data
  tempdat <- EXOz.dtw[["SLOC"]][
    EXOz.dtw[["SLOC"]]$datetimeMT >= start_time &
      EXOz.dtw[["SLOC"]]$datetimeMT <= end_time, ]
  
  tempdatDOe <- EXOz.dtw[["SLOC"]][
    EXOz.dtw[["SLOC"]]$datetimeMT >= start_time_event &
      EXOz.dtw[["SLOC"]]$datetimeMT <= end_time_event, ]  
  
  
  #Plots
  g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  #geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
  
  g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  
  g3 <- ggplot(tempdat, aes(x = datetimeMT, y = -DTW_m)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
             inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time),
                     date_labels = "%b %d %H:%M") +
    labs(y = "SpCond (µS/cm)", x = "Datetime") +
    geom_line(na.rm = TRUE) + theme_minimal() +
    theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
  
  # Combine with patchwork
  full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
    plot_layout(ncol = 1, heights = rep(1, 6)) & 
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
  
  # Save
  ggsave(
    filename = paste0("plots/delineations/SLOC/events/SLOC_megaplot_v3_", i, ".pdf"),
    plot = full_plot,
    width = 6, height = 11 
  )
}

# generate plot for each event in SLOW
for (i in seq_along(BEGI_events[["DO_events"]][["SLOW_DO"]])) {
  
  dz <- BEGI_events[["DO_events"]][["SLOW_DO"]][[i]]
  
  #Time window: 6 hours before event to 6 hours after event end. Adding and subtracting time objects occurs in seconds. There are 3600 seconds in an hour and 86400 seconds in 24 hours
  start_time <- min(dz$datetimeMT, na.rm = TRUE) - 3600*3
  end_time <- max(dz$datetimeMT, na.rm = TRUE) + 3600*3
  
  start_time_event <- min(dz$datetimeMT, na.rm = TRUE)
  end_time_event <- max(dz$datetimeMT, na.rm = TRUE)
  
  #Subset data
  tempdat <- EXOz.dtw[["SLOW"]][
    EXOz.dtw[["SLOW"]]$datetimeMT >= start_time &
      EXOz.dtw[["SLOW"]]$datetimeMT <= end_time, ]
  
  tempdatDOe <- EXOz.dtw[["SLOW"]][
    EXOz.dtw[["SLOW"]]$datetimeMT >= start_time_event &
      EXOz.dtw[["SLOW"]]$datetimeMT <= end_time_event, ]  
  
  #Plots
  g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  #geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
  
  g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  
  g3 <- ggplot(tempdat, aes(x = datetimeMT, y = -DTW_m)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time),
                     date_labels = "%b %d %H:%M") +
    labs(y = "SpCond (µS/cm)", x = "Datetime") +
    geom_line(na.rm = TRUE) + theme_minimal() +
    theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
  
  # Combine with patchwork
  full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
    plot_layout(ncol = 1, heights = rep(1, 6)) & 
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
  
  # Save
  ggsave(
    filename = paste0("plots/delineations/SLOW/events/SLOW_megaplot_v3_", i, ".pdf"),
    plot = full_plot,
    width = 6, height = 11 
  )
}

# generate plot for each event in VDOW
for (i in seq_along(BEGI_events[["DO_events"]][["VDOW_DO"]])) {
  
  dz <- BEGI_events[["DO_events"]][["VDOW_DO"]][[i]]
  
  #Time window: 6 hours before event to 6 hours after event end. Adding and subtracting time objects occurs in seconds. There are 3600 seconds in an hour and 86400 seconds in 24 hours
  start_time <- min(dz$datetimeMT, na.rm = TRUE) - 3600*3
  end_time <- max(dz$datetimeMT, na.rm = TRUE) + 3600*3
  
  start_time_event <- min(dz$datetimeMT, na.rm = TRUE)
  end_time_event <- max(dz$datetimeMT, na.rm = TRUE)
  
  #Subset data
  tempdat <- EXOz.dtw[["VDOW"]][
    EXOz.dtw[["VDOW"]]$datetimeMT >= start_time &
      EXOz.dtw[["VDOW"]]$datetimeMT <= end_time, ]
  
  tempdatDOe <- EXOz.dtw[["VDOW"]][
    EXOz.dtw[["VDOW"]]$datetimeMT >= start_time_event &
      EXOz.dtw[["VDOW"]]$datetimeMT <= end_time_event, ]  

  #Plots
  g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  #geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
  
  g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  
  g3 <- ggplot(tempdat, aes(x = datetimeMT, y = -DTW_m)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
    geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time),
                     date_labels = "%b %d %H:%M") +
    labs(y = "SpCond (µS/cm)", x = "Datetime") +
    geom_line(na.rm = TRUE) + theme_minimal() +
    theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
  
  # Combine with patchwork
  full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
    plot_layout(ncol = 1, heights = rep(1, 6)) & 
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
  
  # Save
  ggsave(
    filename = paste0("plots/delineations/VDOW/events/VDOW_megaplot_v3_", i, ".pdf"),
    plot = full_plot,
    width = 6, height = 11 
  )
}

# generate plot for each event in VDOS
for (i in seq_along(BEGI_events[["DO_events"]][["VDOS_DO"]])) {
  
  dz <- BEGI_events[["DO_events"]][["VDOS_DO"]][[i]]
  
  #Time window: 6 hours before event to 6 hours after event end. Adding and subtracting time objects occurs in seconds. There are 3600 seconds in an hour and 86400 seconds in 24 hours
  start_time <- min(dz$datetimeMT, na.rm = TRUE) - 3600*3
  end_time <- max(dz$datetimeMT, na.rm = TRUE) + 3600*3
  
  start_time_event <- min(dz$datetimeMT, na.rm = TRUE)
  end_time_event <- max(dz$datetimeMT, na.rm = TRUE)
  
  #Subset data
  tempdat <- EXOz.dtw[["VDOS"]][
    EXOz.dtw[["VDOS"]]$datetimeMT >= start_time &
      EXOz.dtw[["VDOS"]]$datetimeMT <= end_time, ]
  
  tempdatDOe <- EXOz.dtw[["VDOS"]][
    EXOz.dtw[["VDOS"]]$datetimeMT >= start_time_event &
      EXOz.dtw[["VDOS"]]$datetimeMT <= end_time_event, ]  
  
  #Plots
  g1 <- ggplot(tempdat, aes(x = datetimeMT, y = ODO.mg.L.mn)) + 
    #geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    #         inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  #geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
  
  g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
    # geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    #          inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  
  g3 <- ggplot(tempdat, aes(x = datetimeMT, y = -DTW_m)) +
    # geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    #          inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
    #geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    #         inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
    #geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    #         inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
    # geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    #           inherit.aes = FALSE, fill = "lightgrey", alpha = 0.5) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time),
                     date_labels = "%b %d %H:%M") +
    labs(y = "SpCond (µS/cm)", x = "Datetime") +
    geom_line(na.rm = TRUE) + theme_minimal() +
    theme( plot.title = element_blank(), plot.margin = margin(0, 5, 0, 5))
  
  # Combine with patchwork
  full_plot <- g1 / g2 / g3 / g4 / g5 / g6 +
    plot_layout(ncol = 1, heights = rep(1, 6)) & 
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
  
  # Save
  ggsave(
    filename = paste0("plots/delineations/VDOS/events/VDOS_megaplot_v3_", i, ".pdf"),
    plot = full_plot,
    width = 6, height = 11 
  )
}

#### Combine all events for a well into one mega-megaplot multi-panel summary figure per well ####

vars_to_plot <- c("ODO.mg.L.mn", "fDOM.QSU.mn", "DTW_m", "Turbidity.FNU.mn", "Temp..C.mn", "SpCond.µS.cm.mn")
var_labels   <- c("DO (mg/L)", "fDOM (QSU)", "GW Depth (m)", "Turbidity (FNU)", "Temp (°C)", "SpCond (µS/cm)")

plot_all_events_combined <- function(site, event_list, buffer_hours = 3) {
  
  # stack all events into one long data frame, each tagged with its own event number
  event_data <- do.call(rbind, lapply(seq_along(event_list), function(i) {
    dz <- event_list[[i]]
    start_time <- min(dz$datetimeMT, na.rm = TRUE) - 3600 * buffer_hours
    end_time   <- max(dz$datetimeMT, na.rm = TRUE) + 3600 * buffer_hours
    
    tempdat <- EXOz.dtw[[site]][
      EXOz.dtw[[site]]$datetimeMT >= start_time &
        EXOz.dtw[[site]]$datetimeMT <= end_time, ]
    
    tempdat$event_id <- i
    tempdat$DTW_m_neg <- -tempdat$DTW_m
    tempdat
  }))
  
  # event column labels as event number
  event_labels <- paste0("Event ", seq_along(event_list))
  event_data$event_id <- factor(event_data$event_id, levels = seq_along(event_list),
                                labels = event_labels)
  
  # reshape to long format: one row per variable per timestamp
  long_data <- do.call(rbind, lapply(seq_along(vars_to_plot), function(v) {
    col <- if (vars_to_plot[v] == "DTW_m") "DTW_m_neg" else vars_to_plot[v]
    data.frame(
      event_id = event_data$event_id,
      datetimeMT = event_data$datetimeMT,
      variable = var_labels[v],
      value = event_data[[col]]
    )
  }))
  long_data$variable <- factor(long_data$variable, levels = var_labels)
  
  fig <- ggplot(long_data, aes(x = datetimeMT, y = value)) +
    geom_line(na.rm = TRUE) +
    facet_grid(variable ~ event_id, scales = "free", switch = "y") +
    labs(x = NULL, y = NULL) +
    theme_bw() +
    theme(strip.text.y.left = element_text(angle = 0),
          strip.placement   = "outside",
          panel.spacing     = unit(0.3, "lines"),
          axis.text.x       = element_text(size = 6, angle = 45, hjust = 1),
          strip.text        = element_text(size = 8))
  
  ggsave(paste0("plots/delineations/", site, "/", site, "_all_events_combined.pdf"),
         fig, width = 2 * length(event_list) + 2, height = 10, limitsize = FALSE)
  
  fig
}

plot_all_events_combined("SLOC", BEGI_events[["DO_events"]][["SLOC_DO"]])
plot_all_events_combined("SLOW", BEGI_events[["DO_events"]][["SLOW_DO"]])
plot_all_events_combined("VDOW", BEGI_events[["DO_events"]][["VDOW_DO"]])
plot_all_events_combined("VDOS", BEGI_events[["DO_events"]][["VDOS_DO"]])

#### Clear 24hr and individual event plots, keep only megaplots ####

all_plots <- list.files("plots/delineations", recursive = TRUE, full.names = TRUE, pattern = "\\.pdf$")

# keep individual megaplots (SITE_megaplot_v3_#.pdf) and the combined summary megaplot (SITE_all_events_combined.pdf); everything else (24hr plots and individual event plots) gets removed
keep <- grepl("megaplot_v3", all_plots) | grepl("all_events_combined", all_plots)

to_remove <- all_plots[!keep]
file.remove(to_remove)
