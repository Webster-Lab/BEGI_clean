#### read me ####
# the purpose of this script is to use corrected sensor depth data and  gw depth data to create a continuous dataset of depth to gw in the 4 wells used for the BEGI pilot study.

# sensor depth data will first be corrected to account for any jumps in sensor data that occurred as a result of the cable length changing.
# sensor depth data will be plotted against manual readings of depth to groundwater (DTW). The slope and intercept of the relationship will be calculated to estimate depth to gw.

# Sensor depth data was obtained by compensating in-well PT data for atmospheric pressure using the HOBO software wizard. Atmospheric pressure was recorded on site by a HOBO PT installed at the top of a well casing (out of water) for the dates/times 2023-10-20 12:30:00 to 2024-06-24 11:15:00. However, after 2024-06-24 11:15:00, the storage on the on-site in-air PT was exceeded and no more atmospheric pressure data is available on-site. Instead, we downloaded sea level pressure data from the Albuquerque airport from https://www.weather.gov/wrh/timeseries?site=KABQ&hourly=true, which is ~ 10 km northeast and 84 m in elevation higher than the site. We corrected this data from sea level to local atmospheric pressure using the equation [where the BP readings MUST be in mm Hg) is: True BP = [Corrected BP] – [2.5 * (Local Altitude in ft above sea level/100)]. Note that Inches of Hg x 25.4 = mm Hg]. We elected to use the airport data to compensate the entire in-well PT dataset to provide consistency of the approach. In-well PT data was compensated using "option 1" in the HOBO software wizard, which compensates for the data only where the two datasets overlap, interpolating between points that do not exactly align. 

#
#### libraries ####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)
library(dataRetrieval) # Download USGS discharge data
options(scipen=999)
library(viridis)
library(gridExtra)
library(patchwork)

#
#### load and wrangle PT data from google drive ####

ls_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1FBwR7Bz4ayynuARNE7drXXl2lSX9rZcN")
2
for (file_id in ls_tibble$id) {
  try({googledrive::drive_download(as_id(file_id))})
}
# add overwrite = TRUE if for some reason you want to replace files previously downloaded. 

#+++++++++ stitch together data files for each well
# import data
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
BEGI_PTz = list()
for(i in siteIDz){
  file_list <- list.files(recursive=F, pattern=paste(i, "_correctedful.csv", sep=""))
  BEGI_PTz[[i]] = lapply(file_list, read.csv, 
                         stringsAsFactors=FALSE, skip=1,header=T,
                         fileEncoding="utf-8")
}

# it looks like there was one instance of the datetime format getting changed to GMT0700 when the data was downloaded. All other data is in GMT0600. The GMT/UTC minus 6 hours offset is used in the Mountain Time Zone when operating in Daylight Saving Time. I will convert this instance of GMT0700 to GMT0600 so that everything is in Mountain Daylight Savings Time, and then convert it all to R's "US/Mountain", which accounts for the time change. Hopefully that will catch the time change.
BEGI_PTz[["VDOW"]][[2]][["Date.Time..GMT.06.00"]] = as.POSIXct(BEGI_PTz[["VDOW"]][[2]][["Date.Time..GMT.07.00"]], 
                                                               format = "%m/%d/%y %I:%M:%S %p",
                                                               tz="Etc/GMT+7")
BEGI_PTz[["VDOW"]][[2]][["Date.Time..GMT.06.00"]] = BEGI_PTz[["VDOW"]][[2]][["Date.Time..GMT.06.00"]]+(60*60)

BEGI_PTz[["VDOW"]][[2]][["Date.Time..GMT.07.00"]] =NULL

# it looks like the time zone updated whenever data was downloaded, so some data is in GMT0600 and some is in GMT0700 as indicated by the column header. I will convert it all to R's "US/Mountain" and hopefully that will catch the time change
for(i in siteIDz){
  # convert to POIXct and set timezone
  BEGI_PTz[[i]][[1]]$Date.Time..GMT.06.00<-as.POSIXct( BEGI_PTz[[i]][[1]]$Date.Time..GMT.06.00, 
                                                       format = "%m/%d/%y %I:%M:%S %p",
                                                       tz="Etc/GMT+6")
  BEGI_PTz[[i]][[2]]$Date.Time..GMT.06.00<-as.POSIXct( BEGI_PTz[[i]][[2]]$Date.Time..GMT.06.00, 
                                                       format = "%m/%d/%y %I:%M:%S %p",
                                                       tz="Etc/GMT+6")
  BEGI_PTz[[i]][[3]]$Date.Time..GMT.06.00<-as.POSIXct( BEGI_PTz[[i]][[3]]$Date.Time..GMT.06.00, 
                                                       format = "%m/%d/%y %I:%M:%S %p",
                                                       tz="Etc/GMT+6")
}
# change to mountain time
for(i in siteIDz){
  BEGI_PTz[[i]][[1]]$datetimeMT = with_tz(BEGI_PTz[[i]][[1]]$Date.Time..GMT.06.00, "US/Mountain")
  BEGI_PTz[[i]][[2]]$datetimeMT = with_tz(BEGI_PTz[[i]][[2]]$Date.Time..GMT.06.00, "US/Mountain")
  BEGI_PTz[[i]][[3]]$datetimeMT = with_tz(BEGI_PTz[[i]][[3]]$Date.Time..GMT.06.00, "US/Mountain")
}

# add column with SN of PT
BEGI_PTz[["VDOW"]][[1]]$sensorSN = 21821234
BEGI_PTz[["VDOW"]][[2]]$sensorSN = 21821234
BEGI_PTz[["VDOW"]][[3]]$sensorSN = 21821234
BEGI_PTz[["VDOS"]][[1]]$sensorSN = 21821232
BEGI_PTz[["VDOS"]][[2]]$sensorSN = 21821232
BEGI_PTz[["VDOS"]][[3]]$sensorSN = 21821232
BEGI_PTz[["SLOW"]][[1]]$sensorSN = 21821227
BEGI_PTz[["SLOW"]][[2]]$sensorSN = 21821227
BEGI_PTz[["SLOW"]][[3]]$sensorSN = 21821227
BEGI_PTz[["SLOC"]][[1]]$sensorSN = 21821230
BEGI_PTz[["SLOC"]][[2]]$sensorSN = 21821230
BEGI_PTz[["SLOC"]][[3]]$sensorSN = 21821230
# simplify headers
BEGI_PTz[["VDOW"]][[1]]$SensorDepth_m = BEGI_PTz[["VDOW"]][[1]]$Sensor.Depth..meters..LGR.S.N..21821234.
BEGI_PTz[["VDOW"]][[2]]$SensorDepth_m = BEGI_PTz[["VDOW"]][[2]]$Sensor.Depth..meters..LGR.S.N..21821234.
BEGI_PTz[["VDOW"]][[3]]$SensorDepth_m = BEGI_PTz[["VDOW"]][[3]]$Sensor.Depth..meters..LGR.S.N..21821234.
BEGI_PTz[["VDOS"]][[1]]$SensorDepth_m = BEGI_PTz[["VDOS"]][[1]]$Sensor.Depth..meters..LGR.S.N..21821232.
BEGI_PTz[["VDOS"]][[2]]$SensorDepth_m = BEGI_PTz[["VDOS"]][[2]]$Sensor.Depth..meters..LGR.S.N..21821232.
BEGI_PTz[["VDOS"]][[3]]$SensorDepth_m = BEGI_PTz[["VDOS"]][[3]]$Sensor.Depth..meters..LGR.S.N..21821232.
BEGI_PTz[["SLOW"]][[1]]$SensorDepth_m = BEGI_PTz[["SLOW"]][[1]]$Sensor.Depth..meters..LGR.S.N..21821227.
BEGI_PTz[["SLOW"]][[2]]$SensorDepth_m = BEGI_PTz[["SLOW"]][[2]]$Sensor.Depth..meters..LGR.S.N..21821227.
BEGI_PTz[["SLOW"]][[3]]$SensorDepth_m = BEGI_PTz[["SLOW"]][[3]]$Sensor.Depth..meters..LGR.S.N..21821227.
BEGI_PTz[["SLOC"]][[1]]$SensorDepth_m = BEGI_PTz[["SLOC"]][[1]]$Sensor.Depth..meters..LGR.S.N..21821230.
BEGI_PTz[["SLOC"]][[2]]$SensorDepth_m = BEGI_PTz[["SLOC"]][[2]]$Sensor.Depth..meters..LGR.S.N..21821230.
BEGI_PTz[["SLOC"]][[3]]$SensorDepth_m = BEGI_PTz[["SLOC"]][[3]]$Sensor.Depth..meters..LGR.S.N..21821230.

# use a set of column names as a template and match columns in all other files to that one. Note that this drops columns
universalnames = c("datetimeMT","sensorSN","SensorDepth_m")
for(i in siteIDz){
  for(n in 1:length(BEGI_PTz[[i]])){
    BEGI_PTz[[i]][[n]] = 
      BEGI_PTz[[i]][[n]] [, intersect(universalnames, names(BEGI_PTz[[i]][[n]] )), drop=FALSE]
  }
}

# remove rows with no sensor depth data
for(i in siteIDz){
  BEGI_PTz[[i]][[1]] = BEGI_PTz[[i]][[1]][complete.cases(BEGI_PTz[[i]][[1]]),]
  BEGI_PTz[[i]][[2]] = BEGI_PTz[[i]][[2]][complete.cases(BEGI_PTz[[i]][[2]]),]
  BEGI_PTz[[i]][[3]] = BEGI_PTz[[i]][[3]][complete.cases(BEGI_PTz[[i]][[3]]),]
}

# bind rows and remove duplicates
for(i in siteIDz){
  BEGI_PTz[[i]] = rbind(BEGI_PTz[[i]][[1]],BEGI_PTz[[i]][[2]],BEGI_PTz[[i]][[3]])
}
sum(duplicated(BEGI_PTz[["VDOW"]]$datetimeMT))
for(i in siteIDz){
  BEGI_PTz[[i]] = BEGI_PTz[[i]][order(BEGI_PTz[[i]][,'datetimeMT'],-BEGI_PTz[[i]][,'SensorDepth_m']),]
  BEGI_PTz[[i]] = BEGI_PTz[[i]][!duplicated(BEGI_PTz[[i]]$datetimeMT),]
}
sum(duplicated(BEGI_PTz[["VDOW"]]$datetimeMT))
#### complete timeseries with all possible time stamps ####

BEGI_PTz.ts = BEGI_PTz

max(c(BEGI_PTz.ts[["VDOW"]]$datetimeMT, BEGI_PTz.ts[["VDOS"]]$datetimeMT, BEGI_PTz.ts[["SLOC"]]$datetimeMT, BEGI_PTz.ts[["SLOW"]]$datetimeMT), na.rm = T)

min(c(BEGI_PTz.ts[["VDOW"]]$datetimeMT, BEGI_PTz.ts[["VDOS"]]$datetimeMT, BEGI_PTz.ts[["SLOC"]]$datetimeMT, BEGI_PTz.ts[["SLOW"]]$datetimeMT), na.rm = T)

time <- data.frame(
  datetimeMT = seq.POSIXt(
    from = ISOdatetime(2023,09,15,0,0,0, tz = "US/Mountain"),
    to = ISOdatetime(2024,12,03,0,0,0, tz= "US/Mountain"),
    by = "15 min" ))
# 42725 rows

# round time to nearest 15 min - lubridate::round_date(x, "15 minutes") 
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for(i in siteIDz){
  BEGI_PTz.ts[[i]]$datetimeMT<- lubridate::round_date(BEGI_PTz.ts[[i]]$datetimeMT, "15 minutes") 
}

# join to clean time stamps
BEGI_PTz.ts2 = list()
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for(i in siteIDz){
  BEGI_PTz.ts2[[i]] <- left_join(time, BEGI_PTz.ts[[i]], by="datetimeMT", keep=FALSE)
}

# plot to check
ggplot(data=BEGI_PTz.ts2[["VDOW"]], aes(datetimeMT, (SensorDepth_m*-1)))+
  geom_line()+
  geom_point()
ggplot(data=BEGI_PTz.ts2[["VDOS"]], aes(datetimeMT, (SensorDepth_m*-1)))+
  geom_line()+
  geom_point()
ggplot(data=BEGI_PTz.ts2[["SLOW"]], aes(datetimeMT, (SensorDepth_m*-1)))+
  geom_line()+
  geom_point()
ggplot(data=BEGI_PTz.ts2[["SLOC"]], aes(datetimeMT, (SensorDepth_m*-1)))+
  geom_line()+
  geom_point()

#### remove obvious outliers ####

# get service date/times from googledrive
service_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1quyArAKgI5qn_lz4n1vjnoMrM0XJWdDl")
2
googledrive::drive_download(as_id(service_tibble$id[service_tibble$name=="sensor_event_log.xlsx"]), overwrite = TRUE,path="googledrive/sensor_event_log.xlsx")
# read in file and filter to EXO1 removal and deployments
service = readxl::read_excel("googledrive/sensor_event_log.xlsx")
service = service[service$model=="EXO1",]
service = service[service$observation=="removed" | service$observation=="deployed",]
# format date and time
service$datetime = paste(service$date,  service$time, sep = " ")
# convert to POIXct and set timezone
service$datetimeMT<-as.POSIXct(service$datetime, 
                               format = "%Y-%m-%d %H:%M",
                               tz="US/Mountain")
service$date = as.Date(service$date)
service$...9=NULL
service
service.VDOS = service$datetimeMT[service$location=="VDOS"]
service.VDOW = service$datetimeMT[service$location=="VDOW"]
service.SLOW = service$datetimeMT[service$location=="SLOW"]
service.SLOC = service$datetimeMT[service$location=="SLOC"]


# VDOW
BEGI_PTz.ts2[["VDOW"]]$SensorDepth_m_C = BEGI_PTz.ts2[["VDOW"]]$SensorDepth_m
# fcn to remove with identify()
outlierremover <- function(data)
{
  date <- data$datetimeMT
  plot(date, data$SensorDepth_m_C)
  abline(v=service.VDOW, col="red")
  outliers <- identify(date, data$SensorDepth_m)
  x1 <- data$datetimeMT[outliers]
  return(x1)
}
# remove identified points
v1 = outlierremover(BEGI_PTz.ts2[["VDOW"]])
BEGI_PTz.ts2[["VDOW"]][(BEGI_PTz.ts2[["VDOW"]]$datetimeMT %in% v1),
                       "SensorDepth_m_C"] <- NA 
# for getting a range of dates removed
BEGI_PTz.ts2[["VDOW"]][(BEGI_PTz.ts2[["VDOW"]]$datetimeMT>min(v1) & BEGI_PTz.ts2[["VDOW"]]$datetimeMT<max(v1)),
                       "SensorDepth_m_C"] <- NA 
# plot
plot(ymd_hms(BEGI_PTz.ts2[["VDOW"]]$datetimeMT),(BEGI_PTz.ts2[["VDOW"]]$SensorDepth_m_C),pch=20)
# repeat "remove identified points" as necessary


# VDOS
BEGI_PTz.ts2[["VDOS"]]$SensorDepth_m_C = BEGI_PTz.ts2[["VDOS"]]$SensorDepth_m
# fcn to remove with identify()
outlierremover <- function(data)
{
  date <- data$datetimeMT
  plot(date, data$SensorDepth_m_C)
  abline(v=service.VDOS, col="red")
  outliers <- identify(date, data$SensorDepth_m)
  x1 <- data$datetimeMT[outliers]
  return(x1)
}
# remove identified points
v1 = outlierremover(BEGI_PTz.ts2[["VDOS"]])
BEGI_PTz.ts2[["VDOS"]][(BEGI_PTz.ts2[["VDOS"]]$datetimeMT %in% v1),
                       "SensorDepth_m_C"] <- NA 
# # for getting a range of dates removed. only select points within a range when using this!!
# BEGI_PTz.ts2[["VDOS"]][(BEGI_PTz.ts2[["VDOS"]]$datetimeMT>min(v1) & BEGI_PTz.ts2[["VDOS"]]$datetimeMT<max(v1)),
#                        "SensorDepth_m_C"] <- NA 
# plot
plot(ymd_hms(BEGI_PTz.ts2[["VDOS"]]$datetimeMT),(BEGI_PTz.ts2[["VDOS"]]$SensorDepth_m_C),pch=20)
# repeat "remove identified points" as necessary


# SLOW
BEGI_PTz.ts2[["SLOW"]]$SensorDepth_m_C = BEGI_PTz.ts2[["SLOW"]]$SensorDepth_m
# fcn to remove with identify()
outlierremover <- function(data)
{
  date <- data$datetimeMT
  plot(date, data$SensorDepth_m_C)
  abline(v=service.SLOW, col="red")
  outliers <- identify(date, data$SensorDepth_m)
  x1 <- data$datetimeMT[outliers]
  return(x1)
}
# remove identified points
v1 = outlierremover(BEGI_PTz.ts2[["SLOW"]])
BEGI_PTz.ts2[["SLOW"]][(BEGI_PTz.ts2[["SLOW"]]$datetimeMT %in% v1),
                       "SensorDepth_m_C"] <- NA 
# # for getting a range of dates removed. only select points within a range when using this!!
# BEGI_PTz.ts2[["SLOW"]][(BEGI_PTz.ts2[["SLOW"]]$datetimeMT>min(v1) & BEGI_PTz.ts2[["SLOW"]]$datetimeMT<max(v1)),
#                        "SensorDepth_m_C"] <- NA
# plot
plot(ymd_hms(BEGI_PTz.ts2[["SLOW"]]$datetimeMT),(BEGI_PTz.ts2[["SLOW"]]$SensorDepth_m_C),pch=20)
# repeat "remove identified points" as necessary


# SLOC
BEGI_PTz.ts2[["SLOC"]]$SensorDepth_m_C = BEGI_PTz.ts2[["SLOC"]]$SensorDepth_m
# fcn to remove with identify()
outlierremover <- function(data)
{
  date <- data$datetimeMT
  plot(date, data$SensorDepth_m_C)
  abline(v=service.SLOC, col="red")
  outliers <- identify(date, data$SensorDepth_m)
  x1 <- data$datetimeMT[outliers]
  return(x1)
}
# remove identified points
v1 = outlierremover(BEGI_PTz.ts2[["SLOC"]])
BEGI_PTz.ts2[["SLOC"]][(BEGI_PTz.ts2[["SLOC"]]$datetimeMT %in% v1),
                       "SensorDepth_m_C"] <- NA 
# # for getting a range of dates removed. only select points within a range when using this!!
# BEGI_PTz.ts2[["SLOC"]][(BEGI_PTz.ts2[["SLOC"]]$datetimeMT>min(v1) & BEGI_PTz.ts2[["SLOC"]]$datetimeMT<max(v1)),
#                        "SensorDepth_m_C"] <- NA
# plot
plot(ymd_hms(BEGI_PTz.ts2[["SLOC"]]$datetimeMT),(BEGI_PTz.ts2[["SLOC"]]$SensorDepth_m_C),pch=20)
# repeat "remove identified points" as necessary


# plot to check
ggplot(data=BEGI_PTz.ts2[["VDOW"]], aes(datetimeMT, (SensorDepth_m_C*-1)))+
  geom_line()+
  geom_point()
ggplot(data=BEGI_PTz.ts2[["VDOS"]], aes(datetimeMT, (SensorDepth_m_C*-1)))+
  geom_line()+
  geom_point()
ggplot(data=BEGI_PTz.ts2[["SLOW"]], aes(datetimeMT, (SensorDepth_m_C*-1)))+
  geom_line()+
  geom_point()
ggplot(data=BEGI_PTz.ts2[["SLOC"]], aes(datetimeMT, (SensorDepth_m*-1)))+
  geom_line()+
  geom_point()

# # fix the removal of timestamps, sensorSN, and uncorrected SensorDepth_m. I should modify the outlier removal script to prevent the introduction of NAs into all columns in future 
# # join to clean time stamps
# BEGI_PTz.ts3 = list()
# siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
# for(i in siteIDz){
#   BEGI_PTz.ts3[[i]] <- left_join(time, BEGI_PTz.ts2[[i]], by="datetimeMT", keep=FALSE)
# }
# BEGI_PTz.Conly = list()
# for(i in siteIDz){
#   BEGI_PTz.Conly[[i]] = BEGI_PTz.ts3[[i]]
#   BEGI_PTz.Conly[[i]]$sensorSN = NULL
#   BEGI_PTz.Conly[[i]]$SensorDepth_m = NULL
# }
# BEGI_PTz.noC = list()
# for(i in siteIDz){
#   BEGI_PTz.noC[[i]] = BEGI_PTz.ts[[i]]
# }
# BEGI_PTz.ts4 = list()
# for(i in siteIDz){
#   BEGI_PTz.ts4[[i]] <- left_join(BEGI_PTz.Conly[[i]], BEGI_PTz.noC[[i]], by="datetimeMT")
# }


#### save and re-add cleaned sensor depth data ####


saveRDS(BEGI_PTz.ts2, "data_clean/DTW_compiled/BEGI_PTz.ts2.rds")
rm(list = ls())
BEGI_PTz.ts4 = readRDS("data_clean/DTW_compiled/BEGI_PTz.ts2.rds")

#### correct baseline jumps ####

# Baseline jumps likely resulted from the cable getting tangled such that its length was temporarily changed when sondes were being serviced. 

# baseline-corrected data will be saved as BEGI_PTz.ts5 as SensorDepth_m_CBC

BEGI_PTz.ts5 = BEGI_PTz.ts4

# get service date/times from googledrive
service_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1quyArAKgI5qn_lz4n1vjnoMrM0XJWdDl")
2
googledrive::drive_download(as_id(service_tibble$id[service_tibble$name=="sensor_event_log.xlsx"]), overwrite = TRUE,path="googledrive/sensor_event_log.xlsx")

# read in file and filter to EXO1 removal and deployments
service = readxl::read_excel("googledrive/sensor_event_log.xlsx")
service = service[service$model=="EXO1",]
service = service[service$observation=="removed" | service$observation=="deployed",]

# format date and time
service$datetime = paste(service$date,  service$time, sep = " ")
# convert to POIXct and set timezone
service$datetimeMT<-as.POSIXct(service$datetime, 
                               format = "%Y-%m-%d %H:%M",
                               tz="US/Mountain")
service$date = as.Date(service$date)
service$...9=NULL

### plot data in chunks to find baseline jumps
## VDOW ##
temp =  BEGI_PTz.ts4[["VDOW"]][BEGI_PTz.ts4[["VDOW"]]$datetimeMT>=as.POSIXct("2023-09-15 OO:OO:OO") &
                                 BEGI_PTz.ts4[["VDOW"]]$datetimeMT<as.POSIXct("2023-11-01 OO:OO:OO"),]
plot(temp$datetimeMT, temp$SensorDepth_m_C, type="o")
# no jumps in VDOW
BEGI_PTz.ts5[["VDOW"]]$SensorDepth_m_CBC = BEGI_PTz.ts5[["VDOW"]]$SensorDepth_m_C

## VDOS ##
service.VDOS = service$datetimeMT[service$location=="VDOS"]
temp =  BEGI_PTz.ts4[["VDOS"]][BEGI_PTz.ts4[["VDOS"]]$datetimeMT>=as.POSIXct("2024-10-01 OO:OO:OO") &
                                 BEGI_PTz.ts4[["VDOS"]]$datetimeMT<as.POSIXct("2024-11-01 OO:OO:OO"),]
plot(temp$datetimeMT, temp$SensorDepth_m_C, type="o");abline(v=as.POSIXct(service.VDOS), col="red")
# correction 1
temp =  BEGI_PTz.ts4[["VDOS"]][BEGI_PTz.ts4[["VDOS"]]$datetimeMT>=as.POSIXct("2023-11-21 OO:OO:OO") &
                                 BEGI_PTz.ts4[["VDOS"]]$datetimeMT<as.POSIXct("2023-11-29 OO:OO:OO"),]
plot(temp$datetimeMT, temp$SensorDepth_m_C, type="o")
abline(v=as.POSIXct(service.VDOS), col="red")
BEGI_PTz.ts5[["VDOS"]]$SensorDepth_m_CBC = BEGI_PTz.ts5[["VDOS"]]$SensorDepth_m_C
BEGI_PTz.ts5[["VDOS"]]$SensorDepth_m_CBC[BEGI_PTz.ts5[["VDOS"]]$datetimeMT>=as.POSIXct("2023-11-22 14:30:00") &
                                           BEGI_PTz.ts5[["VDOS"]]$datetimeMT<=as.POSIXct("2023-11-27 09:45:00")] = 
  BEGI_PTz.ts5[["VDOS"]]$SensorDepth_m_C[BEGI_PTz.ts5[["VDOS"]]$datetimeMT>=as.POSIXct("2023-11-22 14:30:00") &
                                           BEGI_PTz.ts5[["VDOS"]]$datetimeMT<=as.POSIXct("2023-11-27 09:45:00")] - (1.930-1.898)
temp =  BEGI_PTz.ts5[["VDOS"]][BEGI_PTz.ts5[["VDOS"]]$datetimeMT>=as.POSIXct("2023-11-21 OO:OO:OO") &
                                 BEGI_PTz.ts5[["VDOS"]]$datetimeMT<as.POSIXct("2023-11-29 OO:OO:OO"),]
plot(temp$datetimeMT, temp$SensorDepth_m_CBC, type="o")
abline(v=as.POSIXct(service.VDOS), col="red")
# end correction 1
# # remove outliers I missed previously
# BEGI_PTz.ts5[["VDOS"]][BEGI_PTz.ts5[["VDOS"]]$datetimeMT==as.POSIXct("2024-03-19 13:30:00"),
#                        "SensorDepth_m_CBC"] = NA
# BEGI_PTz.ts5[["VDOS"]][BEGI_PTz.ts5[["VDOS"]]$datetimeMT==as.POSIXct("2024-04-30 09:45:00"),
#                        "SensorDepth_m_CBC"] = NA
# BEGI_PTz.ts5[["VDOS"]][BEGI_PTz.ts5[["VDOS"]]$datetimeMT>as.POSIXct("2024-05-28 12:00:00")&
#                          BEGI_PTz.ts5[["VDOS"]]$datetimeMT<as.POSIXct("2024-05-28 13:45:00"),
#                        "SensorDepth_m_CBC"] = NA
# BEGI_PTz.ts5[["VDOS"]][BEGI_PTz.ts5[["VDOS"]]$datetimeMT==as.POSIXct("2024-08-21 10:00:00"),
#                        "SensorDepth_m_CBC"] = NA
# temp =  BEGI_PTz.ts5[["VDOS"]][BEGI_PTz.ts5[["VDOS"]]$datetimeMT>=as.POSIXct("2024-08-20 OO:OO:OO") &
#                                  BEGI_PTz.ts5[["VDOS"]]$datetimeMT<as.POSIXct("2024-08-22 OO:OO:OO"),]
# plot(temp$datetimeMT, temp$SensorDepth_m_CBC, type="o");abline(v=as.POSIXct(service.VDOS), col="red")


## SLOW ##
service.SLOW = service$datetimeMT[service$location=="SLOW"]
temp =  BEGI_PTz.ts4[["SLOW"]][BEGI_PTz.ts4[["SLOW"]]$datetimeMT>=as.POSIXct("2023-12-01 OO:OO:OO") &
                                 BEGI_PTz.ts4[["SLOW"]]$datetimeMT<as.POSIXct("2024-02-15 OO:OO:OO"),]
plot(temp$datetimeMT, temp$SensorDepth_m_C, type="o");abline(v=as.POSIXct(service.SLOW), col="red")
# correction 1 & 2
temp =  BEGI_PTz.ts4[["SLOW"]][BEGI_PTz.ts4[["SLOW"]]$datetimeMT>=as.POSIXct("2024-01-25 00:00:00") &
                                 BEGI_PTz.ts4[["SLOW"]]$datetimeMT<as.POSIXct("2024-02-06 20:00:00"),]
plot(temp$datetimeMT, temp$SensorDepth_m_C, type="o"); abline(v=as.POSIXct(service.SLOW), col="red")
BEGI_PTz.ts5[["SLOW"]]$SensorDepth_m_CBC = BEGI_PTz.ts5[["SLOW"]]$SensorDepth_m_C
#1
BEGI_PTz.ts5[["SLOW"]]$SensorDepth_m_CBC[BEGI_PTz.ts5[["SLOW"]]$datetimeMT>=as.POSIXct("2023-12-21 16:00:00") &
                                           BEGI_PTz.ts5[["SLOW"]]$datetimeMT<=as.POSIXct("2024-01-05 16:30:00")] = 
  BEGI_PTz.ts5[["SLOW"]]$SensorDepth_m_C[BEGI_PTz.ts5[["SLOW"]]$datetimeMT>=as.POSIXct("2023-12-21 16:00:00") &
                                           BEGI_PTz.ts5[["SLOW"]]$datetimeMT<=as.POSIXct("2024-01-05 16:30:00")] + (2.460-2.390)
#2
BEGI_PTz.ts5[["SLOW"]]$SensorDepth_m_CBC[BEGI_PTz.ts5[["SLOW"]]$datetimeMT>=as.POSIXct("2024-01-25 13:45:00") &
                                           BEGI_PTz.ts5[["SLOW"]]$datetimeMT<=as.POSIXct("2024-02-06 11:15:00")] = 
  BEGI_PTz.ts5[["SLOW"]]$SensorDepth_m_C[BEGI_PTz.ts5[["SLOW"]]$datetimeMT>=as.POSIXct("2024-01-25 13:45:00") &
                                           BEGI_PTz.ts5[["SLOW"]]$datetimeMT<=as.POSIXct("2024-02-06 11:15:00")] + (2.139-1.999)

temp =  BEGI_PTz.ts5[["SLOW"]][BEGI_PTz.ts5[["SLOW"]]$datetimeMT>=as.POSIXct("2023-12-21 14:00:00") &
                                 BEGI_PTz.ts5[["SLOW"]]$datetimeMT<as.POSIXct("2024-03-05 20:00:00"),]
plot(temp$datetimeMT, temp$SensorDepth_m_CBC, type="o"); abline(v=as.POSIXct(service.SLOW), col="red")
# end correction 1 & 2

## SLOC ##
service.SLOC = service$datetimeMT[service$location=="SLOC"]
temp =  BEGI_PTz.ts4[["SLOC"]][BEGI_PTz.ts4[["SLOC"]]$datetimeMT>=as.POSIXct("2023-11-01 00:00:00") &
                                 BEGI_PTz.ts4[["SLOC"]]$datetimeMT<as.POSIXct("2024-01-01 00:00:00"),]
plot(temp$datetimeMT, temp$SensorDepth_m_C*-1, type="o");abline(v=as.POSIXct(service.SLOC), col="red")
## correction 1-4
BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_CBC = BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_C
#1
jump1 = as.POSIXct("2023-10-06 10:45:00")
endjump1 = as.POSIXct("2023-10-13 10:15:00")
dif1 = 0.911-0.882
BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_CBC[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump1 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump1] = 
  BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_C[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump1 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump1] + dif1
#2
jump2 = as.POSIXct("2023-10-13 11:00:00")
endjump2 = as.POSIXct("2023-10-16 14:15:00")
dif2 = (0.915-0.853)+dif1
BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_CBC[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump2 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump2] = 
  BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_C[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump2 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump2] + dif2
#3
jump3 = as.POSIXct("2023-10-16 14:30:00")
endjump3 = as.POSIXct("2023-10-20 10:15:00")
dif3 = (0.880-0.739)+dif2
BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_CBC[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump3 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump3] = 
  BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_C[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump3 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump3] + dif3
#4
jump4 = as.POSIXct("2023-10-27 11:15:00")
endjump4 = as.POSIXct("2023-11-03 10:30:00")
dif4 = (0.989-0.940)
BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_CBC[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump4 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump4] =
  BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_C[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump4 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump4] + dif4
# plot to check
temp =  BEGI_PTz.ts5[["SLOC"]][BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=as.POSIXct("2023-09-01 00:00:00") &
                                 BEGI_PTz.ts5[["SLOC"]]$datetimeMT<as.POSIXct("2024-01-01 00:00:00"),]
plot(temp$datetimeMT, temp$SensorDepth_m_C, col="red")
points(temp$datetimeMT, temp$SensorDepth_m_CBC, type="b"); abline(v=as.POSIXct(service.SLOC), col="red")

## correction 5
jump5 = as.POSIXct("2024-01-19 16:30:00")
endjump5 = as.POSIXct("2024-01-25 13:00:00")
dif5 = 0.828-0.711
BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_CBC[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump5 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump5] = 
  BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_C[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump5 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump5] + dif5
# plot to check
temp =  BEGI_PTz.ts5[["SLOC"]][BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=as.POSIXct("2024-01-19 00:00:00") &
                                 BEGI_PTz.ts5[["SLOC"]]$datetimeMT<as.POSIXct("2024-02-15 00:00:00"),]
plot(temp$datetimeMT, temp$SensorDepth_m_CBC, type="o"); abline(v=as.POSIXct(service.SLOC), col="red")

## correction 6
jump6 = as.POSIXct("2024-01-25 13:30:00")
endjump6 = as.POSIXct("2024-02-06 10:15:00")
dif6 = 0.860-0.830
BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_CBC[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump6 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump6] = 
  BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_C[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump6 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump6] + dif6
# plot to check
temp =  BEGI_PTz.ts5[["SLOC"]][BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=as.POSIXct("2024-01-01 00:00:00") &
                                 BEGI_PTz.ts5[["SLOC"]]$datetimeMT<as.POSIXct("2024-05-15 00:00:00"),]
plot(temp$datetimeMT, temp$SensorDepth_m_CBC, type="o"); abline(v=as.POSIXct(service.SLOC), col="red")

## correction 7
jump7 = as.POSIXct("2024-04-17 09:15:00")
endjump7 = as.POSIXct("2024-04-30 10:15:00")
dif7 = 0.963-0.746
BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_CBC[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump7 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump7] = 
  BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_C[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump7 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump7] + dif7
# plot to check
temp =  BEGI_PTz.ts5[["SLOC"]][BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=as.POSIXct("2024-07-17 00:00:00") &
                                 BEGI_PTz.ts5[["SLOC"]]$datetimeMT<as.POSIXct("2024-11-30 20:00:00"),]
plot(temp$datetimeMT, temp$SensorDepth_m_CBC, type="o"); abline(v=as.POSIXct(service.SLOC), col="red")

#### Smooth and interpolate data ####

# smoothed and gap-filled data (<6 hr gaps only) will be saved as BEGI_PTz.ts6 as SensorDepth_m_CBC_sm

# fill gaps < 6 hr for each well individually
BEGI_PTz.ts6 = BEGI_PTz.ts5
# VDOW
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling
# Make univariate zoo time series #
ts.temp<-read.zoo(BEGI_PTz.ts6[["VDOW"]][,c(1,5)], index.column=1, format="%Y-%m-%d %H:%M:%S", tz="US/Mountain")
# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)
# Apply NA interpolation method
ts.filled = na.spline(ts.temp, na.rm = T, maxgap = 4*6)
plot(ts.filled)
#par(mfrow=c(1,1)) # reset plotting window
# revert back to df
ts.filled = as.data.frame(ts.filled)
ts.filled$datetimeMT = BEGI_PTz.ts6[["VDOW"]]$datetimeMT
names(ts.filled) = c("SensorDepth_m_CBC_sm","datetimeMT")
BEGI_PTz.ts6[["VDOW"]] = left_join(BEGI_PTz.ts6[["VDOW"]], ts.filled, by="datetimeMT")
# check NAs that are left
sum(is.na(BEGI_PTz.ts6[["VDOW"]]$SensorDepth_m_CBC))
sum(is.na(BEGI_PTz.ts6[["VDOW"]]$SensorDepth_m_CBC_sm))
###
# VDOS
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling
# Make univariate zoo time series #
ts.temp<-read.zoo(BEGI_PTz.ts6[["VDOS"]][,c(1,5)], index.column=1, format="%Y-%m-%d %H:%M:%S", tz="US/Mountain")
# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)
# Apply NA interpolation method
ts.filled = na.spline(ts.temp, na.rm = T, maxgap = 4*6)
plot(ts.filled)
#par(mfrow=c(1,1)) # reset plotting window
# revert back to df
ts.filled = as.data.frame(ts.filled)
ts.filled$datetimeMT = BEGI_PTz.ts6[["VDOS"]]$datetimeMT
names(ts.filled) = c("SensorDepth_m_CBC_sm","datetimeMT")
BEGI_PTz.ts6[["VDOS"]] = left_join(BEGI_PTz.ts6[["VDOS"]], ts.filled, by="datetimeMT")
# check NAs that are left
sum(is.na(BEGI_PTz.ts6[["VDOS"]]$SensorDepth_m_CBC))
sum(is.na(BEGI_PTz.ts6[["VDOS"]]$SensorDepth_m_CBC_sm))
###
# SLOW
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling
# Make univariate zoo time series #
ts.temp<-read.zoo(BEGI_PTz.ts6[["SLOW"]][,c(1,5)], index.column=1, format="%Y-%m-%d %H:%M:%S", tz="US/Mountain")
# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)
# Apply NA interpolation method
ts.filled = na.spline(ts.temp, na.rm = T, maxgap = 4*6)
plot(ts.filled)
#par(mfrow=c(1,1)) # reset plotting window
# revert back to df
ts.filled = as.data.frame(ts.filled)
ts.filled$datetimeMT = BEGI_PTz.ts6[["SLOW"]]$datetimeMT
names(ts.filled) = c("SensorDepth_m_CBC_sm","datetimeMT")
BEGI_PTz.ts6[["SLOW"]] = left_join(BEGI_PTz.ts6[["SLOW"]], ts.filled, by="datetimeMT")
# check NAs that are left
sum(is.na(BEGI_PTz.ts6[["SLOW"]]$SensorDepth_m_CBC))
sum(is.na(BEGI_PTz.ts6[["SLOW"]]$SensorDepth_m_CBC_sm))
##
# SLOC
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling
# Make univariate zoo time series #
ts.temp<-read.zoo(BEGI_PTz.ts6[["SLOC"]][,c(1,5)], index.column=1, format="%Y-%m-%d %H:%M:%S", tz="US/Mountain")
# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)
# Apply NA interpolation method
ts.filled = na.spline(ts.temp, na.rm = T, maxgap = 4*3)
plot(ts.filled)
par(mfrow=c(1,1)) # reset plotting window
# revert back to df
ts.filled = as.data.frame(ts.filled)
ts.filled$datetimeMT = BEGI_PTz.ts6[["SLOC"]]$datetimeMT
names(ts.filled) = c("SensorDepth_m_CBC_sm","datetimeMT")
BEGI_PTz.ts6[["SLOC"]] = left_join(BEGI_PTz.ts6[["SLOC"]], ts.filled, by="datetimeMT")
# check NAs that are left
sum(is.na(BEGI_PTz.ts6[["SLOC"]]$SensorDepth_m_CBC))
sum(is.na(BEGI_PTz.ts6[["SLOC"]]$SensorDepth_m_CBC_sm))


# smooth
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for(i in siteIDz){
  BEGI_PTz.ts6[[i]]$SensorDepth_m_CBC_sm = zoo::rollmean(BEGI_PTz.ts6[[i]]$SensorDepth_m_CBC_sm, 3, na.pad = TRUE)
}
# plot to check
ggplot(data=BEGI_PTz.ts6[["VDOW"]], aes(datetimeMT, (SensorDepth_m_CBC_sm*-1)))+
  geom_line()
ggplot(data=BEGI_PTz.ts6[["VDOS"]], aes(datetimeMT, (SensorDepth_m_CBC_sm*-1)))+
  geom_line()
ggplot(data=BEGI_PTz.ts6[["SLOW"]], aes(datetimeMT, (SensorDepth_m_CBC_sm*-1)))+
  geom_line()
ggplot(data=BEGI_PTz.ts6[["SLOC"]], aes(datetimeMT, (SensorDepth_m_CBC_sm*-1)))+
  geom_line()


#

#### save and re-add cleaned sensor depth data ####


saveRDS(BEGI_PTz.ts6, "data_clean/DTW_compiled/BEGI_PTz.rds")
rm(list = ls())
BEGI_PTz = readRDS("data_clean/DTW_compiled/BEGI_PTz.rds")

#
#### load in manual DTW dataset ####

# get data from googledrive
beeper_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1L5ywkdYUOxhE3GPm7vbMiwgObOyn3awF")
2
googledrive::drive_download(as_id(beeper_tibble$id[beeper_tibble$name=="BEGI_beeper"]), overwrite = TRUE,
                            path="googledrive/BEGI_beeper.csv")
beeper = read.csv("googledrive/BEGI_beeper.csv")

# format date/times
beeper$datetimeMT = as.POSIXct(
  paste(beeper$date, beeper$time, sep=" "),
  "%Y-%m-%d %H:%M", tz="US/Mountain")
beeper$date = as.Date(beeper$date)

beeper = beeper[!is.na(beeper$datetimeMT),]
beeper = beeper[!is.na(beeper$waterlevelbelowsurface_cm),]
beeper_r = data.frame(datetimeMT = beeper$datetimeMT, 
                      siteID = beeper$siteID,
                      wellID = beeper$wellID,
                      DTW_beeper = beeper$waterlevelbelowsurface_cm)

#### join beeper DTW to PT sensor depth data ####

# add site and well IDs
BEGI_PTz[["VDOW"]]$siteID = "VDO"
BEGI_PTz[["VDOW"]]$wellID = "VDOW"
BEGI_PTz[["VDOS"]]$siteID = "VDO"
BEGI_PTz[["VDOS"]]$wellID = "VDOS"
BEGI_PTz[["SLOW"]]$siteID = "SLO"
BEGI_PTz[["SLOW"]]$wellID = "SLOW"
BEGI_PTz[["SLOC"]]$siteID = "SLO"
BEGI_PTz[["SLOC"]]$wellID = "SLOC"

# join
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for(i in siteIDz){
  BEGI_PTz[[i]] = left_join(BEGI_PTz[[i]], beeper_r, by=c("datetimeMT","siteID","wellID"))
}
# convert DTW_beeper from cm to m
for(i in siteIDz){
  BEGI_PTz[[i]]$DTW_beeper_m = BEGI_PTz[[i]]$DTW_beeper/100
}
# convert SensorDepth_m_CBC_sm positive to negative to reflect relative depth from surface
for(i in siteIDz){
  BEGI_PTz[[i]]$SensorDepth_m_CBC_sm_neg = BEGI_PTz[[i]]$SensorDepth_m_CBC_sm*-1
}

# plot to check
ggplot(data=BEGI_PTz[["VDOW"]])+
  geom_line(aes(datetimeMT, SensorDepth_m_CBC_sm_neg))+
  geom_point(aes(datetimeMT, DTW_beeper_m), size=3, color="red")
ggplot(data=BEGI_PTz[["VDOS"]])+
  geom_line(aes(datetimeMT, SensorDepth_m_CBC_sm_neg))+
  geom_point(aes(datetimeMT, DTW_beeper_m), size=3, color="red")
ggplot(data=BEGI_PTz[["SLOW"]])+
  geom_line(aes(datetimeMT, SensorDepth_m_CBC_sm_neg))+
  geom_point(aes(datetimeMT, DTW_beeper_m), size=3, color="red")
ggplot(data=BEGI_PTz[["SLOC"]])+
  geom_line(aes(datetimeMT, SensorDepth_m_CBC_sm_neg))+
  geom_point(aes(datetimeMT, DTW_beeper_m), size=3, color="red")


#### for each well, plot PT data v. measurements and model ####

# VDOW
plot(BEGI_PTz[["VDOW"]]$DTW_beeper_m ~ BEGI_PTz[["VDOW"]]$SensorDepth_m_CBC_sm_neg)
m.VDOW = lm(BEGI_PTz[["VDOW"]]$DTW_beeper_m ~ BEGI_PTz[["VDOW"]]$SensorDepth_m_CBC_sm_neg)
abline(m.VDOW)
summary(m.VDOW)
cf <- coef(m.VDOW)
Intercept <- cf[1]
Slope <- cf[2]
BEGI_PTz[["VDOW"]]$DTW_m = BEGI_PTz[["VDOW"]]$SensorDepth_m_CBC_sm_neg*Slope + Intercept
ggplot(data=BEGI_PTz[["VDOW"]])+
  geom_line(aes(datetimeMT, DTW_m))+
  geom_point(aes(datetimeMT, DTW_beeper_m), size=3, color="red")
ggplot(data=BEGI_PTz[["VDOW"]])+ geom_line(aes(datetimeMT, DTW_m*-1))+ylim(c(-3,0))

# VDOS
plot(BEGI_PTz[["VDOS"]]$DTW_beeper_m ~ BEGI_PTz[["VDOS"]]$SensorDepth_m_CBC_sm_neg)
m.VDOS = lm(BEGI_PTz[["VDOS"]]$DTW_beeper_m ~ BEGI_PTz[["VDOS"]]$SensorDepth_m_CBC_sm_neg)
abline(m.VDOS)
summary(m.VDOS)
cf <- coef(m.VDOS)
Intercept <- cf[1]
Slope <- cf[2]
BEGI_PTz[["VDOS"]]$DTW_m = BEGI_PTz[["VDOS"]]$SensorDepth_m_CBC_sm_neg*Slope + Intercept
ggplot(data=BEGI_PTz[["VDOS"]])+
  geom_line(aes(datetimeMT, DTW_m))+
  geom_point(aes(datetimeMT, DTW_beeper_m), size=3, color="red")
ggplot(data=BEGI_PTz[["VDOS"]])+ geom_line(aes(datetimeMT, DTW_m*-1))+ylim(c(-3,0))

# SLOW
plot(BEGI_PTz[["SLOW"]]$DTW_beeper_m ~ BEGI_PTz[["SLOW"]]$SensorDepth_m_CBC_sm_neg)
m.SLOW = lm(BEGI_PTz[["SLOW"]]$DTW_beeper_m ~ BEGI_PTz[["SLOW"]]$SensorDepth_m_CBC_sm_neg)
abline(m.SLOW)
summary(m.SLOW)
cf <- coef(m.SLOW)
Intercept <- cf[1]
Slope <- cf[2]
BEGI_PTz[["SLOW"]]$DTW_m = BEGI_PTz[["SLOW"]]$SensorDepth_m_CBC_sm_neg*Slope + Intercept
ggplot(data=BEGI_PTz[["SLOW"]])+
  geom_line(aes(datetimeMT, DTW_m))+
  geom_point(aes(datetimeMT, DTW_beeper_m), size=3, color="red")
ggplot(data=BEGI_PTz[["SLOW"]])+ geom_line(aes(datetimeMT, DTW_m*-1))+ylim(c(-3,.5))

# SLOC
plot(BEGI_PTz[["SLOC"]]$DTW_beeper_m ~ BEGI_PTz[["SLOC"]]$SensorDepth_m_CBC_sm_neg)
m.SLOC = lm(BEGI_PTz[["SLOC"]]$DTW_beeper_m ~ BEGI_PTz[["SLOC"]]$SensorDepth_m_CBC_sm_neg)
abline(m.SLOC)
summary(m.SLOC)
cf <- coef(m.SLOC)
Intercept <- cf[1]
Slope <- cf[2]
BEGI_PTz[["SLOC"]]$DTW_m = BEGI_PTz[["SLOC"]]$SensorDepth_m_CBC_sm_neg*Slope + Intercept
ggplot(data=BEGI_PTz[["SLOC"]])+
  geom_line(aes(datetimeMT, DTW_m))+
  geom_point(aes(datetimeMT, DTW_beeper_m), size=3, color="red")
ggplot(data=BEGI_PTz[["SLOC"]])+ geom_line(aes(datetimeMT, DTW_m*-1))+ylim(c(-3,.5))

#### save finalized DTW data ####

# save as list
saveRDS(BEGI_PTz, "data_clean/DTW_compiled/BEGI_PTz_DTW.rds")

# save as dataframe
BEGI_PT_DTW_all = rbind(BEGI_PTz[["VDOW"]],BEGI_PTz[["VDOS"]],BEGI_PTz[["SLOW"]],BEGI_PTz[["SLOC"]])
saveRDS(BEGI_PT_DTW_all, "data_clean/DTW_compiled/BEGI_PT_DTW_all.rds")

#### add discharge data from Rio Grande ####

# USGS station: Rio Grande at Isleta Lakes NR Isleta, NM - 08330875
NM_retrieve_usgs_data <- function(start_date, end_date, site_no = "08330875", p_code = "00060") {
  #Retrieve the USGS discharge data as an instantaneous (uv) data type.
  usgs_data <- readNWISuv(siteNumbers = site_no, parameterCd = p_code, startDate = start_date, endDate = end_date)
  #Rename columns to more user-friendly names.
  usgs_data <- renameNWISColumns(usgs_data)
}
#retrieve data
NM_USGS <- NM_retrieve_usgs_data("2023-09-15", "2024-12-03")
(attributes(NM_USGS))
# reformat for my needs
# discharge is retrieved as cubic feet per second. Below I convert to L/sec
NM_USGS_2 = data.frame(datetimeMT = NM_USGS$dateTime, Q_Lsec = (NM_USGS$Flow_Inst)*28.3168)
NM_USGS_2$datetimeMT = force_tz(NM_USGS_2$datetimeMT, tzone="US/Mountain")
tz(NM_USGS_2$datetimeMT)
# round time to nearest 15 min - lubridate::round_date(x, "15 minutes") 
NM_USGS_2$datetimeMT<- lubridate::round_date(NM_USGS_2$datetimeMT, "15 minutes") 

# plot to check
ggplot(NM_USGS_2, aes(datetimeMT,Q_Lsec)) +
  xlab("") +
  ylab("Q (L/sec)") +
  geom_line()

# add to clean time stamps
time <- data.frame(
  datetimeMT = seq.POSIXt(
    from = ISOdatetime(2023,09,15,0,0,0, tz = "US/Mountain"),
    to = ISOdatetime(2024,12,03,0,0,0, tz= "US/Mountain"),
    by = "15 min" ))
NM_USGS_3 = left_join(time, NM_USGS_2, by="datetimeMT")
# interpolate USGS data when it goes to longer time intervals
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling
# Make univariate zoo time series #
ts.temp<-read.zoo(NM_USGS_3, index.column=1, format="%Y-%m-%d %H:%M:%S", tz="US/Mountain")
# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)
# Apply NA interpolation method
ts.filled = na.spline(ts.temp, na.rm = T, maxgap = 4*6)
plot(ts.filled)
# remove leading tail that interpolated poorly
ts.filled[c(1:24)]=NA
#par(mfrow=c(1,1)) # reset plotting window
# revert back to df
ts.filled = as.data.frame(ts.filled)
ts.filled$datetimeMT = NM_USGS_3$datetimeMT
names(ts.filled) = c("Q_Lsec","datetimeMT")
NM_USGS_4 = ts.filled
# check NAs that are left
sum(is.na(NM_USGS_3$Q_Lsec))
sum(is.na(NM_USGS_4$Q_Lsec))

# join to well data
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for(i in siteIDz){
  BEGI_PTz[[i]]$Q_Lsec=NULL
  BEGI_PTz[[i]] = left_join(BEGI_PTz[[i]], NM_USGS_4, by=c("datetimeMT"))
}
BEGI_PT_DTW_all = rbind(BEGI_PTz[["VDOW"]],BEGI_PTz[["VDOS"]],BEGI_PTz[["SLOW"]],BEGI_PTz[["SLOC"]])
BEGI_PT_DTW_all = BEGI_PT_DTW_all %>%
  group_by(datetimeMT, siteID, wellID) %>%
  summarise_all(mean, na.rm = TRUE)%>% 
  mutate_all(~ifelse(is.nan(.), NA, .))
BEGI_PT_DTW_all$siteID = as.factor(BEGI_PT_DTW_all$siteID)
BEGI_PT_DTW_all$wellID = as.factor(BEGI_PT_DTW_all$wellID)

# plot to check
ggplot(BEGI_PT_DTW_all, aes(datetimeMT, Q_Lsec, color=wellID)) +
  xlab("") +
  ylab("Q (L/sec)") +
  geom_line() + geom_path()+ 
  facet_grid(~siteID)
ggplot(BEGI_PT_DTW_all, aes(datetimeMT, DTW_m, color=wellID)) +
  xlab("") +
  ylab("DTW") +
  geom_line() +
  facet_grid(~siteID)

#### save data with Rio Grande discharge included ####

# save as list
saveRDS(BEGI_PTz, "data_clean/DTW_compiled/BEGI_PTz_DTW.rds")

# save as dataframe
saveRDS(BEGI_PT_DTW_all, "data_clean/DTW_compiled/BEGI_PT_DTW_all.rds")

#### re-add data and fill gap in VDOW ####

# there is a large datgap in VDOW from 2023-10-20 08:15:00 to 2024-02-06 10:30:00. This well is very physically close to VDOS and the data looks extremely similar other than a small difference in depth below the surface. The R-sq of their linear relationship is 0.9891. I therefore think it's appropriate to use VDOS to predict VDOW and fill the gap. 

DTW = readRDS("data_clean/DTW_compiled/BEGI_PTz_DTW.rds")
DTW_df = readRDS("data_clean/DTW_compiled/BEGI_PT_DTW_all.rds")

# plot VDO wells
ggplot(DTW_df[DTW_df$siteID=="VDO",], aes(datetimeMT, DTW_m*-1, color=wellID)) +
  xlab("") +
  ylab("Water Depth Below Surface (m)")+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_line(key_glyph = "timeseries",linewidth=1,alpha=0.75) +
  facet_grid(rows=vars(siteID))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))+
  scale_color_viridis(discrete = TRUE, option = "D")

# define relationship between VDOS and VDOW
plot(DTW_df$DTW_m[DTW_df$wellID=="VDOW"] ~ DTW_df$DTW_m[DTW_df$wellID=="VDOS"])
m.VDOW = lm(DTW_df$DTW_m[DTW_df$wellID=="VDOW"] ~ DTW_df$DTW_m[DTW_df$wellID=="VDOS"])
abline(m.VDOW)
summary(m.VDOW)
cf <- coef(m.VDOW)
Intercept <- cf[1]
Slope <- cf[2]

# predict data gap
DTW_df$DTW_m[DTW_df$wellID=="VDOW" 
             & DTW_df$datetimeMT >= as.POSIXct("2023-10-20 08:15:00", tz="US/Mountain")
             & DTW_df$datetimeMT <= as.POSIXct("2024-02-06 10:30:00", tz="US/Mountain")] = # length=10478
  DTW_df$DTW_m[DTW_df$wellID=="VDOS"
               & DTW_df$datetimeMT >= as.POSIXct("2023-10-20 08:15:00", tz="US/Mountain")
               & DTW_df$datetimeMT <= as.POSIXct("2024-02-06 10:30:00", tz="US/Mountain")] *Slope + Intercept # length=10478

# plot to check
ggplot(DTW_df[DTW_df$siteID=="VDO"
              & DTW_df$datetimeMT >= as.POSIXct("2023-10-10 08:15:00", tz="US/Mountain")
              & DTW_df$datetimeMT <= as.POSIXct("2024-02-12 10:30:00", tz="US/Mountain"),], 
       aes(datetimeMT, DTW_m*-1, color=wellID)) +
  xlab("") +
  ylab("Water Depth Below Surface (m)")+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_vline(xintercept=as.POSIXct("2023-10-20 08:15:00", tz="US/Mountain"), linetype = 'dashed') +
  geom_vline(xintercept=as.POSIXct("2024-02-06 10:30:00", tz="US/Mountain"), linetype = 'dashed') +
  geom_line(key_glyph = "timeseries",linewidth=1,alpha=0.75) +
  facet_grid(rows=vars(siteID))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))+
  scale_color_viridis(discrete = TRUE, option = "D")

# replace in list version of data

DTW[["VDOW"]]$DTW_m = DTW_df$DTW_m[DTW_df$wellID=="VDOW"]

#### save data with VDOW gap filled ####

# save as list
saveRDS(DTW, "data_clean/DTW_compiled/BEGI_PTz_DTW.rds")

# save as dataframe
saveRDS(DTW_df, "data_clean/DTW_compiled/BEGI_PT_DTW_all.rds")

#
#### plot all final DTW data together ####

DTW_df = readRDS("DTW_compiled/BEGI_PT_DTW_all.rds")

Q = 
  ggplot(DTW_df, aes(datetimeMT, Q_Lsec)) +
  xlab("") +
  ylab("Q (L/sec)") +
  geom_line(linewidth=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 20))

DTW = 
  ggplot(DTW_df, aes(datetimeMT, DTW_m*-1, color=wellID)) +
  xlab("") +
  ylab("Water Depth Below Surface (m)")+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_line(key_glyph = "timeseries",linewidth=1,alpha=0.75) +
  facet_grid(rows=vars(siteID))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))+
  scale_color_viridis(discrete = TRUE, option = "D")

#grid.arrange(Q, DTW, nrow = 2)

Q_DTW = Q+ DTW+ plot_layout(ncol = 1, widths = c(1,.84), heights=c(1,2))
ggsave("plots/RGdischarge_allwellsDTW.png", Q_DTW, width=11,height=8, units="in")

#### plot 48 hr periods ####

# I noticed in the 16_tscluster.R analysis that a lot of 48 hr periods have more than 2 peaks/troughs in depth to water, which is unexpected for an ET signal. I am checking this here to see if it is "real" or not.

# load dtw data
DTW_df = readRDS("DTW_compiled/BEGI_PT_DTW_all.rds")


# load DO event data
dat = readRDS("DTW_compiled/event_dtw.rds")
dat[,193] # these are the start of each DO event
# name events and make names into row names
dat$ename = paste("e", c(1:59), sep="")
rownames(dat) = dat$ename
# save date/time stamps of events separately 
times = dat[,193:194]
dat[,193:194] = NULL
# raw data
matplot((t(dat))[,6], type = "l")
# the first >2 peak event is in row 6
times[6,]

SLOCe6 = DTW_df[DTW_df$wellID=="SLOC" &
                  DTW_df$datetimeMT <= as.POSIXct("2023-11-17 20:15:00", tz="US/Mountain")&
                  DTW_df$datetimeMT > as.POSIXct("2023-11-15 20:15:00", tz="US/Mountain"),]

par(mfrow=c(2,1), mar=c(2,2,2,2))
plot(SLOCe6$datetimeMT, SLOCe6$SensorDepth_m_C, main="PT raw-ish sensor depth")
plot(SLOCe6$datetimeMT, SLOCe6$DTW_m, main="Depth to water")
plot(SLOCe6$datetimeMT, SLOCe6$Q_Lsec, main="Rio Grande Q")
