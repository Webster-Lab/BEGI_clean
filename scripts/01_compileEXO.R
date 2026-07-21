#### READ ME ####

# the purpose of this script is to compile and plot EXO1 files from the Webster Lab BEGI project (data collected 2023-2024), temperature correct fDOM data, and compare it to grab samples of DOC.

# Requirements: Google Drive access to the Webster Lab BEGI Drive folders for raw EXO1 files, manual water level readings (well soundings or "beeps"), fDOM temperature experiment data, DOC data, and sonde servicing times. 
#     ---->>>> These should all be replaced with reference to files on HydroShare using an API - see draft HydroShare block below.

# Outputs for downstream use:
# 1. Iterative lists of dataframes (saved as RDS files) of data after each major processing step, with the final one used in all downstream workflows being "BEGI_EXO.or2.RDS".
# 2. CSV files that contain all the servicing times at each well (e.g., "service.SLOC.csv". These are when sondes were taken out of water and are used in future scripts. 
# 3. Timeseries plots of all focal data streams from EXO1 sondes and manual water depth readings (from "BEGI_EXO.or2.RDS"). The plots aren't used downstream, but are useful for reference. 

#
#### Libraries ####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)
library(readxl)

#
#### Check/make file structure ####

# make sure output folders exist before anything tries to write to them
dir.create("EXO_compiled", recursive = TRUE, showWarnings = FALSE)
dir.create("plots", recursive = TRUE, showWarnings = FALSE)
dir.create("googledrive", recursive = TRUE, showWarnings = FALSE)

#### Clear all files from the googledrive folder to start fresh

# NOTE: DO NOT push raw EXO1 files to the github repo! there are too many to push all at once. THe purpose of the google drive is to handle all these files, whereas github handles the script :)

googledrive_files <- list.files("googledrive", full.names = TRUE, recursive = TRUE)
if (length(googledrive_files) > 0) {
  file.remove(googledrive_files)
}

#### Load raw EXO1 data from Google Drive ####

# ID link
ls_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1TRJ8E4O8bFly-n9GeViOTRR8I9Wtq3Fr")
2 # authenticate
# download for googledrive folder
for (i in seq_len(nrow(ls_tibble))) {
  try({
    googledrive::drive_download(
      as_id(ls_tibble$id[i]),
      path = file.path("googledrive", ls_tibble$name[i]),
      overwrite = TRUE
    )
  })
}

#### DRAFT --->>> load raw EXO1 files from HydroShare instead of Google Drive ####
#
# Once the raw EXO1 files are archived on HydroShare (https://www.hydroshare.org), this block is meant to replace the googledrive block above, removing the dependency on Drive OAuth access for anyone re-running this pipeline. We should do the 

# Uses HSClientR (in-development, not on CRAN yet):
#   remotes::install_github("program--/HSClientR")
# Docs: https://hsclientr.justinsingh.me/  |  API: https://www.hydroshare.org/hsapi/
#
# This is a scaffold, not a working integration yet - it's wrapped in `if (FALSE)`
# so it doesn't run. To activate it:
#   1. Deposit the raw EXO1 files as a HydroShare resource and fill in
#      HS_RESOURCE_ID below with that resource's ID (the string after
#      /resource/ in its URL).
#   2. Confirm the exact function signatures against the HSClientR docs -
#      the package is labeled "experimental" and its file-download API
#      (hs_files(), download_request()) is less documented than
#      googledrive's, so treat the calls below as a starting point to verify,
#      not a guarantee they work as written.
#   3. Delete (or stop wrapping in `if (FALSE)`) once verified, and remove/retire
#      the googledrive block above.

if (FALSE) {
  # install.packages("remotes"); remotes::install_github("program--/HSClientR")
  library(HSClientR)
  
  HS_RESOURCE_ID <- "TODO-fill-in-hydroshare-resource-id"
  
  # hs_auth() only needed for private resources / POST-PUT-DELETE calls;
  # GET calls (listing/downloading from a public resource) may not require it
  hs_auth(set_headers = TRUE)
  
  # list the files contained in the resource
  hs_resource_files <- hs_files(id = HS_RESOURCE_ID)
  
  # download each file into the working directory (mirrors the googledrive
  # loop above) - confirm the right accessor/argument names against
  # hs_resource_files's actual structure and hs_files()/download_request()'s
  # documented arguments before relying on this
  for (f in hs_resource_files$file_name) {
    try({
      download_request(id = HS_RESOURCE_ID, file = f, destfile = f)
    })
  }
}

#### Load and stitch EXO data ####

# import data
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
BEGI_EXOz = list()
for(i in siteIDz){
  file_list <- list.files("googledrive", recursive=F, pattern=paste(i, ".csv", sep=""), full.names=TRUE)
  BEGI_EXOz[[i]] = lapply(file_list, read.csv, 
                          stringsAsFactors=FALSE, skip=8,header=T,
                          fileEncoding="utf-8") # this line makes it such that if there are any offending utf-16 encodings, it will show the offending file in the error message. If any utf-16 files are found, be sure to fix them in the Google Drive, not just your locally saved file!!
}

# use a set of column names as a template and match columns in all other files to that one. Note that this drops columns like Depth where the sensor isn't avialable on all sondes
universalnames = c("Date..MM.DD.YYYY.","Time..HH.mm.ss.","Time..Fract..Sec.","Site.Name","Cond.µS.cm","fDOM.QSU","fDOM.RFU","nLF.Cond.µS.cm","ODO...sat","ODO...local","ODO.mg.L","Sal.psu",  "SpCond.µS.cm","TDS.mg.L","Turbidity.FNU","TSS.mg.L","Temp..C","Battery.V","Cable.Pwr.V")
for(i in siteIDz){
  for(n in 1:length(BEGI_EXOz[[i]])){
    BEGI_EXOz[[i]][[n]] = 
      BEGI_EXOz[[i]][[n]] [, intersect(universalnames, names(BEGI_EXOz[[i]][[n]] )), drop=FALSE]
  }
}

# bind files within sites into one dataframe per site
for(i in siteIDz){
  BEGI_EXOz[[i]] = do.call(plyr::rbind.fill, BEGI_EXOz[[i]])
}


#
#### Format dates

for(i in siteIDz){
  # put date and time in same column
  BEGI_EXOz[[i]]$datetime = paste( BEGI_EXOz[[i]]$Date..MM.DD.YYYY.,  BEGI_EXOz[[i]]$Time..HH.mm.ss., sep = " ")
  # convert to POIXct and set timezone
  BEGI_EXOz[[i]]$datetimeMT<-as.POSIXct( BEGI_EXOz[[i]]$datetime, 
                                         format = "%m/%d/%Y %H:%M:%S",
                                         tz="US/Mountain")
  # replace two digit years that are converted incorrectly
  BEGI_EXOz[[i]]$year = year(BEGI_EXOz[[i]]$datetimeMT)
  BEGI_EXOz[[i]]$year[BEGI_EXOz[[i]]$year==0023] = 2023
  BEGI_EXOz[[i]]$year[BEGI_EXOz[[i]]$year==0024] = 2024
  year(BEGI_EXOz[[i]]$datetimeMT) = BEGI_EXOz[[i]]$year
}


#
#### Check variable names
#check the variable order for each sonde and edit names if necessary

names(BEGI_EXOz[["VDOW"]]) == names(BEGI_EXOz[["VDOS"]])
names(BEGI_EXOz[["VDOW"]]) == names(BEGI_EXOz[["SLOW"]])
names(BEGI_EXOz[["VDOW"]]) == names(BEGI_EXOz[["SLOC"]])



#### Compile bursts within 1 min ####

# make sure all columns with numeric data data are numeric
BEGI_EXOz <- lapply(BEGI_EXOz, function(x) {x[5:19] <- lapply(x[5:19], as.numeric);x})

# get means and standard deviations of numeric burst values
BEGI_EXO.stz = list()
for(i in siteIDz){
  min<-round_date(BEGI_EXOz[[i]]$datetimeMT, "minute") # note rounding instead of using the function cut()!! cut was what was causing our memory issues!!
  BEGI_EXO.stz[[i]] <- as.data.frame(as.list(aggregate(cbind(Cond.µS.cm, fDOM.QSU, fDOM.RFU,
                                                             nLF.Cond.µS.cm,
                                                             ODO...sat,ODO.mg.L,
                                                             Sal.psu,SpCond.µS.cm,
                                                             TDS.mg.L,Turbidity.FNU,TSS.mg.L,Temp..C,
                                                             Battery.V,Cable.Pwr.V) 
                                                       ~ min, data=BEGI_EXOz[[i]], na.action=na.pass, FUN=function(x) c(mn=mean(x), SD=sd(x)))))
  BEGI_EXO.stz[[i]]$datetimeMT<-as.POSIXct(BEGI_EXO.stz[[i]]$min, "%Y-%m-%d %H:%M:%S", tz="US/Mountain")
}

#### Save and re-add burst-compiled files

saveRDS(BEGI_EXO.stz, "EXO_compiled/BEGI_EXO.stz.rds")
rm(list = ls())
BEGI_EXO.stz = readRDS("EXO_compiled/BEGI_EXO.stz.rds")



#### Stitch in manual water level data ("beeps") ####

BEGI_EXO.stza = BEGI_EXO.stz
rm(BEGI_EXO.stz)

# get data from googledrive
beeper_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1J6iYi6RLIC-9ao8Tgo7twiPB_Afq3o9H")

googledrive::drive_download(as_id(beeper_tibble$id[beeper_tibble$name=="BEGI_beeper"]), overwrite = TRUE,
                            path="googledrive/BEGI_beeper.csv")
beeper = read.csv("googledrive/BEGI_beeper.csv")

# format date/times
beeper$date = as.Date(beeper$date)

# format siteID to be same as wellID
beeper$siteID = beeper$wellID

# join
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for (i in siteIDz){
  BEGI_EXO.stza[[i]]$date = as.Date(BEGI_EXO.stza[[i]]$datetimeMT, tz="US/Mountain")
  BEGI_EXO.stza[[i]]$siteID = i
  BEGI_EXO.stza[[i]] = left_join(BEGI_EXO.stza[[i]], beeper, by=c("date", "siteID"))
}

saveRDS(BEGI_EXO.stza, "EXO_compiled/BEGI_EXO.stza.rds")
rm(list = ls())
BEGI_EXO.stza = readRDS("EXO_compiled/BEGI_EXO.stza.rds")

#### Complete timeseries with all possible time stamps ####

# there is  randomly a datapoint from the year 2072 in the VDOS dataset. Removing any years that are way off here:
BEGI_EXO.stza[["VDOS"]][BEGI_EXO.stza[["VDOS"]]$datetimeMT>as.POSIXct("2025-01-01 01:00:00", tz="US/Mountain"),] = NA

max(c(BEGI_EXO.stza[["VDOW"]]$datetimeMT, BEGI_EXO.stza[["VDOS"]]$datetimeMT, BEGI_EXO.stza[["SLOC"]]$datetimeMT, BEGI_EXO.stza[["SLOW"]]$datetimeMT), na.rm = T)

min(c(BEGI_EXO.stza[["VDOW"]]$datetimeMT, BEGI_EXO.stza[["VDOS"]]$datetimeMT, BEGI_EXO.stza[["SLOC"]]$datetimeMT, BEGI_EXO.stza[["SLOW"]]$datetimeMT), na.rm = T)

time <- data.frame(
  datetimeMT = seq.POSIXt(
    from = ISOdatetime(2023,09,15,0,0,0, tz = "US/Mountain"),
    to = ISOdatetime(2024,09,04,0,0,0, tz= "US/Mountain"),
    by = "15 min" ))
# 34081 rows

# round time to nearest 15 min - lubridate::round_date(x, "15 minutes") 
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for(i in siteIDz){
  BEGI_EXO.stza[[i]]$datetimeMT<- lubridate::round_date(BEGI_EXO.stza[[i]]$datetimeMT, "15 minutes") 
}

# join to clean time stamps
BEGI_EXO.ts = BEGI_EXO.stza
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for(i in siteIDz){
  BEGI_EXO.ts[[i]] <- left_join(time, BEGI_EXO.ts[[i]], by="datetimeMT")
}


# check for duplicate time stamps
any(duplicated(BEGI_EXO.ts[["VDOW"]]$datetimeMT))
any(duplicated(BEGI_EXO.ts[["VDOS"]]$datetimeMT))
any(duplicated(BEGI_EXO.ts[["SLOW"]]$datetimeMT)) # true
any(duplicated(BEGI_EXO.ts[["SLOC"]]$datetimeMT))

# examine duplicate rows
dup = BEGI_EXO.ts[["SLOW"]][duplicated(BEGI_EXO.ts[["SLOW"]]$datetimeMT, fromLast=TRUE),]
dup.2 = BEGI_EXO.ts[["SLOW"]][duplicated(BEGI_EXO.ts[["SLOW"]]$datetimeMT, fromLast=FALSE),]
dup.all = rbind(dup, dup.2)
# remove duplicate row where a reading was taken on the 15 min + 1 sec for some reason (2024-05-14 15:01:00)
BEGI_EXO.ts[["SLOW"]] = BEGI_EXO.ts[["SLOW"]][! BEGI_EXO.ts[["SLOW"]]$min %in% dup.2$min,]
#reran dup check, and dup is gone!

#### Save and re-add clean time series data

saveRDS(BEGI_EXO.ts, "EXO_compiled/BEGI_EXO.ts.rds")
rm(list = ls())
BEGI_EXO.ts = readRDS("EXO_compiled/BEGI_EXO.ts.rds")


#### Temp correction of fdom ####

# get data from googledrive
tempcal_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1ToqSa027D2EkL7iXGhxlrcW-9bp5wwsd")


# import info from sonde 3231 experiment 
googledrive::drive_download(as_id(tempcal_tibble$id[tempcal_tibble$name=="20241204_3231_fdom.csv"]), overwrite = TRUE,
                            path="googledrive/20241204_3231_fdom.csv")
tempcal1 = read.csv("googledrive/20241204_3231_fdom.csv", skip=8)
tempcal1_sn = read.csv("googledrive/20241204_3231_fdom.csv", skip=7, head=FALSE)
tempcal1_sn = tempcal1_sn[1:2,]

# import info from sonde 5009 experiment 
googledrive::drive_download(as_id(tempcal_tibble$id[tempcal_tibble$name=="20241204_5009_fdom.csv"]), overwrite = TRUE,
                            path="googledrive/20241204_5009_fdom.csv")
tempcal2 = read.csv("googledrive/20241204_5009_fdom.csv", skip=8)
tempcal2_sn = read.csv("googledrive/20241204_5009_fdom.csv", skip=7, head=FALSE)
tempcal2_sn = tempcal2_sn[1:2,]


#### data wrangling

tempcal1 = tempcal1[ , which(names(tempcal1) %in% c("fDOM.QSU", "fDOM.QSU.1",
                                                    "Temp..C","Temp..C.1"))]
names(tempcal1) = c("fDOM.QSU_23C101705", "fDOM.QSU_23C101758",
                    "Temp.C_23G102566","Temp.C_23G102567")

tempcal2 = tempcal2[ , which(names(tempcal2) %in% c("fDOM.QSU", "fDOM.QSU.1",
                                                    "Temp..C","Temp..C.1"))]
names(tempcal2) = c("fDOM.QSU_23C101759", "fDOM.QSU_23C101760",
                    "Temp.C_23G102560","Temp.C_23G102568")

tempcal2 = tempcal2[1:259,]

tempcalall = cbind(tempcal1,tempcal2)

# make data frames

sonde_3231_tempcal = as.data.frame(cbind(tempcalall[,"Temp.C_23G102566"],tempcalall[,"fDOM.QSU_23C101705"]))
names(sonde_3231_tempcal) = c("temp_C","fDOM_QSU")
sonde_3231_tempcal$t = c(1:259)

sonde_5009_tempcal = as.data.frame(cbind(tempcalall[,"Temp.C_23G102568"],tempcalall[,"fDOM.QSU_23C101760"]))
names(sonde_5009_tempcal) = c("temp_C","fDOM_QSU")

sonde_3230_tempcal = as.data.frame(cbind(tempcalall[,"Temp.C_23G102567"],tempcalall[,"fDOM.QSU_23C101758"]))
names(sonde_3230_tempcal) = c("temp_C","fDOM_QSU")

sonde_3229_tempcal = as.data.frame(cbind(tempcalall[,"Temp.C_23G102560"],tempcalall[,"fDOM.QSU_23C101759"]))
names(sonde_3229_tempcal) = c("temp_C","fDOM_QSU")


# using visual inspection, trim data frames to exclude data points from the sensor settling or being out of water

plot(sonde_3231_tempcal$fDOM_QSU ~ sonde_3231_tempcal$t)
sonde_3231_tempcal = sonde_3231_tempcal[-c(250:259),]
plot(sonde_3231_tempcal$fDOM_QSU ~ sonde_3231_tempcal$t)

plot(sonde_5009_tempcal$fDOM_QSU ~ sonde_5009_tempcal$t)
# does not need trimming

plot(sonde_3230_tempcal$fDOM_QSU ~ sonde_3230_tempcal$t)
sonde_3230_tempcal = sonde_3230_tempcal[-c(250:259),]
plot(sonde_3230_tempcal$fDOM_QSU ~ sonde_3230_tempcal$t)

plot(sonde_3229_tempcal$fDOM_QSU ~ sonde_3229_tempcal$t)
sonde_3229_tempcal = sonde_3229_tempcal[-c(1:70),]
plot(sonde_3229_tempcal$fDOM_QSU ~ sonde_3229_tempcal$t)

#

#### Define linear relationships and rhos

# sonde_3231 #
plot(sonde_3231_tempcal$fDOM_QSU ~ sonde_3231_tempcal$temp_C)
m.3231 = lm(sonde_3231_tempcal$fDOM_QSU ~ sonde_3231_tempcal$temp_C)
abline(m.3231)
summary(m.3231)

# sonde_5009 #
plot(sonde_5009_tempcal$fDOM_QSU ~ sonde_5009_tempcal$temp_C)
m.5009 = lm(sonde_5009_tempcal$fDOM_QSU ~ sonde_5009_tempcal$temp_C)
abline(m.5009)
summary(m.5009)

# sonde_3230 #
plot(sonde_3230_tempcal$fDOM_QSU ~ sonde_3230_tempcal$temp_C)
m.3230 = lm(sonde_3230_tempcal$fDOM_QSU ~ sonde_3230_tempcal$temp_C)
abline(m.3230)
summary(m.3230)

# sonde_3229 #
plot(sonde_3229_tempcal$fDOM_QSU ~ sonde_3229_tempcal$temp_C)
m.3229 = lm(sonde_3229_tempcal$fDOM_QSU ~ sonde_3229_tempcal$temp_C)
abline(m.3229)
summary(m.3229)

# define rhos

sondeID = c("s3231", "s5009", "s3230", "s3229")
siteID = c("SLOW", "SLOC", "VDOS", "VDOW")
Tref = c(25, 25, 25, 25) #Tref (reference temperature) in fDOM correction is a standard reference temp. Most people seem to use 25 deg C (Watras et al., 2011, Saraceno et al., 2017...).
rho = c(m.3231$coefficients[2]/m.3231$coefficients[1],
        m.5009$coefficients[2]/m.5009$coefficients[1],
        m.3230$coefficients[2]/m.3230$coefficients[1],
        m.3229$coefficients[2]/m.3229$coefficients[1])
rhos = data.frame(siteID, sondeID, rho, Tref)

#set rho (after 'Define linear relationships and rhos' section )
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for (i in siteIDz){
  BEGI_EXO.ts[[i]]$siteID = i
  BEGI_EXO.ts[[i]] = left_join(BEGI_EXO.ts[[i]], rhos, by=c("siteID"))
}

#apply temp correction
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for (i in siteIDz){
  BEGI_EXO.ts[[i]]$siteID = i
  BEGI_EXO.ts[[i]]$fDOM.QSU.mn.Tc = BEGI_EXO.ts[[i]]$fDOM.QSU.mn / ( 1 + (BEGI_EXO.ts[[i]]$rho * (BEGI_EXO.ts[[i]]$Temp..C.mn - BEGI_EXO.ts[[i]]$Tref)))
}

#plot to check
# SLOC
tempdat = BEGI_EXO.ts[["SLOC"]][BEGI_EXO.ts[["SLOC"]]$datetimeMT < as.POSIXct("2024-09-15 00:00:01 MDT") &
                                  BEGI_EXO.ts[["SLOC"]]$datetimeMT > as.POSIXct("2024-08-15 00:00:01 MDT"),]
plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="l")#,ylim=c(22.5,24.5))
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn.Tc),
      pch=20,col="blue", xlab="", xaxt = "n", type="l")

#### Save RDS of temp-corrected fdom 
saveRDS(BEGI_EXO.ts, "EXO_compiled/BEGI_EXOz.ts.tc.rds")
rm(list = ls())
BEGI_EXO.ts.tc = readRDS("EXO_compiled/BEGI_EXOz.ts.tc.rds")

#
#### Remove servicing times from data ####

# get data from googledrive
service_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1J6iYi6RLIC-9ao8Tgo7twiPB_Afq3o9H")
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

# remove rows with no exact times
servicetimes = service[!is.na(service$datetimeMT),]

## make list of times of each service sequence, adding 6 hours to the end of each

# VDOW
VDOW_servicedates = unique(servicetimes$date[servicetimes$location=="VDOW"])
VDOW_servicetimes = list()
for(i in seq_along(VDOW_servicedates)) {
  from_val <- servicetimes$datetimeMT[servicetimes$location=="VDOW" & servicetimes$observation=="removed" & servicetimes$date==VDOW_servicedates[i]]
  to_val   <- servicetimes$datetimeMT[servicetimes$location=="VDOW" & servicetimes$observation=="deployed" & servicetimes$date==VDOW_servicedates[i]]
  
  if(length(from_val) != 1 | length(to_val) != 1) {
    warning(paste("Skipping i =", i, "- from:", length(from_val), "to:", length(to_val)))
    next
  }
  
  VDOW_servicetimes[[i]] <- seq(from = from_val, to = to_val + 60*60*6, by = "15 min")
}
VDOW_servicetimes_vector = do.call("c", VDOW_servicetimes)

# VDOS
VDOS_servicedates = unique(servicetimes$date[servicetimes$location=="VDOS"])
VDOS_servicetimes = list()
for(i in seq_along(VDOS_servicedates)) {
  from_val <- servicetimes$datetimeMT[servicetimes$location=="VDOS" & servicetimes$observation=="removed" & servicetimes$date==VDOS_servicedates[i]]
  to_val   <- servicetimes$datetimeMT[servicetimes$location=="VDOS" & servicetimes$observation=="deployed" & servicetimes$date==VDOS_servicedates[i]]
  
  if(length(from_val) != 1 | length(to_val) != 1) {
    warning(paste("Skipping i =", i, "- from:", length(from_val), "to:", length(to_val)))
    next
  }
  
  VDOS_servicetimes[[i]] <- seq(from = from_val, to = to_val + 60*60*6, by = "15 min")
}
VDOS_servicetimes_vector = do.call("c", VDOS_servicetimes)

# SLOC
SLOC_servicedates = unique(servicetimes$date[servicetimes$location=="SLOC"])
# remove dates where it was taken from the field since this function doesn't work for across-day gaps
SLOC_servicedates = SLOC_servicedates[!SLOC_servicedates %in% c("2024-06-28","2024-07-02")]
SLOC_servicetimes = list()
for(i in seq_along(SLOC_servicedates)) {
  from_val <- servicetimes$datetimeMT[servicetimes$location=="SLOC" & servicetimes$observation=="removed" & servicetimes$date==SLOC_servicedates[i]]
  to_val   <- servicetimes$datetimeMT[servicetimes$location=="SLOC" & servicetimes$observation=="deployed" & servicetimes$date==SLOC_servicedates[i]]
  
  if(length(from_val) != 1 | length(to_val) != 1) {
    warning(paste("Skipping i =", i, "- from:", length(from_val), "to:", length(to_val)))
    next
  }
  
  SLOC_servicetimes[[i]] <- seq(from = from_val, to = to_val + 60*60*6, by = "15 min")
}
# add missing date/times
SLOC_servicetimes <- append(SLOC_servicetimes, list(
  seq(from = as.POSIXct("2024-06-28 14:00", tz="US/Mountain"),
      to   = as.POSIXct("2024-07-02 13:45", tz="US/Mountain") + (60*60*6),
      by   = "15 min")
))
SLOC_servicetimes_vector = do.call("c", SLOC_servicetimes)

# SLOW
SLOW_servicedates = unique(servicetimes$date[servicetimes$location=="SLOW"])
SLOW_servicedates = SLOW_servicedates[! SLOW_servicedates %in% as.Date(c("2024-04-17","2024-04-19"))]
SLOW_servicetimes = list()
for(i in seq_along(SLOW_servicedates)) {
  from_val <- servicetimes$datetimeMT[servicetimes$location=="SLOW" & servicetimes$observation=="removed" & servicetimes$date==SLOW_servicedates[i]]
  to_val   <- servicetimes$datetimeMT[servicetimes$location=="SLOW" & servicetimes$observation=="deployed" & servicetimes$date==SLOW_servicedates[i]]
  
  if(length(from_val) != 1 | length(to_val) != 1) {
    warning(paste("Skipping i =", i, "- from:", length(from_val), "to:", length(to_val)))
    next
  }
  
  SLOW_servicetimes[[i]] <- seq(from = from_val, to = to_val + 60*60*6, by = "15 min")
}
# add missing date/times
SLOW_servicetimes <- append(SLOW_servicetimes, list(
  seq(from = as.POSIXct("2024-04-17 18:15", tz="US/Mountain"),
      to   = as.POSIXct("2024-04-19 17:30", tz="US/Mountain") + (60*60*6),
      by   = "15 min"),
  seq(from = as.POSIXct("2023-11-03 14:15:00", tz="US/Mountain"),
      to   = as.POSIXct("2023-11-03 15:00:00", tz="US/Mountain") + (60*60*6),
      by   = "15 min")
))
# compile
SLOW_servicetimes_vector = do.call("c", SLOW_servicetimes)


## remove EXO data from servicing times
BEGI_EXO.or = BEGI_EXO.ts.tc

# columns that should NEVER be blanked out - everything else gets NA'd
id_cols <- c("min", "datetimeMT", "date", "time","siteID", "wellID", "sondeID", "rho", "Tref")

service_vectors <- list(
  VDOW = VDOW_servicetimes_vector,
  VDOS = VDOS_servicetimes_vector,
  SLOC = SLOC_servicetimes_vector,
  SLOW = SLOW_servicetimes_vector
)

siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for (i in siteIDz) {
  value_cols <- setdiff(names(BEGI_EXO.or[[i]]), id_cols)
  rows_in_service <- BEGI_EXO.or[[i]]$datetimeMT %in% service_vectors[[i]]
  BEGI_EXO.or[[i]][rows_in_service, value_cols] <- NA
}



#### Save and re-add data with servicing times removed

saveRDS(BEGI_EXO.or, "EXO_compiled/BEGI_EXO.or.rds")
rm(list = ls())
BEGI_EXO.or = readRDS("EXO_compiled/BEGI_EXO.or.rds")



#### Import DOC data to compare fDOM to DOC ####
#get DOC data from google drive
doc_tibble <- googledrive::as_id("https://drive.google.com/drive/folders/1J6iYi6RLIC-9ao8Tgo7twiPB_Afq3o9H")

doc <- googledrive::drive_ls(path = doc_tibble, type = "xlsx")
2

googledrive::drive_download(file = doc$id[doc$name=="240620_BEGI_Data.xlsx"],
                            path = "240620_BEGI_Data.xlsx",
                            overwrite = T)
docdata <- read_xlsx("240620_BEGI_Data.xlsx")

#clean up
names(docdata)[names(docdata) == 'Date_Collected'] <- 'date'
names(docdata)[names(docdata) == 'NPOC_mg_L'] <- 'NPOC'
names(docdata)[names(docdata) == 'TN_mg_L'] <- 'TN'

#filter by well
docdata <- docdata %>%
  spread (WellID, NPOC)
docdata$date <- as.Date(docdata$date)

#read in servicing data#
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

# remove rows with no exact times
servicetimes = service[!is.na(service$datetimeMT),]

# service dates
service.VDOW = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="VDOW"]
service.VDOS = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="VDOS"]
service.SLOC = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="SLOC"]
service.SLOW = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="SLOW"]

# filter docdata to df of each well
docVDOW <- data.frame(docdata$date,
                      docdata$Sample_ID,
                      docdata$TN,
                      docdata$VDOW)
docVDOW <- na.omit(docVDOW)
docVDOW <- docVDOW[-1,] #no 9/15 measurements

docVDOS <- data.frame(docdata$date,
                      docdata$Sample_ID,
                      docdata$TN,
                      docdata$VDOS)
docVDOS <- na.omit(docVDOS)
docVDOS <- docVDOS[-1,]

docSLOC <- data.frame(docdata$date,
                      docdata$Sample_ID,
                      docdata$TN,
                      docdata$SLOC)
docSLOC <- na.omit(docSLOC)
docSLOC <- docSLOC[-1,]

docSLOW <- data.frame(docdata$date,
                      docdata$Sample_ID,
                      docdata$TN,
                      docdata$SLOW)
docSLOW <- na.omit(docSLOW)
docSLOW <- docSLOW[-1,]


#VDOW
fDOM_df <- data.frame(
  datetimeMT = as.POSIXct(BEGI_EXO.or[["VDOW"]]$datetimeMT),
  date = as.Date(BEGI_EXO.or[["VDOW"]]$datetimeMT),
  fDOM = BEGI_EXO.or[["VDOW"]]$fDOM.QSU.mn.Tc)

#remove NAs to get post-service fdom
fDOM_df <- na.omit(fDOM_df)
#

# Index of last fDOM measurement before service datetime
prev_index <- findInterval(service.VDOW, fDOM_df$datetimeMT) -1
valid <- prev_index > 0

#Index of fDOM measurement AFTER service datetime (and after fdom measurements returned to "normal")
next_index <- findInterval(service.VDOW, fDOM_df$datetimeMT) +1

# Get matched times and values
matched_service_time <- service.VDOW[valid]
matched_fDOM_time <- fDOM_df$datetimeMT[prev_index[valid]]
matched_fDOM_vals <- fDOM_df$fDOM[prev_index[valid]]

npoc_vals <- docVDOW$docdata.VDOW[valid]

merged_df <- data.frame(
  service_time = matched_service_time,
  fDOM_time = matched_fDOM_time,
  NPOC = npoc_vals,
  fDOM = matched_fDOM_vals
)

plot(merged_df$NPOC, merged_df$fDOM,
     xlab = "NPOC (VDOW)",
     ylab = "fDOM (before sample)",
     main = "fDOM vs NPOC (preceeding fDOM measurement)")
m.VDOW <- lm(fDOM ~ NPOC, data = merged_df)
abline(m.VDOW, col = "blue", lwd = 2)
summary(m.VDOW)


#VDOS
fDOM_df <- data.frame(
  datetimeMT = as.POSIXct(BEGI_EXO.or[["VDOS"]]$datetimeMT),
  date = as.Date(BEGI_EXO.or[["VDOS"]]$datetimeMT),
  fDOM = BEGI_EXO.or[["VDOS"]]$fDOM.QSU.mn.Tc)
#remove NAs to get post-service fdom
fDOM_df <- na.omit(fDOM_df)
#

# Index of last fDOM measurement before service datetime
prev_index <- findInterval(service.VDOS, fDOM_df$datetimeMT) -1
valid <- prev_index > 0 #or next_index

#Index of fDOM measurement AFTER service datetime (and after fdom measurements returned to "normal")
next_index <- findInterval(service.VDOS, fDOM_df$datetimeMT) +1

# Get matched times and values
matched_service_time <- service.VDOS[valid]
matched_fDOM_time <- fDOM_df$datetimeMT[prev_index[valid]]
matched_fDOM_vals <- fDOM_df$fDOM[prev_index[valid]]

npoc_vals <- docVDOS$docdata.VDOS[valid]

merged_df <- data.frame(
  service_time = matched_service_time,
  fDOM_time = matched_fDOM_time,
  NPOC = npoc_vals,
  fDOM = matched_fDOM_vals
)

plot(merged_df$NPOC, merged_df$fDOM,
     xlab = "NPOC (VDOS)",
     ylab = "fDOM (before sample)",
     main = "fDOM vs NPOC (preceding fDOM measurement)")
m.VDOS <- lm(fDOM ~ NPOC, data = merged_df)
abline(m.VDOS, col = "blue", lwd = 2)
summary(m.VDOS)

#SLOC
fDOM_df <- data.frame(
  datetimeMT = as.POSIXct(BEGI_EXO.or[["SLOC"]]$datetimeMT),
  date = as.Date(BEGI_EXO.or[["SLOC"]]$datetimeMT),
  fDOM = BEGI_EXO.or[["SLOC"]]$fDOM.QSU.mn.Tc)
#remove NAs to get post-service fdom
fDOM_df <- na.omit(fDOM_df)
#

# Index of last fDOM measurement before service datetime
prev_index <- findInterval(service.SLOC, fDOM_df$datetimeMT) -1
valid <- prev_index > 0

#Index of fDOM measurement AFTER service datetime (and after fdom measurements returned to "normal")
next_index <- findInterval(service.SLOC, fDOM_df$datetimeMT) +1

# Get matched times and values
matched_service_time <- service.SLOC[valid]
matched_fDOM_time <- fDOM_df$datetimeMT[prev_index[valid]]
matched_fDOM_vals <- fDOM_df$fDOM[prev_index[valid]]

npoc_vals <- docSLOC$docdata.SLOC[valid]

merged_df <- data.frame(
  service_time = matched_service_time,
  fDOM_time = matched_fDOM_time,
  NPOC = npoc_vals,
  fDOM = matched_fDOM_vals
)

#remove outlier to see if R2 improves. it doesn't..
#merged_df <- merged_df[-23,]

plot(merged_df$NPOC, merged_df$fDOM,
     xlab = "NPOC (SLOC)",
     ylab = "fDOM (before sample)",
     main = "fDOM vs NPOC (preceding fDOM measurement)")
m.SLOC <- lm(fDOM ~ NPOC, data = merged_df)
abline(m.SLOC, col = "blue", lwd = 2)
summary(m.SLOC)

#SLOW
fDOM_df <- data.frame(
  datetimeMT = as.POSIXct(BEGI_EXO.or[["SLOW"]]$datetimeMT),
  date = as.Date(BEGI_EXO.or[["SLOW"]]$datetimeMT),
  fDOM = BEGI_EXO.or[["SLOW"]]$fDOM.QSU.mn.Tc)
#remove NAs to get post-service fdom
fDOM_df <- na.omit(fDOM_df)
#

# Index of last fDOM measurement before service datetime
prev_index <- findInterval(service.SLOW, fDOM_df$datetimeMT) -1
valid <- prev_index > 0

#Index of fDOM measurement AFTER service datetime (and after fdom measurements returned to "normal")
next_index <- findInterval(service.SLOW, fDOM_df$datetimeMT) +1

# Get matched times and values
matched_service_time <- service.SLOW[valid]
matched_fDOM_time <- fDOM_df$datetimeMT[prev_index[valid]]
matched_fDOM_vals <- fDOM_df$fDOM[prev_index[valid]]

npoc_vals <- docSLOW$docdata.SLOW[valid]

merged_df <- data.frame(
  service_time = matched_service_time,
  fDOM_time = matched_fDOM_time,
  NPOC = npoc_vals,
  fDOM = matched_fDOM_vals
)

plot(merged_df$NPOC, merged_df$fDOM,
     xlab = "NPOC (SLOW)",
     ylab = "fDOM (before sample)",
     main = "fDOM vs NPOC (preceding fDOM measurement)")
m.SLOW <- lm(fDOM ~ NPOC, data = merged_df)
abline(m.SLOW, col = "blue", lwd = 2)
summary(m.SLOW)

#### Remove obvious out of water and faulting readings ####

BEGI_EXO.or2 = BEGI_EXO.or

# columns that should NEVER be blanked out - everything else gets NA'd
id_cols <- c("min", "datetimeMT", "date", "time","siteID", "wellID", "sondeID", "rho", "Tref")

siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for (i in siteIDz) {
  value_cols <- setdiff(names(BEGI_EXO.or2[[i]]), id_cols)
  
  rows <- !is.na(BEGI_EXO.or2[[i]]$ODO.mg.L.mn) & BEGI_EXO.or2[[i]]$ODO.mg.L.mn > 20
  BEGI_EXO.or2[[i]][rows, value_cols] <- NA
  
  rows <- !is.na(BEGI_EXO.or2[[i]]$SpCond.µS.cm.mn) & BEGI_EXO.or2[[i]]$SpCond.µS.cm.mn < 2
  BEGI_EXO.or2[[i]][rows, value_cols] <- NA
  
  rows <- !is.na(BEGI_EXO.or2[[i]]$Temp..C.mn) & BEGI_EXO.or2[[i]]$Temp..C.mn < -10
  BEGI_EXO.or2[[i]][rows, value_cols] <- NA
  
  rows <- !is.na(BEGI_EXO.or2[[i]]$Temp..C.mn) & BEGI_EXO.or2[[i]]$Temp..C.mn > 40
  BEGI_EXO.or2[[i]][rows, value_cols] <- NA
  
  rows <- !is.na(BEGI_EXO.or2[[i]]$Turbidity.FNU.mn) & BEGI_EXO.or2[[i]]$Turbidity.FNU.mn < -10
  BEGI_EXO.or2[[i]][rows, value_cols] <- NA
  
  rows <- !is.na(BEGI_EXO.or2[[i]]$fDOM.QSU.mn) & BEGI_EXO.or2[[i]]$fDOM.QSU.mn < 5
  BEGI_EXO.or2[[i]][rows, value_cols] <- NA
}

#### Save and re-add data with out of water and faulting readings removed

saveRDS(BEGI_EXO.or2, "EXO_compiled/BEGI_EXO.or2.rds")
rm(list = ls())
BEGI_EXO.or2 = readRDS("EXO_compiled/BEGI_EXO.or2.rds")

#### Plot to check ####

#### Get service times and sunrise/sunset for plotting

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

# remove rows with no exact times
servicetimes = service[!is.na(service$datetimeMT),]


# service dates

service.VDOW = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="VDOW"]
service.VDOS = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="VDOS"]
service.SLOC = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="SLOC"]
service.SLOW = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="SLOW"]

# save service date/times to repo

write.csv(service.VDOW, "EXO_compiled/service.VDOW.csv")
write.csv(service.VDOS, "EXO_compiled/service.VDOS.csv")
write.csv(service.SLOC, "EXO_compiled/service.SLOC.csv")
write.csv(service.SLOW, "EXO_compiled/service.SLOW.csv")

# sunrise/sunset

suntimes = 
  getSunlightTimes(date = seq.Date(from = as.Date("2023-09-14"), to = as.Date("2024-09-5"), by = 1),
                   keep = c("sunrise", "sunset"),
                   lat = 34.9, lon = -106.7, tz = "US/Mountain")

pm.pts = suntimes$sunset[-(nrow(suntimes))]
am.pts = suntimes$sunrise[-1]



# custom plotting function of all data
plot_site_diagnostics <- function(data, service_times, file_path,
                                  ylim_waterlevel = NULL,
                                  ylim_odo = NULL,
                                  ylim_fdom = NULL,
                                  ylim_spcond = NULL,
                                  ylim_battery = NULL,
                                  battery_low_line = FALSE) {
  dt <- ymd_hms(data$datetimeMT, tz = "US/Mountain")
  
  jpeg(file_path, width = 12, height = 8, units = "in", res = 1000)
  plot.new()
  par(mfrow = c(4, 2), mar = c(4, 4, 2, 1.5))
  
  # Water depth below surface (cm)
  plot(dt, data$waterlevelbelowsurface_cm * -1,
       pch = 20, col = "black", xlab = "", xaxt = "n", type = "n", ylab = "",
       ylim = ylim_waterlevel)
  rect(xleft = pm.pts, xright = am.pts, ybottom = -350, ytop = 100, col = "lightgrey", lwd = 0)
  lines(dt, data$waterlevelbelowsurface_cm * -1,
        pch = 20, col = "black", xlab = "", xaxt = "n", type = "b")
  abline(v = as.POSIXct(service_times), col = "red")
  abline(h = 0, col = "green")
  axis.POSIXct(side = 1, at = cut(data$datetimeMT, breaks = "24 hours"), format = "%m-%d", las = 2)
  title(main = "Water depth below surface (cm)")
  
  # Turbidity (FNU)
  plot(dt, data$Turbidity.FNU.mn,
       pch = 20, col = "black", xlab = "", xaxt = "n", type = "n", ylab = "")
  rect(xleft = pm.pts, xright = am.pts, ybottom = -4, ytop = 2000, col = "lightgrey", lwd = 0)
  lines(dt, data$Turbidity.FNU.mn,
        pch = 20, col = "black", xlab = "", xaxt = "n", type = "o")
  abline(v = as.POSIXct(service_times), col = "red")
  axis.POSIXct(side = 1, at = cut(data$datetimeMT, breaks = "24 hours"), format = "%m-%d", las = 2)
  title(main = "Turbidity (FNU)")
  
  # Dissolved Oxygen (mg/L)
  plot(dt, data$ODO.mg.L.mn,
       pch = 20, col = "black", xlab = "", xaxt = "n", type = "n", ylab = "")
  rect(xleft = pm.pts, xright = am.pts, ybottom = -4, ytop = 100, col = "lightgrey", lwd = 0)
  lines(dt, data$ODO.mg.L.mn,
        pch = 20, col = "black", xlab = "", xaxt = "n", type = "o", ylim = ylim_odo)
  abline(v = as.POSIXct(service_times), col = "red")
  axis.POSIXct(side = 1, at = cut(data$datetimeMT, breaks = "24 hours"), format = "%m-%d", las = 2)
  title(main = "Dissolved Oxygen (mg/L)")
  
  # Temperature (deg C)
  plot(dt, data$Temp..C.mn,
       pch = 20, col = "black", xlab = "", xaxt = "n", type = "n", ylab = "")
  rect(xleft = pm.pts, xright = am.pts, ybottom = -4, ytop = 100, col = "lightgrey", lwd = 0)
  lines(dt, data$Temp..C.mn,
        pch = 20, col = "black", xlab = "", xaxt = "n", type = "o")
  abline(v = as.POSIXct(service_times), col = "red")
  axis.POSIXct(side = 1, at = cut(data$datetimeMT, breaks = "24 hours"), format = "%m-%d", las = 2)
  title(main = "Temperature (deg C)")
  
  # fDOM (QSU) - temp corrected
  plot(dt, data$fDOM.QSU.mn.Tc,
       pch = 20, col = "black", xlab = "", xaxt = "n", type = "n", ylab = "n",
       ylim = ylim_fdom)
  rect(xleft = pm.pts, xright = am.pts, ybottom = -4, ytop = 1000, col = "lightgrey", lwd = 0)
  lines(dt, data$fDOM.QSU.mn,
        pch = 20, col = "black", xlab = "", xaxt = "n", type = "o")
  abline(v = as.POSIXct(service_times), col = "red")
  axis.POSIXct(side = 1, at = cut(data$datetimeMT, breaks = "24 hours"), format = "%m-%d", las = 2)
  title(main = "fDOM (QSU) temp. corrected")
  
  # Specific Conductance (us/cm)
  plot(dt, data$SpCond.µS.cm.mn,
       pch = 20, col = "black", xlab = "", xaxt = "n", type = "n", ylab = "",
       ylim = ylim_spcond)
  rect(xleft = pm.pts, xright = am.pts, ybottom = -4, ytop = 2000, col = "lightgrey", lwd = 0)
  lines(dt, data$SpCond.µS.cm.mn,
        pch = 20, col = "black", xlab = "", xaxt = "n", type = "o")
  abline(v = as.POSIXct(service_times), col = "red")
  axis.POSIXct(side = 1, at = cut(data$datetimeMT, breaks = "24 hours"), format = "%m-%d", las = 2)
  title(main = "Specific Conductance (us/cm)")
  
  # Battery (volts)
  plot(dt, data$Battery.V.mn,
       pch = 20, col = "black", xlab = "", xaxt = "n", type = "n", ylab = "",
       ylim = ylim_battery)
  rect(xleft = pm.pts, xright = am.pts, ybottom = -4, ytop = 2000, col = "lightgrey", lwd = 0)
  lines(dt, data$Battery.V.mn,
        pch = 20, col = "black", xlab = "", xaxt = "n", type = "o")
  abline(v = as.POSIXct(service_times), col = "red")
  if (battery_low_line) abline(h = 2.2, col = "red")
  axis.POSIXct(side = 1, at = cut(data$datetimeMT, breaks = "24 hours"), format = "%m-%d", las = 2)
  title(main = "Battery (volts)")
  
  dev.off()
}
# custom plotting function of last month of data
last_month <- function(data) {
  data[data$datetimeMT < as.POSIXct("2024-09-15 00:00:01 MDT") &
         data$datetimeMT > as.POSIXct("2024-08-15 00:00:01 MDT"), ]
}

## SLOC ##
plot_site_diagnostics(last_month(BEGI_EXO.or2[["SLOC"]]), service.SLOC,
                      "plots/SLOC_lastmonth.jpg",
                      ylim_battery = c(-.2, 4), battery_low_line = TRUE)
plot_site_diagnostics(BEGI_EXO.or2[["SLOC"]], service.SLOC,
                      "plots/SLOC.jpg",
                      ylim_battery = c(-.2, 4), battery_low_line = TRUE)

## SLOW ##
plot_site_diagnostics(last_month(BEGI_EXO.or2[["SLOW"]]), service.SLOW,
                      "plots/SLOW_lastmonth.jpg")
plot_site_diagnostics(BEGI_EXO.or2[["SLOW"]], service.SLOW,
                      "plots/SLOW.jpg",
                      ylim_waterlevel = c(-200, 10), ylim_odo = c(-.4, 1),
                      ylim_fdom = c(0, 120), ylim_spcond = c(-1, 1300))

 ## VDOS ##
plot_site_diagnostics(last_month(BEGI_EXO.or2[["VDOS"]]), service.VDOS,
                      "plots/VDOS_lastmonth.jpg",
                      ylim_battery = c(-.2, 4), battery_low_line = TRUE)
plot_site_diagnostics(BEGI_EXO.or2[["VDOS"]], service.VDOS,
                      "plots/VDOS.jpg",
                      ylim_battery = c(-.2, 4), battery_low_line = TRUE)

## VDOW ##
plot_site_diagnostics(last_month(BEGI_EXO.or2[["VDOW"]]), service.VDOW,
                      "plots/VDOW_lastmonth.jpg",
                      ylim_battery = c(-.2, 4), battery_low_line = TRUE)
plot_site_diagnostics(BEGI_EXO.or2[["VDOW"]], service.VDOW,
                      "plots/VDOW.jpg",
                      ylim_battery = c(-.2, 4), battery_low_line = TRUE)


#### Clear all Google Drive files from local folder to end fresh ####

# NOTE: DO NOT push raw EXO1 files to the github repo! there are too many to push all at once. THe purpose of the google drive is to handle all these files, whereas github handles the script :)

googledrive_files <- list.files("googledrive", full.names = TRUE, recursive = TRUE)
if (length(googledrive_files) > 0) {
  file.remove(googledrive_files)
}

# now that your environment is cleaned up, now is a good time to commit, push/pull, and restart the R session to get ready for the next script in the workflow!