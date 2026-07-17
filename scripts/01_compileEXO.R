#### read me ####

# the purpose of this script is to compile and plot EXO1 files from the Webster Lab BEGI project,
# temperature correct fDOM data, and convert it to DOC

# NOTE: It is STRONGLY recommended that you delete all your local EXO1 files from the last time you downloaded them from google drive, then use the script below to import them anew EACH TIME you run this script. 
# As some point, I should write this into the script!
# DO NOT push raw EXO1 files to the github repo! there are too many to push all at once. THe purpose of the google drive is to handle all these files, whereas github handles the script :)

#### libraries ####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)


#### load data from google drive ####

# NOTE: It is STRONGLY recommended that you delete all your local EXO1 files from the last time you downloaded them from google drive, then use the script below to import them anew EACH TIME you run this script. 
# As some point, I should write this into the script!
# DO NOT push raw EXO1 files to the github repo! there are too many to push all at once. THe purpose of the google drive is to handle all these files, whereas github handles the script :)

ls_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1qsjKPD5T4opFas37clgFX8CqV5R1PHxn")
2
for (file_id in ls_tibble$id) {
  try({googledrive::drive_download(as_id(file_id))})
}
# add overwrite = TRUE if for some reason you want to replace files previously downloaded. 



#### load and stitch EXO data ####

# import data
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
BEGI_EXOz = list()
for(i in siteIDz){
  file_list <- list.files(recursive=F, pattern=paste(i, ".csv", sep=""))
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
#### format dates ####

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
#### Check variable names ####
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

#### save and re-add burst-compiled files ####

saveRDS(BEGI_EXO.stz, "EXO_compiled/BEGI_EXO.stz.rds")
rm(list = ls())
BEGI_EXO.stz = readRDS("EXO_compiled/BEGI_EXO.stz.rds")



#### stitch in water level data ####

BEGI_EXO.stza = BEGI_EXO.stz
rm(BEGI_EXO.stz)

# get data from googledrive
beeper_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1L5ywkdYUOxhE3GPm7vbMiwgObOyn3awF")
2
googledrive::drive_download(as_id(beeper_tibble$id[beeper_tibble$name=="BEGI_beeper"]), overwrite = TRUE,
                            path="googledrive/BEGI_beeper.csv")
beeper = read.csv("googledrive/BEGI_beeper.csv")

# format date/times
# beeper$time[is.na(beeper$time)] = "12:00"
# beeper$datetimeMT = as.POSIXct(
#   paste(beeper$date, beeper$time, sep=" "), 
#   "%Y-%m-%d %H:%M", tz="US/Mountain")
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

#### complete timeseries with all possible time stamps ####

# there is  randomly a datapoint from the year 2072 in the VDOS dataset. Removing any years that are way off here:
BEGI_EXO.stza[["VDOS"]][BEGI_EXO.stza[["VDOS"]]$datetimeMT>as.POSIXct("2025-01-01 01:00:00"),] = NA

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

#### save and re-add clean time series data ####

saveRDS(BEGI_EXO.ts, "EXO_compiled/BEGI_EXO.ts.rds")
rm(list = ls())
BEGI_EXO.ts = readRDS("EXO_compiled/BEGI_EXO.ts.rds")


#### temp correction of fdom WITHOUT service times removed ####
# get data from googledrive
tempcal_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1u4yAZIoqYC2d1BSkt8iG5IT3lPUp3ALo")
2

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


#### data wrangling ####

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


# trim data frames

plot(sonde_3231_tempcal$fDOM_QSU ~ sonde_3231_tempcal$t)
sonde_3231_tempcal = sonde_3231_tempcal[-c(250:259),]
plot(sonde_3231_tempcal$fDOM_QSU ~ sonde_3231_tempcal$t)

plot(sonde_5009_tempcal$fDOM_QSU ~ sonde_5009_tempcal$t)
#sonde_5009_tempcal = sonde_5009_tempcal[-c(250:259),]
#plot(sonde_5009_tempcal$fDOM_QSU ~ sonde_5009_tempcal$t)

plot(sonde_3230_tempcal$fDOM_QSU ~ sonde_3230_tempcal$t)
sonde_3230_tempcal = sonde_3230_tempcal[-c(250:259),]
plot(sonde_3230_tempcal$fDOM_QSU ~ sonde_3230_tempcal$t)

plot(sonde_3229_tempcal$fDOM_QSU ~ sonde_3229_tempcal$t)
sonde_3229_tempcal = sonde_3229_tempcal[-c(1:70),]
plot(sonde_3229_tempcal$fDOM_QSU ~ sonde_3229_tempcal$t)

#

#### Define linear relationships and rhos ####

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

#### save RDS of temp-corrected fdom WITHOUT servicing times removed ####
saveRDS(BEGI_EXO.ts, "EXO_compiled/BEGI_EXOz.ts.tc.rds")


#### import DOC data to convert fDOM to DOC ####
#get DOC data from google drive
doc_tibble <- googledrive::as_id("https://drive.google.com/drive/folders/1zdzsIXO5LIzbcg2RzfE4mz3dBKvBmrO-")

doc <- googledrive::drive_ls(path = doc_tibble, type = "xlsx")
2

#import DOC data without values removed (complete DOC data)
googledrive::drive_download(file = doc$id[doc$name=="NPOC-TN_2025-06-09_BEGI-Matrix-Spikes.xlsx"], 
                            path = "NPOC-TN_2025-06-09_BEGI-Matrix-Spikes.xlsx",
                            overwrite = T)
docdata <- read_xlsx("NPOC-TN_2025-06-09_BEGI-Matrix-Spikes.xlsx", sheet = 8)

## wrangle DOC data ##

names(docdata)[names(docdata) == 'Conc (mg/L)'] <- 'NPOC'
names(docdata)[names(docdata) == 'Matrix Spike'] <- 'MatrixSpike'

#filter by well
docdata <- docdata %>%
  spread (Well, NPOC)

# filter docdata to df of each well
#VDOW
docVDOW <- data.frame(docdata$Sample,
                      docdata$VDOW,
                      docdata$MatrixSpike)
docVDOW <- na.omit(docVDOW)

#VDOS
docVDOS <- data.frame(docdata$Sample,
                      docdata$VDOS,
                      docdata$MatrixSpike)
docVDOS <- na.omit(docVDOS)

#SLOC
docSLOC <- data.frame(docdata$Sample,
                      docdata$SLOC,
                      docdata$MatrixSpike)
docSLOC <- na.omit(docSLOC)

#SLOW
docSLOW <- data.frame(docdata$Sample,
                      docdata$SLOW,
                      docdata$MatrixSpike)
docSLOW <- na.omit(docSLOW)


#### Partition/clean fDOM data ####
#add a column to label each chunk as Matrix spike

#VDOW#
BEGI_EXO.ts[["VDOW"]] <- BEGI_EXO.ts[["VDOW"]] %>%
  mutate(MatrixSpike = case_when(
    datetimeMT >= as.POSIXct("2025-06-10 13:45:00") & datetimeMT <= as.POSIXct("2025-06-10 14:08:00") ~ 0,
    datetimeMT >= as.POSIXct("2025-06-10 14:34:00") & datetimeMT <= as.POSIXct("2025-06-10 14:49:00") ~ 0.5,
    datetimeMT >= as.POSIXct("2025-06-10 15:13:00") & datetimeMT <= as.POSIXct("2025-06-10 15:27:00") ~ 1,
    datetimeMT >= as.POSIXct("2025-06-10 15:48:00") & datetimeMT <= as.POSIXct("2025-06-10 16:14:00") ~ 2,
    datetimeMT >= as.POSIXct("2025-06-10 16:36:00") & datetimeMT <= as.POSIXct("2025-06-10 17:03:00") ~ 5,
    datetimeMT >= as.POSIXct("2025-06-10 17:23:00") & datetimeMT <= as.POSIXct("2025-06-10 17:45:00") ~ 10,
  ))

#VDOS#
BEGI_EXO.ts[["VDOS"]] <- BEGI_EXO.ts[["VDOS"]] %>%
  mutate(MatrixSpike = case_when(
    datetimeMT >= as.POSIXct("2025-06-10 13:55:00") & datetimeMT <= as.POSIXct("2025-06-10 14:14:00") ~ 0,
    datetimeMT >= as.POSIXct("2025-06-10 14:36:00") & datetimeMT <= as.POSIXct("2025-06-10 14:51:00") ~ 0.5,
    datetimeMT >= as.POSIXct("2025-06-10 15:15:00") & datetimeMT <= as.POSIXct("2025-06-10 15:29:00") ~ 1,
    datetimeMT >= as.POSIXct("2025-06-10 15:52:00") & datetimeMT <= as.POSIXct("2025-06-10 16:17:00") ~ 2,
    datetimeMT >= as.POSIXct("2025-06-10 16:37:00") & datetimeMT <= as.POSIXct("2025-06-10 17:06:00") ~ 5,
    datetimeMT >= as.POSIXct("2025-06-10 17:25:00") & datetimeMT <= as.POSIXct("2025-06-10 17:47:00") ~ 10,
  ))

#SLOC#
BEGI_EXO.ts[["SLOC"]] <- BEGI_EXO.ts[["SLOC"]] %>%
  mutate(MatrixSpike = case_when(
    datetimeMT >= as.POSIXct("2025-06-10 13:59:00") & datetimeMT <= as.POSIXct("2025-06-10 14:17:00") ~ 0,
    datetimeMT >= as.POSIXct("2025-06-10 14:40:00") & datetimeMT <= as.POSIXct("2025-06-10 14:53:00") ~ 0.5,
    datetimeMT >= as.POSIXct("2025-06-10 15:17:00") & datetimeMT <= as.POSIXct("2025-06-10 15:32:00") ~ 1,
    datetimeMT >= as.POSIXct("2025-06-10 15:59:00") & datetimeMT <= as.POSIXct("2025-06-10 16:21:00") ~ 2,
    datetimeMT >= as.POSIXct("2025-06-10 16:39:00") & datetimeMT <= as.POSIXct("2025-06-10 17:08:00") ~ 5,
    datetimeMT >= as.POSIXct("2025-06-10 17:27:00") & datetimeMT <= as.POSIXct("2025-06-10 17:49:00") ~ 10,
  ))

#SLOW#
BEGI_EXO.ts[["SLOW"]] <- BEGI_EXO.ts[["SLOW"]] %>%
  mutate(MatrixSpike = case_when(
    datetimeMT >= as.POSIXct("2025-06-10 14:04:00") & datetimeMT <= as.POSIXct("2025-06-10 14:20:00") ~ 0,
    datetimeMT >= as.POSIXct("2025-06-10 14:43:00") & datetimeMT <= as.POSIXct("2025-06-10 14:57:00") ~ 0.5,
    datetimeMT >= as.POSIXct("2025-06-10 15:20:00") & datetimeMT <= as.POSIXct("2025-06-10 15:33:00") ~ 1,
    datetimeMT >= as.POSIXct("2025-06-10 16:01:00") & datetimeMT <= as.POSIXct("2025-06-10 16:24:00") ~ 2,
    datetimeMT >= as.POSIXct("2025-06-10 16:40:00") & datetimeMT <= as.POSIXct("2025-06-10 17:12:00") ~ 5,
    datetimeMT >= as.POSIXct("2025-06-10 17:28:00") & datetimeMT <= as.POSIXct("2025-06-10 17:51:00") ~ 10,
  ))

#filter each matrix spike group to only include first 50% of datapoints, take average
#VDOW#
VDOW_mean_fdom <- BEGI_EXO.ts[["VDOW"]] %>%
  filter(!is.na(MatrixSpike)) %>%
  group_by(MatrixSpike) %>%
  arrange(datetimeMT, .by_group = TRUE) %>%
  mutate(
    row_num = row_number(),
    group_size = n(),
    cutoff = floor(group_size / 2)  
  ) %>%
  filter(row_num <= cutoff) %>%
  summarise(mean_fDOM = mean(fDOM.QSU.mn, na.rm = TRUE), .groups = "drop") %>%
  deframe()  


#VDOS#
VDOS_mean_fdom <- BEGI_EXO.ts[["VDOS"]] %>%
  filter(!is.na(MatrixSpike)) %>%
  group_by(MatrixSpike) %>%
  arrange(datetimeMT, .by_group = TRUE) %>%
  mutate(
    row_num = row_number(),
    group_size = n(),
    cutoff = floor(group_size / 2)
  ) %>%
  filter(row_num <= cutoff) %>%
  summarise(mean_fDOM = mean(fDOM.QSU.mn, na.rm = TRUE), .groups = "drop") %>%
  deframe()  

#SLOC#
SLOC_mean_fdom <- BEGI_EXO.ts[["SLOC"]] %>%
  filter(!is.na(MatrixSpike)) %>%
  group_by(MatrixSpike) %>%
  arrange(datetimeMT, .by_group = TRUE) %>%
  mutate(
    row_num = row_number(),
    group_size = n(),
    cutoff = floor(group_size / 2)  
  ) %>%
  filter(row_num <= cutoff) %>%
  summarise(mean_fDOM = mean(fDOM.QSU.mn, na.rm = TRUE), .groups = "drop") %>%
  deframe()  

#SLOW#
SLOW_mean_fdom <- BEGI_EXO.ts[["SLOW"]] %>%
  filter(!is.na(MatrixSpike)) %>%
  group_by(MatrixSpike) %>%
  arrange(datetimeMT, .by_group = TRUE) %>%
  mutate(
    row_num = row_number(),
    group_size = n(),
    cutoff = floor(group_size / 2)  
  ) %>%
  filter(row_num <= cutoff) %>%
  summarise(mean_fDOM = mean(fDOM.QSU.mn, na.rm = TRUE), .groups = "drop") %>%
  deframe()  


#### combined doc/fdom df for each well ####
#complete DOC data
#VDOW#
docVDOW$fdom <- VDOW_mean_fdom[match(docVDOW$docdata.MatrixSpike,names(VDOW_mean_fdom))]
#docVDOW <- docVDOW[-c(3,5),] #removes 5 and 10 mg/L 
#VDOS#
docVDOS$fdom <- VDOS_mean_fdom[match(docVDOS$docdata.MatrixSpike,names(VDOS_mean_fdom))]
#docVDOS <- docVDOS[-c(3,5),] #removes 5 and 10 mg/L
#SLOC#
docSLOC$fdom <- SLOC_mean_fdom[match(docSLOC$docdata.MatrixSpike,names(SLOC_mean_fdom))]
#docSLOC <- docSLOC[-c(3,5),] #removes 5 and 10 mg/L
#SLOW#
docSLOW$fdom <- SLOW_mean_fdom[match(docSLOW$docdata.MatrixSpike,names(SLOW_mean_fdom))]
#docSLOW <- docSLOW[-c(3,5),] #removes 5 and 10 mg/L



#### linear regression fdom2doc ####
#VDOW#
plot(docVDOW$docdata.VDOW, docVDOW$fdom,
     xlab = "NPOC (VDOW)",
     ylab = "fDOM",
     main = "fDOM vs NPOC")
m.VDOW <- lm(fdom ~ docdata.VDOW, data = docVDOW)
abline(m.VDOW, col = "blue", lwd = 2)
summary(m.VDOW) #R2 = 0.68 with 10 mg/L removed

#VDOS#
plot(docVDOS$docdata.VDOS, docVDOS$fdom,
     xlab = "NPOC (VDOS)",
     ylab = "fDOM",
     main = "fDOM vs NPOC")
m.VDOS <- lm(fdom ~ docdata.VDOS, data = docVDOS)
abline(m.VDOS, col = "blue", lwd = 2)
summary(m.VDOS)

#SLOC#
plot(docSLOC$docdata.SLOC, docSLOC$fdom,
     xlab = "NPOC (SLOC)",
     ylab = "fDOM",
     main = "fDOM vs NPOC")
m.SLOC <- lm(fdom ~ docdata.SLOC, data = docSLOC)
abline(m.SLOC, col = "blue", lwd = 2)
summary(m.SLOC)

#SLOW#
plot(docSLOW$docdata.SLOW, docSLOW$fdom,
     xlab = "NPOC (SLOW)",
     ylab = "fDOM",
     main = "fDOM vs NPOC")
m.SLOW <- lm(fdom ~ docdata.SLOW, data = docSLOW)
abline(m.SLOW, col = "blue", lwd = 2)
summary(m.SLOW)

# none of the relationships strong enough to convert fDOM to DOC




#### remove servicing times from data ####

# get data from googledrive
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

# remove rows with no exact times
servicetimes = service[!is.na(service$datetimeMT),]

## make list of times of each service sequence, adding 6 hours to the end of each

# VDOW
VDOW_servicedates = unique(servicetimes$date[servicetimes$location=="VDOW"])
VDOW_servicetimes = list()
# for(i in c(1:length(VDOW_servicedates))){
#   VDOW_servicetimes[[i]] = seq(
#     from = servicetimes$datetimeMT[
#       servicetimes$location=="VDOW" & servicetimes$observation=="removed" & servicetimes$date==VDOW_servicedates[i]],
#     to = (servicetimes$datetimeMT[
#       servicetimes$location=="VDOW" & servicetimes$observation=="deployed" & servicetimes$date==VDOW_servicedates[i]])+(60*60*6),
#     by = "15 min")
# }
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
# for(i in c(1:length(VDOS_servicedates))){
#   VDOS_servicetimes[[i]] = seq(
#     from = servicetimes$datetimeMT[
#       servicetimes$location=="VDOS" & servicetimes$observation=="removed" & servicetimes$date==VDOS_servicedates[i]],
#     to = servicetimes$datetimeMT[
#       servicetimes$location=="VDOS" & servicetimes$observation=="deployed" & servicetimes$date==VDOS_servicedates[i]]+(60*60*6),
#     by = "5 min")
# }
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
SLOC_servicetimes[[31]] = seq(
  from = as.POSIXct("2024-06-28 14:00", tz="US/Mountain"),
  to = as.POSIXct("2024-07-02 13:45", tz="US/Mountain")+(60*60*6),
  by = "15 min")
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
SLOW_servicetimes[[33]] = seq(
  from = as.POSIXct("2024-04-17 18:15", tz="US/Mountain"),
  to = as.POSIXct("2024-04-19 17:30", tz="US/Mountain")+(60*60*6),
  by = "15 min")
SLOW_servicetimes[[34]] = seq(
  from = as.POSIXct("2023-11-03 14:15:00", tz="US/Mountain"),
  to = as.POSIXct("2023-11-03 15:00:00", tz="US/Mountain")+(60*60*6),
  by = "15 min")
# compile
SLOW_servicetimes_vector = do.call("c", SLOW_servicetimes)


## remove EXO data from servicing times
BEGI_EXO.or = BEGI_EXO.ts
# VDOW
BEGI_EXO.or[["VDOW"]][2:26] [BEGI_EXO.or[["VDOW"]]$datetimeMT %in% VDOW_servicetimes_vector,] = NA
# VDOS
BEGI_EXO.or[["VDOS"]][2:26] [BEGI_EXO.or[["VDOS"]]$datetimeMT %in% VDOS_servicetimes_vector,] = NA
# SLOC
BEGI_EXO.or[["SLOC"]][2:26] [BEGI_EXO.or[["SLOC"]]$datetimeMT %in% SLOC_servicetimes_vector,] = NA
# SLOW
BEGI_EXO.or[["SLOW"]][2:26] [BEGI_EXO.or[["SLOW"]]$datetimeMT %in% SLOW_servicetimes_vector,] = NA





#### save and re-add data with servicing times removed ####

saveRDS(BEGI_EXO.or, "EXO_compiled/BEGI_EXO.or.rds")
#rm(list = ls())
BEGI_EXO.or = readRDS("EXO_compiled/BEGI_EXO.or.rds")



#### remove obvious out of water and faulting readings ####

BEGI_EXO.or2 = BEGI_EXO.or

siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for(i in siteIDz){
  # remove out of water readings
  BEGI_EXO.or2[[i]][2:26] [!is.na(BEGI_EXO.or2[[i]]$ODO.mg.L.mn) & 
                             BEGI_EXO.or2[[i]]$ODO.mg.L.mn > 20,] = NA
  # remove fault readings
  BEGI_EXO.or2[[i]][2:26] [!is.na(BEGI_EXO.or2[[i]]$SpCond.µS.cm.mn) & 
                             BEGI_EXO.or2[[i]]$SpCond.µS.cm.mn < 2,] = NA
  BEGI_EXO.or2[[i]][2:26] [!is.na(BEGI_EXO.or2[[i]]$Temp..C.mn) & 
                             BEGI_EXO.or2[[i]]$Temp..C.mn < -10,] = NA
  BEGI_EXO.or2[[i]][2:26] [!is.na(BEGI_EXO.or2[[i]]$Temp..C.mn) & 
                             BEGI_EXO.or2[[i]]$Temp..C.mn > 40,] = NA
  BEGI_EXO.or2[[i]][2:26] [!is.na(BEGI_EXO.or2[[i]]$Turbidity.FNU.mn) & 
                             BEGI_EXO.or2[[i]]$Turbidity.FNU.mn < -10,] = NA
  BEGI_EXO.or2[[i]][2:26] [!is.na(BEGI_EXO.or2[[i]]$fDOM.QSU.mn) & 
                             BEGI_EXO.or2[[i]]$fDOM.QSU.mn < 5,] = NA
}

#### save and re-add data with out of water and faulting readings removed ####

saveRDS(BEGI_EXO.or2, "EXO_compiled/BEGI_EXO.or2.rds")
#rm(list = ls())
BEGI_EXO.or2 = readRDS("EXO_compiled/BEGI_EXO.or2.rds")




#### get service times and sunrise/sunset for plotting ####

# service dates

service.VDOW = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="VDOW"]
service.VDOS = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="VDOS"]
service.SLOC = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="SLOC"]
service.SLOW = servicetimes$datetimeMT[servicetimes$observation=="removed" & servicetimes$location=="SLOW"]

#### save service date/times to repo ####

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


#### plot to check - SLOC ####

## SLOC last month ##
tempdat = BEGI_EXO.or2[["SLOC"]][BEGI_EXO.or2[["SLOC"]]$datetimeMT < as.POSIXct("2024-09-15 00:00:01 MDT") &
                                   BEGI_EXO.or2[["SLOC"]]$datetimeMT > as.POSIXct("2024-08-15 00:00:01 MDT"),]

# Save plot 
jpeg("plots/SLOC_lastmonth.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service.SLOC), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.SLOC), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.SLOC), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service.SLOC), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service.SLOC), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.SLOC), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-.2,4))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.SLOC), col="red")
abline(h=2.2, col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()

## SLOC all ##
tempdat = BEGI_EXO.or2[["SLOC"]]

# Save plot 
jpeg("plots/SLOC.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service.SLOC), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.SLOC), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.SLOC), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service.SLOC), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service.SLOC), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.SLOC), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-.2,4))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.SLOC), col="red")
abline(h=2.2, col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()




#### plot to check - SLOW ####

## SLOW last month ##
tempdat = BEGI_EXO.or2[["SLOW"]][BEGI_EXO.or2[["SLOW"]]$datetimeMT < as.POSIXct("2024-09-15 00:00:01 MDT") &
                                   BEGI_EXO.or2[["SLOW"]]$datetimeMT > as.POSIXct("2024-08-15 00:00:01 MDT"),]

# Save plot 
jpeg("plots/SLOW_lastmonth.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service.SLOW), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.SLOW), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.SLOW), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service.SLOW), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service.SLOW), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.SLOW), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.SLOW), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()


## SLOW ##

# Save plot 
jpeg("plots/SLOW.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service.SLOW), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.SLOW), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")


plot(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service.SLOW), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service.SLOW), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n",ylim=c(0,120), ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service.SLOW), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-1,1300), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.SLOW), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.SLOW), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()



#### plot to check - VDOS ####


## VDOS last month ##
tempdat = BEGI_EXO.or2[["VDOS"]][BEGI_EXO.or2[["VDOS"]]$datetimeMT < as.POSIXct("2024-09-15 00:00:01 MDT") &
                                   BEGI_EXO.or2[["VDOS"]]$datetimeMT > as.POSIXct("2024-08-15 00:00:01 MDT"),]

# Save plot 
jpeg("plots/VDOS_lastmonth.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service.VDOS), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.VDOS), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.VDOS), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service.VDOS), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service.VDOS), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.VDOS), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-.2,4))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.VDOS), col="red")
abline(h=2.2, col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()

## VDOS all ##
tempdat = BEGI_EXO.or2[["VDOS"]]

# Save plot 
jpeg("plots/VDOS.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service.VDOS), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.VDOS), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.VDOS), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service.VDOS), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service.VDOS), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.VDOS), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-.2,4))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.VDOS), col="red")
abline(h=2.2, col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()

#### plot to check - VDOW ####


## VDOW last month ##
tempdat = BEGI_EXO.or2[["VDOW"]][BEGI_EXO.or2[["VDOW"]]$datetimeMT < as.POSIXct("2024-09-15 00:00:01 MDT") &
                                   BEGI_EXO.or2[["VDOW"]]$datetimeMT > as.POSIXct("2024-08-15 00:00:01 MDT"),]

# Save plot 
jpeg("plots/VDOW_lastmonth.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service.VDOW), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.VDOW), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.VDOW), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service.VDOW), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service.VDOW), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.VDOW), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-.2,4))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.VDOW), col="red")
abline(h=2.2, col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()

## VDOW all ##
tempdat = BEGI_EXO.or2[["VDOW"]]

# Save plot 
jpeg("plots/VDOW.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service.VDOW), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.VDOW), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.VDOW), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service.VDOW), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service.VDOW), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.VDOW), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-.2,4))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service.VDOW), col="red")
abline(h=2.2, col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()




#
#### just well depths ####

# plot all together
plot.new()
par(mfrow=c(2,2), mar=c(4,4,2,1.5))

plot(ymd_hms(BEGI_EXO.or2[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["VDOS"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
points(ymd_hms(BEGI_EXO.or2[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["VDOS"]]$waterlevelbelowsurface_cm*-1),
       pch=20,col="black", xlab="", xaxt = "n", ylab="", ylim=c(-200, 10))
#abline(v=as.POSIXct(service), col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="VDO South")

plot(ymd_hms(BEGI_EXO.or2[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["VDOW"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
points(ymd_hms(BEGI_EXO.or2[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["VDOW"]]$waterlevelbelowsurface_cm*-1),
       pch=20,col="black", xlab="", xaxt = "n", ylab="", ylim=c(-200, 10))
#abline(v=as.POSIXct(service), col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="VDO West")

plot(ymd_hms(BEGI_EXO.or2[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOC"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
points(ymd_hms(BEGI_EXO.or2[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOC"]]$waterlevelbelowsurface_cm*-1),
       pch=20,col="black", xlab="", xaxt = "n", ylab="", ylim=c(-200, 10))
#abline(v=as.POSIXct(service), col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="SLO Center")

plot(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
points(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$waterlevelbelowsurface_cm*-1),
       pch=20,col="black", xlab="", xaxt = "n", ylab="", ylim=c(-200, 10))
#abline(v=as.POSIXct(service), col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="SLO West")


#### extra plots ####

# smoothed DO close up
BEGI_EXO.or2[["SLOC"]]$ODO.mg.L.mn_sm =
  c(rollmean(BEGI_EXO.or2[["SLOC"]]$ODO.mg.L.mn, 4, align="left"),
    NA,NA,NA)

BEGI_EXO.or2[["SLOC"]]$ODO.mg.L.mn_sm_bc =
  BEGI_EXO.or2[["SLOC"]]$ODO.mg.L.mn_sm +
  abs(min(BEGI_EXO.or2[["SLOC"]]$ODO.mg.L.mn_sm, na.rm = T))

BEGI_EXO.or2[["SLOC"]]$ODO.mg.L.mn_sm_bc_c = BEGI_EXO.or2[["SLOC"]]$ODO.mg.L.mn_sm_bc

# BEGI_EXO.or2[["SLOC"]]$ODO.mg.L.mn_sm_bc_c[BEGI_EXO.or2[["SLOC"]]$datetimeMT >
#                                              as.POSIXct("2023-11-01 08:00:00") &
#                                              BEGI_EXO.or2[["SLOC"]]$datetimeMT <
#                                              as.POSIXct("2023-12-09 15:00:00")   ] = NA
plot.new()
par(mfrow=c(1,1), mar=c(7,4,2,1.5))
plot(ymd_hms(BEGI_EXO.or2[["SLOC"]]$datetimeMT, tz="US/Mountain"),
     BEGI_EXO.or2[["SLOC"]]$ODO.mg.L.mn_sm_bc_c,
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(0,0.8), type="n", ylab="",
     xlim=c(as.POSIXct("2023-11-01"),as.POSIXct("2024-01-06")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or2[["SLOC"]]$datetimeMT, tz="US/Mountain"),
      BEGI_EXO.or2[["SLOC"]]$ODO.mg.L.mn_sm_bc_c,
      pch=20,col="black", xlab="", xaxt = "n", type="o")
#abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d %R", las=2)
title(main="Dissolved Oxygen (mg/L)")


### just latest DO and fDOM ###

# SLOC
plot.new()
par(mfrow=c(2,1), mar=c(7,4,2,1.5))
# DO
plot(ymd_hms(BEGI_EXO.or2[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOC"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,3), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or2[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOC"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")
# fDOM
plot(ymd_hms(BEGI_EXO.or2[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOC"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(40,100), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or2[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOC"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

# SLOW
plot.new()
par(mfrow=c(2,1), mar=c(7,4,2,1.5))
# DO
plot(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,3), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")
# fDOM
plot(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(40,100), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or2[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["SLOW"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

# VDOW
plot.new()
par(mfrow=c(2,1), mar=c(7,4,2,1.5))
# DO
plot(ymd_hms(BEGI_EXO.or2[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["VDOW"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,3), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or2[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["VDOW"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")
# fDOM
plot(ymd_hms(BEGI_EXO.or2[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["VDOW"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(40,100), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or2[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["VDOW"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")


# VDOS
plot.new()
par(mfrow=c(2,1), mar=c(7,4,2,1.5))
# DO
plot(ymd_hms(BEGI_EXO.or2[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["VDOS"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,3), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or2[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["VDOS"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")
# fDOM
plot(ymd_hms(BEGI_EXO.or2[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["VDOS"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(40,100), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or2[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or2[["VDOS"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or2[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")


