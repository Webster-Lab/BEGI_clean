#### READ ME ####

# The purpose of this script is to use manually measured groundwater depth data with pressure transducer data (HOBO U20s, hereafter "PT data") to create a continuous (every 15 min) dataset of depth to gw in the 4 wells used for the BEGI 2023-2024 study.

# PT data is first corrected to account for any jumps in data that occurred as a result of the cable length changing.
# PT data is then compared to manual readings of depth to groundwater (DTW) from sounding wells with a water level reader (aka "beeper"). The slope and intercept of the relationship is then used to convert PT data to DTW.

# In-water PT data was compensated for atmospheric pressure using the HOBO software wizard. Atmospheric pressure was recorded on site by a HOBO PT installed at the top of a well casing (out of water) for the dates/times 2023-10-20 12:30:00 to 2024-06-24 11:15:00. However, after 2024-06-24 11:15:00, the storage on the on-site in-air PT was exceeded and no more atmospheric pressure data is available on-site. Instead, we downloaded sea level pressure data from the Albuquerque airport (KABQ) from https://www.weather.gov/wrh/timeseries?site=KABQ&hourly=true, which is ~ 10 km northeast and 84 m in elevation higher than the site. We corrected this data from sea level to local atmospheric pressure using the equation [where the BP readings MUST be in mm Hg) is: True BP = [Corrected BP] – [2.5 * (Local Altitude in ft above sea level/100)]. Note that Inches of Hg x 25.4 = mm Hg]. We elected to use the airport data to compensate the entire in-water PT dataset to ensure consistency of the approach. In-water PT data was compensated using "option 1" in the HOBO software wizard, which compensates for the data only where the two datasets overlap, interpolating between points that do not exactly align. 

# Requirements: Google Drive access to the Webster Lab BEGI Drive folders for compensated PT data files.
#     ---->>>> This connection to Google Drive should be replaced with reference to files on HydroShare using an API - see draft HydroShare block below.

# Outputs for downstream use:
# 1. Iterative lists of dataframes (saved as RDS files) of water depth and PT data after each major processing step, with the final two used in all downstream workflows being "BEGI_PT_DTW_all.RDS" (a single dataframe) and "BEGI_PTz_DTW.RDS" (a list of 4 dataframes, one for each well).
# 2. A multi-panel timeseries plot of river discharge + depth to water in all 4 wells: "RGdischarge_allwellsDTW.png"

#
#### Libraries ####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)
library(dataRetrieval) # To download USGS discharge data
library(viridis)
library(gridExtra)
library(patchwork)

# turn off scientific notation
options(scipen=999)

#

#### Check/make file structure ####

# make sure output folders exist before anything tries to write to them
dir.create("DTW_compiled", recursive = TRUE, showWarnings = FALSE)
dir.create("plots", recursive = TRUE, showWarnings = FALSE)
dir.create("googledrive", recursive = TRUE, showWarnings = FALSE)

#### Clear all files from the googledrive folder to start fresh

googledrive_files <- list.files("googledrive", full.names = TRUE, recursive = TRUE)
if (length(googledrive_files) > 0) {
  file.remove(googledrive_files)
}

#
#### Load and wrangle PT data from Google Drive ####

ls_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/10Wp8MgiJdrgCNssj4Ig34y5hMM_EfRE3")
# authenticate 
2
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

#+++++++++ stitch together data files for each well
# import data
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
BEGI_PTz = list()
for(i in siteIDz){
  file_list <- list.files("googledrive", recursive=F, pattern=paste(i, "_correctedful.csv", sep=""), full.names=TRUE)
  BEGI_PTz[[i]] = lapply(file_list, read.csv, 
                         stringsAsFactors=FALSE, skip=1,header=T,
                         fileEncoding="utf-8")
}

# it looks like there was one instance of the datetime format getting changed to GMT0700 when the data was downloaded. All other data in VDOW and other sites is in GMT0600. The GMT/UTC minus 6 hours offset is used in the Mountain Time Zone when operating in Daylight Saving Time. I will convert this instance of GMT0700 to GMT0600 so that everything is in Mountain Daylight Savings Time, and then convert it all to R's "US/Mountain", which accounts for the time change. 
BEGI_PTz[["VDOW"]][[2]][["Date.Time..GMT.06.00"]] <- as.POSIXct(
  BEGI_PTz[["VDOW"]][[2]][["Date.Time..GMT.07.00"]],
  format = "%m/%d/%y %I:%M:%S %p",
  tz = "Etc/GMT+6"
)
BEGI_PTz[["VDOW"]][[2]][["Date.Time..GMT.07.00"]] <- NULL

# convert it all to R's "US/Mountain"
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

#
#### Complete timeseries with all possible time stamps ####

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
ggplot(data=BEGI_PTz.ts2[["VDOW"]], aes(datetimeMT, (SensorDepth_m)))+
  geom_line()+
  geom_point()
ggplot(data=BEGI_PTz.ts2[["VDOS"]], aes(datetimeMT, (SensorDepth_m)))+
  geom_line()+
  geom_point()
ggplot(data=BEGI_PTz.ts2[["SLOW"]], aes(datetimeMT, (SensorDepth_m)))+
  geom_line()+
  geom_point()
ggplot(data=BEGI_PTz.ts2[["SLOC"]], aes(datetimeMT, (SensorDepth_m)))+
  geom_line()+
  geom_point()

#### Remove obvious outliers ####

# get service date/times
service.VDOS = as.POSIXct(read.csv("EXO_compiled/service.VDOS.csv")[,2],tz="US/Mountain")
service.VDOW = as.POSIXct(read.csv("EXO_compiled/service.VDOW.csv")[,2],tz="US/Mountain")
service.SLOW = as.POSIXct(read.csv("EXO_compiled/service.SLOW.csv")[,2],tz="US/Mountain")
service.SLOC = as.POSIXct(read.csv("EXO_compiled/service.SLOC.csv")[,2],tz="US/Mountain")

# This function flags and removes "out of water" servicing artifacts: sudden jumps in SensorDepth_m that happen when the PT is physically removed from (or redeployed into) the well. Detection is confined to a buffer window around each known service event, so genuine rapid natural water-level changes elsewhere in the record aren't mistakenly flagged. Within that window, each point is compared to the rolling median of its neighbors rather than a rolling mean, since a median isn't dragged off by the jump itself the way a mean would be - it stays anchored to the "normal" surrounding readings.

flag_service_outliers <- function(data,
                                  datetime_col          = "datetimeMT",
                                  value_col             = "SensorDepth_m_C",
                                  source_col            = "SensorDepth_m",
                                  service_times,
                                  buffer_hours          = 12,
                                  roll_width            = 18,
                                  jump_threshold        = 0.05,
                                  global_jump_threshold = 0.15) {
  
  if (is.null(data[[value_col]])) {
    data[[value_col]] <- data[[source_col]]
  }
  
  dt  <- data[[datetime_col]]
  val <- data[[value_col]]
  
  in_service_window <- rep(FALSE, length(dt))
  for (st in service_times) {
    in_service_window <- in_service_window |
      (dt >= st - buffer_hours * 3600 & dt <= st + buffer_hours * 3600)
  }
  
  # trailing rolling median, excluding the point itself - each reading is compared only to what came BEFORE it
  roll_med_incl_self <- zoo::rollapply(val, width = roll_width, FUN = median,
                                       na.rm = TRUE, fill = NA, align = "right")
  roll_med <- c(NA, head(roll_med_incl_self, -1))  # shift by 1 so the window excludes the current point
  
  jump <- abs(val - roll_med)
  
  is_outlier_service <- in_service_window & !is.na(val) & !is.na(roll_med) &
    jump > jump_threshold
  
  is_outlier_global <- !in_service_window & !is.na(val) & !is.na(roll_med) &
    jump > global_jump_threshold
  
  is_outlier <- is_outlier_service | is_outlier_global
  
  data[[paste0(value_col, "_flagged")]] <- is_outlier
  data[[paste0(value_col, "_flag_reason")]] <- ifelse(
    is_outlier_service, "service_window",
    ifelse(is_outlier_global, "global_jump", NA)
  )
  data[[value_col]][is_outlier] <- NA
  data
}


# remove out of water outliers, apply outlier removal function to each site, and plot to check

# VDOW
BEGI_PTz.ts2[["VDOW"]]$SensorDepth_m_C = BEGI_PTz.ts2[["VDOW"]]$SensorDepth_m
# remove the first 9 real readings of VDOW's record (too early for the rolling-window method to have a baseline to compare against)
first_9 <- which(!is.na(BEGI_PTz.ts2[["VDOW"]]$SensorDepth_m_C))[1:9]
BEGI_PTz.ts2[["VDOW"]]$SensorDepth_m_C[first_9] <- NA
# remove other stray out of water readings
BEGI_PTz.ts2[["VDOW"]]$SensorDepth_m_C[BEGI_PTz.ts2[["VDOW"]]$SensorDepth_m_C < 1] <- NA
# apply rolling function
BEGI_PTz.ts2[["VDOW"]] <- flag_service_outliers(BEGI_PTz.ts2[["VDOW"]], service_times = service.VDOW)
# plot to check
dt      <- BEGI_PTz.ts2[["VDOW"]]$datetimeMT
cleaned <- BEGI_PTz.ts2[["VDOW"]]$SensorDepth_m_C
raw     <- BEGI_PTz.ts2[["VDOW"]]$SensorDepth_m
flagged <- BEGI_PTz.ts2[["VDOW"]]$SensorDepth_m_C_flagged
reason  <- BEGI_PTz.ts2[["VDOW"]]$SensorDepth_m_C_flag_reason
n_service <- sum(flagged & reason == "service_window", na.rm = TRUE)
n_global  <- sum(flagged & reason == "global_jump", na.rm = TRUE)
plot(dt, cleaned, type = "l", col = "black",
     xlab = "", ylab = "SensorDepth_m_C",
     main = paste0("VDOW  (removed: ", n_service, " service-window, ", n_global, " global)"))
points(dt[flagged & reason == "service_window"], raw[flagged & reason == "service_window"],
       col = "red", pch = 19, cex = 0.8)
points(dt[flagged & reason == "global_jump"], raw[flagged & reason == "global_jump"],
       col = "orange", pch = 17, cex = 0.8)
abline(v = as.POSIXct(service.VDOW), col = "grey70", lty = 2)
legend("topright",
       legend = c("cleaned data", "removed: service window", "removed: global jump", "logged service event"),
       col = c("black", "red", "orange", "grey70"),
       lty = c(1, NA, NA, 2), pch = c(NA, 19, 17, NA),
       bty = "n", cex = 0.75)

# VDOS
BEGI_PTz.ts2[["VDOS"]]$SensorDepth_m_C = BEGI_PTz.ts2[["VDOS"]]$SensorDepth_m
# remove the first 31 real readings of VDOS's record (too early for the rolling-window method to have a baseline to compare against)
first_31 <- which(!is.na(BEGI_PTz.ts2[["VDOS"]]$SensorDepth_m_C))[1:31]
BEGI_PTz.ts2[["VDOS"]]$SensorDepth_m_C[first_31] <- NA
# remove other stray out of water readings
BEGI_PTz.ts2[["VDOS"]]$SensorDepth_m_C[BEGI_PTz.ts2[["VDOS"]]$SensorDepth_m_C < 1] <- NA
# apply rolling function
BEGI_PTz.ts2[["VDOS"]] <- flag_service_outliers(BEGI_PTz.ts2[["VDOS"]], service_times = service.VDOS)
# plot to check
dt      <- BEGI_PTz.ts2[["VDOS"]]$datetimeMT
cleaned <- BEGI_PTz.ts2[["VDOS"]]$SensorDepth_m_C
raw     <- BEGI_PTz.ts2[["VDOS"]]$SensorDepth_m
flagged <- BEGI_PTz.ts2[["VDOS"]]$SensorDepth_m_C_flagged
reason  <- BEGI_PTz.ts2[["VDOS"]]$SensorDepth_m_C_flag_reason
n_service <- sum(flagged & reason == "service_window", na.rm = TRUE)
n_global  <- sum(flagged & reason == "global_jump", na.rm = TRUE)
plot(dt, cleaned, type = "l", col = "black",
     xlab = "", ylab = "SensorDepth_m_C",
     main = paste0("VDOS  (removed: ", n_service, " service-window, ", n_global, " global)"))
points(dt[flagged & reason == "service_window"], raw[flagged & reason == "service_window"],
       col = "red", pch = 19, cex = 0.8)
points(dt[flagged & reason == "global_jump"], raw[flagged & reason == "global_jump"],
       col = "orange", pch = 17, cex = 0.8)
abline(v = as.POSIXct(service.VDOS), col = "grey70", lty = 2)
legend("topright",
       legend = c("cleaned data", "removed: service window", "removed: global jump", "logged service event"),
       col = c("black", "red", "orange", "grey70"),
       lty = c(1, NA, NA, 2), pch = c(NA, 19, 17, NA),
       bty = "n", cex = 0.75)


# SLOW
BEGI_PTz.ts2[["SLOW"]]$SensorDepth_m_C = BEGI_PTz.ts2[["SLOW"]]$SensorDepth_m
# remove the first 39 real readings of SLOW's record (too early for the rolling-window method to have a baseline to compare against)
first_39 <- which(!is.na(BEGI_PTz.ts2[["SLOW"]]$SensorDepth_m_C))[1:39]
BEGI_PTz.ts2[["SLOW"]]$SensorDepth_m_C[first_39] <- NA
# remove other stray out of water readings
BEGI_PTz.ts2[["SLOW"]]$SensorDepth_m_C[BEGI_PTz.ts2[["SLOW"]]$SensorDepth_m_C < 1.5] <- NA
# apply rolling function
BEGI_PTz.ts2[["SLOW"]] <- flag_service_outliers(BEGI_PTz.ts2[["SLOW"]], service_times = service.SLOW)
# plot to check
dt      <- BEGI_PTz.ts2[["SLOW"]]$datetimeMT
cleaned <- BEGI_PTz.ts2[["SLOW"]]$SensorDepth_m_C
raw     <- BEGI_PTz.ts2[["SLOW"]]$SensorDepth_m
flagged <- BEGI_PTz.ts2[["SLOW"]]$SensorDepth_m_C_flagged
reason  <- BEGI_PTz.ts2[["SLOW"]]$SensorDepth_m_C_flag_reason
n_service <- sum(flagged & reason == "service_window", na.rm = TRUE)
n_global  <- sum(flagged & reason == "global_jump", na.rm = TRUE)
plot(dt, cleaned, type = "l", col = "black",
     xlab = "", ylab = "SensorDepth_m_C",
     main = paste0("SLOW  (removed: ", n_service, " service-window, ", n_global, " global)"))
points(dt[flagged & reason == "service_window"], raw[flagged & reason == "service_window"],
       col = "red", pch = 19, cex = 0.8)
points(dt[flagged & reason == "global_jump"], raw[flagged & reason == "global_jump"],
       col = "orange", pch = 17, cex = 0.8)
abline(v = as.POSIXct(service.SLOW), col = "grey70", lty = 2)
legend("topright",
       legend = c("cleaned data", "removed: service window", "removed: global jump", "logged service event"),
       col = c("black", "red", "orange", "grey70"),
       lty = c(1, NA, NA, 2), pch = c(NA, 19, 17, NA),
       bty = "n", cex = 0.75)


# SLOC
BEGI_PTz.ts2[["SLOC"]]$SensorDepth_m_C = BEGI_PTz.ts2[["SLOC"]]$SensorDepth_m
# remove the first 37 real readings of SLOC's record (too early for the rolling-window method to have a baseline to compare against)
first_37 <- which(!is.na(BEGI_PTz.ts2[["SLOC"]]$SensorDepth_m_C))[1:37]
BEGI_PTz.ts2[["SLOC"]]$SensorDepth_m_C[first_37] <- NA
# remove other stray out of water readings
BEGI_PTz.ts2[["SLOC"]]$SensorDepth_m_C[BEGI_PTz.ts2[["SLOC"]]$SensorDepth_m_C < 0.2] <- NA
# apply rolling function
BEGI_PTz.ts2[["SLOC"]] <- flag_service_outliers(BEGI_PTz.ts2[["SLOC"]], service_times = service.SLOC)
# plot to check
dt      <- BEGI_PTz.ts2[["SLOC"]]$datetimeMT
cleaned <- BEGI_PTz.ts2[["SLOC"]]$SensorDepth_m_C
raw     <- BEGI_PTz.ts2[["SLOC"]]$SensorDepth_m
flagged <- BEGI_PTz.ts2[["SLOC"]]$SensorDepth_m_C_flagged
reason  <- BEGI_PTz.ts2[["SLOC"]]$SensorDepth_m_C_flag_reason
n_service <- sum(flagged & reason == "service_window", na.rm = TRUE)
n_global  <- sum(flagged & reason == "global_jump", na.rm = TRUE)
plot(dt, cleaned, type = "l", col = "black",
     xlab = "", ylab = "SensorDepth_m_C",
     main = paste0("SLOC  (removed: ", n_service, " service-window, ", n_global, " global)"))
points(dt[flagged & reason == "service_window"], raw[flagged & reason == "service_window"],
       col = "red", pch = 19, cex = 0.8)
points(dt[flagged & reason == "global_jump"], raw[flagged & reason == "global_jump"],
       col = "orange", pch = 17, cex = 0.8)
abline(v = as.POSIXct(service.SLOC), col = "grey70", lty = 2)
legend("topright",
       legend = c("cleaned data", "removed: service window", "removed: global jump", "logged service event"),
       col = c("black", "red", "orange", "grey70"),
       lty = c(1, NA, NA, 2), pch = c(NA, 19, 17, NA),
       bty = "n", cex = 0.75)





#### save and re-add cleaned sensor depth data

saveRDS(BEGI_PTz.ts2, "DTW_compiled/BEGI_PTz.ts2.rds")
rm(list = ls())
BEGI_PTz.ts4 = readRDS("DTW_compiled/BEGI_PTz.ts2.rds")

#### Correct baseline jumps ####

# Baseline jumps likely resulted from the cable getting tangled such that its length was temporarily changed when sondes were being serviced. 
# Baseline-corrected data will be saved in BEGI_PTz.ts5 as SensorDepth_m_CBC

BEGI_PTz.ts5 = BEGI_PTz.ts4

# get service date/times
service.VDOS = as.POSIXct(read.csv("EXO_compiled/service.VDOS.csv")[,2],tz="US/Mountain")
service.VDOW = as.POSIXct(read.csv("EXO_compiled/service.VDOW.csv")[,2],tz="US/Mountain")
service.SLOW = as.POSIXct(read.csv("EXO_compiled/service.SLOW.csv")[,2],tz="US/Mountain")
service.SLOC = as.POSIXct(read.csv("EXO_compiled/service.SLOC.csv")[,2],tz="US/Mountain")

### plot to check
# VDOW
ggplot(data=BEGI_PTz.ts5[["VDOW"]], aes(datetimeMT, (SensorDepth_m_C)))+
  geom_line()+
  geom_point()
# no jumps in VDOW

# VDOS
ggplot(data=BEGI_PTz.ts5[["VDOS"]], aes(datetimeMT, (SensorDepth_m_C)))+
  geom_line()+
  geom_point()
# one small jump in November 2023

# SLOW
ggplot(data=BEGI_PTz.ts5[["SLOW"]], aes(datetimeMT, (SensorDepth_m_C)))+
  geom_line()+
  geom_point()
# two jumps in Dec-Feb

# SLOC
ggplot(data=BEGI_PTz.ts5[["SLOC"]], aes(datetimeMT, (SensorDepth_m_C)))+
  geom_line()+
  geom_point()
# lots of jumps throughout record. This well is the shallowest and most prone to tangles.


## VDOW ##
BEGI_PTz.ts5[["VDOW"]]$SensorDepth_m_CBC = BEGI_PTz.ts5[["VDOW"]]$SensorDepth_m_C

## VDOS ##
# correction 1
temp =  BEGI_PTz.ts4[["VDOS"]][BEGI_PTz.ts4[["VDOS"]]$datetimeMT>=as.POSIXct("2023-11-21 00:00:00") &
                                 BEGI_PTz.ts4[["VDOS"]]$datetimeMT<as.POSIXct("2023-11-29 00:00:00"),]
plot(temp$datetimeMT, temp$SensorDepth_m_C, type="o")
abline(v=as.POSIXct(service.VDOS), col="red")
BEGI_PTz.ts5[["VDOS"]]$SensorDepth_m_CBC = BEGI_PTz.ts5[["VDOS"]]$SensorDepth_m_C
BEGI_PTz.ts5[["VDOS"]]$SensorDepth_m_CBC[BEGI_PTz.ts5[["VDOS"]]$datetimeMT>=as.POSIXct("2023-11-22 14:30:00") &
                                           BEGI_PTz.ts5[["VDOS"]]$datetimeMT<=as.POSIXct("2023-11-27 09:45:00")] = 
  BEGI_PTz.ts5[["VDOS"]]$SensorDepth_m_C[BEGI_PTz.ts5[["VDOS"]]$datetimeMT>=as.POSIXct("2023-11-22 14:30:00") &
                                           BEGI_PTz.ts5[["VDOS"]]$datetimeMT<=as.POSIXct("2023-11-27 09:45:00")] - (1.930-1.898)
temp =  BEGI_PTz.ts5[["VDOS"]][BEGI_PTz.ts5[["VDOS"]]$datetimeMT>=as.POSIXct("2023-11-21 00:00:00") &
                                 BEGI_PTz.ts5[["VDOS"]]$datetimeMT<as.POSIXct("2023-11-29 00:00:00"),]
plot(temp$datetimeMT, temp$SensorDepth_m_CBC, type="o")
abline(v=as.POSIXct(service.VDOS), col="red")
# end correction 1


## SLOW ##
temp =  BEGI_PTz.ts4[["SLOW"]][BEGI_PTz.ts4[["SLOW"]]$datetimeMT>=as.POSIXct("2023-12-01 00:00:00") &
                                 BEGI_PTz.ts4[["SLOW"]]$datetimeMT<as.POSIXct("2024-02-15 00:00:00"),]
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
# correction 3
temp =  BEGI_PTz.ts4[["SLOW"]][BEGI_PTz.ts4[["SLOW"]]$datetimeMT>=as.POSIXct("2023-11-21 00:00:00") &
                                 BEGI_PTz.ts4[["SLOW"]]$datetimeMT<as.POSIXct("2023-11-24 00:00:00"),]
plot(temp$datetimeMT, temp$SensorDepth_m_C, type="o");abline(v=as.POSIXct(service.SLOW), col="red")
BEGI_PTz.ts5[["SLOW"]]$SensorDepth_m_CBC[BEGI_PTz.ts5[["SLOW"]]$datetimeMT>=as.POSIXct("2023-11-21 19:00:00") &
                                           BEGI_PTz.ts5[["SLOW"]]$datetimeMT<=as.POSIXct("2023-11-22 15:00:00")] = 
  BEGI_PTz.ts5[["SLOW"]]$SensorDepth_m_C[BEGI_PTz.ts5[["SLOW"]]$datetimeMT>=as.POSIXct("2023-11-21 19:00:00") &
                                           BEGI_PTz.ts5[["SLOW"]]$datetimeMT<=as.POSIXct("2023-11-22 15:00:00")] + (2.368-2.181)
temp =  BEGI_PTz.ts5[["SLOW"]][BEGI_PTz.ts5[["SLOW"]]$datetimeMT>=as.POSIXct("2023-11-21 00:00:00") &
                                 BEGI_PTz.ts5[["SLOW"]]$datetimeMT<as.POSIXct("2023-11-24 00:00:00"),]
plot(temp$datetimeMT, temp$SensorDepth_m_CBC, type="o");abline(v=as.POSIXct(service.SLOW), col="red")


## SLOC ##
temp =  BEGI_PTz.ts4[["SLOC"]][BEGI_PTz.ts4[["SLOC"]]$datetimeMT>=as.POSIXct("2023-11-01 00:00:00") &
                                 BEGI_PTz.ts4[["SLOC"]]$datetimeMT<as.POSIXct("2024-01-01 00:00:00"),]
plot(temp$datetimeMT, temp$SensorDepth_m_C*-1, type="o");abline(v=as.POSIXct(service.SLOC), col="red")
## correction 1-4
BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_CBC = BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_C
#1
jump1 = as.POSIXct("2023-10-06 10:45:00", tz="US/Mountain")
endjump1 = as.POSIXct("2023-10-13 10:15:00", tz="US/Mountain")
dif1 = 0.911-0.882
BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_CBC[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump1 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump1] = 
  BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_C[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump1 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump1] + dif1
#2
jump2 = as.POSIXct("2023-10-13 11:00:00", tz="US/Mountain")
endjump2 = as.POSIXct("2023-10-16 14:15:00", tz="US/Mountain")
dif2 = (0.915-0.853)+dif1
BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_CBC[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump2 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump2] = 
  BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_C[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump2 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump2] + dif2
#3
jump3 = as.POSIXct("2023-10-16 14:30:00", tz="US/Mountain")
endjump3 = as.POSIXct("2023-10-20 10:15:00", tz="US/Mountain")
dif3 = (0.880-0.739)+dif2
BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_CBC[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump3 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump3] = 
  BEGI_PTz.ts5[["SLOC"]]$SensorDepth_m_C[BEGI_PTz.ts5[["SLOC"]]$datetimeMT>=jump3 &
                                           BEGI_PTz.ts5[["SLOC"]]$datetimeMT<=endjump3] + dif3
#4
jump4 = as.POSIXct("2023-10-27 11:15:00", tz="US/Mountain")
endjump4 = as.POSIXct("2023-11-03 10:30:00", tz="US/Mountain")
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
jump5 = as.POSIXct("2024-01-19 16:30:00", tz="US/Mountain")
endjump5 = as.POSIXct("2024-01-25 13:00:00", tz="US/Mountain")
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
jump6 = as.POSIXct("2024-01-25 13:30:00", tz="US/Mountain")
endjump6 = as.POSIXct("2024-02-06 10:15:00", tz="US/Mountain")
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
jump7 = as.POSIXct("2024-04-17 09:15:00", tz="US/Mountain")
endjump7 = as.POSIXct("2024-04-30 10:15:00", tz="US/Mountain")
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
ts.temp<-read.zoo(BEGI_PTz.ts6[["VDOW"]][,c(1,7)], index.column=1, format="%Y-%m-%d %H:%M:%S", tz="US/Mountain")
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
ts.temp<-read.zoo(BEGI_PTz.ts6[["VDOS"]][,c(1,7)], index.column=1, format="%Y-%m-%d %H:%M:%S", tz="US/Mountain")
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
ts.temp<-read.zoo(BEGI_PTz.ts6[["SLOW"]][,c(1,7)], index.column=1, format="%Y-%m-%d %H:%M:%S", tz="US/Mountain")
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
ts.temp<-read.zoo(BEGI_PTz.ts6[["SLOC"]][,c(1,7)], index.column=1, format="%Y-%m-%d %H:%M:%S", tz="US/Mountain")
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
ggplot(data=BEGI_PTz.ts6[["VDOW"]], aes(datetimeMT, (SensorDepth_m_CBC_sm)))+
  geom_line()
ggplot(data=BEGI_PTz.ts6[["VDOS"]], aes(datetimeMT, (SensorDepth_m_CBC_sm)))+
  geom_line()
ggplot(data=BEGI_PTz.ts6[["SLOW"]], aes(datetimeMT, (SensorDepth_m_CBC_sm)))+
  geom_line()
ggplot(data=BEGI_PTz.ts6[["SLOC"]], aes(datetimeMT, (SensorDepth_m_CBC_sm)))+
  geom_line()


#

#### save and re-add cleaned sensor depth data
saveRDS(BEGI_PTz.ts6, "DTW_compiled/BEGI_PTz.rds")
rm(list = ls())
BEGI_PTz = readRDS("DTW_compiled/BEGI_PTz.rds")

#
#### Add manual DTW dataset ####

# get data from googledrive
beeper_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1J6iYi6RLIC-9ao8Tgo7twiPB_Afq3o9H")
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

#### join beeper DTW to PT sensor depth data

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
# convert DTW_beeper_m from positive to negative to reflect relative depth from surface, where zero is the ground surface
for(i in siteIDz){
  BEGI_PTz[[i]]$DTW_beeper_m_neg = BEGI_PTz[[i]]$DTW_beeper_m*-1
}

# plot to check
ggplot(data=BEGI_PTz[["VDOW"]])+
  geom_line(aes(datetimeMT, SensorDepth_m_CBC_sm))+
  geom_point(aes(datetimeMT, DTW_beeper_m_neg), size=3, color="red")
ggplot(data=BEGI_PTz[["VDOS"]])+
  geom_line(aes(datetimeMT, SensorDepth_m_CBC_sm))+
  geom_point(aes(datetimeMT, DTW_beeper_m_neg), size=3, color="red")
ggplot(data=BEGI_PTz[["SLOW"]])+
  geom_line(aes(datetimeMT, SensorDepth_m_CBC_sm))+
  geom_point(aes(datetimeMT, DTW_beeper_m_neg), size=3, color="red")
ggplot(data=BEGI_PTz[["SLOC"]])+
  geom_line(aes(datetimeMT, SensorDepth_m_CBC_sm))+
  geom_point(aes(datetimeMT, DTW_beeper_m_neg), size=3, color="red")


#### Convert PT depth to DTW ####

# VDOW
plot(BEGI_PTz[["VDOW"]]$DTW_beeper_m_neg ~ BEGI_PTz[["VDOW"]]$SensorDepth_m_CBC_sm)
m.VDOW = lm(BEGI_PTz[["VDOW"]]$DTW_beeper_m_neg ~ BEGI_PTz[["VDOW"]]$SensorDepth_m_CBC_sm)
abline(m.VDOW)
summary(m.VDOW)
cf <- coef(m.VDOW)
Intercept <- cf[1]
Slope <- cf[2]
BEGI_PTz[["VDOW"]]$DTW_m = BEGI_PTz[["VDOW"]]$SensorDepth_m_CBC_sm*Slope + Intercept
ggplot(data=BEGI_PTz[["VDOW"]])+
  geom_line(aes(datetimeMT, DTW_m))+
  geom_point(aes(datetimeMT, DTW_beeper_m_neg), size=3, color="red")
ggplot(data=BEGI_PTz[["VDOW"]])+ geom_line(aes(datetimeMT, DTW_m))+ylim(c(-3,0))

# VDOS
plot(BEGI_PTz[["VDOS"]]$DTW_beeper_m_neg ~ BEGI_PTz[["VDOS"]]$SensorDepth_m_CBC_sm)
m.VDOS = lm(BEGI_PTz[["VDOS"]]$DTW_beeper_m_neg ~ BEGI_PTz[["VDOS"]]$SensorDepth_m_CBC_sm)
abline(m.VDOS)
summary(m.VDOS)
cf <- coef(m.VDOS)
Intercept <- cf[1]
Slope <- cf[2]
BEGI_PTz[["VDOS"]]$DTW_m = BEGI_PTz[["VDOS"]]$SensorDepth_m_CBC_sm*Slope + Intercept
ggplot(data=BEGI_PTz[["VDOS"]])+
  geom_line(aes(datetimeMT, DTW_m))+
  geom_point(aes(datetimeMT, DTW_beeper_m_neg), size=3, color="red")
ggplot(data=BEGI_PTz[["VDOS"]])+ geom_line(aes(datetimeMT, DTW_m))+ylim(c(-3,0))

# SLOW
plot(BEGI_PTz[["SLOW"]]$DTW_beeper_m_neg ~ BEGI_PTz[["SLOW"]]$SensorDepth_m_CBC_sm)
m.SLOW = lm(BEGI_PTz[["SLOW"]]$DTW_beeper_m_neg ~ BEGI_PTz[["SLOW"]]$SensorDepth_m_CBC_sm)
abline(m.SLOW)
summary(m.SLOW)
cf <- coef(m.SLOW)
Intercept <- cf[1]
Slope <- cf[2]
BEGI_PTz[["SLOW"]]$DTW_m = BEGI_PTz[["SLOW"]]$SensorDepth_m_CBC_sm*Slope + Intercept
ggplot(data=BEGI_PTz[["SLOW"]])+
  geom_line(aes(datetimeMT, DTW_m))+
  geom_point(aes(datetimeMT, DTW_beeper_m_neg), size=3, color="red")
ggplot(data=BEGI_PTz[["SLOW"]])+ geom_line(aes(datetimeMT, DTW_m))+ylim(c(-3,.5))

# SLOC
plot(BEGI_PTz[["SLOC"]]$DTW_beeper_m_neg ~ BEGI_PTz[["SLOC"]]$SensorDepth_m_CBC_sm)
m.SLOC = lm(BEGI_PTz[["SLOC"]]$DTW_beeper_m_neg ~ BEGI_PTz[["SLOC"]]$SensorDepth_m_CBC_sm)
abline(m.SLOC)
summary(m.SLOC)
cf <- coef(m.SLOC)
Intercept <- cf[1]
Slope <- cf[2]
BEGI_PTz[["SLOC"]]$DTW_m = BEGI_PTz[["SLOC"]]$SensorDepth_m_CBC_sm*Slope + Intercept
ggplot(data=BEGI_PTz[["SLOC"]])+
  geom_line(aes(datetimeMT, DTW_m))+
  geom_point(aes(datetimeMT, DTW_beeper_m_neg), size=3, color="red")
ggplot(data=BEGI_PTz[["SLOC"]])+ geom_line(aes(datetimeMT, DTW_m))+ylim(c(-3,.5))


#### save finalized DTW data

# save as list
saveRDS(BEGI_PTz, "DTW_compiled/BEGI_PTz_DTW.rds")

# save as dataframe
BEGI_PT_DTW_all = rbind(BEGI_PTz[["VDOW"]],BEGI_PTz[["VDOS"]],BEGI_PTz[["SLOW"]],BEGI_PTz[["SLOC"]])
saveRDS(BEGI_PT_DTW_all, "DTW_compiled/BEGI_PT_DTW_all.rds")

#### Fill gap in VDOW ####

# there is a large datgap in VDOW from 2023-10-20 08:15:00 to 2024-02-06 10:30:00. This well is very physically close to VDOS and the data looks extremely similar other than a small difference in depth below the surface. The R-sq of their linear relationship is 0.9891. I therefore think it's appropriate to use VDOS to predict VDOW and fill the gap. 

DTW = readRDS("DTW_compiled/BEGI_PTz_DTW.rds")
DTW_df = readRDS("DTW_compiled/BEGI_PT_DTW_all.rds")

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

#### save data with VDOW gap filled

# save as list
saveRDS(DTW, "DTW_compiled/BEGI_PTz_DTW.rds")

# save as dataframe
saveRDS(DTW_df, "DTW_compiled/BEGI_PT_DTW_all.rds")

#




#### Add discharge data from Rio Grande ####

#### clean environment and re-add finalized DTW data
rm(list = ls())
BEGI_PTz = readRDS("DTW_compiled/BEGI_PTz_DTW.rds")

# USGS station: Rio Grande at Valle DE Oro, NM - USGS - 08330830
NM_retrieve_usgs_data <- function(start_date, end_date, site_no = "08330830", p_code = "00060") {
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

#### save data with Rio Grande discharge included

# save as list
saveRDS(BEGI_PTz, "DTW_compiled/BEGI_PTz_DTW.rds")

# save as dataframe
saveRDS(BEGI_PT_DTW_all, "DTW_compiled/BEGI_PT_DTW_all.rds")


#### Plot all final DTW data together ####

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
  ggplot(DTW_df, aes(datetimeMT, DTW_m, color=wellID)) +
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


#### Clear all Google Drive files from local folder to end fresh ####

# NOTE: DO NOT push large files to the github repo! there are too many to push all at once. The purpose of the google drive is to handle all these files, whereas github handles the script :)

googledrive_files <- list.files("googledrive", full.names = TRUE, recursive = TRUE)
if (length(googledrive_files) > 0) {
  file.remove(googledrive_files)
}

# now that your environment is cleaned up, now is a good time to save, commit, push/pull, and restart the R session to get ready for the next script in the workflow!