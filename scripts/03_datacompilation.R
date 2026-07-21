#### READ ME #### 
#the purpose of this script is to compile depth to gw and sonde data into one RDS file to use in subsequent scripts
# this script also cleans dissolved oxygen data to remove outliers and adjust baselines to be consistent among sondes

#### Libraries ####
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)
library(DescTools)
library(dplyr)
library(viridis)

#### Import finalized water level data ####
#as dataframe
BEGI_PT_DTW_all = readRDS("DTW_compiled/BEGI_PT_DTW_all.rds")

#### Trim data frame to match sonde length ####

BEGI_PT_DTW_trim <- BEGI_PT_DTW_all[BEGI_PT_DTW_all$datetimeMT >= "2023-09-15 00:00:00" 
                                    & BEGI_PT_DTW_all$datetimeMT <= "2024-09-04 00:00:00",]


#### Import temp corrected sonde data ####
EXOz.or2 = readRDS("EXO_compiled/BEGI_EXO.or2.rds")

#### Make gw depth dataframe for each well ####
#VDOW
VDOW_dtw <- BEGI_PT_DTW_trim %>%
  filter(wellID == "VDOW") %>%
  select(datetimeMT, wellID, DTW_m)
VDOW_dtw$siteID <- NULL
names(VDOW_dtw)[names(VDOW_dtw) == 'wellID'] <- 'siteID'

#VDOS
VDOS_dtw <- BEGI_PT_DTW_trim %>%
  filter(wellID == "VDOS") %>%
  select(datetimeMT, DTW_m, wellID)
VDOS_dtw$siteID <- NULL
names(VDOS_dtw)[names(VDOS_dtw) == 'wellID'] <- 'siteID'

#SLOW
SLOW_dtw <- BEGI_PT_DTW_trim %>%
  filter(wellID == "SLOW") %>%
  select(datetimeMT, DTW_m, wellID)
SLOW_dtw$siteID <- NULL
names(SLOW_dtw)[names(SLOW_dtw) == 'wellID'] <- 'siteID'

#SLOC
SLOC_dtw <- BEGI_PT_DTW_trim %>%
  filter(wellID == "SLOC") %>%
  select(datetimeMT, DTW_m, wellID)
SLOC_dtw$siteID <- NULL
names(SLOC_dtw)[names(SLOC_dtw) == 'wellID'] <- 'siteID'

#### Stitch gw df to each well df in EXOz.or2 ####
EXOz.or2[["VDOW"]] = full_join(EXOz.or2[["VDOW"]], VDOW_dtw, by=c("datetimeMT","siteID"))
EXOz.or2[["VDOS"]] = full_join(EXOz.or2[["VDOS"]], VDOS_dtw, by=c("datetimeMT","siteID"))
EXOz.or2[["SLOW"]] = full_join(EXOz.or2[["SLOW"]], SLOW_dtw, by=c("datetimeMT","siteID"))
EXOz.or2[["SLOC"]] = full_join(EXOz.or2[["SLOC"]], SLOC_dtw, by=c("datetimeMT","siteID"))

#### save compiled dtw and sonde data
saveRDS(EXOz.or2, "EXO_compiled/BEGI_EXOz_dtw.rds")
EXOz.or2 = readRDS("EXO_compiled/BEGI_EXOz_dtw.rds")

#### Clean DO data ####

#### Correct negative DO values
EXOz.or2[["VDOW"]]$ODO.mg.L.mn <- EXOz.or2[["VDOW"]]$ODO.mg.L.mn + 0.36
EXOz.or2[["VDOW"]]$ODO.mg.L.mn[EXOz.or2[["VDOW"]]$ODO.mg.L.mn < 0] <- 0
EXOz.or2[["VDOS"]]$ODO.mg.L.mn <- EXOz.or2[["VDOS"]]$ODO.mg.L.mn + 0.42
EXOz.or2[["VDOS"]]$ODO.mg.L.mn[EXOz.or2[["VDOS"]]$ODO.mg.L.mn < 0] <- 0
EXOz.or2[["SLOW"]]$ODO.mg.L.mn <- EXOz.or2[["SLOW"]]$ODO.mg.L.mn + 0.32
EXOz.or2[["SLOW"]]$ODO.mg.L.mn[EXOz.or2[["SLOW"]]$ODO.mg.L.mn < 0] <- 0
# SLOC
EXOz.or2[["SLOC"]]$ODO.mg.L.mn <- EXOz.or2[["SLOC"]]$ODO.mg.L.mn + 0.48
# correct baseline jump
temp =  EXOz.or2[["SLOC"]][EXOz.or2[["SLOC"]]$datetimeMT>=as.POSIXct("2024-06-10 00:00:00") &
                             EXOz.or2[["SLOC"]]$datetimeMT<as.POSIXct("2024-07-03 00:00:00"),]
plot(temp$datetimeMT, temp$ODO.mg.L.mn, type="o")
EXOz.or2[["SLOC"]]$ODO.mg.L.mn[EXOz.or2[["SLOC"]]$datetimeMT>=as.POSIXct("2024-06-11 20:00:00") &
                                 EXOz.or2[["SLOC"]]$datetimeMT<=as.POSIXct("2024-06-22 13:45:00")] = 
  EXOz.or2[["SLOC"]]$ODO.mg.L.mn[EXOz.or2[["SLOC"]]$datetimeMT>=as.POSIXct("2024-06-11 20:00:00") &
                                   EXOz.or2[["SLOC"]]$datetimeMT<=as.POSIXct("2024-06-22 13:45:00")] + (0.28+1.62)

#### remove DO data when SpC indicates that sensors were out of water

clean_ODO_by_SpCond <- function(data,
                                odo_col          = "ODO.mg.L.mn",
                                spcond_col       = "SpCond.µS.cm.mn",
                                spcond_threshold = 50) {
  data[[odo_col]][!is.na(data[[spcond_col]]) & data[[spcond_col]] < spcond_threshold] <- NA
  data
}
EXOz.or2 <- lapply(EXOz.or2, clean_ODO_by_SpCond)

#### gap-filled data (<6 hr gaps only)

# fill gaps < 6 hr for each well individually
EXOz.or3 = EXOz.or2
# VDOW
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling
# Make univariate zoo time series #
ts.temp<-read.zoo(EXOz.or3[["VDOW"]][,c(1,13)], index.column=1, format="%Y-%m-%d %H:%M:%S", tz="US/Mountain")
# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)
# Apply NA interpolation method
ts.filled = na.spline(ts.temp, na.rm = T, maxgap = 4*6)
plot(ts.filled)
#par(mfrow=c(1,1)) # reset plotting window
# revert back to df
ts.filled = as.data.frame(ts.filled)
ts.filled$datetimeMT = EXOz.or3[["VDOW"]]$datetimeMT
names(ts.filled) = c("ODO.mg.L.mn_sm","datetimeMT")
EXOz.or3[["VDOW"]] = left_join(EXOz.or3[["VDOW"]], ts.filled, by="datetimeMT")
# check NAs that are left
sum(is.na(EXOz.or3[["VDOW"]]$ODO.mg.L.mn))
sum(is.na(EXOz.or3[["VDOW"]]$ODO.mg.L.mn_sm))
###
# VDOS
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling
# Make univariate zoo time series #
ts.temp<-read.zoo(EXOz.or3[["VDOS"]][,c(1,13)], index.column=1, format="%Y-%m-%d %H:%M:%S", tz="US/Mountain")
# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)
# Apply NA interpolation method
ts.filled = na.spline(ts.temp, na.rm = T, maxgap = 4*6)
plot(ts.filled)
#par(mfrow=c(1,1)) # reset plotting window
# revert back to df
ts.filled = as.data.frame(ts.filled)
ts.filled$datetimeMT = EXOz.or3[["VDOS"]]$datetimeMT
names(ts.filled) = c("ODO.mg.L.mn_sm","datetimeMT")
EXOz.or3[["VDOS"]] = left_join(EXOz.or3[["VDOS"]], ts.filled, by="datetimeMT")
# check NAs that are left
sum(is.na(EXOz.or3[["VDOS"]]$ODO.mg.L.mn))
sum(is.na(EXOz.or3[["VDOS"]]$ODO.mg.L.mn_sm))
###
# SLOW
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling
# Make univariate zoo time series #
ts.temp<-read.zoo(EXOz.or3[["SLOW"]][,c(1,13)], index.column=1, format="%Y-%m-%d %H:%M:%S", tz="US/Mountain")
# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)
# Apply NA interpolation method
ts.filled = na.spline(ts.temp, na.rm = T, maxgap = 4*6)
plot(ts.filled)
#par(mfrow=c(1,1)) # reset plotting window
# revert back to df
ts.filled = as.data.frame(ts.filled)
ts.filled$datetimeMT = EXOz.or3[["SLOW"]]$datetimeMT
names(ts.filled) = c("ODO.mg.L.mn_sm","datetimeMT")
EXOz.or3[["SLOW"]] = left_join(EXOz.or3[["SLOW"]], ts.filled, by="datetimeMT")
# check NAs that are left
sum(is.na(EXOz.or3[["SLOW"]]$ODO.mg.L.mn))
sum(is.na(EXOz.or3[["SLOW"]]$ODO.mg.L.mn_sm))
##
# SLOC
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling
# Make univariate zoo time series #
ts.temp<-read.zoo(EXOz.or3[["SLOC"]][,c(1,13)], index.column=1, format="%Y-%m-%d %H:%M:%S", tz="US/Mountain")
# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)
# Apply NA interpolation method
ts.filled = na.spline(ts.temp, na.rm = T, maxgap = 4*6)
plot(ts.filled)
#par(mfrow=c(1,1)) # reset plotting window
# revert back to df
ts.filled = as.data.frame(ts.filled)
ts.filled$datetimeMT = EXOz.or3[["SLOC"]]$datetimeMT
names(ts.filled) = c("ODO.mg.L.mn_sm","datetimeMT")
EXOz.or3[["SLOC"]] = left_join(EXOz.or3[["SLOC"]], ts.filled, by="datetimeMT")
# check NAs that are left
sum(is.na(EXOz.or3[["SLOC"]]$ODO.mg.L.mn))
sum(is.na(EXOz.or3[["SLOC"]]$ODO.mg.L.mn_sm))

# smooth with a 1 hr rolling mean
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for(i in siteIDz){
  EXOz.or3[[i]]$ODO.mg.L.mn_sm = zoo::rollmean(EXOz.or3[[i]]$ODO.mg.L.mn_sm, 4, na.pad = TRUE)
}

#### Save compiled data ####

# save as list
saveRDS(EXOz.or3, "DTW_compiled/BEGI_EXOz_dtw.rds")


#### Plot DO timeseries ####

# combine the per-site list into one data frame - faceted plotting needs a single data frame, not a list
EXOz_all <- dplyr::bind_rows(EXOz.or3, .id = "wellID")

# derive the two-letter site grouping (VDO/SLO) from wellID,
EXOz_all$siteID <- substr(EXOz_all$wellID, 1, 3)

ODO_fig <- 
  ggplot(EXOz_all, aes(datetimeMT, ODO.mg.L.mn_sm, color = wellID)) +
  xlab("") +
  ylab("Dissolved Oxygen (mg/L)") +
  geom_line(key_glyph = "timeseries",linewidth=1,alpha=0.75) +
  facet_grid(rows = vars(siteID)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20)) +
  scale_color_viridis(discrete = TRUE, option = "D")

ggsave("plots/ODO_timeseries_allwells.png", ODO_fig, width = 11, height = 8, units = "in", dpi = 300)


#### Clear all Google Drive files from local folder to end fresh ####

googledrive_files <- list.files("googledrive", full.names = TRUE, recursive = TRUE)
if (length(googledrive_files) > 0) {
  file.remove(googledrive_files)
}

# now that your environment is cleaned up, now is a good time to save, commit, push/pull, and restart the R session to get ready for the next script in the workflow!