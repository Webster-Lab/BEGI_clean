#### read me #### 
#the purpose of this script is to compile depth to gw and temp corrected sonde data into one RDS file to use in subsequent scripts

#### libraries ####
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)
library(DescTools)
library(dplyr)

#### Import finalized water level data ####
#as dataframe
BEGI_PT_DTW_all = readRDS("data_clean/DTW_compiled/BEGI_PT_DTW_all.rds")

#### Trim data frame to match sonde length and add constant ####

BEGI_PT_DTW_trim <- BEGI_PT_DTW_all[BEGI_PT_DTW_all$datetimeMT >= "2023-09-15 00:00:00" 
                                    & BEGI_PT_DTW_all$datetimeMT <= "2024-09-04 00:00:00",]

BEGI_PT_DTW_trim$DTW_m_con = BEGI_PT_DTW_trim$DTW_m + 1

#### Import temp corrected sonde data ####
EXOz.or2 = readRDS("data_clean/EXO_compiled/BEGI_EXO.or2.rds")

#### Make gw depth dataframe for each well ####
#VDOW
VDOW_dtw <- BEGI_PT_DTW_trim %>%
  filter(wellID == "VDOW") %>%
  select(datetimeMT, DTW_m, DTW_m_con, wellID)
VDOW_dtw$siteID <- NULL
names(VDOW_dtw)[names(VDOW_dtw) == 'wellID'] <- 'siteID'

#VDOS
VDOS_dtw <- BEGI_PT_DTW_trim %>%
  filter(wellID == "VDOS") %>%
  select(datetimeMT, DTW_m, DTW_m_con, wellID)
VDOS_dtw$siteID <- NULL
names(VDOS_dtw)[names(VDOS_dtw) == 'wellID'] <- 'siteID'

#SLOW
SLOW_dtw <- BEGI_PT_DTW_trim %>%
  filter(wellID == "SLOW") %>%
  select(datetimeMT, DTW_m, DTW_m_con, wellID)
SLOW_dtw$siteID <- NULL
names(SLOW_dtw)[names(SLOW_dtw) == 'wellID'] <- 'siteID'

#SLOC
SLOC_dtw <- BEGI_PT_DTW_trim %>%
  filter(wellID == "SLOC") %>%
  select(datetimeMT, DTW_m, DTW_m_con, wellID)
SLOC_dtw$siteID <- NULL
names(SLOC_dtw)[names(SLOC_dtw) == 'wellID'] <- 'siteID'

#### stitch gw df to each well df in EXOz.tc ####
EXOz.or2[["VDOW"]] = full_join(EXOz.tc[["VDOW"]], VDOW_dtw, by=c("datetimeMT","siteID"))
EXOz.or2[["VDOS"]] = full_join(EXOz.tc[["VDOS"]], VDOS_dtw, by=c("datetimeMT","siteID"))
EXOz.or2[["SLOW"]] = full_join(EXOz.tc[["SLOW"]], SLOW_dtw, by=c("datetimeMT","siteID"))
EXOz.or2[["SLOC"]] = full_join(EXOz.tc[["SLOC"]], SLOC_dtw, by=c("datetimeMT","siteID"))

#### save compiled dtw and sonde data ####
saveRDS(EXOz.or2, "data_clean/EXO_compiled/BEGI_EXOz.dtw.rds")


#### plot DO timeseries ####
