#### READ ME ####
# The purpose of this script is to use the r package dtwclust to 
# 1. cluster water depth curves by shape to characterize the nature of the 
# variation that is correlated with DO event size
# 2. cluster fDOM curves following a DO event to visualize events of chemical oxidation
# through fDOM rebound

# resources:
# Manual: https://cran.r-project.org/web/packages/dtwclust/dtwclust.pdf
# R Journal article: https://journal.r-project.org/articles/RJ-2019-023/
# Example paper about turtle dives: https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.4384
# Ethanol example: https://tmastny.github.io/tsrecipes/articles/time-series-clustering.html
# browseVignettes("dtwclust")

# Output:
# 1. plot of depth to groundwater clusters preceding DO event
# 2. plot of fDOM clusters following DO event
# 3. dataframe of DO events EXCLUDING chemical oxidation (where fDOM rebounds; ER_calc_cluster2.rds)

#### libraries ####

library(tidyverse)
library(dtwclust)
library(reshape2)
library(zoo)
library(xts)
library(ggplot2)
library(gridExtra)
library(patchwork)

##########################################
#### 1. groundwater cluster analysis ####

##########################################
#### Event dtw timeseries ####
#this code pulls out the timeseries depth to water data for the 2 days preceding an event to use in DTW cluster analysis
#Import DTW data
DTW_df = readRDS("DTW_compiled/BEGI_EXOz_dtw.rds")

#import BEGI events
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")


#SLOC#
dtw_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["SLOC_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["SLOC_dates"]][[i]]
  start_time <- event_time - 60*60*48
  
  tempdat <- DTW_df[["SLOC"]][
    DTW_df[["SLOC"]]$datetimeMT >= start_time & DTW_df[["SLOC"]]$datetimeMT < event_time, 
  ]
  #remove NAs from dataset  
  valid_SLOC <- tempdat$DTW_m[!is.na(tempdat$DTW_m)]
  #extract 192 values  
  if (length(valid_SLOC) >= 192) {
    ts_values <- valid_SLOC[1:192]
  } else {
    ts_values <- c(valid_SLOC, rep(NA, 192 - length(valid_SLOC)))
  }
  
  dtw_timeseries[[i]] <- ts_values
}

#combine to dataframe
SLOC_dtw <- as.data.frame(do.call(rbind, dtw_timeseries))
colnames(SLOC_dtw) <- paste0("t", seq_len(192))
SLOC_dtw$event_time <- BEGI_events[["Eventdate"]][["SLOC_dates"]]

#SLOW#
dtw_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["SLOW_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["SLOW_dates"]][[i]]
  start_time <- event_time - 60*60*48
  
  tempdat <- DTW_df[["SLOW"]][
    DTW_df[["SLOW"]]$datetimeMT >= start_time & DTW_df[["SLOW"]]$datetimeMT < event_time, 
  ]
  #remove NAs from dataset  
  valid_SLOW <- tempdat$DTW_m[!is.na(tempdat$DTW_m)]
  #extract 192 values  
  if (length(valid_SLOW) >= 192) {
    ts_values <- valid_SLOW[1:192]
  } else {
    ts_values <- c(valid_SLOW, rep(NA, 192 - length(valid_SLOW)))
  }
  
  dtw_timeseries[[i]] <- ts_values
}

#combine to dataframe
SLOW_dtw <- as.data.frame(do.call(rbind, dtw_timeseries))
colnames(SLOW_dtw) <- paste0("t", seq_len(192))
SLOW_dtw$event_time <- BEGI_events[["Eventdate"]][["SLOW_dates"]]


#VDOW#
dtw_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["VDOW_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["VDOW_dates"]][[i]]
  start_time <- event_time - 60*60*48
  
  tempdat <- DTW_df[["VDOW"]][
    DTW_df[["VDOW"]]$datetimeMT >= start_time & DTW_df[["VDOW"]]$datetimeMT < event_time, 
  ]
  #remove NAs from dataset  
  valid_VDOW <- tempdat$DTW_m[!is.na(tempdat$DTW_m)]
  #extract 192 values  
  if (length(valid_VDOW) >= 192) {
    ts_values <- valid_VDOW[1:192]
  } else {
    ts_values <- c(valid_VDOW, rep(NA, 192 - length(valid_VDOW)))
  }
  
  dtw_timeseries[[i]] <- ts_values
}

#combine to dataframe
VDOW_dtw <- as.data.frame(do.call(rbind, dtw_timeseries))
colnames(VDOW_dtw) <- paste0("t", seq_len(192))
VDOW_dtw$event_time <- BEGI_events[["Eventdate"]][["VDOW_dates"]]


#VDOS#
dtw_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["VDOS_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["VDOS_dates"]][[i]]
  start_time <- event_time - 60*60*48
  
  tempdat <- DTW_df[["VDOS"]][
    DTW_df[["VDOS"]]$datetimeMT >= start_time & DTW_df[["VDOS"]]$datetimeMT < event_time, 
  ]
  #remove NAs from dataset  
  valid_VDOS <- tempdat$DTW_m[!is.na(tempdat$DTW_m)]
  #extract 192 values  
  if (length(valid_VDOS) >= 192) {
    ts_values <- valid_VDOS[1:192]
  } else {
    ts_values <- c(valid_VDOS, rep(NA, 192 - length(valid_VDOS)))
  }
  
  dtw_timeseries[[i]] <- ts_values
}

#combine to dataframe
VDOS_dtw <- as.data.frame(do.call(rbind, dtw_timeseries))
colnames(VDOS_dtw) <- paste0("t", seq_len(192))
VDOS_dtw$event_time <- BEGI_events[["Eventdate"]][["VDOS_dates"]]

#combine all dtw dataframes
event_dtw <- rbind(SLOC_dtw,SLOW_dtw,VDOW_dtw,VDOS_dtw)
#save
saveRDS(event_dtw, "DTW_compiled/event_dtw.rds")


#### load and wrangle timeseries data ####
dat = readRDS("DTW_compiled/event_dtw.rds")

# name events and make names into row names
dat$ename = paste("e", c(1:52), sep="")
rownames(dat) = dat$ename

# save date/time stamps of events separately 
times = dat[,193:194]
dat[,193:194] = NULL

# make a version with data normalized to make all scales the same
normalized<-function(y) {
  
  x<-y[!is.na(y)]
  
  x<-(x - min(x)) / (max(x) - min(x))
  
  y[!is.na(y)]<-x
  
  return(y)
}
dat_n = t(apply(dat,1,normalized))

# make versions with data smoothed and normalized
dat.xts = data.frame(t(dat))
d = ( seq.POSIXt(
  from = as.POSIXct("2025-06-10 01:00:00", tz="US/Mountain"),
  to = as.POSIXct("2025-06-10 01:00:00", tz="US/Mountain")+172800,
  by = "15 min"))[1:192]
dat.xts = xts(dat.xts, order.by = d)
dat.xts_s = rollmean(dat.xts, 12, fill=NA, align = "right")
dat_s = as.data.frame(t(dat.xts_s))
names(dat_s) = paste("t", c(1:192), sep="")
dat_s = dat_s[,12:192]

dat_s_n = t(apply(dat_s,1,normalized))


#### plot data ####

# raw data
plot.new()
par(mfrow=c(3,3), mar=c(2,2,2,2))
for(i in c(1:59)){
  matplot((t(dat))[,i], type = "l", main=i)
}
#dev.off()

# norm data
plot.new()
par(mfrow=c(3,3), mar=c(2,2,2,2))
for(i in c(1:52)){
  matplot((t(dat_n))[,i], type = "l", main=i)
}

# smoothed data
plot.new()
par(mfrow=c(3,3), mar=c(2,2,2,2))
for(i in c(1:52)){
  matplot((t(dat_s))[,i], type = "l", main=i)
}

# smoothed and normalized data
plot.new()
par(mfrow=c(3,3), mar=c(2,2,2,2))
for(i in c(1:9)){
  matplot((t(dat_s_n))[,i], type = "l", main=i)
}

#### Evaluate Optimal Cluster Number ####

# NOTE: notes are mostly from the turtle dives paper's code
## Evaluate "optimal" cluster number using CVI's (cluster validity indices)
## internal CVI's: consider partioned data and try to define measure of cluster purity
## external CVI's: compare obtained partition to correct one (need a ground truth for this- we won't use these)
## note: which CVI to use is also subjective/needs testing... can go with "majority vote" from indices but you should check that the final result makes biological sense!
## note: can also use "clue" package to evaluate clusters

# look at cluster no. of 2 to 6 max
# NOTE: this can take a long time to run! suggest running on a computer with lots of RAM/memory...
depth_clust_k <- tsclust(series=dat_s_n, k = 2:7, centroid="pam", distance = "dtw_basic")
names(depth_clust_k) <- paste0("k_", 2:7)
k_table<-sapply(depth_clust_k, cvi, type = "internal")
# print table
k_table
# Note:
## some indices should maximized ("Sil","SF","CH","D") and some should be minimized ("DB","DBstar","COP")
# looks like with new data, 2 or 3 may be ideal

# look at cluster no. of 2 to 6 max
# NOTE: this can take a long time to run! suggest running on a computer with lots of RAM/memory...
depth_clust_k <- tsclust(series=dat_s_n, k = 2:7, centroid="shape", distance = "dtw_basic")
names(depth_clust_k) <- paste0("k_", 2:7)
k_table<-sapply(depth_clust_k, cvi, type = "internal")
# print table
k_table
# Note:
## some indices should maximized ("Sil","SF","CH","D") and some should be minimized ("DB","DBstar","COP")
# looks like with new data, 2 or 3 may be ideal


#### Performing DTW Clustering ####

# Choices of parameters for dtw clustering:

# window size: limits distance that points can be matched to each other
## I don't want a limit ( I want all observations for a depth to be considered)

# k = no. of clusters
## can use selection criteria with dtwclust package to determine optimal no. for k
## we chose k=4 above

# centroid = time series prototype (time-series averaging method = summarizes imp. characteristics for all series in a given cluster)
## PAM centroid is likely the best candidate- time series with minimum sum of distances to others in cluster (also allows series of diff lengths)
## for PAM: cluster centroids are generally one of the time series from the data
## from the manual (Sarda-Espinosa 2019): 
## " partitional clustering creates k number of clusters from data
## k centroids are randomly initialized (choose k objects from dataset at random = k depths)
## each is then assigned to individual clusters
## distance between all data objects (depths) and all centroids (random k depths) is calculated
## each object/depth is assigned to the cluster of its closest centroid (random k depth time series)
## protyping function iS then applied to each cluster to update the corresponding centroid (e.g. median)
## distances and centroids are updated iteratively (until no more objects can change clusters)"
## note: clustering is generally unsupervised but clusters can be evaluated...

# we also use the "dtw_basic" distance measure
## core calculations for distances of dtwclust are performed in C++ (fast)
## basic uses DTW distance measure and has less functionality than other options (?) but is faster

set.seed(666)
# run analysis with k=4
# depth_clust_k4 <- tsclust(series = dat_s_n, k = 4, distance = 'dtw_basic',centroid="pam")
# depth_clust_k4
# plot(depth_clust_k4)

# run analysis with k=2
depth_clust_k2 <- tsclust(series = dat_s_n, k = 2, distance = 'dtw_basic',centroid="pam")
depth_clust_k2
plot(depth_clust_k2)

# run analysis with k=3
depth_clust_k3 <- tsclust(series = dat_s_n, k = 3, distance = 'dtw_basic',centroid="pam")
depth_clust_k3
plot(depth_clust_k3)
#optimal

# # run analysis with k=4
# depth_clust_k4_dba <- tsclust(series = dat_s_n, k = 4, distance = 'dtw_basic',centroid="dba")
# 
# # run analysis with k=4
# depth_clust_k2_shape <- tsclust(series = dat_s_n, k = 2, distance = 'dtw_basic',centroid="shape")
# depth_clust_k2_shape
# plot(depth_clust_k2_shape)
# # run analysis with k=4
# depth_clust_k4_shape <- tsclust(series = dat_s_n, k = 4, distance = 'dtw_basic',centroid="shape")
# depth_clust_k4_shape
# plot(depth_clust_k4_shape)

#### Merge Cluster Data With original Data ####

# format cluster data
## 52 was the # of original curves
cluster_data1<-as.data.frame(list(DTW=list(cumsum(rep(1,52))),cluster=list(depth_clust_k3@cluster)))
colnames(cluster_data1)<-c("DTW_id","cluster")
# merge data df's together
cluster_DTW_data_k3<-cbind(times, cluster_data1,dat_s_n)
# 
# # format cluster data
# ## 59 was the # of original curves
# cluster_data1_dba<-as.data.frame(list(DTW=list(cumsum(rep(1,59))),cluster=list(depth_clust_k4_dba@cluster)))
# colnames(cluster_data1_dba)<-c("DTW_id","cluster")
# # merge data df's together
# cluster_DTW_k4_dba<-cbind(times, cluster_data1_dba,dat_s_n)
# 
# # format cluster data
# ## 59 was the # of original curves
# cluster_data1_shape<-as.data.frame(list(DTW=list(cumsum(rep(1,59))),cluster=list(depth_clust_k4_shape@cluster)))
# colnames(cluster_data1_shape)<-c("DTW_id","cluster")
# # merge data df's together
# cluster_DTW_k4_shape<-cbind(times, cluster_data1_shape,dat_s_n)


#### View Results ####

# view summary of results
# includes the number of curves in each cluster, and the average distance of curves from the "ideal" curve 
depth_clust_k3
# depth_clust_k4_dba
# depth_clust_k4_shape

# there are a couple ways to plot results

## 1- with the output of the dtwclust function
plot(depth_clust_k3)
# plot(depth_clust_k4_dba)
# plot(depth_clust_k4_shape)
# this shows the centroid cluster (the one most representative of the cluster) in thick dashed line
# and the rest of the curves overlain

## 2- extract the centroid from each dtwclust object and plot with ggplot
# can find centroids (by their list number) in the output of the dtwclust function
attr(depth_clust_k3@centroids,"series_id")
## events 24 22 14 are the centroids for each cluster

dat_s_n_forplot = data.frame(t(dat_s_n[c(24,22,14),]))
names(dat_s_n_forplot) = colnames(dat_s_n_forplot)
dat_s_n_forplot$t = rownames(dat_s_n_forplot)
dat_s_n_forplot_long = dat_s_n_forplot %>% pivot_longer(cols='e24':'e14',
                                                        names_to = "event",
                                                        values_to = "DTW_m")
dat_s_n_forplot_long$timestep = as.numeric(gsub('t', '', dat_s_n_forplot_long$t))
dat_s_n_forplot_long =
  dat_s_n_forplot_long %>%
  mutate(cluster = case_match(event, 
                              "e24" ~ 1,
                              "e22" ~ 2,
                              "e14" ~ 3))

centriodcurvesp = 
  ggplot(dat_s_n_forplot_long, aes(x=timestep,y=DTW_m))+
  geom_line(linewidth=2)+
  theme_classic()+
  xlab("Time (min)")+ylab("Normalized Depth (m)")+
  facet_wrap(~cluster)


# plot all curves of each cluster
cluster_DTW_data_k3_long = cluster_DTW_data_k3 %>% pivot_longer(cols='t12':'t192',
                                                                names_to = "timestep",
                                                                values_to = "DTW_m")
cluster_DTW_data_k3_long$timestep = as.numeric(gsub('t', '', cluster_DTW_data_k3_long$timestep))

ggplot(cluster_DTW_data_k3_long, aes(x=timestep,y=DTW_m, color=ename))+
  geom_line(linewidth=1)+
  theme_classic()+
  xlab("Time (min)")+ylab("Depth (m)")+
  facet_wrap(~cluster)


# plot the mean of all curves in each cluster
mean_k3_cluster = 
  cluster_DTW_data_k3_long %>%
  group_by(cluster,timestep) %>%
  summarise(DTW_m_mean = mean(DTW_m))

meancurvesp = ggplot(mean_k3_cluster, aes(x=timestep,y=DTW_m_mean))+
  geom_line(linewidth=2)+
  theme_classic()+
  xlab("Time (min)")+ylab("Normalized Depth (m)")+
  facet_wrap(~cluster)
ggsave("plots/meancurvesp.png", meancurvesp, width = 9, height = 8, units = "in")
#### save results ####

write.csv(cluster_DTW_data_k3, "DTW_compiled/DTW_clusters_k3_smoothed_norm.csv", row.names = FALSE)

# qualitative descriptions based on centroid="pam" k4 results:
# note that the data is depth to water in meters below the surface. We often multiply by -1 to view it as below the surface, but that is not the case in these plots. I am interpreting it in the negative so that the interpretation is more intuative (e.g., "net rise" = water got closer to surface)
# cluster 1 (11 events): net drop in water table, ending on dropping water. >2 peaks.
# cluster 2 (11 events): little to no net change in water table but strong "diel" amplitude. >2 peaks.
# cluster 3 (13 events): strong net drop ending on rising water. 2-4 peaks.
# cluster 4 (24 events): net rise in water table with various numbers of peaks/troughs
# ^ this is the old analysis! change for this one. 

#based on these 3 plots:
plot(depth_clust_k3)
meancurvesp
centriodcurvesp

# Cluster sizes with average intra-cluster distance:
#   
#   size   av_dist
# 1   11 11.489924
# 2   13  9.653225
# 3   10 13.114668
# 4   25 14.163619

#### plot cluster with Well ID ####
cluster_DTW_data_k3 <- read_csv("DTW_compiled/DTW_clusters_k3_smoothed_norm.csv")

#import BEGI events (with tc data)
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")

#Match event_time with Eventdate for each well
#Turns out the events in the csv are in order for each well's Eventdate list :D
cluster_DTW_data_k3$well_id <- rep(c("SLOC","SLOW","VDOW","VDOS"),
                                   times = c(length(BEGI_events[["Eventdate"]][["SLOC_dates"]]),
                                             length(BEGI_events[["Eventdate"]][["SLOW_dates"]]),
                                             length(BEGI_events[["Eventdate"]][["VDOW_dates"]]),
                                             length(BEGI_events[["Eventdate"]][["VDOS_dates"]])))

#count of what clusters occurred in each well
cluster_by_well <- data.frame(SLOC = c(sum(cluster_DTW_data_k3$cluster == 1 & cluster_DTW_data_k3$well_id == 'SLOC'),
                                       sum(cluster_DTW_data_k3$cluster == 2 & cluster_DTW_data_k3$well_id == 'SLOC'),
                                       sum(cluster_DTW_data_k3$cluster == 3 & cluster_DTW_data_k3$well_id == 'SLOC')),
                              SLOW = c(sum(cluster_DTW_data_k3$cluster == 1 & cluster_DTW_data_k3$well_id == 'SLOW'),
                                       sum(cluster_DTW_data_k3$cluster == 2 & cluster_DTW_data_k3$well_id == 'SLOW'),
                                       sum(cluster_DTW_data_k3$cluster == 3 & cluster_DTW_data_k3$well_id == 'SLOW')),
                              VDOW = c(sum(cluster_DTW_data_k3$cluster == 1 & cluster_DTW_data_k3$well_id == 'VDOW'),
                                       sum(cluster_DTW_data_k3$cluster == 2 & cluster_DTW_data_k3$well_id == 'VDOW'),
                                       sum(cluster_DTW_data_k3$cluster == 3 & cluster_DTW_data_k3$well_id == 'VDOW')),
                              VDOS = c(sum(cluster_DTW_data_k3$cluster == 1 & cluster_DTW_data_k3$well_id == 'VDOS'),
                                       sum(cluster_DTW_data_k3$cluster == 2 & cluster_DTW_data_k3$well_id == 'VDOS'),
                                       sum(cluster_DTW_data_k3$cluster == 3 & cluster_DTW_data_k3$well_id == 'VDOS')))

# plot all curves of each cluster
cluster_DTW_data_k3_long = cluster_DTW_data_k3 %>% pivot_longer(cols='t12':'t192',
                                                                names_to = "timestep",
                                                                values_to = "DTW_m")
cluster_DTW_data_k3_long$timestep = as.numeric(gsub('t', '', cluster_DTW_data_k3_long$timestep))

well_clusters<-ggplot(cluster_DTW_data_k3_long, aes(x=timestep,y=DTW_m, group=ename, color=well_id))+
  geom_line(linewidth=1,)+
  theme_classic()+
  xlab("Time (min)")+ylab("Normalized Depth (m)")+
  facet_wrap(~cluster) +
  theme(text = element_text(size = 20))+
  scale_color_manual(values=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))

well_clusters

final_cluster <- well_clusters / tableGrob(cluster_by_well) +
  plot_layout(heights = c(4,1))


ggsave("plots/well_clusters.png", well_clusters, width = 12, height = 7, units = "in")
ggsave("plots/final_cluster.png", final_cluster, width = 14, height = 9, units = "in")

##########################################
#### 2. fDOM cluster analysis ####
rm(list = ls())
##########################################
#### load data ####
fdom_df = readRDS("DTW_compiled/BEGI_EXOz_dtw.rds")
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")

#### create timeseries matrix of fDOM following DO event ####
#SLOC#
fdom_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["SLOC_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["SLOC_dates"]][[i]]
  
  start_time <- max(event_time, na.rm = TRUE)
  end_time   <- start_time + 48 * 60 * 60
  
  tempdat <- fdom_df[["SLOC"]][
    fdom_df[["SLOC"]]$datetimeMT >= start_time & fdom_df[["SLOC"]]$datetimeMT < end_time,
  ]
  #remove NAs from dataset  
  valid_SLOC <- tempdat$fDOM.QSU.mn[!is.na(tempdat$fDOM.QSU.mn)]
  #extract 192 values  
  if (length(valid_SLOC) >= 192) {
    ts_values <- valid_SLOC[1:192]
  } else {
    ts_values <- c(valid_SLOC, rep(NA, 192 - length(valid_SLOC)))
  }
  
  fdom_timeseries[[i]] <- ts_values
}

#combine to dataframe
SLOC_fdom <- as.data.frame(do.call(rbind, fdom_timeseries))
colnames(SLOC_fdom) <- paste0("t", seq_len(192))
SLOC_fdom$event_time <- BEGI_events[["Eventdate"]][["SLOC_dates"]]


#SLOW#
fdom_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["SLOW_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["SLOW_dates"]][[i]]
  
  start_time <- max(event_time, na.rm = TRUE)
  end_time   <- start_time + 48 * 60 * 60
  
  tempdat <- fdom_df[["SLOW"]][
    fdom_df[["SLOW"]]$datetimeMT >= start_time & fdom_df[["SLOW"]]$datetimeMT < end_time,
  ]
  #remove NAs from dataset  
  valid_SLOW <- tempdat$fDOM.QSU.mn[!is.na(tempdat$fDOM.QSU.mn)]
  #extract 192 values  
  if (length(valid_SLOW) >= 192) {
    ts_values <- valid_SLOW[1:192]
  } else {
    ts_values <- c(valid_SLOW, rep(NA, 192 - length(valid_SLOW)))
  }
  
  fdom_timeseries[[i]] <- ts_values
}

#combine to dataframe
SLOW_fdom <- as.data.frame(do.call(rbind, fdom_timeseries))
colnames(SLOW_fdom) <- paste0("t", seq_len(192))
SLOW_fdom$event_time <- BEGI_events[["Eventdate"]][["SLOW_dates"]]


#VDOW#
fdom_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["VDOW_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["VDOW_dates"]][[i]]
  
  start_time <- max(event_time, na.rm = TRUE)
  end_time   <- start_time + 48 * 60 * 60
  
  tempdat <- fdom_df[["VDOW"]][
    fdom_df[["VDOW"]]$datetimeMT >= start_time & fdom_df[["VDOW"]]$datetimeMT < end_time,
  ]
  #remove NAs from dataset  
  valid_VDOW <- tempdat$fDOM.QSU.mn[!is.na(tempdat$fDOM.QSU.mn)]
  #extract 192 values  
  if (length(valid_VDOW) >= 192) {
    ts_values <- valid_VDOW[1:192]
  } else {
    ts_values <- c(valid_VDOW, rep(NA, 192 - length(valid_VDOW)))
  }
  
  fdom_timeseries[[i]] <- ts_values
}

#combine to dataframe
VDOW_fdom <- as.data.frame(do.call(rbind, fdom_timeseries))
colnames(VDOW_fdom) <- paste0("t", seq_len(192))
VDOW_fdom$event_time <- BEGI_events[["Eventdate"]][["VDOW_dates"]]


#VDOS#
fdom_timeseries <- list()

for (i in seq_along(BEGI_events[["Eventdate"]][["VDOS_dates"]])) {
  event_time <- BEGI_events[["Eventdate"]][["VDOS_dates"]][[i]]
  
  start_time <- max(event_time, na.rm = TRUE)
  end_time   <- start_time + 48 * 60 * 60
  
  tempdat <- fdom_df[["VDOS"]][
    fdom_df[["VDOS"]]$datetimeMT >= start_time & fdom_df[["VDOS"]]$datetimeMT < end_time,
  ]
  #remove NAs from dataset  
  valid_VDOS <- tempdat$fDOM.QSU.mn[!is.na(tempdat$fDOM.QSU.mn)]
  #extract 192 values  
  if (length(valid_VDOS) >= 192) {
    ts_values <- valid_VDOS[1:192]
  } else {
    ts_values <- c(valid_VDOS, rep(NA, 192 - length(valid_VDOS)))
  }
  
  fdom_timeseries[[i]] <- ts_values
}

#combine to dataframe
VDOS_fdom <- as.data.frame(do.call(rbind, fdom_timeseries))
colnames(VDOS_fdom) <- paste0("t", seq_len(192))
VDOS_fdom$event_time <- BEGI_events[["Eventdate"]][["VDOS_dates"]]



#combine all fdom dataframes
event_fdom <- rbind(SLOC_fdom,SLOW_fdom,VDOW_fdom,VDOS_fdom)
#save
saveRDS(event_fdom, "EXO_compiled/event_fdom.rds")


#### load and wrangle matrix data ####

dat = readRDS("EXO_compiled/event_fdom.rds") 
# I need to make an equivalent for the (estimated) duration of each event. Which is challenging because the timeframe of during each event varies
# I started with the 36 hour period of fDOM following each event.
# then I can take the average event length to look at gw depth curves during a DO event

dat$ename = paste("e", c(1:52), sep="")
rownames(dat) = dat$ename

# save date/time stamps of events separately 
times = dat[,193:194]
dat[,193:194] = NULL

# gap fill fdom series
dat_filled <- t(apply(dat, 1, function(e) {
  na.approx(e, na.rm = FALSE)
}))
dat_filled <- t(apply(dat_filled, 1, function(e) {
  e <- na.locf(e, na.rm = FALSE)
  na.locf(e, fromLast = TRUE)
}))

# make a version with data normalized to make all scales the same
normalized<-function(y) {
  
  x<-y[!is.na(y)]
  
  x<-(x - min(x)) / (max(x) - min(x))
  
  y[!is.na(y)]<-x
  
  return(y)
}
dat_n = t(apply(dat_filled,1,normalized))

# make versions with data smoothed and normalized
dat.xts = data.frame(t(dat_filled))
d = ( seq.POSIXt(
  from = as.POSIXct("2025-06-10 01:00:00", tz="US/Mountain"),
  to = as.POSIXct("2025-06-10 01:00:00", tz="US/Mountain")+172800,
  by = "15 min"))[1:192]
dat.xts = xts(dat.xts, order.by = d)
#dat.xts_s = rollmean(dat.xts, 12, fill=NA, align = "right")
dat.xts_s <- rollmean(dat.xts,
                      k     = 12,
                      fill  = "extend",   # no NAs at edges
                      align = "center"
)

# interpolate any remaining internal gaps
dat.xts_s <- na.approx(dat.xts_s, na.rm = FALSE)
dat_s = as.data.frame(t(dat.xts_s))
names(dat_s) = paste("t", c(1:192), sep="")
#dat_s = dat_s[,12:144]

dat_s_n = t(apply(dat_s,1,normalized))

#### plot data ####

# raw data
plot.new()
par(mfrow=c(3,3), mar=c(2,2,2,2))
for(i in c(1:52)){
  matplot((t(dat))[,i], type = "l", main=i)
}
#dev.off()

# norm data
plot.new()
par(mfrow=c(3,3), mar=c(2,2,2,2))
for(i in c(1:52)){
  matplot((t(dat_n))[,i], type = "l", main=i)
}

# smoothed data
plot.new()
par(mfrow=c(3,3), mar=c(2,2,2,2))
for(i in c(1:52)){
  matplot((t(dat_s))[,i], type = "l", main=i)
}

# smoothed and normalized data
plot.new()
par(mfrow=c(3,3), mar=c(2,2,2,2))
for(i in c(1:52)){
  matplot((t(dat_s_n))[,i], type = "l", main=i)
}

#### Evaluate Optimal Cluster Number ####

# NOTE: notes are mostly from the turtle dives paper's code
## Evaluate "optimal" cluster number using CVI's (cluster validity indices)
## internal CVI's: consider partioned data and try to define measure of cluster purity
## external CVI's: compare obtained partition to correct one (need a ground truth for this- we won't use these)
## note: which CVI to use is also subjective/needs testing... can go with "majority vote" from indices but you should check that the final result makes biological sense!
## note: can also use "clue" package to evaluate clusters

# look at cluster no. of 2 to 6 max
# NOTE: this can take a long time to run! suggest running on a computer with lots of RAM/memory...
depth_clust_k <- tsclust(series=dat_s_n, k = 2:7, centroid="pam", distance = "dtw_basic")
names(depth_clust_k) <- paste0("k_", 2:7)
k_table<-sapply(depth_clust_k, cvi, type = "internal")
# print table
k_table
# Note:
## some indices should maximized ("Sil","SF","CH","D") and some should be minimized ("DB","DBstar","COP")
# k=2 is optimal, k=4 as secondary option but non ideal

# look at cluster no. of 2 to 6 max
# NOTE: this can take a long time to run! suggest running on a computer with lots of RAM/memory...
depth_clust_k <- tsclust(series=dat_s_n, k = 2:7, centroid="shape", distance = "dtw_basic")
names(depth_clust_k) <- paste0("k_", 2:7)
k_table<-sapply(depth_clust_k, cvi, type = "internal")
# print table
k_table
# Note:
## some indices should maximized ("Sil","SF","CH","D") and some should be minimized ("DB","DBstar","COP")
# k=2 is optimal
#### Performing DTW Clustering ####

# Choices of parameters for dtw clustering:

# window size: limits distance that points can be matched to each other
## I don't want a limit ( I want all observations for a depth to be considered)

# k = no. of clusters
## can use selection criteria with dtwclust package to determine optimal no. for k

# centroid = time series prototype (time-series averaging method = summarizes imp. characteristics for all series in a given cluster)
## PAM centroid is likely the best candidate- time series with minimum sum of distances to others in cluster (also allows series of diff lengths)
## for PAM: cluster centroids are generally one of the time series from the data
## from the manual (Sarda-Espinosa 2019): 
## " partitional clustering creates k number of clusters from data
## k centroids are randomly initialized (choose k objects from dataset at random = k depths)
## each is then assigned to individual clusters
## distance between all data objects (depths) and all centroids (random k depths) is calculated
## each object/depth is assigned to the cluster of its closest centroid (random k depth time series)
## protyping function iS then applied to each cluster to update the corresponding centroid (e.g. median)
## distances and centroids are updated iteratively (until no more objects can change clusters)"
## note: clustering is generally unsupervised but clusters can be evaluated...

# we also use the "dtw_basic" distance measure
## core calculations for distances of dtwclust are performed in C++ (fast)
## basic uses DTW distance measure and has less functionality than other options (?) but is faster

set.seed(666)

# run analysis with k=2 (pam)
depth_clust_k2 <- tsclust(series = dat_s_n, k = 2, distance = 'dtw_basic',centroid="pam")
depth_clust_k2
plot(depth_clust_k2)
# optimal

# run analysis with k=2 (shape)
depth_clust_k2s <- tsclust(series = dat_s_n, k = 2, distance = 'dtw_basic',centroid="shape")
depth_clust_k2s
plot(depth_clust_k2s)
# optimal

# # run analysis with k=3
# depth_clust_k3 <- tsclust(series = dat_s_n, k = 3, distance = 'dtw_basic',centroid="pam")
# depth_clust_k3
# plot(depth_clust_k3)
# 
# # run analysis with k=4
# depth_clust_k4 <- tsclust(series = dat_s_n, k = 4, distance = 'dtw_basic',centroid="pam")
# depth_clust_k4
# plot(depth_clust_k4)


#### Merge Cluster Data With original Data ####

# format cluster data
## 52 was the # of original curves
cluster_data<-as.data.frame(list(DTW=list(cumsum(rep(1,52))),cluster=list(depth_clust_k2@cluster)))
colnames(cluster_data)<-c("DTW_id","cluster")
# merge data df's together
cluster_DTW_data_k2<-cbind(times, cluster_data,dat_s_n)


#### View results ####
# view summary of results
# includes the number of curves in each cluster, and the average distance of curves from the "ideal" curve 
depth_clust_k2

# size   av_dist
# 22 13.174740
# 30  9.539185

# there are a couple ways to plot results

## 1- with the output of the dtwclust function
plot(depth_clust_k2)
# this shows the centroid cluster (the one most representative of the cluster) in thick dashed line
# and the rest of the curves overlain

## 2- extract the centroid from each dtwclust object and plot with ggplot
# can find centroids (by their list number) in the output of the dtwclust function
attr(depth_clust_k2@centroids,"series_id")
## events 27 36 are the centroids for each cluster

dat_s_n_forplot = data.frame(t(dat_s_n[c(27,36),]))
names(dat_s_n_forplot) = colnames(dat_s_n_forplot)
dat_s_n_forplot$t = rownames(dat_s_n_forplot)
dat_s_n_forplot_long = dat_s_n_forplot %>% pivot_longer(cols='e27':'e36',
                                                        names_to = "event",
                                                        values_to = "fDOM.QSU.mn")
dat_s_n_forplot_long$timestep = as.numeric(gsub('t', '', dat_s_n_forplot_long$t))
dat_s_n_forplot_long =
  dat_s_n_forplot_long %>%
  mutate(cluster = case_match(event, 
                              "e27" ~ 1,"e36" ~ 2))

centriodcurvesp = 
  ggplot(dat_s_n_forplot_long, aes(x=timestep,y=fDOM.QSU.mn))+
  geom_line(linewidth=2)+
  theme_classic()+
  xlab("Time (min)")+ylab("Normalized fDOM (QSU)")+
  facet_wrap(~cluster)


# plot all curves of each cluster
cluster_DTW_data_k2_long = cluster_DTW_data_k2 %>% pivot_longer(cols='t12':'t192',
                                                                names_to = "timestep",
                                                                values_to = "fDOM.QSU.mn")
cluster_DTW_data_k2_long$timestep = as.numeric(gsub('t', '', cluster_DTW_data_k2_long$timestep))

ggplot(cluster_DTW_data_k2_long, aes(x=timestep,y=fDOM.QSU.mn, color=ename))+
  geom_line(linewidth=1)+
  theme_classic()+
  xlab("Time (min)")+ylab("Normalized fDOM (QSU)")+
  facet_wrap(~cluster)


# plot the mean of all curves in each cluster
mean_k2_cluster = 
  cluster_DTW_data_k2_long %>%
  group_by(cluster,timestep) %>%
  summarise(fdom_mean = mean(fDOM.QSU.mn))

meancurvesp = ggplot(mean_k2_cluster, aes(x=timestep,y=fdom_mean))+
  geom_line(linewidth=2)+
  theme_classic()+
  xlab("Time (min)")+ylab("Normalized fDOM (QSU)")+
  facet_wrap(~cluster)
meancurvesp
ggsave("plots/meancurves_fdom.png", meancurvesp, width = 9, height = 8, units = "in")

#### plot cluster with Well ID ####
#import BEGI events (with tc data)
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")

#Match event_time with Eventdate for each well
#Turns out the events in the csv are in order for each well's Eventdate list :D
#I NEED TO DOUBLE CHECK THAT THIS IS ACCURATE. IT SHOULD BE CONSIDERING THIS IS THE FDOM 
# FOLLOWING EACH DO EVENT, SO IT SHOULD BE IN LINE
cluster_DTW_data_k2$well_id <- rep(c("SLOC","SLOW","VDOW","VDOS"),
                                   times = c(length(BEGI_events[["Eventdate"]][["SLOC_dates"]]),
                                             length(BEGI_events[["Eventdate"]][["SLOW_dates"]]),
                                             length(BEGI_events[["Eventdate"]][["VDOW_dates"]]),
                                             length(BEGI_events[["Eventdate"]][["VDOS_dates"]])))

# plot with well id
# plot all curves of each cluster
cluster_DTW_data_k2_long = cluster_DTW_data_k2 %>% pivot_longer(cols='t12':'t192',
                                                                names_to = "timestep",
                                                                values_to = "DTW_m")
cluster_DTW_data_k2_long$timestep = as.numeric(gsub('t', '', cluster_DTW_data_k2_long$timestep))

fdom_clusters<-ggplot(cluster_DTW_data_k2_long, aes(x=timestep,y=DTW_m, group=ename, color=well_id))+
  geom_line(linewidth=1,)+
  theme_classic()+
  xlab("Time (min)")+ylab("Normalized fDOM")+
  facet_wrap(~cluster) +
  theme(text = element_text(size = 20))+
  scale_color_manual(values=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))
fdom_clusters

# manuscript fig
# Rename clusters for meaningful facet labels
cluster_DTW_data_k2_long$cluster_label <- factor(
  cluster_DTW_data_k2_long$cluster,
  labels = c("Cluster 1", "Cluster 2")
)

fdom_clusters <- ggplot(
  cluster_DTW_data_k2_long,
  aes(x = timestep, y = DTW_m, group = ename, color = well_id)
) +
  geom_line(linewidth = 0.6, alpha = 0.75) +
  facet_wrap(~cluster_label) +
  scale_color_manual(
    values = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"),
    name = "Well ID"           # cleaner legend title
  ) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 1)) +
  labs(
    x = "Time following DO event (min)",
    y = "Normalized fDOM"
  ) +
  theme_classic(base_size = 11) +
  theme(
    # Facet strip
    strip.background = element_rect(fill = "grey92", color = "black", linewidth = 0.4),
    strip.text       = element_text(size = 11, face = "bold"),
    
    # Axes
    axis.line        = element_line(linewidth = 0.4, color = "black"),
    axis.ticks       = element_line(linewidth = 0.3, color = "black"),
    axis.ticks.length = unit(2, "pt"),
    axis.title       = element_text(size = 11),
    axis.text        = element_text(size = 9, color = "black"),
    
    # Legend
    legend.title     = element_text(size = 10, face = "bold"),
    legend.text      = element_text(size = 9),
    legend.key.size  = unit(12, "pt"),
    legend.key       = element_rect(fill = NA),
    legend.background = element_rect(fill = NA),
    
    # Panel spacing
    panel.spacing    = unit(8, "pt"),
    plot.margin      = margin(6, 6, 6, 6, "pt")
  )

# Export at journal-appropriate resolution
ggsave(
  "fdom_clusters.pdf",   # PDF for vector-based submission
  fdom_clusters,
  width = 6.5, height = 3.5, units = "in"   # fits a 2-column figure
)

# Also save PNG for preprint/preview
ggsave(
  "fdom_clusters.png",
  fdom_clusters,
  width = 6.5, height = 3.5, units = "in",
  dpi = 300
)

#### save results ####

saveRDS(cluster_DTW_data_k2, "DTW_compiled/fdom_clusters_k2_smoothed_norm.rds")


#### Identify fdom rebounding events ####
cluster_data_k2 <- readRDS("DTW_compiled/fdom_clusters_k2_smoothed_norm.rds")
# events 1-52 are in order of how they were compiled in the timeseries matrix
# event_fdom <- rbind(SLOC_fdom,SLOW_fdom,VDOW_fdom,VDOS_fdom)
# match them up with event ID or datetime
# create another df of just the events in cluster 1 (rebounding fdom)

#import BEGI events (with tc data)
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")

# create table of events
do_lookup <- imap_dfr(
  BEGI_events$DO_events,
  function(site_events, site) {
    imap_dfr(
      site_events,
      function(event_df, event_name) {
        tibble(
          site = site,
          event_id = event_name,
          event_time = event_df$datetimeMT[1]  
        )
      }
    )
  }
)

# standardize datetime classes
cluster_data_k2$event_time <- as.POSIXct(cluster_data_k2$event_time, tz = "US/Mountain")
do_lookup$event_time <- as.POSIXct(do_lookup$event_time, tz = "US/Mountain")

# confirm match between BEGI events and cluster matrix
match <- cluster_data_k2$event_time - do_lookup$event_time
# should all be 0

# match cluster data with event
cluster_data_k2$event_id <-do_lookup$event_id

# dataframe of just rebounding fdom (cluster 1)
fdom_rebound <- cluster_data_k2[cluster_data_k2$cluster == 1, ]

fdom_df = readRDS("EXO_compiled/BEGI_EXOz.ts.tc.rds")


#### Create data frame of just events where fDOM DOES NOT rebound ####
cluster2 <- cluster_data_k2[cluster_data_k2$cluster == 2, ]
cluster2_events <- cluster2$event_id


# subset roc_all for 09_DOtoER
ER_calc_all = read_csv("EXO_compiled/ER_calc_all_events.csv")

ER_calc_cluster2 <- lapply(
  ER_calc_all,
  function(well_list) {
    well_list[names(well_list) %in% cluster2_events]
  }
)

saveRDS(ER_calc_cluster2, "EXO_compiled/ER_calc_cluster2.rds")
