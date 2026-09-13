#### Read me ####
# The purpose of this script is to compile code for publication-ready figures
# This script produces figures for 
# Timeseries of Q, gw depth, DO
# boxplots for total event size, total A, and total ER in areal units 
# cluster analysis results for a) gw depth and b) fdom
# megaplots
# model results

#### Libraries ####
library(tidyverse)
library(ggplot2)
library(DescTools)
library(nlme)
library(lme4)
library(lmerTest)
library(viridis)
library(patchwork)
library(cowplot)
library(grid)
library(ggplotify)
library(stringr)
library(dtwclust)
library(reshape2)
library(zoo)
library(xts)
library(gridExtra)
library(dplyr)
library(scales)
library(patchwork)

############################
#### Plot DO timeseries ####
############################
#### Import data ####
EXOz.or3 <- readRDS("DTW_compiled/BEGI_EXOz_dtw.rds")

DTW_df = readRDS("DTW_compiled/BEGI_PT_DTW_all.rds")

#### Wrangle data ####
# Trim DTW data set to match EXOz #
# Find the last datetime in the shorter dataframe
cutoff <- max(EXOz.or3[["VDOW"]]$datetime)

# Trim the longer dataframe to that cutoff
DTW_df <- DTW_df %>%
  filter(datetimeMT <= cutoff)

# combine the per-site list into one data frame - faceted plotting needs a single data frame, not a list
EXOz_all <- dplyr::bind_rows(EXOz.or3, .id = "wellID")

# derive the two-letter site grouping (VDO/SLO) from wellID,
EXOz_all$siteID <- substr(EXOz_all$wellID, 1, 3)


#### Plot final timeseries ####

Q = 
  ggplot(DTW_df, aes(datetimeMT, Q_Lsec)) +
  xlab("") +
  #ylab("Discharge \n(L/sec)") +
  ylab(expression(atop("Discharge", paste("(L ", sec^-1, ")"))))+
  geom_line(linewidth=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 20))+
  geom_text(data = data.frame(siteID = unique(EXOz_all$siteID)[1]),
            aes(x = -Inf, y = Inf), label = "A",
            hjust = -0.5, vjust = 1.5, size = 6, color = "black",
            inherit.aes = FALSE)

DTW = 
  ggplot(DTW_df, aes(datetimeMT, DTW_m, color=wellID)) +
  xlab("") +
  ylab("Water Depth \nBelow Surface (m)")+
  geom_hline(yintercept=0, linetype = 'dashed') +
  geom_line(key_glyph = "timeseries",linewidth=1,alpha=0.75) +
  facet_grid(rows=vars(siteID))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 20)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  geom_text(data = data.frame(siteID = sort(unique(EXOz_all$siteID))[1]),
            aes(x = -Inf, y = Inf), label = "B",
            hjust = -0.5, vjust = 1.5, size = 6, color = "black",
            inherit.aes = FALSE)

ODO_fig = 
  ggplot(EXOz_all, aes(datetimeMT, ODO.mg.L.mn_sm, color = wellID)) +
  xlab("") +
  ylab(expression(atop("Dissolved Oxygen", paste("(mg ", L^-1, ")"))))+
  geom_line(key_glyph = "timeseries",linewidth=1,alpha=0.75) +
  facet_grid(rows = vars(siteID)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  geom_text(data = data.frame(siteID = sort(unique(EXOz_all$siteID))[1]),
            aes(x = -Inf, y = Inf), label = "C",
            hjust = -0.5, vjust = 1.5, size = 6, color = "black",
            inherit.aes = FALSE)


DO_ts = Q+ DTW+ ODO_fig+ plot_layout(ncol = 1, widths = c(1,.84, 1), heights=c(1.4,3.1,3.1))
ggsave("plots/DO_timeseries.png", DO_ts, width=11,height=8, units="in")

#### Clear environment ####
rm(list = ls())
############################
#### Boxplots ####
############################
#### Import data ####
# event calculations
ER_results <- read.csv("EXO_compiled/ER_calc_all_events.csv")

#### boxplots ####
# total event size, total accrual, and total ER in areal units

# total event size # 
eventsize_bp <- ggplot (data = ER_results, 
                        mapping = aes(x = site, y = gross_total_areal_gO2_m2)) + 
  geom_boxplot(fill = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF")) +
  theme_grey(base_size = 18) +
  ylab(expression(atop("Total Event Size", paste("(g" ~ O[2] ~ m^-2 ~")")))) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5,
         size = 6)

# total accrual #
accrual_bp <- ggplot (data = ER_results, 
                        mapping = aes(x = site, y = accrual_total_areal_gO2_m2)) + 
  geom_boxplot(fill = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF")) +
  theme_grey(base_size = 18) +
  ylab(expression(atop("Total Accrual", paste("(g" ~ O[2] ~ m^-2 ~")")))) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5,
           size = 6)

# total ER #
ER_bp <- ggplot (data = ER_results, 
                      mapping = aes(x = site, y = ER_total_areal_gO2_m2)) + 
  geom_boxplot(fill = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF")) +
  theme_grey(base_size = 18) +
  ylab(expression(atop("Ecosystem Respiration", paste("(g" ~ O[2] ~ m^-2 ~")")))) +
  xlab("Well")+
  annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 1.5,
           size = 6)

# plotted together #
finalevent_bp = eventsize_bp+ accrual_bp+ ER_bp+ plot_layout(ncol = 1, widths = c(1, 1, 1), heights=c(3, 3, 3))
ggsave("plots/finalevent_bp.pdf", finalevent_bp, width=6,height=11, units="in")


#### Clear environment ####
rm(list = ls())

############################
#### Cluster Analysis ####
############################
#### Import data ####
# groundwater depth clusters
gw_cluster3 <- read_csv("DTW_compiled/DTW_clusters_k3_smoothed_norm.csv")

# fdom clusters
fdom_cluster2 <- readRDS("DTW_compiled/fdom_clusters_k2_smoothed_norm.rds")

#import BEGI events
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")

#### Plot depth to groundwater clusters ####
#Match event_time with Eventdate for each well
#Turns out the events in the csv are in order for each well's Eventdate list :D
gw_cluster3$well_id <- rep(c("SLOC","SLOW","VDOW","VDOS"),
                                   times = c(length(BEGI_events[["Eventdate"]][["SLOC_dates"]]),
                                             length(BEGI_events[["Eventdate"]][["SLOW_dates"]]),
                                             length(BEGI_events[["Eventdate"]][["VDOW_dates"]]),
                                             length(BEGI_events[["Eventdate"]][["VDOS_dates"]])))

#count of what clusters occurred in each well
cluster_by_well <- data.frame(SLOC = c(sum(gw_cluster3$cluster == 1 & gw_cluster3$well_id == 'SLOC'),
                                       sum(gw_cluster3$cluster == 2 & gw_cluster3$well_id == 'SLOC'),
                                       sum(gw_cluster3$cluster == 3 & gw_cluster3$well_id == 'SLOC')),
                              SLOW = c(sum(gw_cluster3$cluster == 1 & gw_cluster3$well_id == 'SLOW'),
                                       sum(gw_cluster3$cluster == 2 & gw_cluster3$well_id == 'SLOW'),
                                       sum(gw_cluster3$cluster == 3 & gw_cluster3$well_id == 'SLOW')),
                              VDOW = c(sum(gw_cluster3$cluster == 1 & gw_cluster3$well_id == 'VDOW'),
                                       sum(gw_cluster3$cluster == 2 & gw_cluster3$well_id == 'VDOW'),
                                       sum(gw_cluster3$cluster == 3 & gw_cluster3$well_id == 'VDOW')),
                              VDOS = c(sum(gw_cluster3$cluster == 1 & gw_cluster3$well_id == 'VDOS'),
                                       sum(gw_cluster3$cluster == 2 & gw_cluster3$well_id == 'VDOS'),
                                       sum(gw_cluster3$cluster == 3 & gw_cluster3$well_id == 'VDOS')))

# plot all curves of each cluster
gw_cluster3_long = gw_cluster3 %>% pivot_longer(cols='t12':'t192',
                                                                names_to = "timestep",
                                                                values_to = "DTW_m")
gw_cluster3_long$timestep = as.numeric(gsub('t', '', gw_cluster3_long$timestep))

gw_clusters<-ggplot(gw_cluster3_long, aes(x=timestep,y=DTW_m, group=ename, color=well_id))+
  geom_line(linewidth=1,)+
  theme_classic()+
  xlab("Time (min)")+ylab("Normalized Depth (m)")+
  labs(color = "Well") +
  facet_wrap(~cluster) +
  theme(text = element_text(size = 20))+
  scale_color_manual(values=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))

# manuscript fig
# Rename clusters for meaningful facet labels
gw_cluster3_long$cluster_label <- factor(
  gw_cluster3_long$cluster,
  labels = c("Cluster 1", "Cluster 2", "Cluster 3")
)

gw_clusters <- ggplot(
  gw_cluster3_long,
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
    x = "Time preceding DO event (min)",
    y = "Normalized Depth (m)"
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
  "plots/gw_clusters_final.pdf",   # PDF for vector-based submission
  gw_clusters,
  width = 6.5, height = 3.5, units = "in"   # fits a 2-column figure
)

# Also save PNG for preprint/preview
ggsave(
  "plots/gw_clusters_final.png",
  gw_clusters,
  width = 6.5, height = 3.5, units = "in",
  dpi = 300
)



#### Plot fDOM clusters ####
#Match event_time with Eventdate for each well
fdom_cluster2$well_id <- rep(c("SLOC","SLOW","VDOW","VDOS"),
                                   times = c(length(BEGI_events[["Eventdate"]][["SLOC_dates"]]),
                                             length(BEGI_events[["Eventdate"]][["SLOW_dates"]]),
                                             length(BEGI_events[["Eventdate"]][["VDOW_dates"]]),
                                             length(BEGI_events[["Eventdate"]][["VDOS_dates"]])))

# plot with well id
# plot all curves of each cluster
fdom_cluster2_long = fdom_cluster2 %>% pivot_longer(cols='t12':'t192',
                                                                names_to = "timestep",
                                                                values_to = "DTW_m")
fdom_cluster2_long$timestep = as.numeric(gsub('t', '', fdom_cluster2_long$timestep))

fdom_clusters<-ggplot(fdom_cluster2_long, aes(x=timestep,y=DTW_m, group=ename, color=well_id))+
  geom_line(linewidth=1,)+
  theme_classic()+
  xlab("Time (min)")+ylab("Normalized fDOM")+
  labs(color = "Well") +
  facet_wrap(~cluster) +
  theme(text = element_text(size = 20))+
  scale_color_manual(values=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"))
fdom_clusters

# manuscript fig
# Rename clusters for meaningful facet labels
fdom_cluster2_long$cluster_label <- factor(
  fdom_cluster2_long$cluster,
  labels = c("Cluster 1", "Cluster 2")
)

fdom_clusters <- ggplot(
  fdom_cluster2_long,
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
  "plots/fdom_clusters_final.pdf",   # PDF for vector-based submission
  fdom_clusters,
  width = 6.5, height = 3.5, units = "in"   # fits a 2-column figure
)

# Also save PNG for preprint/preview
ggsave(
  "plots/fdom_clusters_final.png",
  fdom_clusters,
  width = 6.5, height = 3.5, units = "in",
  dpi = 300
)





#### Clear environment ####
rm(list = ls())

############################
#### Megaplots ####
############################
#### Import data ####
#import BEGI events
BEGI_events = readRDS("EXO_compiled/BEGI_events.rds")

#import compiled DTW and EXOz.dtw
EXOz.dtw = readRDS("DTW_compiled/BEGI_EXOz_dtw.rds")

# Read in .csv files of service dates and times

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

#### Combine all events for a well into one mega-megaplot multi-panel summary figure per well ####

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
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  #geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
  
  g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  
  g3 <- ggplot(tempdat, aes(x = datetimeMT, y = -DTW_m)) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
    geom_vline(xintercept = as.POSIXct(service.SLOC$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
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
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  #geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
  
  g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  
  g3 <- ggplot(tempdat, aes(x = datetimeMT, y = -DTW_m)) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
    geom_vline(xintercept = as.POSIXct(service.SLOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
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
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  #geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
  
  g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  
  g3 <- ggplot(tempdat, aes(x = datetimeMT, y = -DTW_m)) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
    geom_vline(xintercept = as.POSIXct(service.VDOW$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
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
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "DO (mg/l)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  #geom_line(data=tempdatDOe, aes(x = datetimeMT, y = ODO.mg.L.mn), color="yellow", linewidth=5, alpha=.5)
  
  g2 <- ggplot(tempdat, aes(x = datetimeMT, y = fDOM.QSU.mn)) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() +labs(y = "fDOM (QSU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  
  g3 <- ggplot(tempdat, aes(x = datetimeMT, y = -DTW_m)) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "GW Depth (m)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g4 <- ggplot(tempdat, aes(x = datetimeMT, y = Turbidity.FNU.mn)) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs (y = "Turbidity (FNU)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g5 <- ggplot(tempdat, aes(x = datetimeMT, y = Temp..C.mn)) +
    geom_vline(xintercept = as.POSIXct(service.VDOS$datetimeMT), color = "red", linetype = "dashed") +
    scale_x_datetime(limits = c(start_time, end_time)) +
    geom_line(na.rm = TRUE) + theme_minimal() + labs(y = "Temp (°C)") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_blank(),plot.margin = margin(0, 5, 0, 5))
  
  g6 <- ggplot(tempdat, aes(x = datetimeMT, y = SpCond.µS.cm.mn)) +
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

# combine
vars_to_plot <- c("ODO.mg.L.mn", "fDOM.QSU.mn", "DTW_m", "Turbidity.FNU.mn", "Temp..C.mn", "SpCond.µS.cm.mn")
var_labels   <- c("DO\n(mg/L)", "fDOM\n(QSU)", "GW Depth\n(m)", "Turbidity\n(FNU)", "Temp\n(°C)", "SpCond\n(µS/cm)")

EVENTS_PER_ROW <- 5  # events per row within a well's page; tune to control page height

# Compact date/time label for a facet strip
format_event_range <- function(start, end) {
  if (as.Date(start) == as.Date(end)) {
    sprintf("%s\n%s\u2013%s", format(start, "%b %d"), format(start, "%H:%M"), format(end, "%H:%M"))
  } else {
    sprintf("%s\u2013\n%s", format(start, "%b %d %H:%M"), format(end, "%b %d %H:%M"))
  }
}

# Builds long-format data (x = hours elapsed since each event window's own start, not calendar datetime) plus a matching table of service visits
build_well_data <- function(site, event_list, service_df, buffer_hours = 3) {
  event_meta <- lapply(seq_along(event_list), function(i) {
    dz <- event_list[[i]]
    event_start <- min(dz$datetimeMT, na.rm = TRUE)
    event_end   <- max(dz$datetimeMT, na.rm = TRUE)
    list(i = i,
         event_start = event_start, event_end = event_end,
         start_time  = event_start - 3600 * buffer_hours,
         end_time    = event_end   + 3600 * buffer_hours)
  })
  
  event_label_lookup <- vapply(event_meta, function(m) {
    paste0("Event ", m$i, "\n", format_event_range(m$event_start, m$event_end))
  }, character(1))
  names(event_label_lookup) <- vapply(event_meta, function(m) as.character(m$i), character(1))
  
  event_data <- do.call(rbind, lapply(event_meta, function(m) {
    tempdat <- EXOz.dtw[[site]][
      EXOz.dtw[[site]]$datetimeMT >= m$start_time &
        EXOz.dtw[[site]]$datetimeMT <= m$end_time, ]
    tempdat$event_id  <- m$i
    tempdat$DTW_m_neg <- -tempdat$DTW_m
    tempdat$t_hours   <- as.numeric(difftime(tempdat$datetimeMT, m$start_time, units = "hours"))
    tempdat
  }))
  
  long_data <- do.call(rbind, lapply(seq_along(vars_to_plot), function(v) {
    col <- if (vars_to_plot[v] == "DTW_m") "DTW_m_neg" else vars_to_plot[v]
    data.frame(event_id = event_data$event_id, t_hours = event_data$t_hours,
               variable = var_labels[v], value = event_data[[col]])
  }))
  long_data$variable    <- factor(long_data$variable, levels = var_labels)
  long_data$event_label <- factor(event_label_lookup[as.character(long_data$event_id)],
                                  levels = event_label_lookup)
  
  service_lines <- do.call(rbind, lapply(event_meta, function(m) {
    sd <- service_df$datetimeMT[service_df$datetimeMT >= m$start_time & service_df$datetimeMT <= m$end_time]
    if (length(sd) == 0) return(NULL)
    data.frame(event_id = m$i,
               event_label = factor(event_label_lookup[as.character(m$i)], levels = event_label_lookup),
               t_hours = as.numeric(difftime(sd, m$start_time, units = "hours")))
  }))
  
  list(long_data = long_data, service_lines = service_lines)
}

# Events wrapped into fixed-width rows, each row a variable x event facet grid, stacked vertically, with the well ID as a title.
plot_well_wrapped <- function(site, event_list, service_df, events_per_row = EVENTS_PER_ROW,
                              strip_size = 7, axis_size = 6) {
  
  dat <- build_well_data(site, event_list, service_df)
  long_data <- dat$long_data
  service_lines <- dat$service_lines
  long_data$event_row <- ceiling(long_data$event_id / events_per_row)
  n_rows <- max(long_data$event_row)
  
  row_blocks <- lapply(seq_len(n_rows), function(r) {
    d_r <- long_data[long_data$event_row == r, ]
    sv_r <- if (!is.null(service_lines)) {
      service_lines[service_lines$event_id %in% unique(d_r$event_id), ]
    } else NULL
    
    p <- ggplot(d_r, aes(x = t_hours, y = value)) +
      { if (!is.null(sv_r) && nrow(sv_r) > 0)
        geom_vline(data = sv_r, aes(xintercept = t_hours), color = "red",
                   linetype = "dashed", linewidth = 0.25) } +
      geom_line(linewidth = 0.3, na.rm = TRUE) +
      facet_grid(variable ~ event_label, scales = "free_y", switch = "y") +
      labs(x = NULL, y = NULL) +
      theme_bw(base_size = 9) +
      theme(
        strip.text.y.left = element_text(angle = 0, size = strip_size),
        strip.text.x      = element_text(size = strip_size),
        strip.placement   = "outside",
        strip.background  = element_rect(fill = "grey95", color = NA),
        panel.spacing     = unit(0.15, "lines"),
        axis.text         = element_text(size = axis_size),
        axis.title.x      = element_blank(),
        panel.grid.minor  = element_blank()
      )
    if (r < n_rows) p <- p + theme(axis.text.x = element_blank())
    p
  })
  
  wrap_plots(row_blocks, ncol = 1) +
    plot_annotation(title = site, theme = theme(plot.title = element_text(size = 16, face = "bold")))
}

# Saves one well's figure as its own file, with page height scaled to how many row-blocks that well needs (more events = taller page).
save_well_page <- function(site, event_list, service_df, events_per_row = EVENTS_PER_ROW,
                           width = 11, height_per_row_block = 6.5,
                           out_dir = "plots/delineations") {
  p <- plot_well_wrapped(site, event_list, service_df, events_per_row = events_per_row)
  n_rows <- ceiling(length(event_list) / events_per_row)
  ggsave(file.path(out_dir, paste0("FigS_", site, "_all_events.pdf")),
         p, width = width, height = n_rows * height_per_row_block, limitsize = FALSE)
}

wells <- list(
  SLOC = list(events = BEGI_events[["DO_events"]][["SLOC_DO"]], service = service.SLOC),
  SLOW = list(events = BEGI_events[["DO_events"]][["SLOW_DO"]], service = service.SLOW),
  VDOW = list(events = BEGI_events[["DO_events"]][["VDOW_DO"]], service = service.VDOW),
  VDOS = list(events = BEGI_events[["DO_events"]][["VDOS_DO"]], service = service.VDOS)
)

for (site in names(wells)) {
  save_well_page(site, wells[[site]]$events, wells[[site]]$service)
}


# Pad with blank spacer rows so every well has the same number of vertical slots (max_n_rows)- this keeps row-block height and total page size identical across wells with different event counts.
plot_well_wrapped <- function(site, event_list, service_df, max_n_rows,
                              events_per_row = EVENTS_PER_ROW,
                              strip_size = 7, axis_size = 6) {
  
  dat <- build_well_data(site, event_list, service_df)
  long_data <- dat$long_data
  service_lines <- dat$service_lines
  long_data$event_row <- ceiling(long_data$event_id / events_per_row)
  n_rows <- max(long_data$event_row)
  
  row_blocks <- lapply(seq_len(n_rows), function(r) {
    d_r <- long_data[long_data$event_row == r, ]
    sv_r <- if (!is.null(service_lines)) {
      service_lines[service_lines$event_id %in% unique(d_r$event_id), ]
    } else NULL
    
    p <- ggplot(d_r, aes(x = t_hours, y = value)) +
      { if (!is.null(sv_r) && nrow(sv_r) > 0)
        geom_vline(data = sv_r, aes(xintercept = t_hours), color = "red",
                   linetype = "dashed", linewidth = 0.25) } +
      geom_line(linewidth = 0.3, na.rm = TRUE) +
      facet_grid(variable ~ event_label, scales = "free_y", switch = "y") +
      labs(x = NULL, y = NULL) +
      theme_bw(base_size = 9) +
      theme(
        strip.text.y.left = element_text(angle = 0, size = strip_size),
        strip.text.x      = element_text(size = strip_size),
        strip.placement   = "outside",
        strip.background  = element_rect(fill = "grey95", color = NA),
        panel.spacing     = unit(0.15, "lines"),
        axis.text         = element_text(size = axis_size),
        axis.title.x      = element_blank(),
        panel.grid.minor  = element_blank()
      )
    if (r < n_rows) p <- p + theme(axis.text.x = element_blank())
    p
  })
  
  n_pad <- max_n_rows - n_rows
  if (n_pad > 0) {
    row_blocks <- c(row_blocks, replicate(n_pad, patchwork::plot_spacer(), simplify = FALSE))
  }
  
  wrap_plots(row_blocks, ncol = 1) +
    plot_layout(heights = rep(1, max_n_rows)) +
    plot_annotation(title = site, theme = theme(plot.title = element_text(size = 16, face = "bold")))
}

# height fixed at max_n_rows * height_per_row_block for every well
save_well_page <- function(site, event_list, service_df, max_n_rows,
                           events_per_row = EVENTS_PER_ROW,
                           width = 11, height_per_row_block = 6.5,
                           out_dir = "plots/delineations") {
  p <- plot_well_wrapped(site, event_list, service_df, max_n_rows = max_n_rows,
                         events_per_row = events_per_row)
  ggsave(file.path(out_dir, paste0("FigS_", site, "_all_events.pdf")),
         p, width = width, height = max_n_rows * height_per_row_block, limitsize = FALSE)
}

wells <- list(
  SLOC = list(events = BEGI_events[["DO_events"]][["SLOC_DO"]], service = service.SLOC),
  SLOW = list(events = BEGI_events[["DO_events"]][["SLOW_DO"]], service = service.SLOW),
  VDOW = list(events = BEGI_events[["DO_events"]][["VDOW_DO"]], service = service.VDOW),
  VDOS = list(events = BEGI_events[["DO_events"]][["VDOS_DO"]], service = service.VDOS)
)

max_n_rows <- max(sapply(wells, function(w) ceiling(length(w$events) / EVENTS_PER_ROW)))

for (site in names(wells)) {
  save_well_page(site, wells[[site]]$events, wells[[site]]$service, max_n_rows = max_n_rows)
}
