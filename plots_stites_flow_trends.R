################################################################
# Plots SF Clearwater at Stites Stream Flow Trends
# Using EGRET
# Jason Williams
# last update: 10/13/2022
################################################################

library(EGRET)
library(dplyr)
library(ggplot2)
library(lubridate)



# gather Stites USGS gage daily average discharge data----------
siteID <-"13338500"
startDate <-"1964-10-01" #continuous record starts 1964
endDate <-"2022-12-31" 
daily <-readNWISDaily(siteID, "00060", startDate, endDate)
INFO <-readNWISInfo(siteID, "00060")

# EGRET functions & plots using all data----------------------------
# create eLIst required for EGRET functions
eList <-as.egret(INFO, daily, NA, NA) # all data

# plot daily average flow
plotQTimeDaily(eList, qUnit = 3)

# plot trends for min daily, mean daily, 7 day min, stdev of log(Q)
plotFour(eList, qUnit = 3)


# custom daily average flow plot------------------------------------
# using ggplot

# format data for plot
daily_formatted <-
  daily %>%
  mutate(calendar_year = year(Date)) %>%
  mutate(label = ifelse(calendar_year == "2021", "2021 study period", "other year")) %>%
  mutate(Q_cfs = Q * 35.3146667)

# plot
stites_daily_plot <-
daily_formatted %>%
  ggplot(aes(x = Date, y = (Q_cfs/10^3), color = label)) +
    geom_line() +
    theme_bw() +
    scale_color_manual(values = c("blue", "grey")) +
    labs(x = "calendar year", y = "Daily Mean Discharge (10^3 cfs)") +
    theme(legend.title = element_blank(),
          legend.position = "top")
stites_daily_plot

ggsave("./r_outputs/fig7_stites_daily_flow_plot.png", stites_daily_plot,
       width = 8.0, height = 4.0, units = "in", dpi = 600)

# EGRET functions & plots using spring  high flow period only-----------------
# define spring high flow as Apr, May, Jun

elist_spring <-as.egret(INFO, daily, NA, NA)
elist_spring <-setPA(elist_spring, paStart = 4, paLong = 3)

# plot yearly mean of spring daily mean discharge
plotFlowSingle(elist_spring, istat = 5)

# plot trends for min daily, mean daily, 7 day min, stdev of log(Q)
plotFour(elist_spring, qUnit = 3)
