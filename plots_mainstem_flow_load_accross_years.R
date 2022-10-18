################################################################################
# plots flow and load patterns accross years
# Jason Williams
# last update: 10/18/2022
################################################################################

library(EGRET)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)


# ssc loads data----------------------------------------------------------------

ssc_loads <-read.csv("./r_inputs/ssc_loads_2006_through_2021.csv", header = TRUE)
str(ssc_loads)

# gather Stites USGS gage daily average discharge data----------
siteID <-"13338500"
startDate <-"1964-10-01" #continuous record starts 1964
endDate <-"2022-12-31" 
daily <-readNWISDaily(siteID, "00060", startDate, endDate)
INFO <-readNWISInfo(siteID, "00060")

# format data-------------------------------------------------

# daily average flow data

formatted_for_plot <-
  daily %>%
  mutate(calendar_year = year(Date)) %>%
  mutate(label = ifelse(calendar_year == "2021", "2021 study period", "other year")) %>%
  mutate(Q_cfs = Q * 35.3146667) %>%
  mutate(label2 = ifelse(waterYear == 2006, "WY 2006",
                         ifelse(waterYear == 2007, "WY 2007", 
                                ifelse(waterYear == 2008, "WY 2008",
                                       ifelse(waterYear == 2009, "WY 2009",
                                              ifelse(waterYear == 2010, "WY 2010",
                                                     ifelse(waterYear == 2011, "WY 2011",
                                                            ifelse(Date >= "2020-12-10" & Date <= "2021-11-30", 
                                                                   "2021 study period", NA)))))))) %>%
  mutate(dummy_date = as.Date(paste0(month(Date), "-", day(Date), "-", "1901"),
                              format = "%m-%d-%Y")) %>%
  filter(!is.na(label2)) %>%
  mutate(test_month = month(Date))

# plot daily average flow data--------------------------------------------------
flowplot <-
  formatted_for_plot %>%
  ggplot(aes(x = dummy_date, y = (Q_cfs/10^3), color = label)) +
  geom_line() +
  theme_bw() + 
  facet_wrap(~label2, nrow = 1, scales = "free_x") +
  scale_color_manual(values = c("blue", "grey")) +
  labs(y = "Stites Daily Mean Q (10^3 cfs)") +
  theme(legend.title = element_blank(),
        legend.position = "top", axis.title.x = element_blank(),
        axis.title=element_text(size = 7)) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b")
flowplot

# plot loads-------------------------------------------------------------------

sfcr_loads <-
  ssc_loads %>%
  filter(Data.Source != "TMDL sediment budget") %>%
  ggplot(aes(x = Site, y = Load/1000, color = measuring.agency,
             shape = measuring.agency)) +
    geom_point() +
    geom_errorbar(aes(ymin = Lower/1000, ymax = Upper/1000)) +
    facet_wrap(~label, nrow = 1) +
    theme_bw() +
    theme(axis.title.x = element_blank(), legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
          axis.title=element_text(size=7)) +
    labs(y = "SSC load (10^3 metric tons)") +
    scale_color_manual(values = c("#000000", "#009E73"))
sfcr_loads


# combine into one figure

mainstem_Q_loads <-ggarrange(flowplot, sfcr_loads,
                       labels = c("a)", "b)"), 
                       ncol = 1, nrow = 2)
mainstem_Q_loads


ggsave("./r_outputs/fig13_SFCR_patterns_plot.png", mainstem_Q_loads,
       width = 8.5, height = 6.0, units = "in", dpi = 600)

# loads per unit upstream area plot--------------------------------------------

mainstem_area_loads <-
ssc_loads %>%
  filter(Data.Source != "TMDL sediment budget") %>%
  mutate(area_load = Load/upstream.area.km2) %>%
  ggplot(aes(x = Site, y = area_load, color = measuring.agency)) +
    geom_point() +
    facet_wrap(~label, nrow = 1) +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
    labs(y = "metric tons/km2")
mainstem_area_loads

ggsave("./r_outputs/fig14_sfcr_area_loads.png", mainstem_area_loads,
       width = 8.5, height = 3.0, units = "in", dpi = 600)  
          