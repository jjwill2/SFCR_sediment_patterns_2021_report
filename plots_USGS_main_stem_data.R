################################################################################
# Plots main stem SFCW River data collected by USGS
# Jason Williams, DEQ Lewiston
# last update: 10/11/2022
################################################################################

library(dplyr)
library(ggplot2)
library(scales)
library(ggpubr)

# read data----------------------------------------------------------------------

# usgs discrete wq data
usgs_wq <-read.csv("./r_inputs/usgs_wq.csv", header = TRUE)

# usgs gage instantaneous flow
usgs_inst_flow <-read.csv("./r_inputs/usgs_inst_flow.csv", header = TRUE)

# LOADEST daily loads
elkcity_loadest <-
  read.csv("./r_inputs/SuspendedSedimentConcentration.ind_formatted_Elkcity.csv",
                           header = TRUE) %>%
  mutate(site = "Elk City (13337500)")

wimer_loadest <-
  read.csv("./r_inputs/SuspendedSedimentConcentration.ind_formatted_Wimer.csv",
                         header = TRUE) %>%
  mutate(site = "Harpster (13338100)")

stites_loadest <-
  read.csv("./r_inputs/SuspendedSedimentConcentration.ind_formatted_Stites.csv",
                          header = TRUE) %>%
  mutate(site = "Stites (13338500)")

# format discrete data----------------------------------------------------------

usgs_wq_formatted <-
  usgs_wq %>%
  select(MonitoringLocationIdentifier, ActivityStartDate, 
         ActivityStartTime.Time, CharacteristicName, ResultMeasureValue,
         ResultMeasure.MeasureUnitCode, ResultDetectionConditionText) %>%
  mutate(datetime = as.POSIXct(paste(ActivityStartDate, ActivityStartTime.Time))) %>%
  mutate(sitelabel = ifelse(MonitoringLocationIdentifier == "USGS-13337500", "Elk City (13337500)",
                            ifelse(MonitoringLocationIdentifier == "USGS-13338100", "Harpster (13338100)",
                                   ifelse(MonitoringLocationIdentifier == "USGS-13338500", "Stites (13338500)", NA))))
  
str(usgs_wq_formatted)

# calculate harpster inst flow--------------------------------------------------
# harpster = 0.9962*stites - 31.947, r2 = 0.99,
# based on Clark et al. 2013 and 2006-2010 paired data

harpster_flow <-
  usgs_inst_flow %>%
  filter(site_no == "13338500") %>%
  mutate(calcflow = 0.9962*Flow_Inst-31.947) %>%
  select(-Flow_Inst) %>%
  rename(Flow_Inst = calcflow)

harpster_flow$site_no <-"13338100"


# format inst flow data---------------------------------------------------------

usgs_inst_flow_formatted <-
  usgs_inst_flow %>%
  rbind(harpster_flow) %>%
  mutate(sitelabel = ifelse(site_no == "13337500", "Elk City (13337500)",
                            ifelse(site_no == "13338100", "Harpster (13338100)",
                                   ifelse(site_no == "13338500", "Stites (13338500)", NA)))) %>%
  mutate(date = as.Date(dateTime_formatted, format = "%Y-%m-%d %H:%M:%S"))


# plot gage inst & discrete flow------------------------------------------------

# format
disc_flow <-
  usgs_wq_formatted %>% 
  filter(CharacteristicName == "Stream flow, instantaneous") %>%
  mutate(date = as.Date(datetime, format = "%Y-%m-%d %H:%M:%S"))
str(disc_flow)

# plot
cbPalette <-c("#999999", "#E69F00", "#56B4E9")

usgs_flowplot <-
  ggplot() +
    geom_line(data = usgs_inst_flow_formatted, 
              aes(x = date, y = Flow_Inst, color = sitelabel) ) +
    geom_point(data = disc_flow,
                 aes(x = date, y = ResultMeasureValue, 
                     #color = sitelabel, 
                     shape = sitelabel)) +
    theme_bw() +
    labs(y = "Discharge (ft3/s)") +
    scale_shape_manual(values = c(1, 2, 0)) +
    theme(legend.title = element_blank(), legend.position = "top",
          axis.title.x = element_blank(),
          legend.text = element_text(size = 6)) +
    scale_x_date(date_labels = "%b-%Y", date_breaks = "3 months") +
    scale_color_manual(values = cbPalette)
usgs_flowplot


# plot gage SSC concentration---------------------------------------------------

str(usgs_wq_formatted)

usgs_ssc_plot <-
  usgs_wq_formatted %>%
  mutate(date = as.Date(ActivityStartDate, format = "%Y-%m-%d")) %>%
  filter(CharacteristicName == "Suspended Sediment Concentration (SSC)") %>%
  ggplot(aes(x = date, y = ResultMeasureValue, color = sitelabel,
             shape = sitelabel)) +
    geom_point() +
    #geom_line() +
    labs(y = "Suspended Sediment Concentration (mg/L)") +
    theme_bw() +
    theme(axis.title.x = element_blank(), legend.position = "top",
          legend.title = element_blank(), 
          legend.text = element_text(size = 6)) +
    guides(color = guide_legend(n_row=2), shape = guide_legend(nrow=2)) +
  scale_color_manual(values = cbPalette)
usgs_ssc_plot

# plot gage turbidity-----------------------------------------------------------

usgs_turb_plot <-
  usgs_wq_formatted %>%
  mutate(date = as.Date(ActivityStartDate, format = "%Y-%m-%d")) %>%
  filter(CharacteristicName == "Turbidity") %>%
  ggplot(aes(x = date, y = ResultMeasureValue, color = sitelabel,
             shape = sitelabel)) +
  geom_point() +
  #geom_line() +
  labs(y = "Turbidity (FNU)") +
  theme_bw() +
  theme(axis.title.x = element_blank(), legend.position = "top",
        legend.title = element_blank(), 
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(n_row=2), shape = guide_legend(nrow=2)) +
  scale_color_manual(values = cbPalette)
usgs_turb_plot


# combine flow & ssc plots into single figure-----------------------------------

usgs_fig <-ggarrange(usgs_flowplot, 
                     ggarrange(usgs_ssc_plot, usgs_turb_plot, ncol = 2,
                     labels = c("B", "C")),
                     nrow = 2, labels = "A")
usgs_fig

ggsave("./r_outputs/fig6_usgs_flow_ssc.png", usgs_fig, width = 8, height = 8, units = "in", dpi = 700)

# plot loadest daily loads------------------------------------------------------


loadest_daily <-
  rbind(elkcity_loadest, wimer_loadest, stites_loadest) %>%
  mutate(date_formatted = as.Date(as.character(Date), format= "%Y%m%d", ))
str(loadest_daily)


loadest_daily %>%
  ggplot(aes(x = date_formatted, y = (AMLE*0.005), color = site)) +
    geom_line() +
    theme_bw() +
    labs(y = "SSC tons/day") +
    theme(axis.title.x = element_blank())
