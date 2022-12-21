###############################################################################
# Plots forest tribitary flow and SSC data
# Jason Williams, DEQ Lewiston
# last update; 12/20/2022
################################################################################

library(readxl)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(scales)
library(lubridate)


# read in data--------------------------------------------------------------------------

# tribs instananeous flow data
flow <-
  read.csv("./r_inputs/inst_flow.csv", header = TRUE) %>%
  mutate(datetime = as.POSIXct(date_formatted, format = "%Y-%m-%d %H:%M"))
str(flow)

# measured flow & stage
fs <-
  read.csv("./r_inputs/RAC_flow_stage.csv", header = TRUE) %>%
  mutate(datetime = as.POSIXct(paste(date, measured_flow_time), format = "%m/%d/%Y %H:%M",
                               tz = "US/Pacific")) 
str(fs)

# WQ data
wq <-
  read_excel("./r_inputs/Physical Chemical Template RAC project.xlsx", 
                sheet = "Results - config#7753", col_names = TRUE) %>%
  mutate(date = date(`Activity Start Date`), time = format(`Activity Start Time`, format = "%H:%M:%S")) %>%
  mutate(datetime = as.POSIXct(paste(date(`Activity Start Date`), time), format = "%Y-%m-%d %H:%M:%S",
                               tz = "US/Pacific")) 
str(wq)

# format data------------------------------------------------------------------------

# ssc data
ssc <-
  wq %>%
  filter(`Characteristic Name` == "Suspended Sediment Concentration (SSC)") %>%
  filter(`Activity Type` == "Sample-Routine") %>%
  mutate(result = ifelse(!is.na(`Result Detection Condition`), 
                         `Result Detection/Quantitation Limit Measure`, `Result Value`)) %>%
  mutate(date = date(`Activity Start Date`), time = format(`Activity Start Time`, format = "%H:%M:%S")) %>%
  mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S")) %>%
  select(`Monitoring Location ID`, datetime, result) %>%
  mutate(parameter = "SSC (mg/L)") %>%
  mutate(site = ifelse(`Monitoring Location ID` == "CC", "Cottonwood Cr",
                       ifelse(`Monitoring Location ID` == "TC", "Threemile Cr",
                              ifelse(`Monitoring Location ID` == "BC", "Butcher Cr",
                                     ifelse(`Monitoring Location ID` == "SFRR", "SF Red River",
                                            ifelse(`Monitoring Location ID` == "JC", "Johns Cr",
                                                   ifelse(`Monitoring Location ID` == "RR", "Red River",
                                     `Monitoring Location ID`))))))) %>%
  select(-`Monitoring Location ID`) %>%
  mutate(type = NA)
str(ssc)

# gage flow
flow_formatted <-
  flow %>%
  select(site, datetime, inst_flow_cfs) %>%
  rename(result = inst_flow_cfs) %>%
  mutate(parameter = "flow (cfs)", type = "rating")

str(flow_formatted)

# measured flow
measured_flow <-
  fs %>%
  filter(flow_cfs != 64.47) %>% # exclude rejected 5/25/2021 Red River flow 
  select(site_name, datetime, flow_cfs) %>%
  rename(site = site_name, result = flow_cfs) %>%
  mutate(parameter = "flow (cfs)", type = "measured")

str(measured_flow)

data_combined <-
  rbind(measured_flow, flow_formatted, ssc)

# plot lower tribs flow & ssc---------------------------------------------------

# data
lower_tribs <-
  data_combined %>%
  filter(site %in% c("Butcher Cr", "Cottonwood Cr", "Threemile Cr"))

# plot flow
plot_lower_tribs_flow <-
  ggplot(lower_tribs, aes(x = datetime, y = result)) +
    facet_wrap(~site, scales = "free_y") +
    geom_line(data = subset(lower_tribs, parameter == "flow (cfs)" & type == "rating"),
              color = "gray40") +
    geom_point(data = subset(lower_tribs, parameter == "flow (cfs)" & type == "measured"),
               color = "red", shape = 1) +
    #geom_point(data = subset(lower_tribs, parameter == "SSC (mg/L)"), color = "gray40") +
    theme_tufte() +
    theme(axis.title.x = element_blank(),
          panel.grid.major = element_line(color = "lightgrey")) +
    ylab("Discharge (cfs)")
plot_lower_tribs_flow

# plot ssc
plot_lower_tribs_ssc <-
  ggplot(lower_tribs, aes(x = datetime, y = result)) +
  facet_wrap(~site) +
  geom_point(data = subset(lower_tribs, parameter == "SSC (mg/L)"), color = "gray40") +
  theme_tufte() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_line(color = "lightgrey")) +
  ylab("SSC (mg/L)")
plot_lower_tribs_ssc


# plot forest tribs flow & ssc data---------------------------------------------

# data
forest_tribs <-
  data_combined %>%
  filter(site %in% c("Johns Cr", "SF Red River", "Red River"))

# plot flow
plot_forest_tribs_flow <-
  ggplot(forest_tribs, aes(x = datetime, y = result)) +
  facet_wrap(~site) +
  geom_line(data = subset(forest_tribs, parameter == "flow (cfs)" & type == "rating"),
            color = "gray40") +
  geom_point(data = subset(forest_tribs, parameter == "flow (cfs)" & type == "measured"),
             color = "red", shape = 1) +
  #geom_point(data = subset(forest_tribs, parameter == "SSC (mg/L)"), color = "gray40") +
  theme_tufte() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_line(color = "lightgrey")) +
  ylab("Discharge (cfs)")
plot_forest_tribs_flow

# plot ssc
plot_forest_tribs_ssc <-
  ggplot(forest_tribs, aes(x = datetime, y = result)) +
  facet_wrap(~site) +
  geom_point(data = subset(forest_tribs, parameter == "SSC (mg/L)"), color = "gray40") +
  theme_tufte() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_line(color = "lightgrey")) +
  ylab("SSC (mg/L)")
plot_forest_tribs_ssc

# plot tribs turbidity data-----------------------------------------------------

turb <-
  wq %>%
  filter(`Characteristic Name` == "Turbidity") %>%
  filter(`Activity Type` == "Sample-Routine") %>%
  mutate(parameter = paste(`Characteristic Name`, `Result Unit`)) %>%
  select(`Monitoring Location ID`, `Activity Start Date`, parameter, `Result Value`) %>%
  mutate(site = ifelse(`Monitoring Location ID` == "CC", "Cottonwood Cr",
                       ifelse(`Monitoring Location ID` == "TC", "Threemile Cr",
                              ifelse(`Monitoring Location ID` == "BC", "Butcher Cr",
                                     ifelse(`Monitoring Location ID` == "SFRR", "SF Red River",
                                            ifelse(`Monitoring Location ID` == "JC", "Johns Cr",
                                                   ifelse(`Monitoring Location ID` == "RR", "Red River",
                                                          `Monitoring Location ID`)))))))

# plot lower tribs turb
lower_tribs_turb_plot <-
  turb %>%
  filter(site %in% c("Butcher Cr", "Cottonwood Cr", "Threemile Cr")) %>%
  ggplot(aes(x = `Activity Start Date`, y = `Result Value`,
             color = parameter, shape = parameter)) +
  geom_point() +
  facet_wrap(~site, nrow = 1) +
  theme_tufte() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_line(color = "lightgrey")) +
  theme(legend.title = element_blank(), 
        legend.position = "bottom") +
  labs(y = "Turbidity", x = "Date")
lower_tribs_turb_plot

# plot forest tribs turb
forest_tribs_turb_plot <-
  turb %>%
  filter(site %in% c("Johns Cr", "Red River", "SF Red River")) %>%
  ggplot(aes(x = `Activity Start Date`, y = `Result Value`,
             color = parameter, shape = parameter)) +
  geom_point() +
  facet_wrap(~site, nrow = 1) +
  theme_tufte() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_line(color = "lightgrey")) +
  theme(legend.title = element_blank(), 
        legend.position = "bottom") +
  labs(y = "Turbidity", x = "Date")
forest_tribs_turb_plot

# combine  flow, ssc, turbidity into one figure---------------------------------

# combine into one figure
lower_tribs_fig <-
  ggarrange(plot_lower_tribs_flow, plot_lower_tribs_ssc, lower_tribs_turb_plot,
            nrow = 3, ncol =1, 
            labels = c("A", "B", "C"))
lower_tribs_fig

ggsave("./r_outputs/fig9_lower_tribs_flow_ssc.png", lower_tribs_fig, width = 8, 
       height = 6, units = "in", dpi = 700)


forest_tribs_fig <-
  ggarrange(plot_forest_tribs_flow, plot_forest_tribs_ssc, forest_tribs_turb_plot,
            nrow = 3, ncol =1, 
            labels = c("A", "B", "C"))
forest_tribs_fig

ggsave("./r_outputs/fig10_forest_tribs_flow_ssc.png", forest_tribs_fig, width = 8, 
       height = 6, units = "in", dpi = 700)


