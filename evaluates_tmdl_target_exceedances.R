###############################################################################
# Tests for TMDL target exceedances
# Jason Williams, DEQ Lewiston
# last update; 10/18/2022
################################################################################

library(readxl)
library(tidyverse)

# read in data-----------------------------------------------------------------

# usgs main stemdiscrete wq data
usgs_wq <-read.csv("./r_inputs/usgs_wq.csv", header = TRUE)
str(usgs_wq)

# tribs data
tribs <-read_excel("./formatted data/Physical Chemical Template RAC project.xlsx", 
                     sheet = "Results - config#7753", col_names = TRUE)
str(tribs)


# target info
target_ssc <-read.csv("./r_inputs/target_ssc.csv", header = TRUE, 
                      na.strings = "")
str(target_ssc)

# format------------------------------------------------------------------------

tribs_formatted <-
  tribs %>%
  mutate(date = as.Date(`Activity Start Date`, format = "%Y-%m-%d")) %>%
  filter(`Characteristic Name` == "Suspended Sediment Concentration (SSC)") %>%
  filter(`Activity Type` == "Sample-Routine") %>%
  mutate(result = ifelse(!is.na(`Result Detection Condition`), 
                         `Result Detection/Quantitation Limit Measure`, `Result Value`)) %>%
  select(`Monitoring Location ID`, date, result) %>%
  rename(site = `Monitoring Location ID`, 
         ssc_mgl = result)

str(tribs_formatted)

usgs_wq_formatted <-
  usgs_wq %>%
  mutate(date = as.Date(ActivityStartDate, format = "%Y-%m-%d")) %>%
  filter(CharacteristicName == "Suspended Sediment Concentration (SSC)") %>%
  filter(ActivityTypeCode == "Sample-Routine") %>%
  mutate(result = ResultMeasureValue) %>%
  select(MonitoringLocationIdentifier, date, result) %>%
  rename(ssc_mgl = result,
         site = MonitoringLocationIdentifier)

str(usgs_wq_formatted)

# combine main stem & tribs data
ssc_formatted <-
  rbind(tribs_formatted, usgs_wq_formatted) %>%
  merge(target_ssc, by = "site", all.x = TRUE) %>%
  mutate(target_value = ifelse(site %in% c("BC", "TC", "USGS-13338100", "USGS-13338500"),
                               (ssc_25ntu + (ssc_mgl * RBG)),
                               ifelse(site == "CC", 50, NA))) %>%
  mutate(target_exceeded = ifelse(ssc_mgl > target_value, "Y", NA))
         
        
str(ssc_formatted)

# plot ssc vs target------------------------------------------------------------

target_plot <-
  ssc_formatted %>%
  filter(!is.na(target_value)) %>%
  select(site, site_label, date, ssc_mgl, target_value) %>%
  rename(`SSC (mg/L)` = ssc_mgl, `target SSC (mg/L)` = target_value) %>%
  pivot_longer(cols = -c(site:date), names_to = "parameter", values_to = "value") %>%
  mutate(facet_order = factor(site_label, 
                              levels = c("Butcher Cr", "Cottonwood Cr", 
                                         "Threemile Cr", "Stites (13338500)",
                                         "Harpster (13338100)"))) %>%
  ggplot(aes(x = date, y = value, color = parameter, shape = parameter)) +
    geom_point() +
    facet_wrap(~facet_order) +
    theme_bw() +
    theme(legend.title = element_blank(), axis.title.x = element_blank(),
          legend.position = "top") +
    ylab("SSC (mg/L)")
target_plot

ggsave("./r_outputs/fig12_ssc_target_plot.png", target_plot, width = 8, height = 4, units = "in", dpi = 700)
