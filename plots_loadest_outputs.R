################################################################################
# Plots daily loads based on LOADEST output for tributaries & main stem
# Jason Williams
# last update: 10/18/2022
################################################################################

library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(readxl)

# read in data-----------------------------------------------------------------
# loads units in csv files are lbs/day

# cc loadest predicted load
cc_load <-
  read.csv("./r_inputs/SuspendedSedimentConcentration.ind_formatted_CC.csv",
                   header = TRUE) %>%
  mutate(site = "Cottonwood Cr", area = "tributaries")
str(cc_load)

# tc loadest predicted load
tc_load <-
  read.csv("./r_inputs/SuspendedSedimentConcentration.ind_formatted_TC.csv",
           header = TRUE) %>%
  mutate(site = "Threemile Cr", area = "tributaries")
str(tc_load)

# bc loadest predicted load
bc_load <-
  read.csv("./r_inputs/SuspendedSedimentConcentration.ind_formatted_BC.csv",
           header = TRUE) %>%
  mutate(site = "Butcher Cr", area = "tributaries")
str(bc_load)

# red river
rr_load <-
  read.csv("./r_inputs/SuspendedSedimentConcentration.ind_formatted_RR.csv",
           header = TRUE) %>%
  mutate(site = "Red River", area = "tributaries")
str(rr_load)

# SF red river
sfrr_load <-
  read.csv("./r_inputs/SuspendedSedimentConcentration.ind_formatted_SFRR.csv",
           header = TRUE) %>%
  mutate(site = "SF Red River", area = "tributaries")
str(sfrr_load)

# main stem at stites
stites_load <-
  read.csv("./r_inputs/SuspendedSedimentConcentration.ind_formatted_Stites.csv",
           header = TRUE) %>%
  mutate(site = "Stites (13338500)", area = "mainstem")

# main stem at Wimer Bridge
wimer_load <-
  read.csv("./r_inputs/SuspendedSedimentConcentration.ind_formatted_Wimer.csv",
           header = TRUE) %>%
  mutate(site = "Harpster (13338100)", area = "mainstem")

# main stem near Elk City
elkcity_load <-
  read.csv("./r_inputs/SuspendedSedimentConcentration.ind_formatted_Elkcity.csv",
           header = TRUE) %>%
  mutate(site = "Elk City (13337500)", area = "mainstem")


# loadest site study period totals, upper & lower bounds
loadest_totals <-read.csv("./r_inputs/loadest_totals.csv", header = TRUE)
str(loadest_totals)

# usgs measured suspended sediment discharge (tons/day)
usgs_load <-read.csv("./r_inputs/usgs_wq.csv", header = TRUE) 
str(usgs_load)

# tributaries wq data
wq <-read_excel("./r_inputs/Physical Chemical Template RAC project.xlsx", 
             sheet = "Results - config#7753", col_names = TRUE)
str(wq)

# format LOADEST output--------------------------------------------------------

daily_loads <-
  rbind(cc_load, tc_load, bc_load, rr_load, sfrr_load,
        stites_load, wimer_load, elkcity_load) %>%
  mutate(date_formatted = as.Date(as.character(Date), format = "%Y%m%d"))
str(daily_loads)

# format measured loads---------------------------------------------------------

# main stem
usgs_load_formatted <-
  usgs_load %>%
  filter(CharacteristicName == "Suspended Sediment Discharge") %>%
  mutate(site = ifelse(MonitoringLocationIdentifier == "USGS-13338100", 
                       "Harpster (13338100)",
                       ifelse(MonitoringLocationIdentifier == "USGS-13337500",
                              "Elk City (13337500)",
                              ifelse(MonitoringLocationIdentifier == "USGS-13338500",
                                     "Stites (13338500)", NA)))) %>%
  select(site, ActivityStartDate, CharacteristicName, ResultMeasureValue) %>%
  mutate(date_formatted = as.Date(ActivityStartDate, format = "%Y-%m-%d"))

# tribs
wq_loads <-
  wq %>%
  filter(`Result Status ID` != "Rejected") %>%
  filter(`Characteristic Name`%in% c("Flow", "Suspended Sediment Concentration (SSC)")) %>%
  filter(`Activity Type` %in% c("Field Msr/Obs", "Sample-Routine")) %>%
  mutate(result = ifelse(is.na(`Result Detection Condition`), `Result Value`,
                         `Result Detection/Quantitation Limit Measure`)) %>%
  select(`Monitoring Location ID`, `Activity Start Date`, `Characteristic Name`,
          result) %>%
  mutate(site = ifelse(`Monitoring Location ID` == "BC", "Butcher Cr",
                       ifelse(`Monitoring Location ID` == "CC", "Cottonwood Cr",
                              ifelse(`Monitoring Location ID` == "RR", "Red River",
                                     ifelse(`Monitoring Location ID` == "JC", "Johns Cr",
                                     ifelse(`Monitoring Location ID` == "SFRR", "SF Red River",
                                            ifelse(`Monitoring Location ID` == "TC", "Threemile Cr", NA))))))) %>%
  dcast(`Monitoring Location ID` + site + `Activity Start Date` ~ `Characteristic Name`,
        value.var = "result") %>%
  mutate(ssc_load_lbs = Flow * `Suspended Sediment Concentration (SSC)` * 5.39) %>%
  mutate(ssc_load_metric_tons = ssc_load_lbs * 0.000453592) %>%
  mutate(date_formatted = as.Date(`Activity Start Date`, format = "%Y-%m-%d")) %>%
  filter(date_formatted < "2022-01-01") # exclude 2022 data points from Red/SF Red; no corresponding loadest
str(wq_loads)

# plot LOADEST output-----------------------------------------------------------

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# all sites on one plot
daily_loads_plot <-
  daily_loads %>%
  mutate(load_tons = AMLE * 0.000453592) %>%
  ggplot() +
  geom_line(aes(x = date_formatted, y = load_tons, color = site), size = 1) +
  labs(x = "date", y = "SSC load (tons/day)") +
  theme_bw() +
  theme(legend.title = element_blank(), axis.title.x = element_blank(),
        legend.position = c(0.73, 0.7),
        legend.background = element_blank())  +
  scale_color_manual(values = cbPalette)
daily_loads_plot

# mainstem only

mainstem_pallette <-c("#999999", "#E69F00", "#56B4E9")

for_mainstem_loads_plot <-
  daily_loads %>%
  filter(site %in% c("Stites (13338500)",
                     "Harpster (13338100)",
                     "Elk City (13337500)")) %>%
  mutate(load_tons = AMLE * 0.000453592)
str(for_mainstem_loads_plot)

mainstem_loads_plot <-
  ggplot() +
  geom_line(data = for_mainstem_loads_plot, 
            aes(x = date_formatted, y = load_tons, color = site), size = 1) +
  geom_point(data = usgs_load_formatted, 
             aes(x = date_formatted, y = ResultMeasureValue,
                                   shape = site)) +
  labs(x = "date", y = "SSC load (metric tons/day)") +
  theme_bw() +
  theme(legend.title = element_blank(), axis.title.x = element_blank(),
        legend.position = c(0.73, 0.7),
        legend.background = element_blank(),
        legend.text = element_text(size = 6))  +
  scale_color_manual(values = mainstem_pallette) +
  scale_shape_manual(values = c(0,1,2))
mainstem_loads_plot

# tribs only
tribs_pallette <-c("#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

for_tribs_loads_plot <-
  daily_loads %>%
  filter(!site %in% c("Stites (13338500)",
                     "Harpster (13338100)",
                     "Elk City (13337500)")) %>%
  mutate(load_tons = AMLE * 0.000453592)

unique(for_tribs_loads_plot$site)

tribs_loads_plot <-
  ggplot() +
  geom_line(data = for_tribs_loads_plot, 
            aes(x = date_formatted, y = load_tons, color = site), size = 1) +
  geom_point(data = subset(wq_loads, site != "Johns Cr"), 
             aes(x = date_formatted, y = ssc_load_metric_tons,
                                  shape = site)) +
  labs(x = "date", y = "SSC load (metric tons/day)") +
  theme_bw() +
  theme(legend.title = element_blank(), axis.title.x = element_blank(),
        legend.position = c(0.73, 0.7),
        legend.background = element_blank(),
        legend.text = element_text(size = 6))  +
  scale_color_manual(values = tribs_pallette) +
  scale_shape_manual(values = c(0, 1, 2, 3, 4)) +
  scale_x_date(date_labels = "%b %Y")
tribs_loads_plot

# faceted by area
daily_loads_plot2 <-
  daily_loads %>%
  mutate(load_tons = AMLE * 0.000453592) %>%
  ggplot() +
    geom_line(aes(x = date_formatted, y = load_tons, color = site), size = 1) +
    labs(x = "date", y = "SSC load (tons/day)") +
    facet_wrap(~area, scales = "free_y") +
    theme_bw() +
    theme(legend.title = element_blank(), axis.title.x = element_blank(),
          legend.position = "top", 
          legend.background = element_blank(),
          legend.text = element_text(size = 6)) +
    scale_color_manual(values = cbPalette)
daily_loads_plot2


# total study period load by site----------------------------------------------

loadest_totals_plot <-
  loadest_totals %>%
  mutate(labels = factor(site, 
                         levels = c("Cottonwood Creek", "Threemile Creek",
                                    "Butcher Creek", "Red River", "SF Red River",
                                    "Stites (13338500)", "Harpster (13338100)",
                                    "Elk City (13337500)"))) %>%
  ggplot(aes(x = labels, y = load)) +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    geom_point() +
    theme_bw() +
    labs(x = "site", y = "total study period load (metric tons)") +
    coord_flip() 
loadest_totals_plot

# load per km2
area_load_plot <-
  loadest_totals %>%
  mutate(labels = factor(site, 
                         levels = c("Cottonwood Creek", "Threemile Creek",
                                    "Butcher Creek", "Red River", "SF Red River",
                                    "Stites (13338500)", "Harpster (13338100)",
                                    "Elk City (13337500)"))) %>%
  ggplot(aes(x = labels, y = area_load)) +
  geom_errorbar(aes(ymin = area_load_lower, ymax = area_load_upper)) +
  geom_point() +
  theme_bw() +
  labs(x = "site", y = "area load (metric tons/km2)") +
  coord_flip() 
area_load_plot

# combine into one figure

loads_plot <-ggarrange(mainstem_loads_plot, tribs_loads_plot,
                       loadest_totals_plot, area_load_plot,
                       labels = c("a)", "b)", "c)", "d)"), 
                       ncol = 2, nrow = 2)
loads_plot


ggsave("./r_outputs/fig11_loads_plot.png", loads_plot,
       width = 8.5, height = 6.0, units = "in", dpi = 600)
