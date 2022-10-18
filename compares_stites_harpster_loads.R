#############################################################################
# Compares USGS-measured loads at Stites and Harpster
# cases where measured loads reported by USGS on same data
# Jason Williams
# last update: 10/18/22
#############################################################################

library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(reshape2)

loads <-readWQPqw(siteNumbers = c("USGS-13338500", "USGS-13338100"),
                    parameterCd = c("80155"))
str(loads)


loads_formatted <-
  loads %>%
  select(MonitoringLocationIdentifier, ActivityStartDate, ResultMeasureValue) %>%
  dcast(ActivityStartDate ~ MonitoringLocationIdentifier, 
        value.var = "ResultMeasureValue") %>%
  filter(!is.na(`USGS-13338100`)) %>%
  filter(!is.na(`USGS-13338500`))
str(loads_formatted)

loads_comparison_plot <-
  loads_formatted %>%
  ggplot(aes(x = `USGS-13338100`/1000, y = `USGS-13338500`/1000)) +
    geom_point(shape = 1) +
    geom_abline(slope = 1, linetype = "dashed") +
    labs(x = "Harpster SSC load (10^3 tons/day)", 
         y = "Stites SSC load (10^3 tons/day)") +
    theme_bw() +
    xlim(0, 14) +
    ylim(0, 14)
loads_comparison_plot

ggsave("./r_outputs/Fig15_load_comparison.png", loads_comparison_plot,
       width = 6, height = 3, units = "in", dpi = 600)

# zoom in, exclude 2 outliers
loads_comparison_plot2 <-
  loads_formatted %>%
  ggplot(aes(x = `USGS-13338100`, y = `USGS-13338500`)) +
  geom_point(shape = 1) +
  geom_abline(slope = 1, linetype = "dashed") +
  labs(x = "Harpster SSC load (tons/day)", 
       y = "Stites SSC load (tons/day)") +
  theme_bw() +
  xlim(0, 4000) +
  ylim(0, 4000)
loads_comparison_plot2
