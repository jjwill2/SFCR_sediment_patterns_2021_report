
library(reshape2)

source("plots_USGS_main_stem_data.R")

# data for 'est' file-----------------------------------------------------------

# calculate harpster daily average flow

str(harpster_flow)

harpster_daf <-
  harpster_flow %>%
  mutate(date = as.Date(dateTime_formatted, format = "%Y%m%d")) %>%
  group_by(date) %>%
  summarise(Flow = mean(Flow_Inst)) %>%
  mutate(site_no = "13338100")

str(harpster_daf)

str(gage_daf2)

# formate data from Stites, Elk City
gage_daf3 <-
  gage_daf2 %>%
  mutate(date = as.Date(dateTime, format = "%Y%m%d")) %>%
  select(site_no, date, Flow)

# combine  
main_stem_for_est <-
  rbind(gage_daf3, harpster_daf) %>%
  mutate(EDATE = format(date, format = "%Y%m%d"), ETIME = "1200") %>%
  rename(EFLOW = Flow) %>%
  select(site_no, EDATE, ETIME, EFLOW)

write.csv(main_stem_for_est, "./LOADEST/main_stem_for_est.csv")

# data for 'calib' file---------------------------------------------------------

str(usgs_wq)

main_stem_for_calib <-
  usgs_wq %>%
  filter(USGSPCode %in% c("00061", "80154")) %>%
  select(MonitoringLocationIdentifier, ActivityStartDate, 
         CharacteristicName, ResultMeasureValue) %>%
  distinct(.keep_all = TRUE) %>% # duplicate SSC record 5-6-2021
  dcast(MonitoringLocationIdentifier + ActivityStartDate ~ CharacteristicName, 
        value.var = "ResultMeasureValue") %>%
  mutate(CDATE = format(ActivityStartDate, format = "%Y%m%d"), CTIME = "1200") %>%
  rename(CFLOW = `Stream flow, instantaneous`, CCONC = `Suspended Sediment Concentration (SSC)`) %>%
  select(MonitoringLocationIdentifier, CDATE, CTIME, CFLOW, CCONC)

str(main_stem_for_calib)

write.csv(main_stem_for_calib, "./LOADEST/main_stem_for_calib.csv")

