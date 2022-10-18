library(dplyr)
library(readxl)
library(lubridate)
library(reshape2)

# read in data------------------------------------------------------------------

# instantaneous flow
inst_flow <-read.csv("./rating curves/inst_flow.csv", header = TRUE)
str(inst_flow)

# WQ data
wq <-
  read_excel("./formatted data/Physical Chemical Template RAC project.xlsx", 
             sheet = "Results - config#7753", col_names = TRUE) %>%
  mutate(date = as.Date(`Activity Start Date`, format = "%Y-%m-%d"), 
         time = format(`Activity Start Time`, format = "%H:%M:%S")) %>%
  mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S",
                               tz = "US/Pacific")) 
str(wq)


# calculates daily average flow-------------------------------------------------
# will use for 'est' file input

for_est <-
  inst_flow %>%
  mutate(date = as.Date(date_formatted, format = "%Y-%m-%d %H:%M:%S")) %>%
  group_by(site, date) %>%
  summarise(EFLOW = mean(inst_flow_cfs, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(EDATE = format(date, format = "%Y%m%d"), ETIME = "1200") %>%
  select(site, EDATE, ETIME, EFLOW)


str(for_est)

write.csv(for_est, "./LOADEST/for_est.csv")

# formats calib file data-------------------------------------------------------

for_calib <-
  wq %>%
  filter(`Characteristic Name` %in% c("Flow", "Suspended Sediment Concentration (SSC)")) %>%
  filter(`Activity Type` %in% c("Sample-Routine", "Field Msr/Obs")) %>%
  mutate(Result = ifelse(!is.na(`Result Detection Condition`),
                         `Result Detection/Quantitation Limit Measure`, `Result Value`)) %>%
  select(`Monitoring Location ID`, `Activity Start Date`, `Characteristic Name`, 
         Result) %>%
  dcast(`Monitoring Location ID` + `Activity Start Date` ~ `Characteristic Name`,
        value.var = "Result") %>%
  mutate(date = as.Date(`Activity Start Date`, format = "%Y-%m-%d")) %>%
  mutate(CDATE = format(date, format = "%Y%m%d"), CTIME = "1200") %>%
  rename(CFLOW = Flow, CCONC = `Suspended Sediment Concentration (SSC)`,
         site = `Monitoring Location ID`) %>%
  select(site, CDATE, CTIME, CFLOW, CCONC)


str(for_calib)

write.csv(for_calib, "./LOADEST/for_calib.csv")
