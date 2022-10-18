###############################################################################
# gets & formats USGS main stem data using dataRetrieval package
# Jason Williams
# last update: 9/15/2022
###############################################################################

library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(reshape2)

# discrete flow and water quality from USGS gage sites--------------------------
# use dataRetrieval
# flow, turbidity, ssc (mg/L), SSC load (tons/day)

usgs_wq <-readWQPqw(siteNumbers = c("USGS-13337500", "USGS-13338500", "USGS-13338100"),
                    parameterCd = c("00061", "80154", "80155", "63680"),
                    startDate = "2020-11-01", endDate = "2021-12-31")

write.csv(usgs_wq, "./r_inputs/usgs_wq.csv")


# instantaneous flow from usgs gages--------------------------------------------
# note no inst flow at Harpster (1338100)
# will use Stites-Harpster measured flow regression to predict Harpster inst flow

siteNo <-c("13337500", "13338500", "13338100") 

pCode <-"00060"
start.date <-"2020-10-01"
end.date <-"2021-12-31"

gage_inst_flow <-readNWISuv(siteNumbers = siteNo, 
                         parameterCd = pCode,
                         startDate = start.date,
                         endDate = end.date)
str(gage_inst_flow)

# rename columns
gage_inst_flow2 <-renameNWISColumns(gage_inst_flow)  
str(gage_inst_flow2)

# convert times to pacific
gage_inst_flow3 <-
  gage_inst_flow2 %>%
  mutate(dateTime_formatted = as.POSIXct(format(dateTime, tz = "America/Los_Angeles")))
str(gage_inst_flow3)

write.csv(gage_inst_flow3, "./r_inputs/usgs_inst_flow.csv")

# daily average flow from USGS gages-------------------------------------------

gage_daf <-readNWISdata(sites = siteNo,
                        parameterCd = pCode, 
                        service = "dv",
                        startDate = start.date,
                        endDate = end.date)

# rename columns
gage_daf2 <-renameNWISColumns(gage_daf)
