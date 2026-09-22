# Generates plots of year-to-year snow (SWE) and streamflow (Q) correlation
# years 1999-2025 because these are the years of data available
# streamflow data: Caine, T., J. Morse, and Niwot Ridge LTER. 2026. Streamflow data for Saddle stream, 1999 - ongoing. ver 10. Environmental Data Initiative. https://doi.org/10.6073/pasta/c699680482443efff3ad9f30c2c962a1 (Accessed 2026-08-31).
# snow data: https://wcc.sc.egov.usda.gov/nwcc/site?sitenum=663
# snotelr package info: Hufkens, K. (2022). snotelr: a toolbox to facilitate easy SNOTEL data exploration and downloads in R. Zenodo. https://doi.org/10.5281/zenodo.7012728.
# NEW EDITS: graph with water year on x axis instead of calendar year
# and graph SWE and Q vs time on the same graph to show lag

# load libraries
library(tidyverse)
library(snotelr)
library(EflowStats) #use this package to graph water year on x axis of


# load streamflow data from local csv
# since this is local the file path has to be changed if the data location is changed
niwotStreamflowData <- read_csv("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\year to year snow&streamflow correlation\\saddisch.nc.data.csv")

# download snotel data using snotelr package
niwotSnowData <- snotel_download(
  site_id = 663,
  internal = TRUE
)

# filter/mutate data for graphing
# TO DO: use pipe to filter and mutate snow data in one step so there's only one data frame instead of 2
niwotSnowData$date <- as.Date(niwotSnowData$date, format = "%Y-%m-%d") # convert from character to date format
filteredSnowData <- filter(niwotSnowData, 
                           between(date, as.Date('1999-04-29'), as.Date('2025-08-28'))) %>%# filter between the earliest and latest dates from the streamflow data so a correlation can be graphed
  mutate(snow_water_equivalent_meters = snow_water_equivalent * 0.001) %>%   # since Q units are m^3/day, change SWE units from mm to M
  mutate(waterYear = get_waterYear(date), 
         dayOfWaterYear = get_waterYearDay(date)) #  add a column with the year for facet_wrap while graphing and get the day of year for comparable x-axis on the plots

niwotStreamflowData$date <- as.Date(niwotStreamflowData$date, format = "%m/%d/%Y") # convert from charagter to date format
filteredStreamflowData <- niwotStreamflowData %>%
  filter(!is.na(date)) %>%
  filter(!is.na(discharge)) %>%
  arrange(date) %>% #sort chronologically to show wide change in streamflow
  mutate(waterYear = get_waterYear(date), #
         dayOfWaterYear = get_waterYearDay(date))   #add Year and day of Year columns for same reason as SWE dataset
#filter out invalid discharge data
#filter out invalid date data
# note: the streamflow discharge data is much smaller after filtering because there were many dates and discharge with NaN values

# do a full join to pair SWE and Q data from the same date
pairedData <- full_join(filteredSnowData, filteredStreamflowData, by = "date") %>%
  mutate(scaledDischarge = discharge/6000) #used to scale discharge for lag graphs

#here, this step is performed to be able to graph the entire range of SWE on the same plot as Q
#because Q has so much fewer values this way no data gets dropped
longData <- pairedData %>%
  select(date, waterYear.x, dayOfWaterYear.x, scaledDischarge, snow_water_equivalent_meters) %>% #select the variables I want
  pivot_longer(cols = c(scaledDischarge, snow_water_equivalent_meters),
               names_to = "variable",
               values_to = "value")

#find date of max Q and SWE for each year from 1999-2025 in order to calculate lage in days
dischargeYearlyMaxes <- pairedData %>%
  group_by(waterYear.x) %>%
  #use slice_max instead of summarise to get the date of max
  slice_max(discharge,n = 1, with_ties = FALSE)
SWEyearlyMaxes <- pairedData %>%
  group_by(waterYear.x) %>%
  slice_max(snow_water_equivalent_meters, n = 1, with_ties = FALSE)
yearlyLag <- data.frame(dischargeYearlyMaxes$waterYear.x)
yearlyLag$lag <- -SWEyearlyMaxes[, "dayOfWaterYear.x"] + dischargeYearlyMaxes[, "dayOfWaterYear.x"] #calculate the lag

#beginning of graphing section
#graph year to year SWE vs time
ggplot(filteredSnowData, aes(x = dayOfWaterYear, y = snow_water_equivalent_meters)) + 
  geom_line(color = "#8B3A62") + 
  facet_wrap( ~ waterYear, ncol = 4) + #
  labs(x = "Day of WY", y = "SWE (m)",
       title = "Daily SWE Values" ) + 
  theme_classic(base_size = 9)

#graph year to year discharge vs time
ggplot(filteredStreamflowData, aes(x = dayOfWaterYear, y = discharge)) + 
  geom_line(color = "#556B2F") + 
  facet_wrap( ~ waterYear, ncol = 4) + #
  labs(x = "Day of WY", y = "Q (m^3/Day)",
       title = "Daily Q Values") + 
  theme_classic(base_size = 9)

#graph yearly Q vs SWE graph, with points matched day by day

#this graph is not so useful rn, but leaving it in still
ggplot(pairedData, aes(x = snow_water_equivalent_meters, y = discharge)) + 
  geom_line(color = "#53868B") + 
  facet_wrap( ~ waterYear.y, ncol = 4, scales = "free") + #
  labs(x = "SWE (m)", y = "Q (m^3/day)",
       title = "Discharge vs Snow Water Equivalent (paired by date)")

# graph with double axis to show lag
#x axis is dayofWaterYear, left y axis is swe, right y axis is discharge(scaled)
# graph using data from longData
ggplot(longData, aes(x = dayOfWaterYear.x, y = value, color = variable))+
  geom_line(na.rm = TRUE) + #gets rid of warning message associated with NA values in discharge data
  scale_color_manual(
    values = c("scaledDischarge" = "lightskyblue", "snow_water_equivalent_meters" = "slategrey"),
    labels = c("scaledDischarge" = "Q", "snow_water_equivalent_meters" = "SWE"),
    name = " "
  ) + 
  facet_wrap( ~ waterYear.x, ncol = 4) +
  theme_classic(base_size = 9) + labs(x = "Day of WY", title = "Daily Q and SWE Values")+
  scale_y_continuous(name = "SWE (m)", sec.axis = sec_axis(~.*6000, name = "Discharge (m^3/day"))

#make a box plot of lag data, #days on y axis
ggplot(yearlyLag, aes(x=dischargeYearlyMaxes.waterYear.x, y=lag$dayOfWaterYear.x))+
  geom_col(color = 'forestgreen', fill = 'seagreen')+
  geom_text(aes(label = lag$dayOfWaterYear.x))+
  scale_x_continuous(breaks = unique(yearlyLag$dischargeYearlyMaxes.waterYear.x))+
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  labs(x = "Water Year", y = "Lag (days)",
       title = "Lag between Peak SWE and Q")

