#plotting snow depth for each cluster
#data from 4 sensors in each cluster on one graph
#facet by water year


#load libraries
library(tidyverse)
library(dplyr)
library(EflowStats)
library(lubridate)

#load data (aggregated hourly data, with NaN and negative and non "n" flagged values filtered out)
setwd("C:\\Users\\iameb\\Github\\Niwot_Snow\\aggregated hourly data")

site1 <- read.csv("sn1_hourly.csv")
site6 <- read.csv("sn6_hourly.csv")
site7 <- read.csv("sn7_hourly.csv")
site8 <- read.csv("sn8_hourly.csv")
site9 <- read.csv("sn9_hourly.csv")
site10 <- read.csv("sn10_hourly.csv")
site11 <- read.csv("sn11_hourly.csv")
site12 <- read.csv("sn12_hourly.csv")
site13 <- read.csv("sn13_hourly.csv")
site14 <- read.csv("sn14_hourly.csv")
site15 <- read.csv("sn15_hourly.csv")
site16 <- read.csv("sn16_hourly.csv")
site17 <- read.csv("sn17_hourly.csv")
site19 <- read.csv("sn19_hourly.csv")
site20 <- read.csv("sn20_hourly.csv")
site21 <- read.csv("sn21_hourly.csv")

#make a function that takes in the site df and changes it as desired once i get all the changes right

mutateSite <- function(df1){
  #input: dataframe of hourly sensor data from one site
  #output: same dataframe with:
  #date and date_notime columns changed from char to as.POSITxc and date formats, respectively
  #WYDay and WY columns added
  #X column with row numbers removed
  df1 <- df1 |> 
    mutate(date = parse_date_time(date, orders = c("ymd HMS", "ymd"), tz = "MST")) |> #have to use parse_date_time from lubridate to preserve midnight time stamps
    mutate(date_notime = as.Date(date_notime, format = "%Y-%m-%d")) |> #changed to Date type bc input to get water year day must be Date type
    mutate(WYDay = get_waterYearDay(date_notime)) |> 
    mutate(WY = get_waterYear(date))
  df1$X <- NULL
  return(df1)
}

#CLUSTER 1
#1: 1 6 7 8
site1 <- mutateSite(site1)
site6 <- mutateSite(site6)
site7 <- mutateSite(site7)
site8 <- mutateSite(site8)

cluster1 <- bind_rows(site1, site6, site7, site8) |> 
  filter(!is.na(WY))

ggplot() + 
  geom_line(data = cluster1, aes(x = WYDay, y = site1, color="Sensor 1"), na.rm = TRUE) +
  geom_line(data = cluster1, aes(x=WYDay, y = site6, color = "Sensor 6"), na.rm = TRUE) +
  geom_line(data = cluster1, aes(x=WYDay, y = site7, color = "Sensor 7"), na.rm = TRUE)+
  geom_line(data = cluster1, aes(x=WYDay, y = site8, color = "Sensor 8"), na.rm = TRUE)+
  facet_wrap(~WY, ncol = 4)+
  scale_color_manual(name = "Sensor",
                     values = c("Sensor 1" = "blue",
                                "Sensor 6" = "red",
                                "Sensor 7" = "green",
                                "Sensor 8" = "orange"))+
  labs(title = "Cluster 1 Snowdepth Measurements",
       x = "Day of Water Year",
       y = "snowdepth (m)")+
  theme(legend.position = "bottom")
  
#CLUSTER 2
#2: 12,13,16,20
site12 <- mutateSite(site12)
site13 <- mutateSite(site13)
site16 <- mutateSite(site16)
site20 <- mutateSite(site20)

cluster2 <- bind_rows(site12, site13, site16, site20) |> 
  filter(!is.na(WY))

ggplot() + 
  geom_line(data = cluster2, aes(x = WYDay, y = site12, color="Sensor 12"), na.rm = TRUE) +
  geom_line(data = cluster2, aes(x=WYDay, y = site13, color = "Sensor 13"), na.rm = TRUE) +
  geom_line(data = cluster2, aes(x=WYDay, y = site16, color = "Sensor 16"), na.rm = TRUE)+
  geom_line(data = cluster2, aes(x=WYDay, y = site20, color = "Sensor 20"), na.rm = TRUE)+
  facet_wrap(~WY, ncol = 3)+
  scale_color_manual(name = "Sensor",
                     values = c("Sensor 12" = "blue",
                                "Sensor 13" = "red",
                                "Sensor 16" = "green",
                                "Sensor 20" = "orange"))+
  labs(title = "Cluster 2 Snowdepth Measurements",
       x = "Day of Water Year",
       y = "snowdepth (m)")+
  theme(legend.position = "bottom")

#CLUSTER 3
#3: 11,14,15,19
site11 <- mutateSite(site11)
site14 <- mutateSite(site14)
site15 <- mutateSite(site15)
site19 <- mutateSite(site19)

cluster3 <- bind_rows(site11, site14, site15, site19) |> 
  filter(!is.na(WY))

ggplot() + 
  geom_line(data = cluster3, aes(x=WYDay, y = site11, color = "Sensor 11"), na.rm = TRUE) +
  geom_line(data = cluster3, aes(x=WYDay, y = site14, color = "Sensor 14"), na.rm = TRUE) +
  geom_line(data = cluster3, aes(x=WYDay, y = site15, color = "Sensor 15"), na.rm = TRUE)+
  geom_line(data = cluster3, aes(x=WYDay, y = site19, color = "Sensor 19"), na.rm = TRUE)+
  facet_wrap(~WY, ncol = 3)+
  scale_color_manual(name = "Sensor",
                     values = c("Sensor 11" = "blue",
                                "Sensor 14" = "red",
                                "Sensor 15" = "green",
                                "Sensor 19" = "orange"))+
  labs(title = "Cluster 3 Snowdepth Measurements",
       x = "Day of Water Year",
       y = "snowdepth (m)")+
  theme(legend.position = "bottom")

#CLUSTER 4
#4: 9,10,17,21
site9 <- mutateSite(site9)
site10 <- mutateSite(site10)
site17 <- mutateSite(site17)
site21 <- mutateSite(site21)

cluster4 <- bind_rows(site9, site10, site17, site21) |> 
  filter(!is.na(WY))

ggplot() + 
  geom_line(data = cluster4, aes(x=WYDay, y = site9, color = "Sensor 9"), na.rm = TRUE) +
  geom_line(data = cluster4, aes(x=WYDay, y = site10, color = "Sensor 10"), na.rm = TRUE) +
  geom_line(data = cluster4, aes(x=WYDay, y = site17, color = "Sensor 17"), na.rm = TRUE)+
  geom_line(data = cluster4, aes(x=WYDay, y = site21, color = "Sensor 21"), na.rm = TRUE)+
  facet_wrap(~WY, ncol = 3)+
  scale_color_manual(name = "Sensor",
                     values = c("Sensor 9" = "blue",
                                "Sensor 10" = "red",
                                "Sensor 17" = "green",
                                "Sensor 21" = "orange"))+
  labs(title = "Cluster 4 Snowdepth Measurements",
       x = "Day of Water Year",
       y = "snowdepth (m)")+
  theme(legend.position = "bottom")
