#goal: aggregate 10 min data to hourly and save as a new dataframe going forward
#6 measurements per hour: 0 10 20 30 40 50 

# want a new dataframe for each sensor, including only data with flag 'n', snowdepth median and date values, 
# and that is not NaN

#set working directory where I will save new dfs to
library(tidyverse)
library(dplyr)
library(EflowStats)
library(lubridate)

setwd("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\SNA 10 minute data")

#read in all site data
site1 <- read.csv("sn_01_tenminute.jm.data.csv")
site6 <- read.csv("sn_06_tenminute.jm.data.csv")
site7 <- read.csv("sn_07_tenminute.jm.data.csv")
site8 <- read.csv("sn_08_tenminute.jm.data.csv")
site9 <- read.csv("sn_09_tenminute.jm.data.csv")
site10 <- read.csv("sn_10_tenminute.jm.data.csv")
site11 <- read.csv("sn_11_tenminute.jm.data.csv")
site12 <- read.csv("sn_12_tenminute.jm.data.csv")
site13 <- read.csv("sn_13_tenminute.jm.data.csv")
site14 <- read.csv("sn_14_tenminute.jm.data.csv")
site15 <- read.csv("sn_15_tenminute.jm.data.csv")
site16 <- read.csv("sn_16_tenminute.jm.data.csv")
site17 <- read.csv("sn_17_tenminute.jm.data.csv")
site19 <- read.csv("sn_19_tenminute.jm.data.csv")
site20 <- read.csv("sn_20_tenminute.jm.data.csv")
site21 <- read.csv("sn_21_tenminute.jm.data.csv")

#filter and select data for each site, and rename data columns so each column name is site specific
site1 <- site1 |> 
  dplyr::select(date, snowdepth_median, flag_snowdepth_median) |> 
  filter(flag_snowdepth_median=="n", !is.na(snowdepth_median)) #select only 'normal' data and non NaN data
colnames(site1)[2]<-"site1"

site6 <- site6 |> 
  dplyr::select(date, snowdepth_median, flag_snowdepth_median) |> 
  filter(flag_snowdepth_median=="n", !is.na(snowdepth_median)) #select only 'normal' data and non NaN data
colnames(site6)[2]<-"site6"

site7 <- site7 |> 
  dplyr::select(date, snowdepth_median, flag_snowdepth_median) |> 
  filter(flag_snowdepth_median=="n", !is.na(snowdepth_median)) #select only 'normal' data and non NaN data
colnames(site7)[2]<-"site7"

site8 <- site8 |> 
  dplyr::select(date, snowdepth_median, flag_snowdepth_median) |> 
  filter(flag_snowdepth_median=="n", !is.na(snowdepth_median)) #select only 'normal' data and non NaN data
colnames(site8)[2]<-"site8"

site9 <- site9 |> 
  dplyr::select(date, snowdepth_median, flag_snowdepth_median) |> 
  filter(flag_snowdepth_median=="n", !is.na(snowdepth_median)) #select only 'normal' data and non NaN data
colnames(site9)[2]<-"site9"

site10 <- site10 |> 
  dplyr::select(date, snowdepth_median, flag_snowdepth_median) |> 
  filter(flag_snowdepth_median=="n", !is.na(snowdepth_median)) #select only 'normal' data and non NaN data
colnames(site10)[2]<-"site10"

site11 <- site11 |> 
  dplyr::select(date, snowdepth_median, flag_snowdepth_median) |> 
  filter(flag_snowdepth_median=="n", !is.na(snowdepth_median)) #select only 'normal' data and non NaN data
colnames(site11)[2]<-"site11"

site12 <- site12 |> 
  dplyr::select(date, snowdepth_median, flag_snowdepth_median) |> 
  filter(flag_snowdepth_median=="n", !is.na(snowdepth_median)) #select only 'normal' data and non NaN data
colnames(site12)[2]<-"site12"

site13 <- site13 |> 
  dplyr::select(date, snowdepth_median, flag_snowdepth_median) |> 
  filter(flag_snowdepth_median=="n", !is.na(snowdepth_median)) #select only 'normal' data and non NaN data
colnames(site13)[2]<-"site13"

site14 <- site14 |> 
  dplyr::select(date, snowdepth_median, flag_snowdepth_median) |> 
  filter(flag_snowdepth_median=="n", !is.na(snowdepth_median)) #select only 'normal' data and non NaN data
colnames(site14)[2]<-"site14"

site15 <- site15 |> 
  dplyr::select(date, snowdepth_median, flag_snowdepth_median) |> 
  filter(flag_snowdepth_median=="n", !is.na(snowdepth_median)) #select only 'normal' data and non NaN data
colnames(site15)[2]<-"site15"

site16 <- site16 |> 
  dplyr::select(date, snowdepth_median, flag_snowdepth_median) |> 
  filter(flag_snowdepth_median=="n", !is.na(snowdepth_median)) #select only 'normal' data and non NaN data
colnames(site16)[2]<-"site16"

site17 <- site17 |> 
  dplyr::select(date, snowdepth_median, flag_snowdepth_median) |> 
  filter(flag_snowdepth_median=="n", !is.na(snowdepth_median)) #select only 'normal' data and non NaN data
colnames(site17)[2]<-"site17"

site19 <- site19 |> 
  dplyr::select(date, snowdepth_median, flag_snowdepth_median) |> 
  filter(flag_snowdepth_median=="n", !is.na(snowdepth_median)) #select only 'normal' data and non NaN data
colnames(site19)[2]<-"site19"

site20 <- site20 |> 
  dplyr::select(date, snowdepth_median, flag_snowdepth_median) |> 
  filter(flag_snowdepth_median=="n", !is.na(snowdepth_median)) #select only 'normal' data and non NaN data
colnames(site20)[2]<-"site20"

site21 <- site21 |> 
  dplyr::select(date, snowdepth_median, flag_snowdepth_median) |> 
  filter(flag_snowdepth_median=="n", !is.na(snowdepth_median)) #select only 'normal' data and non NaN data
colnames(site21)[2]<-"site21"

#convert date to date and take hourly average
site1 <- site1 |> 
  filter(site1 > 0 ) |> #get ride of negative vals (often at end of WY)
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) |> #convert from text to datetime object
  mutate(date = floor_date(date, "hour")) |> #'floor' each time measurement to the hour it occurs in (i.e. 10:20 to 10:00)
  mutate(date_notime = as.Date(date)) |> #save date value to use for plotting later
  group_by(date, date_notime) |> #group rows by hour and calendar day
  summarise(site1 = mean(site1))

site6 <- site6 |> 
  filter(site6 > 0 ) |> #get ride of negative vals (often at end of WY)
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) |> #convert from text to datetime object
  mutate(date = floor_date(date, "hour")) |> #'floor' each time measurement to the hour it occurs in (i.e. 10:20 to 10:00)
  mutate(date_notime = as.Date(date)) |> #save date value to use for plotting later
  group_by(date, date_notime) |> #group rows by hour and calendar day
  summarise(site6 = mean(site6))

site7 <- site7 |> 
  filter(site7 > 0 ) |> #get ride of negative vals (often at end of WY)
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) |> #convert from text to datetime object
  mutate(date = floor_date(date, "hour")) |> #'floor' each time measurement to the hour it occurs in (i.e. 10:20 to 10:00)
  mutate(date_notime = as.Date(date)) |> #save date value to use for plotting later
  group_by(date, date_notime) |> #group rows by hour and calendar day
  summarise(site7 = mean(site7))

site8 <- site8 |> 
  filter(site8 > 0 ) |> #get ride of negative vals (often at end of WY)
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) |> #convert from text to datetime object
  mutate(date = floor_date(date, "hour")) |> #'floor' each time measurement to the hour it occurs in (i.e. 10:20 to 10:00)
  mutate(date_notime = as.Date(date)) |> #save date value to use for plotting later
  group_by(date, date_notime) |> #group rows by hour and calendar day
  summarise(site8 = mean(site8))

site9 <- site9 |> 
  filter(site9 > 0 ) |> #get ride of negative vals (often at end of WY)
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) |> #convert from text to datetime object
  mutate(date = floor_date(date, "hour")) |> #'floor' each time measurement to the hour it occurs in (i.e. 10:20 to 10:00)
  mutate(date_notime = as.Date(date)) |> #save date value to use for plotting later
  group_by(date, date_notime) |> #group rows by hour and calendar day
  summarise(site9 = mean(site9))

site10 <- site10 |> 
  filter(site10 > 0 ) |> #get ride of negative vals (often at end of WY)
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) |> #convert from text to datetime object
  mutate(date = floor_date(date, "hour")) |> #'floor' each time measurement to the hour it occurs in (i.e. 10:20 to 10:00)
  mutate(date_notime = as.Date(date)) |> #save date value to use for plotting later
  group_by(date, date_notime) |> #group rows by hour and calendar day
  summarise(site10 = mean(site10))

site11 <- site11 |> 
  filter(site11 > 0 ) |> #get ride of negative vals (often at end of WY)
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) |> #convert from text to datetime object
  mutate(date = floor_date(date, "hour")) |> #'floor' each time measurement to the hour it occurs in (i.e. 10:20 to 10:00)
  mutate(date_notime = as.Date(date)) |> #save date value to use for plotting later
  group_by(date, date_notime) |> #group rows by hour and calendar day
  summarise(site11 = mean(site11))

site12 <- site12 |> 
  filter(site12 > 0 ) |> #get ride of negative vals (often at end of WY)
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) |> #convert from text to datetime object
  mutate(date = floor_date(date, "hour")) |> #'floor' each time measurement to the hour it occurs in (i.e. 10:20 to 10:00)
  mutate(date_notime = as.Date(date)) |> #save date value to use for plotting later
  group_by(date, date_notime) |> #group rows by hour and calendar day
  summarise(site12 = mean(site12))

site13 <- site13 |> 
  filter(site13 > 0 ) |> #get ride of negative vals (often at end of WY)
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) |> #convert from text to datetime object
  mutate(date = floor_date(date, "hour")) |> #'floor' each time measurement to the hour it occurs in (i.e. 10:20 to 10:00)
  mutate(date_notime = as.Date(date)) |> #save date value to use for plotting later
  group_by(date, date_notime) |> #group rows by hour and calendar day
  summarise(site13 = mean(site13))

site14 <- site14 |> 
  filter(site14 > 0 ) |> #get ride of negative vals (often at end of WY)
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) |> #convert from text to datetime object
  mutate(date = floor_date(date, "hour")) |> #'floor' each time measurement to the hour it occurs in (i.e. 10:20 to 10:00)
  mutate(date_notime = as.Date(date)) |> #save date value to use for plotting later
  group_by(date, date_notime) |> #group rows by hour and calendar day
  summarise(site14 = mean(site14))

site15 <- site15 |> 
  filter(site15 > 0 ) |> #get ride of negative vals (often at end of WY)
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) |> #convert from text to datetime object
  mutate(date = floor_date(date, "hour")) |> #'floor' each time measurement to the hour it occurs in (i.e. 10:20 to 10:00)
  mutate(date_notime = as.Date(date)) |> #save date value to use for plotting later
  group_by(date, date_notime) |> #group rows by hour and calendar day
  summarise(site15 = mean(site15))

site16 <- site16 |> 
  filter(site16 > 0 ) |> #get ride of negative vals (often at end of WY)
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) |> #convert from text to datetime object
  mutate(date = floor_date(date, "hour")) |> #'floor' each time measurement to the hour it occurs in (i.e. 10:20 to 10:00)
  mutate(date_notime = as.Date(date)) |> #save date value to use for plotting later
  group_by(date, date_notime) |> #group rows by hour and calendar day
  summarise(site16 = mean(site16))

site17 <- site17 |> 
  filter(site17 > 0 ) |> #get ride of negative vals (often at end of WY)
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) |> #convert from text to datetime object
  mutate(date = floor_date(date, "hour")) |> #'floor' each time measurement to the hour it occurs in (i.e. 10:20 to 10:00)
  mutate(date_notime = as.Date(date)) |> #save date value to use for plotting later
  group_by(date, date_notime) |> #group rows by hour and calendar day
  summarise(site17 = mean(site17))

site19 <- site19 |> 
  filter(site19 > 0 ) |> #get ride of negative vals (often at end of WY)
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) |> #convert from text to datetime object
  mutate(date = floor_date(date, "hour")) |> #'floor' each time measurement to the hour it occurs in (i.e. 10:20 to 10:00)
  mutate(date_notime = as.Date(date)) |> #save date value to use for plotting later
  group_by(date, date_notime) |> #group rows by hour and calendar day
  summarise(site19 = mean(site19))

site20 <- site20 |> 
  filter(site20 > 0 ) |> #get ride of negative vals (often at end of WY)
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) |> #convert from text to datetime object
  mutate(date = floor_date(date, "hour")) |> #'floor' each time measurement to the hour it occurs in (i.e. 10:20 to 10:00)
  mutate(date_notime = as.Date(date)) |> #save date value to use for plotting later
  group_by(date, date_notime) |> #group rows by hour and calendar day
  summarise(site20 = mean(site20))

site21 <- site21 |> 
  filter(site21 > 0 ) |> #get ride of negative vals (often at end of WY)
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) |> #convert from text to datetime object
  mutate(date = floor_date(date, "hour")) |> #'floor' each time measurement to the hour it occurs in (i.e. 10:20 to 10:00)
  mutate(date_notime = as.Date(date)) |> #save date value to use for plotting later
  group_by(date, date_notime) |> #group rows by hour and calendar day
  summarise(site21 = mean(site21))

#add hourly average to new csv for each site so I can just load these in the future
write.csv(site1, "sn1_hourly.csv")
write.csv(site6, "sn6_hourly.csv")
write.csv(site7, "sn7_hourly.csv")
write.csv(site8, "sn8_hourly.csv")
write.csv(site9, "sn9_hourly.csv")
write.csv(site10, "sn10_hourly.csv")
write.csv(site11, "sn11_hourly.csv")
write.csv(site12, "sn12_hourly.csv")
write.csv(site13, "sn13_hourly.csv")
write.csv(site14, "sn14_hourly.csv")
write.csv(site15, "sn15_hourly.csv")
write.csv(site16, "sn16_hourly.csv")
write.csv(site17, "sn17_hourly.csv")
write.csv(site19, "sn19_hourly.csv")
write.csv(site20, "sn20_hourly.csv")
write.csv(site21, "sn21_hourly.csv")
