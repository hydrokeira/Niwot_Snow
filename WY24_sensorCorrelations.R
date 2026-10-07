require(lubridate)
library(tidyverse)
library(dplyr)
library(tidyr)
library(EflowStats)
library("egg")

#read in data for sensors I want to investigate
#sensors 1,6,13,14,15,16,19
setwd("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\SNA 10 minute data")

site1 <- read.csv("sn1_hourly.csv")
site6 <- read.csv("sn6_hourly.csv")
site13 <- read.csv("sn13_hourly.csv")
site14 <- read.csv("sn14_hourly.csv")
site15 <- read.csv("sn15_hourly.csv")
site16 <- read.csv("sn16_hourly.csv")
site19 <- read.csv("sn19_hourly.csv")


#sites 16 and 15
#future improvement: make this into a function since will want to look at these graphs for a lot of sensors
#combine data, convert date to date, take average by hour across sites
sites_combined1615 <- site16 %>%
  left_join(site15, by = "date") %>%
  filter(!is.na(site16)) %>%
  filter(!is.na(site15)) %>%
  filter(site16 > 0 & site15 > 0) %>%
  mutate(date=as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate(date = lubridate::floor_date(date, "hour")) %>%
  mutate(date_notime = as.Date(date)) %>%
  group_by(date, date_notime) %>%
  summarise(site16 = mean(site16, na.rm = TRUE),
            site15 = mean(site15, na.rm = TRUE))


#plot
p1<-ggplot(sites_combined1615, aes(site16, site15))+geom_point(aes(col=get_waterYearDay(date_notime)))+facet_wrap(~get_waterYear(date_notime))+
  scale_color_viridis_c(option = "F")+theme_bw()+labs(col="Day of WY")+ggtitle("Hourly")

p2<-ggplot(sites_combined1615, aes(site16, site15))+geom_point(aes(col=get_waterYearDay(date_notime)))+facet_wrap(~get_waterYear(date_notime))+
  scale_color_viridis_c(option = "F")+theme_bw()+labs(col="Day of WY")+ggtitle("Daily")

ggarrange(p1, p2, nrow = 2)

#sites 16 and 19
#combine data, convert date to date, take average by hour across sites
sites_combined1619 <- site16 %>%
  left_join(site19, by = "date") %>%
  filter(!is.na(site16)) %>%
  filter(!is.na(site19)) %>%
  filter(site16 > 0 & site19 > 0) %>%
  mutate(date=as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate(date = lubridate::floor_date(date, "hour")) %>%
  mutate(date_notime = as.Date(date)) %>%
  group_by(date, date_notime) %>%
  summarise(site16 = mean(site16, na.rm = TRUE),
            site19 = mean(site19, na.rm = TRUE))


#plot
p3<-ggplot(sites_combined1619, aes(site16, site19))+geom_point(aes(col=get_waterYearDay(date_notime)))+facet_wrap(~get_waterYear(date_notime))+
  scale_color_viridis_c(option = "F")+theme_bw()+labs(col="Day of WY")+ggtitle("Hourly")

p4<-ggplot(sites_combined1619, aes(site16, site19))+geom_point(aes(col=get_waterYearDay(date_notime)))+facet_wrap(~get_waterYear(date_notime))+
  scale_color_viridis_c(option = "F")+theme_bw()+labs(col="Day of WY")+ggtitle("Daily")

ggarrange(p3, p4, nrow = 2)

#sites 15 and 19
#combine data, convert date to date, take average by hour across sites
sites_combined1519 <- site15 %>%
  left_join(site19, by = "date") %>%
  filter(!is.na(site15)) %>%
  filter(!is.na(site19)) %>%
  filter(site15 > 0 & site19 > 0) %>%
  mutate(date=as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate(date = lubridate::floor_date(date, "hour")) %>%
  mutate(date_notime = as.Date(date)) %>%
  group_by(date, date_notime) %>%
  summarise(site15 = mean(site15, na.rm = TRUE),
            site19 = mean(site19, na.rm = TRUE))


#plot
p5<-ggplot(sites_combined1519, aes(site15, site19))+geom_point(aes(col=get_waterYearDay(date_notime)))+facet_wrap(~get_waterYear(date_notime))+
  scale_color_viridis_c(option = "F")+theme_bw()+labs(col="Day of WY")+ggtitle("Hourly")

p6<-ggplot(sites_combined1519, aes(site15, site19))+geom_point(aes(col=get_waterYearDay(date_notime)))+facet_wrap(~get_waterYear(date_notime))+
  scale_color_viridis_c(option = "F")+theme_bw()+labs(col="Day of WY")+ggtitle("Daily")

ggarrange(p5, p6, nrow = 2)

#sites 13 and 14
#combine data, convert date to date, take average by hour across sites
sites_combined1314 <- site13 %>%
  left_join(site14, by = "date") %>%
  filter(!is.na(site13)) %>%
  filter(!is.na(site14)) %>%
  filter(site13 > 0 & site14 > 0) %>%
  mutate(date=as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate(date = lubridate::floor_date(date, "hour")) %>%
  mutate(date_notime = as.Date(date)) %>%
  group_by(date, date_notime) %>%
  summarise(site13 = mean(site13, na.rm = TRUE),
            site14 = mean(site14, na.rm = TRUE))


#plot
p7<-ggplot(sites_combined1314, aes(site13, site14))+geom_point(aes(col=get_waterYearDay(date_notime)))+facet_wrap(~get_waterYear(date_notime))+
  scale_color_viridis_c(option = "F")+theme_bw()+labs(col="Day of WY")+ggtitle("Hourly")

p8<-ggplot(sites_combined1314, aes(site13, site14))+geom_point(aes(col=get_waterYearDay(date_notime)))+facet_wrap(~get_waterYear(date_notime))+
  scale_color_viridis_c(option = "F")+theme_bw()+labs(col="Day of WY")+ggtitle("Daily")

ggarrange(p7, p8, nrow = 2)

#sites 6 and 1
#combine data, convert date to date, take average by hour across sites
sites_combined61 <- site6 %>%
  left_join(site1, by = "date") %>%
  filter(!is.na(site6)) %>%
  filter(!is.na(site1)) %>%
  filter(site6 > 0 & site1 > 0) %>%
  mutate(date=as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate(date = lubridate::floor_date(date, "hour")) %>%
  mutate(date_notime = as.Date(date)) %>%
  group_by(date, date_notime) %>%
  summarise(site6 = mean(site6, na.rm = TRUE),
            site1 = mean(site1, na.rm = TRUE))


#plot
p9<-ggplot(sites_combined61, aes(site6, site1))+geom_point(aes(col=get_waterYearDay(date_notime)))+
  facet_wrap(~get_waterYear(date_notime))+
  scale_color_viridis_c(option = "F")+theme_bw()+labs(col="Day of WY")+ggtitle("Hourly")

p10<-ggplot(sites_combined61, aes(site6, site1))+geom_point(aes(col=get_waterYearDay(date_notime)))+
  facet_wrap(~get_waterYear(date_notime))+
  scale_color_viridis_c(option = "F")+theme_bw()+labs(col="Day of WY")+ggtitle("Daily")

ggarrange(p9, p10, nrow = 2)


#get started on interpolating
#need to filter to just WY 2024
site6_WY2024 <- site6 |> 
  filter(get_waterYear(date_notime) == 2024)

site1_WY2024 <- site1 |> 
  filter(get_waterYear(date_notime) == 2024)

sites_combined61_WY2024 <-  sites_combined61 |> 
  filter(get_waterYear(date_notime) == 2024)

#response ~ predictor
model_61 <- lm(site1 ~ site6, data = sites_combined61_WY2024)

#IN PROGRESS CODE from here to do predictions
summary(model_61)
df <- sites_combined61_WY2024 |> 
  mutate(complete_date = seq(min(date), max(date), by = "hour"))
predictions <- predict(model_61)





