#same as DTW clustering but extended back through WY 2021
#1, 13, 19, 20 need to be removed from WY2021
#19, 20 need to be removed from WY2022

library(tidyverse)
library(dplyr)
library(EflowStats)
library(lubridate)
library(dtw)
library(data.table)


setwd("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\SNA data")

Data <- read.csv("SNA_Interpolated_Updated.csv")

Data2 <- Data |> #add datetime, water year, and day of water year columns
  mutate(datetime = ifelse(grepl(" ", date), date, paste(date, "00:00:00"))) |> 
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")) |> 
  mutate(waterYear = get_waterYear(as.Date(datetime))) |> 
  mutate(waterYearDay = get_waterYearDay(as.Date(datetime)))

DailySnow <- Data2 |> 
  group_by(sensor, waterYear, waterYearDay) |> #splits data into groups by sensor, then water year, then day
  summarize(dailySnowDepth = median(snow_depth, na.rm = T)) #collapses each group into 1 row (median per sensor for each day of the water year)
DailySnow <- DailySnow |> #filter out years with only predicted values
  filter_out(sensor == "sn_01_tenminute.jm.data.csv" & waterYear == 2021) |> 
  filter_out(sensor == "sn_13_tenminute.jm.data.csv" & waterYear == 2021) |> 
  filter_out(sensor == "sn_19_tenminute.jm.data.csv" & waterYear == 2021) |> 
  filter_out(sensor == "sn_20_tenminute.jm.data.csv" & waterYear == 2021) |> 
  filter_out(sensor == "sn_19_tenminute.jm.data.csv" & waterYear == 2022) |> 
  filter_out(sensor == "sn_20_tenminute.jm.data.csv" & waterYear == 2022)

dailySnowWide <- DailySnow |> #change to wide format with sensor and water year as row names and day of water year as column names
  pivot_wider(id_cols = c(sensor, waterYear), names_from = waterYearDay, values_from = dailySnowDepth) |> 
  select(-`366`) #select everything except day 366 (get rid of leap year days)

dailySnowWide <- dailySnowWide[complete.cases(dailySnowWide),] #keep only complete rows (Get rid of 2026 water year)

dist_mat_all <- dtwDist(dailySnowWide[,3:367], dailySnowWide[,3:367]) #make a distance matrix

heatmap(dist_mat_all) #make a heat map showing distance matrix

hc_result <- hclust(as.dist(dist_mat_all), method = "complete") #perform hierarchical clustering using the distance matrix

plot(hc_result, hang = -1) #plot hierarchical clustering result

cluster_assignments <- cutree(hc_result, k = 7) #check that 7 is the right choice, assign cluster 1-7 to each sensor in each WY

print(cluster_assignments)

dailySnowWide$cluster <- cluster_assignments #add column to dailySnowWide with the cluster assignments

dailySnowLong <- dailySnowWide |> #change back to long format
  pivot_longer(cols = 3:367, names_to = "waterYearDay", values_to = "snowDepth")

fwrite(dailySnowLong, "C:\\Users\\iameb\\Github\\Niwot_Snow\\clusteredData.csv") #save to csv
sensorClusterPairs <- dailySnowWide |> select(sensor, waterYear, cluster)

ggplot(dailySnowLong, aes(x = as.numeric(waterYearDay), y = snowDepth, col = sensor))+ #plot cluster 1-7 with water year day on x, snowdepth on y, and color coded by sensor
  geom_line(aes(group = interaction(sensor, waterYear)))+
  facet_wrap(~cluster)

ggplot(dailySnowLong, aes(x = as.numeric(waterYearDay), y = snowDepth, col = as.factor(cluster)))+ #plot sensor 1-16 with water year day on x, snowdepth on y, and color coded by cluster
  geom_line(aes(group = interaction(sensor, waterYear)))+
  facet_wrap(~sensor)

#look at aggomerative clustering, kmeans, maybe try normalizing first and then do hierarchical clustering for shape, look at cluster 3 more
#hierarchical clustering is good for now: think abt how analyses fit into questions
#look at topography across the clusters
#what geographic lcoations are more variable to interannual variations in climate