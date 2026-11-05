library(tidyverse)
library(dplyr)
library(EflowStats)
library(lubridate)
library(dtw)


setwd("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\SNA data")

data <- read.csv("SNA_InterpolatedCSV.csv")

data2 <- data |> 
  mutate(datetime = ifelse(grepl(" ", date), date, paste(date, "00:00"))) |> 
  mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M")) |> 
  mutate(waterYear = get_waterYear(as.Date(datetime))) |> 
  mutate(waterYearDay = get_waterYearDay(as.Date(datetime)))

dailySnow <- data2 |> 
  group_by(sensor, waterYear, waterYearDay) |> 
  summarize(dailySnowDepth = median(snow_depth, na.rm = T))

dailySnowWide <- dailySnow |> 
  pivot_wider(id_cols = c(sensor, waterYear), names_from = waterYearDay, values_from = dailySnowDepth) |> 
  select(-`366`)

dailySnowWide <- dailySnowWide[complete.cases(dailySnowWide),]

dist_mat_all <- dtwDist(dailySnowWide[,3:367], dailySnowWide[,3:367])

heatmap(dist_mat_all)

hc_result <- hclust(as.dist(dist_mat_all), method = "complete")

plot(hc_result, hang = -1)

cluster_assignments <- cutree(hc_result, k = 5) #5 because

print(cluster_assignments)

dailySnowWide$cluster <- cluster_assignments

dailySnowLong <- dailySnowWide |> 
  pivot_longer(cols = 3:367, names_to = "waterYearDay", values_to = "snowDepth")

ggplot(dailySnowLong, aes(x = as.numeric(waterYearDay), y = snowDepth, col = sensor))+
  geom_line(aes(group = interaction(sensor, waterYear)))+
  facet_wrap(~cluster)

ggplot(dailySnowLong, aes(x = as.numeric(waterYearDay), y = snowDepth, col = as.factor(cluster)))+
  geom_line(aes(group = interaction(sensor, waterYear)))+
  facet_wrap(~sensor)

#look at aggomerative clustering, kmeans, maybe try normalizing first and then do hierarchical clustering for shape, look at cluster 3 more
#hierarchical clustering is good for now: think abt how analyses fit into questions
#look at topography across the clusters
#what geographic lcoations are more variable to interannual variations in climate