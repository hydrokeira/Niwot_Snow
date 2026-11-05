library(tidyverse)
library(dplyr)
library(tidyr)
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

mutateSite <- function(df1){
  #input: dataframe of hourly sensor data from one site
  #output: same dataframe with:
  #date and date_notime columns changed from char to as.POSITxc and date formats, respectively
  #and filtered to only include measurements from WY 2024
  #X column with row numbers removed
  df1 <- df1 |> 
    mutate(date = parse_date_time(date, orders = c("ymd HMS", "ymd"), tz = "MST")) |> #have to use parse_date_time from lubridate to preserve midnight time stamps
    mutate(date_notime = as.Date(date_notime, format = "%Y-%m-%d")) |> #changed to Date type bc input to get water year day must be Date type
    filter(get_waterYear(date_notime) == 2024)
  df1$X <- NULL
  return(df1)
}

site1_2024 <- mutateSite(site1)
site6_2024 <- mutateSite(site6)

#combine data sets, keeping every hour from either df (so there are NAs when one has data and the other doesn't)
sitesCombined_2024 <- full_join(
  site1_2024 |> 
    select(date, Sensor1 = site1),
  site6_2024 |> 
    select(date, Sensor6 = site6),
  by = "date"
)

#use a linear fit model 
#note: lm drops NAs by default
fit1_6 <- lm(Sensor1 ~ Sensor6, data = sitesCombined_2024) #predicting 1 from 6
fit6_1 <- lm(Sensor6 ~ Sensor1, data = sitesCombined_2024) #predicting 6 from 1

#summary of linear model to see if its a good fit
#improvements: how to tell if a linear model is a good fit beyond just the r^2
summary(fit1_6)
summary(fit6_1)

#predict to fill in gaps
#AI note: i used claude to show an example of how to do this prediction portion
sitesCombined_2024 <- sitesCombined_2024 |> 
  mutate(
    #predict returns NA if both datasets have missing data for timestamp
    sensor1_p = predict(fit1_6, newdata = sitesCombined_2024),
    sensor6_p = predict(fit6_1, newdata = sitesCombined_2024),
    #coalesce keeps the observed value if it exists
    sensor1_filled = coalesce(Sensor1, sensor1_p),
    sensor6_filled = coalesce(Sensor6, sensor6_p),
    #source flags keep track of what is og and what is predicted
    sensor1_source = if_else(is.na(Sensor1) & !is.na(sensor1_p), "predicted","observed"),
    sensor6_source = if_else(is.na(Sensor6) & !is.na(sensor6_p), "predicted","observed")
    
  )

ggplot()+
  geom_point(data = sitesCombined_2024, aes(x=date, y = sensor1_filled, color = "Site 1 Filled"), na.rm = TRUE) +
  geom_point(data = sitesCombined_2024, aes(x=date, y = sensor6_filled, color = "Site 6 Filled"), na.rm = TRUE) +
  geom_point(data = sitesCombined_2024, aes(x=date, y = Sensor1, color = "Original Site 1"), na.rm = TRUE) +
  geom_point(data = sitesCombined_2024, aes(x=date, y = Sensor6, color = "Original Site 6"), na.rm = TRUE) +
  scale_color_manual(name = " ",
                     values = c("Original Site 1" = "black",
                                "Original Site 6" = "black",
                                "Site 1 Filled" = "red",
                                "Site 6 Filled" = "red"))+
  labs(title = "Original and Predicted Snowdepth Measurements",
       x = "Date",
       y = "snowdepth (m)")+
  theme(legend.position = "bottom")

ggplot()+
  geom_line(data = sitesCombined_2024, aes(x=date, y = Sensor1, color = "Original Site 1"), na.rm = TRUE) +
  geom_line(data = sitesCombined_2024, aes(x=date, y = Sensor6, color = "Original Site 6"), na.rm = TRUE) +
  scale_color_manual(name = " ",
                     values = c("Original Site 1" = "skyblue",
                                "Original Site 6" = "lightgreen"))+
  labs(title = "Original Snowdepth Measurements",
       x = "Date",
       y = "snowdepth (m)")+
  theme(legend.position = "bottom")

ggplot()+
  geom_line(data = sitesCombined_2024, aes(x=date, y = sensor1_filled, color = "Site 1 Filled"), na.rm = TRUE) +
  geom_line(data = sitesCombined_2024, aes(x=date, y = sensor6_filled, color = "Site 6 Filled"), na.rm = TRUE) +
  scale_color_manual(name = " ",
                     values = c("Site 1 Filled" = "navy",
                                "Site 6 Filled" = "darkgreen"))+
  labs(title = "Predicted Snowdepth Measurements",
       x = "Date",
       y = "snowdepth (m)")+
  theme(legend.position = "bottom")



