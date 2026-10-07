library(tidyverse)
library(dplyr)
library(EflowStats)
library(hms)
library(GGally)

#import data from each sensor
sn1 <- read_csv("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\sn_01_tenminute.jm.data.csv")
sn6 <- read_csv("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\sn_06_tenminute.jm.data.csv")
sn7 <- read_csv("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\sn_07_tenminute.jm.data.csv")
sn8 <- read_csv("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\sn_08_tenminute.jm.data.csv")
sn9 <- read_csv("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\sn_09_tenminute.jm.data.csv")
sn10 <- read_csv("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\sn_10_tenminute.jm.data.csv")
sn11 <- read_csv("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\sn_11_tenminute.jm.data.csv")
sn12 <- read_csv("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\sn_12_tenminute.jm.data.csv")
sn13 <- read_csv("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\sn_13_tenminute.jm.data.csv")
sn14 <- read_csv("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\sn_14_tenminute.jm.data.csv")
sn15 <- read_csv("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\sn_15_tenminute.jm.data.csv")
sn16 <- read_csv("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\sn_16_tenminute.jm.data.csv")
sn17 <- read_csv("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\sn_17_tenminute.jm.data.csv")
sn19 <- read_csv("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\sn_19_tenminute.jm.data.csv")
sn20 <- read_csv("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\sn_20_tenminute.jm.data.csv")
sn21 <- read_csv("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\sn_21_tenminute.jm.data.csv")

#filter all sn data to just have date,median snow depth, and flag
#future improvments: make a function to do the following filtering so im not copy pasting so much
#can maybe delete water year information since I am not using it now, and combine with selecting steps used for ggpairs
SN1 <- select(sn1, date, snowdepth_median, flag_snowdepth_median) %>%
  mutate(dateOnly = as.Date(date),
         timeOnly = as_hms(date)) %>%
  mutate(waterYear = get_waterYear(dateOnly), dayOfWaterYear = get_waterYearDay(dateOnly))
SN6 <- select(sn6, date, snowdepth_median, flag_snowdepth_median) %>%
  mutate(dateOnly = as.Date(date),
         timeOnly = as_hms(date)) %>%
  mutate(waterYear = get_waterYear(dateOnly), dayOfWaterYear = get_waterYearDay(dateOnly))
SN7 <- select(sn7, date, snowdepth_median, flag_snowdepth_median) %>%
  mutate(dateOnly = as.Date(date),
         timeOnly = as_hms(date)) %>%
  mutate(waterYear = get_waterYear(dateOnly), dayOfWaterYear = get_waterYearDay(dateOnly))
SN8 <- select(sn8, date, snowdepth_median, flag_snowdepth_median) %>%
  mutate(dateOnly = as.Date(date),
         timeOnly = as_hms(date)) %>%
  mutate(waterYear = get_waterYear(dateOnly), dayOfWaterYear = get_waterYearDay(dateOnly))
SN9 <- select(sn9, date, snowdepth_median, flag_snowdepth_median) %>%
  mutate(dateOnly = as.Date(date),
         timeOnly = as_hms(date)) %>%
  mutate(waterYear = get_waterYear(dateOnly), dayOfWaterYear = get_waterYearDay(dateOnly))
SN10 <- select(sn10, date, snowdepth_median, flag_snowdepth_median) %>%
  mutate(dateOnly = as.Date(date),
         timeOnly = as_hms(date)) %>%
  mutate(waterYear = get_waterYear(dateOnly), dayOfWaterYear = get_waterYearDay(dateOnly))
SN11 <- select(sn11, date, snowdepth_median, flag_snowdepth_median) %>%
  mutate(dateOnly = as.Date(date),
         timeOnly = as_hms(date)) %>%
  mutate(waterYear = get_waterYear(dateOnly), dayOfWaterYear = get_waterYearDay(dateOnly))
SN11 <- select(sn11, date, snowdepth_median, flag_snowdepth_median) %>%
  mutate(dateOnly = as.Date(date),
         timeOnly = as_hms(date)) %>%
  mutate(waterYear = get_waterYear(dateOnly), dayOfWaterYear = get_waterYearDay(dateOnly))
SN12 <- select(sn12, date, snowdepth_median, flag_snowdepth_median) %>%
  mutate(dateOnly = as.Date(date),
         timeOnly = as_hms(date)) %>%
  mutate(waterYear = get_waterYear(dateOnly), dayOfWaterYear = get_waterYearDay(dateOnly))
SN13 <- select(sn13, date, snowdepth_median, flag_snowdepth_median) %>%
  mutate(dateOnly = as.Date(date),
         timeOnly = as_hms(date)) %>%
  mutate(waterYear = get_waterYear(dateOnly), dayOfWaterYear = get_waterYearDay(dateOnly))
SN14 <- select(sn14, date, snowdepth_median, flag_snowdepth_median) %>%
  mutate(dateOnly = as.Date(date),
         timeOnly = as_hms(date)) %>%
  mutate(waterYear = get_waterYear(dateOnly), dayOfWaterYear = get_waterYearDay(dateOnly))
SN15 <- select(sn15, date, snowdepth_median, flag_snowdepth_median) %>%
  mutate(dateOnly = as.Date(date),
         timeOnly = as_hms(date)) %>%
  mutate(waterYear = get_waterYear(dateOnly), dayOfWaterYear = get_waterYearDay(dateOnly))
SN16 <- select(sn16, date, snowdepth_median, flag_snowdepth_median) %>%
  mutate(dateOnly = as.Date(date),
         timeOnly = as_hms(date)) %>%
  mutate(waterYear = get_waterYear(dateOnly), dayOfWaterYear = get_waterYearDay(dateOnly))
SN17 <- select(sn17, date, snowdepth_median, flag_snowdepth_median) %>%
  mutate(dateOnly = as.Date(date),
         timeOnly = as_hms(date)) %>%
  mutate(waterYear = get_waterYear(dateOnly), dayOfWaterYear = get_waterYearDay(dateOnly))
SN19 <- select(sn19, date, snowdepth_median, flag_snowdepth_median) %>%
  mutate(dateOnly = as.Date(date),
         timeOnly = as_hms(date)) %>%
  mutate(waterYear = get_waterYear(dateOnly), dayOfWaterYear = get_waterYearDay(dateOnly))
SN20 <- select(sn20, date, snowdepth_median, flag_snowdepth_median) %>%
  mutate(dateOnly = as.Date(date),
         timeOnly = as_hms(date)) %>%
  mutate(waterYear = get_waterYear(dateOnly), dayOfWaterYear = get_waterYearDay(dateOnly))
SN21 <- select(sn21, date, snowdepth_median, flag_snowdepth_median) %>%
  mutate(dateOnly = as.Date(date),
         timeOnly = as_hms(date)) %>%
  mutate(waterYear = get_waterYear(dateOnly), dayOfWaterYear = get_waterYearDay(dateOnly))

#make a plot for each sensor of time vs snow depth and color code by flag 
#for now I have just graphed plots for a few sensors, but maybe it would be useful to look at ones that
#are geographically spread out - need to find map of sensor locations for this
#future improvement: make flag color coding standard across each graph
ggplot(SN1, aes(x = date, y = snowdepth_median, color = flag_snowdepth_median))+ #date values on x axis are posixct data type
  geom_point(alpha = 0.5, na.rm = TRUE) + 
  labs(x = "day/time", y = "median Snow Depth Measurement", color = "flag",
       title = "SN1 Data Color Coded by Flag")+
  theme_minimal()
ggplot(SN8, aes(x = date, y = snowdepth_median, color = flag_snowdepth_median))+
  geom_point(alpha = 0.5, na.rm = TRUE) + 
  labs(x = "day/time", y = "median Snow Depth Measurement", color = "flag",
       title = "SN8 Data Color Coded by Flag")+
  theme_minimal()
ggplot(SN16, aes(x = date, y = snowdepth_median, color = flag_snowdepth_median))+
  geom_point(alpha = 0.5, na.rm = TRUE) + 
  labs(x = "day/time", y = "median Snow Depth Measurement", color = "flag",
       title = "SN16 Data Color Coded by Flag")+
  theme_minimal()

#use ggpairs to plot data from sensors against each other
#future improvments: use map2 function to do this more concisely/efficiently
sensor1 <- SN1 |> filter(flag_snowdepth_median == "n") |> 
  select("date", "snowdepth_median") |> rename(Snowdepth1 = snowdepth_median) #prepare data frames for full join
sensor6 <- SN6 |> filter(flag_snowdepth_median == "n") |>
  select("date", "snowdepth_median") |> rename(Snowdepth6 = snowdepth_median)
sensor7 <- SN7 |> filter(flag_snowdepth_median == "n") |>
  select("date", "snowdepth_median") |> rename(Snowdepth7 = snowdepth_median)
sensor8 <- SN8 |> filter(flag_snowdepth_median == "n") |>
  select("date", "snowdepth_median") |> rename(Snowdepth8 = snowdepth_median)
sensor9 <- SN9 |> filter(flag_snowdepth_median == "n") |>
  select("date", "snowdepth_median") |> rename(Snowdepth9 = snowdepth_median)
sensor10 <- SN10 |> filter(flag_snowdepth_median == "n") |>
  select("date", "snowdepth_median") |> rename(Snowdepth10 = snowdepth_median)
sensor11 <- SN11 |> filter(flag_snowdepth_median == "n") |>
  select("date", "snowdepth_median") |> rename(Snowdepth11 = snowdepth_median)
sensor12 <- SN12 |> filter(flag_snowdepth_median == "n") |>
  select("date", "snowdepth_median") |> rename(Snowdepth12 = snowdepth_median)
sensor13 <- SN13 |> filter(flag_snowdepth_median == "n") |>
  select("date", "snowdepth_median") |> rename(Snowdepth13 = snowdepth_median)
sensor14 <- SN14 |> filter(flag_snowdepth_median == "n") |>
  select("date", "snowdepth_median") |> rename(Snowdepth14 = snowdepth_median)
sensor15 <- SN15 |> filter(flag_snowdepth_median == "n") |>
  select("date", "snowdepth_median") |> rename(Snowdepth15 = snowdepth_median)
sensor16 <- SN16 |> filter(flag_snowdepth_median == "n") |>
  select("date", "snowdepth_median") |> rename(Snowdepth16 = snowdepth_median)
sensor17 <- SN17 |> filter(flag_snowdepth_median == "n") |>
  select("date", "snowdepth_median") |> rename(Snowdepth17 = snowdepth_median)
sensor19 <- SN19 |> filter(flag_snowdepth_median == "n") |>
  select("date", "snowdepth_median") |> rename(Snowdepth19 = snowdepth_median)
sensor20 <- SN20 |> filter(flag_snowdepth_median == "n") |>
  select("date", "snowdepth_median") |> rename(Snowdepth20 = snowdepth_median)
sensor21 <- SN21 |> filter(flag_snowdepth_median == "n") |>
  select("date", "snowdepth_median") |> rename(Snowdepth21 = snowdepth_median)

#use full join to combine all the median snow depth and date time info into one dataframe
#i get a many to many relationship warning when doing this full-join: i think this is expected but need to double check
allSN <- sensor1 |> 
  full_join(sensor6, by = "date") |> 
  full_join(sensor7, by = "date") |>
  full_join(sensor8, by = "date") |>
  full_join(sensor9, by = "date") |>
  full_join(sensor10, by = "date") |>
  full_join(sensor11, by = "date") |>
  full_join(sensor12, by = "date") |>
  full_join(sensor13, by = "date") |>
  full_join(sensor14, by = "date") |>
  full_join(sensor15, by = "date") |>
  full_join(sensor16, by = "date") |>
  full_join(sensor17, by = "date") |>
  full_join(sensor19, by = "date") |>
  full_join(sensor20, by = "date") |>
  full_join(sensor21, by = "date") |> 
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))

#plotting time! using ggpairs to plot sensor snowdepth measurements against each other
#because there are 16 sensors, the 256 panel graph is not really useful
#ggpairs(allSN, columns = 2:17) 
# so first, used another function called GGally to show correlation coefficients
ggcorr(allSN[, 2:17], label = TRUE, label_size = 4, hjust = .5, size = 2)
# now use ggpairs for the 4 sensors with the highest correlation: 6,12,15,16
# COMMENTED OUT FOR NOW: need to make this more efficient before running because it took a long time
#ggpairs(allSN, columns = c(3, 9, 12, 13))
