#presence absence plot
#to identify gaps in sna data
#uses HOURLY data because gapfilling will be done with hourly data

##future improvement: facet wrap by water year
#    mutate(date_notime=as.POSIXct(date_notime, format = "%Y-%m-%d")) #for facet wrap


#load libraries
library(tidyverse)
library(dplyr)
library(purrr) #to use map_dfr

#load data (aggregated hourly data, with NaN and negative and non "n" flagged values filtered out)
setwd("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\SNA 10 minute data")

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

#combine dataframes into a list
datasets <-  list("Site 1" =  site1, "Site 6" = site6, "Site 7" = site7, "Site 8" = site8, "Site 9" = site9,"Site 10" = site10, "Site 11" = site11,
                          "Site 12" = site12, "Site 13" = site13, "Site 14" = site14, "Site 15" = site15, "Site 16" = site16, "Site 17" = site17,
                          "Site 19" = site19, "Site 20" = site20, "Site 21" = site21)
time_col <- "date"
value_col = "site1"

get_segments <- function(df, name){ #function takes in a dataframe and returns a table with one row per continuous stretch of data
  #AI note: I used claude to help make this function
  value_col <- setdiff(names(df), time_col)[3] #set value column to the second non date column (site1, site2, etc.)
  df |> 
    transmute(t = as.POSIXct(.data[[time_col]], format = "%Y-%m-%d %H:%M:%S")) |> #atp. only time stamps matter to know at what times there is data
    arrange(t) |> #sort by time (should be in order already but just to make sure)
    mutate( #add a col with # of hrs since previous reading
      step = as.numeric(difftime(t, lag(t), units = "hours")),
      new_seg = is.na(step) | step > 1.5, #T wheneber a new continuous stretch begins (either no previous value or a jump greater than 1.5 hours)
      seg_id = cumsum(new_seg) #running total so each continuous stretch has its own ID number
    ) |> 
    group_by(seg_id) |> 
    summarise(start = min(t), end = max(t) +3600,
               .groups = "drop") |> #the +3600 adds one hour so each reading covers a full hour
    mutate(sensor = value_col) #add sensor column with the value columns name to label each continuous segment with the associated sensor
}

segments <- map_dfr(datasets, get_segments) #runs get_segments function on each df in the list and combines resulting tabels into 1 df
segments$sensor <- factor(segments$sensor, levels = unique(segments$sensor)) #converts sensor to a factor

ggplot(segments) + 
  geom_rect(aes(xmin = start, xmax = end,
                ymin = as.numeric(sensor) - 0.1, 
                ymax = as.numeric(sensor) + 0.1)) + 
  facet_wrap(~get_waterYear(date_notime), ncol = 4)
  scale_y_continuous(breaks = seq_along(levels(segments$sensor)), 
                     labels = levels(segments$sensor)) + 
  labs(x = "Time", y = "Sensor") + 
  theme_minimal()

#improvements: facet wrap by water year to get a plot like this for each water year so it is easier to read the


#graph data (first attempt)
#site1 |> 
 # ggplot(aes(date, site1)) + 
  #geom_point(shape = "-", size = 1) + 
  #theme_minimal()
