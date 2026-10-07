

#use Keira's code to create compiled dataframe of snowdepth data comtaining only:
#1) data with flag 'n', 2)snowdepth_median values, 3) date (positx type)

library(tidyverse)
library(dplyr)

setwd("C:\\Users\\iameb\\OneDrive - UCB-O365\\Eva DLA Folder\\SNA\\SNA 10 minute data")

files_list<-list.files(path = ".", pattern = "sn_")

sn_snow<-list()

for (i in 1:length(files_list)) {
  
  print(i)
  
  sn<-read.csv(files_list[i])
  
  sn$date<-as.POSIXct(sn$date)
  
  sn_snow[[i]]<-sn %>%
    filter(flag_snowdepth_median=="n", !is.na(snowdepth_median)) %>% #filter out NaN and only include n
    dplyr::select(date, snowdepth_median) %>%
    mutate(sn=files_list[i])
  
}

sn_all<-do.call(bind_rows, sn_snow)

write.csv(sn_all, "All_SNA_compiled.csv") #csv with all sensor data


