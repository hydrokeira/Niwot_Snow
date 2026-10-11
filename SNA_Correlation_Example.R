require(lubridate)

#read in data and clean
setwd("/Users/keirajohnson/Library/CloudStorage/OneDrive-UCB-O365/Keira_Johnson_CU/Niwot/SNA_NWT")

site1<-read.csv("sn_16_tenminute.jm.data.csv")

site1<-site1 %>%
  dplyr::select(date, snowdepth_median, flag_snowdepth_median) %>%
  filter(flag_snowdepth_median=="n")

colnames(site1)[2]<-"site1"

site2<-read.csv("sn_06_tenminute.jm.data.csv")

site2<-site2 %>%
  select(date, snowdepth_median,flag_snowdepth_median) %>%
  filter(flag_snowdepth_median=="n")

colnames(site2)[2]<-"site2"

#combine data, convert date to date, take average by hour across sites
sites_combined<-site1 %>%
  left_join(site2) %>%
  filter(!is.na(site2)) %>%
  filter(!is.na(site1)) %>%
  filter(site2 > 0 & site1 > 0) %>%
  mutate(date=as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate(date = lubridate::floor_date(date, "hour")) %>%
  mutate(date_notime = as.Date(date)) %>%
  group_by(date, date_notime) %>%
  summarise(site1 = mean(site1, na.rm = TRUE),
            site2 = mean(site2, na.rm = TRUE))


#plot
p1<-ggplot(sites_combined, aes(site1, site2))+geom_point(aes(col=get_waterYearDay(date_notime)))+facet_wrap(~get_waterYear(date_notime))+
  scale_color_viridis_c(option = "F")+theme_bw()+labs(col="Day of WY")+ggtitle("Hourly")

p2<-ggplot(sites_combined, aes(site1, site2))+geom_point(aes(col=get_waterYearDay(date_notime)))+facet_wrap(~get_waterYear(date_notime))+
  scale_color_viridis_c(option = "F")+theme_bw()+labs(col="Day of WY")+ggtitle("Daily")

ggarrange(p1, p2, nrow = 2)

#some example code to get you started on interpolating
model1<-lm(site1~site2, data = sites_combined)

summary(model1)

#use predict function to interpolate all missing dates
predict(model1)

