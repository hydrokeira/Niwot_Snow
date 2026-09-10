setwd("/Users/keirajohnson/Library/CloudStorage/OneDrive-UCB-O365/Keira_Johnson_CU/Niwot/SNA_NWT")

files_list<-list.files(path = ".", pattern = "sn_")

sn_snow<-list()

for (i in 1:length(files_list)) {
  
  print(i)
  
  sn<-read.csv(files_list[i])
  
  sn$date<-as.POSIXct(sn$date)
  
  sn_snow[[i]]<-sn %>%
    filter(flag_snowdepth_median=="n") %>%
    dplyr::select(date, snowdepth_median) %>%
    mutate(sn=files_list[i])
  
}

sn_all<-do.call(bind_rows, sn_snow)

write.csv(sn_all, "All_SNA_compiled.csv")

pdf("SNA_plotted_faceted.pdf", width = 14, height = 8)

ggplot(sn_all, aes(date, snowdepth_median))+geom_line(aes(colour = sn))+theme_classic()+
  xlim(as.POSIXct("2020-01-01"), as.POSIXct("2025-10-01"))+facet_wrap(~sn, scales="free_y")

dev.off()

sn_1 %>%
  filter(flag_snowdepth_median=="n") %>%
  ggplot(aes(date, snowdepth_median))+geom_line()
