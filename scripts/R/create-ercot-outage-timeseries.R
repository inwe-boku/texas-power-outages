library(tidyverse)
library(readxl)
library(lubridate)
library(feather)


outages_ercot<-read_xlsx("../../data/input/outages/Unit_Outage_Data_20210312.xlsx",sheet=2)
outages_edgar<-read_xlsx("../../data/input/outages/EGOVA-Database-Shareable.xlsx")

outages_ercot$START<-as.POSIXct(outages_ercot$START)
outages_ercot$END<-as.POSIXct(outages_ercot$END)

outages_edgar$`Outage Event Start`<-as.POSIXct(outages_edgar$`Outage Event Start`)
outages_edgar$`Outage Event End`<-as.POSIXct(outages_edgar$`Outage Event End`)


one_line<-function(line){
  
  ts<-seq(line$START[1],line$END[1], by="1 min")
  t<-tibble(ts, station=line$STATION[1], unit=line$`UNIT NAME`[1], reduction=line$`MW REDUCTION FROM OUTAGE/DERATE`[1], cap_available=line$`AVAILABLE MW AFTER OUTAGE/DERATE`[1], fuel=line$`FUEL TYPE`[1], cap_max=line$`SEASONAL MAX MW (HSL)`[1], Longitude=0, Latitude=0)
  return(t)
}

one_line_edgar<-function(line){
  
  ts<-seq(line$`Outage Event Start`[1],line$`Outage Event End`[1], by="1 min")
  t<-tibble(ts, station=line$`Original Station Name`[1], unit=line$`Unit ID`[1], reduction=line$`Capacity Reduction During Event (MW)`[1], cap_available=line$`Observed Capacity During Event (MW)`[1], fuel=line$`Fuel Type`[1], cap_max=line$`SARA Winter Rated Capacity \r\n(MW)`[1],Longitude=line$Longitude[1],Latitude=line$Latitude[1])
  return(t)
}

convert_to_sequential_table<-function(tab, get_one_line_function=one_line){

l<-list()
for(i in 1:nrow(tab)){

  print(paste0("Line: ", i))
  lines <- get_one_line_function(tab[i,])
  l[[i]]<-lines
  
}

res<-bind_rows(l) %>% 
  mutate(fullname=paste(station,unit,sep=".")) %>% 
  arrange(fullname,ts)

return(res)
}

outages_ercot_sequential<-convert_to_sequential_table(outages_ercot) %>% mutate(dataset="ercot")
outages_edgar_sequential<-convert_to_sequential_table(outages_edgar, one_line_edgar) %>% 
  mutate(dataset="edgar") %>% 
  mutate(fuel=ifelse(fuel == "Wind", "WIND", fuel)) %>% 
  mutate(fuel=ifelse(fuel=="Coal", "COAL", fuel)) %>% 
  mutate(fuel=ifelse(fuel=="Energy Storage", "ESR", fuel)) %>% 
  mutate(fuel=ifelse(fuel=="Hydro", "HYDRO", fuel)) %>% 
  mutate(fuel=ifelse(fuel=="Natural Gas", "NG", fuel)) %>% 
  mutate(fuel=ifelse(fuel=="Nuclear", "NUCLEAR", fuel)) %>% 
  mutate(fuel=ifelse(fuel=="Solar", "SOLAR", fuel))

outages_all<-bind_rows(outages_ercot_sequential,
                       outages_edgar_sequential)


outages_all %>% write_feather("../../data/interim/outages/outages.feather")

outages_all_floor <- outages_all %>% as_tibble() %>% filter(month(ts)<3) %>%   
  mutate(hourly_timestamp = floor_date(ts, unit = "hour")) 

outages_hourly_aggregate <- outages_all_floor  %>% 
  group_by(hourly_timestamp,station,unit,fuel,fullname,dataset,Longitude,Latitude) %>% 
  summarize(reduction=mean(reduction),
            cap_available=mean(cap_available),
            cap_max=mean(cap_max)) 

outages_hourly_aggregate %>% write_feather("../../data/interim/outages/outages-hourly.feather")
