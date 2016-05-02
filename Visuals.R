<<<<<<< HEAD
install.packages("rworldmap")
install.packages(c("maps", "mapproj"))
install.packages("leaflet")
install.packages("dygraphs")
require(ggmap)
=======
# install.packages("rworldmap")
# install.packages(c("maps", "mapproj"))
# install.packages("leaflet")
# install.packages("dygraphs")
>>>>>>> 4e18ed12f216adf1985cd9309d16b44f61f442f5
require(dygraphs)
require(leaflet)
require(readr)
require(ggplot2)
require(tidyr)
require(dplyr)
require(pander)
require(knitr)
require(maps)
require(mapproj)
require(rworldmap)
require(lubridate)
library(ggmap)
library(stringr)
#http://yihui.name/en/2014/07/library-vs-require/

#Get data
load("fetchMbtaData.RData")
allstops<-stops %>% select(stop_code, stop_name, stop_lat, stop_lon) %>% 
  filter(is.na(stop_code)==FALSE) %>% mutate(from_stop=stop_code,to_stop=stop_code)
finished_dataset$from_stop <- as.integer(finished_dataset$from_stop)
finished_dataset$to_stop <- as.integer(finished_dataset$to_stop)

#Merge latitude and longitude values
dataset<-finished_dataset %>% 
  left_join(allstops,by="from_stop") %>% select(-stop_code,-to_stop.y)
colnames(dataset)[7]<-"to_stop"
colnames(dataset)[11]<-"start_stop"
colnames(dataset)[12]<-"start_lat"
colnames(dataset)[13]<-"start_long"

dataset<-dataset %>% 
  left_join(allstops,by="to_stop") 
dataset<-dataset %>% select(-stop_code,-from_stop.y)
colnames(dataset)[14]<-"end_stop"
colnames(dataset)[15]<-"end_lat"
colnames(dataset)[16]<-"end_long"
colnames(dataset)[6]<-"from_stop"

t.lub <- ymd_hms(dataset$dep_dt)
dataset$time <- round((hour(t.lub) + minute(t.lub)/60), digits=2)
fulldata<-dataset

#Weather data
weather<-read.csv("weather.csv")
weather<-weather %>% 
  filter(DATE>20160124,STATION_NAME=="BOSTON LOGAN INTERNATIONAL AIRPORT MA US") %>%
  select(DATE,precipitation,snowfall) 
colnames(weather)[1]<-"date"
dataset<-dataset %>% left_join(weather,by="date")
write.csv(dataset,file="map_dataset.csv")
fulldata<-dataset

#RedLine
Red <-  RedLineRoute$stop[[1]] %>%
  select(stop_order, stop_id, stop_name, stop_lat, stop_lon)
Red$stop_lat<-as.numeric(Red$stop_lat)
Red$stop_lon<-as.numeric(Red$stop_lon)
Red<-Red %>% mutate(line="Red")

#GreenBLine
GreenB <-  GreenBLineRoute$stop[[1]] %>%
  select(stop_order, stop_id, stop_name, stop_lat, stop_lon)
GreenB$stop_lat<-as.numeric(GreenB$stop_lat)
GreenB$stop_lon<-as.numeric(GreenB$stop_lon)
GreenB<-GreenB %>% mutate(line="GreenB")

#GreenCLine
GreenC <-  GreenCLineRoute$stop[[1]] %>%
  select(stop_order, stop_id, stop_name, stop_lat, stop_lon)
GreenC$stop_lat<-as.numeric(GreenC$stop_lat)
GreenC$stop_lon<-as.numeric(GreenC$stop_lon)
GreenC<-GreenC %>% mutate(line="GreenC")

#GreenDLine
GreenD <-  GreenDLineRoute$stop[[1]] %>%
  select(stop_order, stop_id, stop_name, stop_lat, stop_lon)
GreenD$stop_lat<-as.numeric(GreenD$stop_lat)
GreenD$stop_lon<-as.numeric(GreenD$stop_lon)
GreenD<-GreenD %>% mutate(line="GreenD")

#GreenELine
GreenE <-  GreenELineRoute$stop[[1]] %>%
  select(stop_order, stop_id, stop_name, stop_lat, stop_lon)
GreenE$stop_lat<-as.numeric(GreenE$stop_lat)
GreenE$stop_lon<-as.numeric(GreenE$stop_lon)
GreenE<-GreenE %>% mutate(line="GreenE")

#OrangeLine
Orange <-  OrangeLineRoute$stop[[1]] %>%
  select(stop_order, stop_id, stop_name, stop_lat, stop_lon)
Orange$stop_lat<-as.numeric(Orange$stop_lat)
Orange$stop_lon<-as.numeric(Orange$stop_lon)
Orange<-Orange %>% mutate(line="Orange")

#BlueLine
Blue <-  BlueLineRoute$stop[[1]] %>%
  select(stop_order, stop_id, stop_name, stop_lat, stop_lon)
Blue$stop_lat<-as.numeric(Blue$stop_lat)
Blue$stop_lon<-as.numeric(Blue$stop_lon)
Blue<-Blue %>% mutate(line="Blue")

all_lines<-rbind(Red,GreenB,GreenC,GreenD,GreenE,Orange,Blue)
all_lines$stop_lat<-as.numeric(all_lines$stop_lat)
all_lines$stop_lon<-as.numeric(all_lines$stop_lon)

all_lines <- all_lines %>% 
  separate(stop_name, into = c("stop_name", "extra"), sep="-", fill="right") %>%
  select(-extra)
write.csv(all_lines,file="T_lines.csv")

#Map of Boston
lon_range <- extendrange(dataset$end_long)
lat_range <- extendrange(dataset$end_lat)
calc_zoom(lon_range, lat_range)

gc <- geocode("boston massachusetts")
map <- get_map(gc,maptype = "toner-lite",zoom=12)

ggmap(map)+
  geom_path(data=Red,aes(x=stop_lon,y=stop_lat),color="red",size=1)+
  geom_path(data=GreenB,aes(x=stop_lon,y=stop_lat),color="green",size=1)+
  geom_path(data=GreenC,aes(x=stop_lon,y=stop_lat),color="green",size=1)+
  geom_path(data=GreenD,aes(x=stop_lon,y=stop_lat),color="green",size=1)+
  geom_path(data=GreenE,aes(x=stop_lon,y=stop_lat),color="green",size=1)+
  geom_path(data=Orange,aes(x=stop_lon,y=stop_lat),color="orange",size=1)+
  geom_path(data=Blue,aes(x=stop_lon,y=stop_lat),color="blue",size=1)+
  geom_point(data=Red,aes(x=stop_lon,y=stop_lat),color="black",size=0.5)+
  geom_point(data=GreenB,aes(x=stop_lon,y=stop_lat),color="black",size=0.5)+
  geom_point(data=GreenC,aes(x=stop_lon,y=stop_lat),color="black",size=0.5)+
  geom_point(data=GreenD,aes(x=stop_lon,y=stop_lat),color="black",size=0.5)+
  geom_point(data=GreenE,aes(x=stop_lon,y=stop_lat),color="black",size=0.5)+
  geom_point(data=Orange,aes(x=stop_lon,y=stop_lat),color="black",size=0.5)+
  geom_point(data=Blue,aes(x=stop_lon,y=stop_lat),color="black",size=0.5)+
  theme(axis.ticks = element_blank(), axis.text = element_blank(),axis.title=element_blank())
     
#Using Leaflet
redline<-all_lines %>% filter(line=="Red")
greenBline<-all_lines %>% filter(line=="GreenB")
greenCline<-all_lines %>% filter(line=="GreenC")
greenDline<-all_lines %>% filter(line=="GreenD")
greenEline<-all_lines %>% filter(line=="GreenE")
orangeline<-all_lines %>% filter(line=="Orange")
blueline<-all_lines %>% filter(line=="Blue")

boston <- leaflet() %>% 
  setView(lng = -71.0589, lat = 42.3601, zoom = 12) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolylines(data=redline,~stop_lon,~stop_lat,color="red") %>%
  addPolylines(data=greenBline,~stop_lon,~stop_lat,color="springgreen") %>%
  addPolylines(data=greenCline,~stop_lon,~stop_lat,color="palegreen") %>%
  addPolylines(data=greenDline,~stop_lon,~stop_lat,color="limegreen") %>%
  addPolylines(data=greenEline,~stop_lon,~stop_lat,color="green") %>%
  addPolylines(data=orangeline,~stop_lon,~stop_lat,color="orange") %>%
  addPolylines(data=blueline,~stop_lon,~stop_lat,color="blue") %>%
  addCircleMarkers(data=redline, ~stop_lon,~stop_lat,color="red",radius=1,popup=~stop_name) %>%
  addCircleMarkers(data=greenBline,~stop_lon,~stop_lat,color="springgreen",radius=1,popup=~stop_name) %>%
  addCircleMarkers(data=greenCline,~stop_lon,~stop_lat,color="palegreen",radius=1,popup=~stop_name) %>%
  addCircleMarkers(data=greenDline,~stop_lon,~stop_lat,color="limegreen",radius=1,popup=~stop_name) %>%
  addCircleMarkers(data=greenEline,~stop_lon,~stop_lat,color="green",radius=1,popup=~stop_name) %>%
  addCircleMarkers(data=orangeline,~stop_lon,~stop_lat,color="orange",radius=1,popup=~stop_name) %>%
  addCircleMarkers(data=blueline,~stop_lon,~stop_lat,color="blue",radius=1,popup=~stop_name)
boston

#Average travel times between stops
traveltime<fulldata %>% group_by(start_stop,end_stop) %>%
  summarize(avg_traveltime=mean(travel_time_sec))

betweenstops<-fulldata %>% left_join(traveltime,by=c("start_stop","end_stop")) %>%
  mutate(residualtime=avg_traveltime-travel_time_sec)
  
travel<-betweenstops %>% 
  group_by(c(start_stop,end_stop),time) %>%
  summarize(avgresid=mean(residualtime))

#Delay vs. hour of day plot, input using start and stop_codes
traveltimes_day<-function(fromstation,tostation){
travel<-betweenstops %>% 
  filter(start_stop==fromstation & end_stop==tostation) %>%
  group_by(time) %>% summarize(mean(residualtime))
dygraph(travel) %>% 
  dyAxis("y", label = "Delay in Seconds") %>% 
  dyAxis("x", label="Hour of Day") %>% dyRangeSelector()}

#Example Between MGH and MIT
traveltimes_day("Charles/MGH - Outbound","Kendall/MIT - Outbound")

#Tweets
train_travel_times<-read.csv("/Users/Admin/Documents/FinalProject/DataProject/train_travel_times.csv",stringsAsFactors = FALSE)
rtweets<-read.csv("/Users/Admin/Documents/FinalProject/DataProject/red_line_tweets_enhanced.csv",stringsAsFactors = FALSE)
train_travel_times$arr_dt<-ymd_hms(train_travel_times$arr_dt)
train_travel_times$dep_dt<-ymd_hms(train_travel_times$dep_dt)
rtweets$arr_dt<-ymd_hms(rtweets$arr_dt)
train_travel_times <- train_travel_times %>% full_join(rtweets) %>% 
  filter(is.na(severity)==FALSE)

#Station of concern from alert
regexp <- "[[:digit:]]+"
train_travel_times$stop_code<-as.integer(str_extract(train_travel_times$alerts_at_station_code, regexp))

#Get latitude and longitude values
train_travel_times<-train_travel_times %>% left_join(allstops,by="stop_code") %>%
  select(dep_dt,arr_dt,severity,stop_code,stop_lat,stop_lon) %>%
  filter(is.na(stop_lat)==FALSE) %>% filter(is.na(stop_lon)==FALSE)

#Red line severity of alerts
leaflet(data = train_travel_times) %>% addTiles() %>% addProviderTiles("CartoDB.Positron") %>%
  addPolylines(data=redline,~stop_lon,~stop_lat,color="red") %>%
  addCircleMarkers(data=redline, ~stop_lon,~stop_lat,color="red",radius=1,popup=~stop_name) %>%
  addMarkers(~stop_lon, ~stop_lat, clusterOptions = markerClusterOptions())

