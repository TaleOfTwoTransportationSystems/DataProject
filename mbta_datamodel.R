library(dplyr)
library(jsonlite)
library(lubridate)
library(readr)

# https://developers.google.com/transit/gtfs/reference#feed-files
# https://transitfeeds.com/p/mbta/91

# API information for the real-time feed
TKeyJeff <- "?api_key=tNprgYF1K027zEIVluAkCw"
#TKeyAPIDoc <- "?api_key=wX9NwuHnZU2ToO7GmGR9uw"
TRouteURL <- "http://realtime.mbta.com/developer/api/v2/stopsbyroute"
TTravelURL <- "http://realtime.mbta.com/developer/api/v2.1/traveltimes"
TFormat <- "&format=json"
startTime <- as.POSIXct("2016-01-25 04:00:00")

# Nice list of stations for mapping, etc.
RedLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Red", sep=""))[[1]]
MattapanLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Mattapan", sep=""))[[1]]

# Routes we're not using yet
#GreenBLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Green-B", sep=""))[[1]]
#GreenCLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Green-C", sep=""))[[1]]
#GreenDLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Green-D", sep=""))[[1]]
#GreenELineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Green-E", sep=""))[[1]]
#BlueLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Blue", sep=""))[[1]]
#OrangeLineRoute <- fromJSON(paste(TRouteURL, TKeyJeff, TFormat, "&route=Orange", sep=""))[[1]]

# Build a list of all stop pairs of interest.
red_south <-  RedLineRoute$stop[[1]] %>%
  select(stop_order, stop_id) %>%
  arrange(stop_order) %>%
  mutate(next_stop = lead(stop_id)) %>%
  select(stop_id, next_stop) %>%
  filter(!is.na(next_stop))

red_north <-  RedLineRoute$stop[[2]] %>%
  select(stop_order, stop_id) %>%
  arrange(stop_order) %>%
  mutate(next_stop = lead(stop_id)) %>%
  select(stop_id, next_stop) %>%
  filter(!is.na(next_stop))

matt_out <-  MattapanLineRoute$stop[[1]] %>%
  select(stop_order, stop_id) %>%
  arrange(stop_order) %>%
  mutate(next_stop = lead(stop_id)) %>%
  select(stop_id, next_stop) %>%
  filter(!is.na(next_stop))

matt_in <-  MattapanLineRoute$stop[[2]] %>%
  select(stop_order, stop_id) %>%
  arrange(stop_order) %>%
  mutate(next_stop = lead(stop_id)) %>%
  select(stop_id, next_stop) %>%
  filter(!is.na(next_stop))

distinct_stop_pairs <- rbind(red_north, red_south, matt_out, matt_in)
rm(matt_in, matt_out, red_north, red_south)

distinct_stop_pairs_full <- distinct_stop_pairs


# create a holding frame for the data; we do this outside the loops so that it will persist.
finished_dataset <- data.frame(direction=as.numeric(character()),
                    dep_dt=as.POSIXct(character()), 
                    arr_dt=as.POSIXct(character()), 
                    travel_time_sec=as.numeric(character()),
                    benchmark_travel_time_sec=as.numeric(character()),
                    from_stop=character(), 
                    to_stop=character()) 

# How many seven day periods from start to now?
numWeeks <- as.integer(unclass(now() - startTime)/7)

# The outer loop cycles through every distinct pair of stops.
for(j in 1:nrow(distinct_stop_pairs)) {
  from_j <- distinct_stop_pairs[j,]$stop_id
  to_j <- distinct_stop_pairs[j,]$next_stop
  fromStop <- paste("&from_stop=", from_j, sep="")
  toStop <- paste("&to_stop=", to_j, sep="")
  print(paste("Requesting", from_j, "to", to_j))
  
  # The inner loop cycles through each week of interest.  
  for(i in 0:numWeeks) {
    fromTime <- paste("&from_datetime=", as.numeric(startTime + days(i * 7)), sep="")
    toTime <- paste("&to_datetime=", as.numeric(startTime + days(i * 7) + days(7) - minutes(1)), sep="")
    TRequest <- paste(TTravelURL, TKeyJeff, TFormat, fromStop, toStop, fromTime, toTime, sep="")
    foo <- fromJSON(TRequest)[[1]]
    
    # Assuming we get a result back, we process it within the
    # inner loop, reformatting columns and dropping any we don't
    # plan to use. We then append it to finished_dataset.
    if (length(foo) > 0) {
      bar <- foo %>%
        mutate(from_stop = from_j,
          to_stop = to_j,
          dep_dt = as.POSIXct(as.integer(dep_dt), origin="1970-01-01"),
          arr_dt = as.POSIXct(as.integer(arr_dt), origin="1970-01-01"),
          travel_time_sec = as.numeric(travel_time_sec),
          benchmark_travel_time_sec = as.numeric(benchmark_travel_time_sec)) %>%
        select(-route_id, -contains("threshold"))
      #finished_dataset <- union(finished_dataset, bar) # this results in a long list of lists
      finished_dataset <- rbind(finished_dataset, bar)
    } else {
      print(paste("Nothing returned for", fromStop, "to", toStop, "during period", fromTime, "-", toTime))
    }
    Sys.sleep(2) #slow down a bit
  }
}

write.csv(finished_dataset, "train_travel_times.csv")

#finished_dataset <- read_csv("train_travel_times.csv") # can skip loops and load this instead

# adding a name to our unnamed first column -- perhaps this should be done when we first make the table?
names(finished_dataset)[1] <- "index"

# splitting date & time
# (note, we may want to shift the times ahead four hours so we don't need to deal with trains that travel after midnight)
finished_dataset <- mutate(finished_dataset, dep_d=as.Date(dep_dt), 
                                             dep_t=format(as.POSIXct(dep_dt), format="%H:%M:%S"), 
                                             arr_d=as.Date(arr_dt), 
                                             arr_t=format(as.POSIXct(arr_dt), format="%H:%M:%S"))
# adding parent_station_name, lat and lon
finished_dataset <- bind_rows(RedLineRoute$stop[1][[1]], RedLineRoute$stop[2][[1]]) %>% 
                    select(stop_id, parent_station_name, stop_lat, stop_lon) %>% 
                    mutate(stop_id=as.integer(stop_id)) %>% 
                    rename(to_stop = stop_id, to_name = parent_station_name, to_lat = stop_lat, to_lon = stop_lon) %>% 
                    inner_join(finished_dataset, by="to_stop")
finished_dataset <- bind_rows(RedLineRoute$stop[1][[1]], RedLineRoute$stop[2][[1]]) %>% 
                    select(stop_id, parent_station_name, stop_lat, stop_lon) %>% 
                    mutate(stop_id=as.integer(stop_id)) %>% 
                    rename(from_stop = stop_id, from_name = parent_station_name, from_lat = stop_lat, from_lon = stop_lon) %>% 
                    inner_join(finished_dataset, by="from_stop")
finished_dataset <- arrange(finished_dataset, direction, dep_dt)


# Get schedule data:
Tarchive_cal <- read_csv("~/Google Drive/CSCIe107/MBTA/20151211/calendar.txt")
Tarchive_trips <- read.csv("~/Google Drive/CSCIe107/MBTA/20151211/trips.txt")
Tarchive_stopTimes <- read.csv("~/Google Drive/CSCIe107/MBTA/20151211/stop_times.txt")
#Tarchive_calDates <- read_csv("~/Google Drive/CSCIe107/MBTA/20151211/calendar_dates.txt")
#Tarchive_stops <- read_csv("~/Google Drive/CSCIe107/MBTA/20151211/stops.txt")

# for a date in the finished_dataset, find matching date ranges in calendar.txt (filtering by service_ids starting with RTL)
# for matching date ranges, take the corresponding service_ids (will be multiple types of service per day)
# then in trips, get corresonding trip_ids (per line, and on a per-direction basis)
# finally, in stop_times.txt, get the arrival and departure times for the various stops for these trips.
# why does calendar_dates.txt have more entries than calendar.txt?

# OK, as a first step, let's trim back the Tarchive tables to just the subway (RTL) data:
Tarchive_cal <- filter(Tarchive_cal, grepl("RTL", service_id))
Tarchive_trips <- filter(Tarchive_trips, grepl("RTL", service_id))
Tarchive_stopTimes <- filter(Tarchive_stopTimes, trip_id %in% Tarchive_trips$trip_id)

# Oh, and we span 2 archives:
Tarchive_cal2 <- read_csv("~/Google Drive/CSCIe107/MBTA/20160309/calendar.txt")
Tarchive_trips2 <- read.csv("~/Google Drive/CSCIe107/MBTA/20160309/trips.txt")
Tarchive_stopTimes2 <- read.csv("~/Google Drive/CSCIe107/MBTA/20160309/stop_times.txt")

Tarchive_cal2 <- filter(Tarchive_cal2, grepl("RTL", service_id))
Tarchive_trips2 <- filter(Tarchive_trips2, grepl("RTL", service_id))
Tarchive_stopTimes2 <- filter(Tarchive_stopTimes2, trip_id %in% Tarchive_trips$trip_id)

Tarchive_cal <- bind_rows(Tarchive_cal, Tarchive_cal2)
Tarchive_trips <- bind_rows(Tarchive_trips, Tarchive_trips2)
Tarchive_stopTimes <- bind_rows(Tarchive_stopTimes, Tarchive_stopTimes2)

rm(Tarchive_cal2)
rm(Tarchive_trips2)
rm(Tarchive_stopTimes2)

# Convert the dates:
Tarchive_cal <- mutate(Tarchive_cal, start_date=as.Date(as.character(start_date), format="%Y%m%d", origin=), 
                                     end_date=as.Date(as.character(end_date), format="%Y%m%d"))

# Now, loop through each day between the start date and today,
# for each day, get the relevant service_ids and,
# find the corresponding trip_ids, then
# pull all the corresponding train arrival and departure times
for(i in 0:(as.integer(unclass(now() - startTime)))) {
    iDate <- as.Date(startTime+days(i))
    #service_ids
    todaysServices <- filter(Tarchive_cal, start_date<=iDate & end_date>=iDate) %>%
                      select(service_id)
    if(wday(iDate)==1) {
        #Sunday
        todaysServices <- filter(todaysServices, grepl("Sunday", service_id))
    } else if(wday(iDate)==7) {
        #Saturday
        todaysServices <- filter(todaysServices, grepl("Saturday", service_id))
    } else {
        todaysServices <- filter(todaysServices, grepl("Weekday", service_id))
    }
    
    #trip_ids
    todayTrips <- filter(Tarchive_trips, service_id %in% todaysServices$service_id) %>% filter(route_id=="Red")
    print(nrow(todayTrips))
    # stop_times
    
}
