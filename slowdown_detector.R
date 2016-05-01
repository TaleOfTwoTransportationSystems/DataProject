library(dplyr)
library(ggplot2)
library(lubridate)

travel_times <- read.csv("train_travel_times.csv")
travel_times$X <- NULL
travel_times$direction <- as.character(travel_times$direction)
travel_times$from_stop <- as.character(travel_times$from_stop)
travel_times$to_stop <- as.character(travel_times$to_stop)
travel_times$dep_dt <- as.POSIXct(travel_times$dep_dt)
travel_times$arr_dt <- as.POSIXct(travel_times$arr_dt)
travel_times$dt <- (travel_times$dep_dt - seconds(14400))

train_gaps <- travel_times %>%
  mutate(departing = paste(from_stop, direction, sep="_")) %>%
  select(dt, departing, from_stop, direction, dep_dt) %>%
  arrange(dt, from_stop, direction, dep_dt) %>%
  group_by(from_stop, direction, departing, dt) %>%
  mutate(time_since_last = lag(dep_dt)) %>%
  mutate(between_trains = as.numeric(dep_dt - time_since_last, unit = 'secs')) %>%
  select(dt, from_stop, direction, departing, dep_dt, between_trains)

train_travels <- travel_times %>%
  mutate(departing = paste(from_stop, direction, sep="_")) %>%
  select(dt, departing, from_stop, direction, dep_dt, travel_time_sec)

plot(as.numeric(travel_times$dep_dt - as.POSIXct(travel_times$dt), unit = 'secs'), travel_times$from_stop)

ggplot(train_gaps, aes(x=between_trains)) +
  geom_density(aes(group=departing, colour=departing, fill=departing), alpha=0.3) +
  xlim(0, 3000)

ggplot(train_travels, aes(x=travel_time_sec)) +
  geom_density(aes(group=departing, colour=departing, fill=departing), alpha=0.3) +
  xlim(0, 200)
