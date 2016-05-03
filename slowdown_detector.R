library(dplyr)
library(ggplot2)
library(lubridate)
library(grid)
library(gridExtra)

# First we read in the travel times csv output by the other script
travel_times <- read.csv(gzfile("train_travel_times.csv.gz"), as.is = TRUE)
headway_times <- read.csv(gzfile("train_headway_times.csv.gz"), as.is = TRUE)

# Do some cleanup of field types
travel_times$X <- NULL
travel_times$direction <- as.character(travel_times$direction)
travel_times$from_stop <- as.character(travel_times$from_stop)
travel_times$to_stop <- as.character(travel_times$to_stop)
travel_times$dep_dt <- as.POSIXct(travel_times$dep_dt, tz="EST")
travel_times$arr_dt <- as.POSIXct(travel_times$arr_dt, tz="EST")
travel_times$dep_d <- as.POSIXct(travel_times$dep_d, tz="EST")
travel_times$arr_d <- as.POSIXct(travel_times$arr_d, tz="EST")

headway_times$X <- NULL
headway_times$direction <- as.character(headway_times$direction)
headway_times$from_stop <- as.character(headway_times$from_stop)
headway_times$to_stop <- as.character(headway_times$to_stop)
headway_times$current_dep_dt <- as.POSIXct(headway_times$current_dep_dt, tz="EST")
headway_times$previous_dep_dt <- as.POSIXct(headway_times$previous_dep_dt, tz="EST")
headway_times$current_dep_d <- as.POSIXct(headway_times$current_dep_d, tz="EST")
headway_times$previous_dep_d <- as.POSIXct(headway_times$previous_dep_d, tz="EST")

# Create a list of all stop pairs; the Route info does not cotain sufficient information separate the Ashmont (a) and
# Braintree (b) lines. Digging this out of the schedule archive is possible but would have taken longer. Start with Southbound...
south_main <- data.frame(
  stop_id = c("70061","70063","70065","70067","70069","70071","70073","70075","70077","70079","70081"),
  next_stop = c("70063","70065","70067","70069","70071","70073","70075","70077","70079","70081","70083"))
south_a <- data.frame(
  stop_id = c("70083","70085","70087","70089","70091"),
  next_stop = c("70085","70087","70089","70091","70093"))
south_b <- data.frame(
  stop_id = c("70083","70095","70097","70099","70101","70103"),
  next_stop = c("70095","70097","70099","70101","70103","70105"))

# Repeat for Northbound...
north_main <- data.frame(
  stop_id = c("70084","70082","70080","70078","70076","70074","70072","70070","70068","70066","70064"),
  next_stop = c("70082","70080","70078","70076","70074","70072","70070","70068","70066","70064","70061"))
north_a <- data.frame(
  stop_id = c("70094","70092","70090","70088","70086"),
  next_stop = c("70092","70090","70088","70086","70084"))
north_b <- data.frame(
  stop_id = c("70105","70104","70102","70100","70098","70096"),
  next_stop = c("70104","70102","70100","70098","70096","70084"))

# Then rbind everything together.
distinct_stop_pairs <- rbind(south_main, south_a, south_b, north_main, north_a, north_b)

# Add a column for the "effective service date", so that late-night trains can be easily
# counted as part of the previous day.
travel_times$dt <- as.POSIXct(trunc(travel_times$dep_dt - seconds(14400), units = "days"))
headway_times$dt <- as.POSIXct(trunc(headway_times$current_dep_dt - seconds(14400), units = "days"))

travel_times$is_weekend <- as.POSIXlt(travel_times$dt)$wday %in% c(0,6)
headway_times$is_weekend <- as.POSIXlt(headway_times$dt)$wday %in% c(0,6)

# Add a column that combines both stop and heading.
travel_times$departing <- paste(travel_times$from_stop, travel_times$direction, sep="_")
headway_times$departing <- paste(headway_times$from_stop, headway_times$direction, sep="_")

# Add a column for how far off of benchmark each value is. In theory slowdowns are transferred
# through the system linearly so one minute of delay is one minute of delay.

travel_times$time_delta <- travel_times$travel_time_sec - travel_times$benchmark_travel_time_sec
headway_times$time_delta <- headway_times$headway_time_sec - headway_times$benchmark_headway_time_sec

# A simple scatter plot of train departures from Porter Square heading inbound over time, to
# demonstrate what the data looks like.
plot(as.numeric(travel_times[travel_times$from_stop == "70065",]$dep_dt - travel_times[travel_times$from_stop == "70065",]$dt,unit = 'mins'),
     travel_times[travel_times$from_stop == "70065",]$dt,
     main="train departures from Porter Square heading inbound",
     xlab="Minutes since midnight", ylab = "Date of Trip", pch=".")
# Simple plot of wait times for weekday inbound trains. If all trains travel the same
# route, in a perfect clockwork world, wait times should be uniform at all stations
# (i.e., trains should arrive at every station at the same rate they are entered into
# service, off-set uniformly by the travel times between stations). Of course, anyone 
# who has ever taken a train knows the world isn't clockwork, so let's see what kind
# interesting patterns emerge.

stop_sequence_0 <- RedLineRoute$stop[[1]] %>%
  filter(stop_id %in% unique(travel_times$from_stop)) %>%
  arrange(as.integer(stop_order)) %>%
  mutate(stop_seq = row_number(), heading = "Southbound") %>%
  select (stop_id, stop_name, parent_station_name, heading, stop_seq)

stop_sequence_1 <- RedLineRoute$stop[[2]] %>%
  filter(stop_id %in% unique(travel_times$from_stop)) %>%
  arrange(as.integer(stop_order)) %>%
  mutate(stop_seq = row_number(), heading = "Northbound") %>%
  select (stop_id, stop_name, parent_station_name, heading, stop_seq)

stop_sequence <- rbind(stop_sequence_0, stop_sequence_1)
rm(stop_sequence_0, stop_sequence_1)

headway_times <- headway_times %>%
  mutate(dep_time = current_dep_dt - dt) %>%
  left_join(.,stop_sequence, by=c("from_stop" = "stop_id"))

travel_times <- travel_times %>%
  mutate(dep_time = dep_dt - dt) %>%
  left_join(.,stop_sequence, by=c("from_stop" = "stop_id"))

# Plot of times between trains by time of day, weekday Southbound
ggplot(headway_times %>% filter(is_weekend == FALSE, direction == "0"), aes(x = as.numeric(dep_time, units="hours"), y = time_delta)) +
  geom_point(alpha=0.1) +
  geom_density2d() +
  xlab("Hours (24+ is past midnight)") +
  ylab("Seconds from Benchmark") +
  ggtitle("Times between trains by time of day, weekday Southbound")

# Boxplot of times between trains by station, weekday Southbound.
boxplot(time_delta~stop_seq,
     data = headway_times %>% filter(direction == "0"),
     main="times between trains by station, weekday Southbound",
     xlab="Stop Sequence", ylab = "wait time (seconds)", pch=".", ylim=c(-800, 1600))

# density plot of times between trains by station.
headway_plots <- list()
headway_plots[[1]] <- ggplot(headway_times %>% filter(direction == "0", is_weekend == FALSE), aes(x=time_delta)) +
  geom_density(aes(group = stop_name, colour= stop_name, fill= stop_name), alpha=0.3)

headway_plots[[2]] <- ggplot(headway_times %>% filter(direction == "1", is_weekend == FALSE), aes(x=time_delta)) +
  geom_density(aes(group = stop_name, colour= stop_name, fill= stop_name), alpha=0.3)

headway_plots[[3]] <- ggplot(headway_times %>% filter(direction == "0", is_weekend == FALSE), aes(x=time_delta)) +
  geom_density(aes(group = stop_name, colour= stop_name, fill= stop_name), alpha=0.3)

headway_plots[[4]] <- ggplot(headway_times %>% filter(direction == "1", is_weekend == FALSE), aes(x=time_delta)) +
  geom_density(aes(group = stop_name, colour= stop_name, fill= stop_name), alpha=0.3)

grid.arrange(headway_plots[[1]], headway_plots[[2]],headway_plots[[3]],headway_plots[[4]], ncol=2, left="Type of Day")
grid.rect(gp=gpar(fill=NA, col="gray"))

# And a simple plot of the densities of travel times for Northbound trains of weekdays, all times of day.
ggplot((train_travels %>% filter(direction == 1, is_weekend == FALSE)), aes(x=travel_time_sec)) +
  geom_density(aes(group=departing, colour=departing, fill=departing), alpha=0.3) +
  ggtitle("plot of the densities of travel times for Northbound trains of weekdays") +
  xlim(0, 200) + ylim(0, .25)
