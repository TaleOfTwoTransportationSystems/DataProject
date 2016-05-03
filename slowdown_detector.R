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

# Create a simple ordered list of stops so simple charts can have a sense of order will
# still have odd behavior around the Red Line's fork, but more advanced visualizations
# will take this into account.

stop_sequence <- rbind(
# Southbound
  RedLineRoute$stop[[1]] %>%
  arrange(as.integer(stop_order)) %>%
  mutate(stop_seq = row_number(), heading = "Southbound") %>%
  select (stop_id, stop_name, parent_station_name, heading, stop_seq),
# Northbound
  RedLineRoute$stop[[2]] %>%
  arrange(as.integer(stop_order)) %>%
  mutate(stop_seq = row_number(), heading = "Northbound") %>%
  select (stop_id, stop_name, parent_station_name, heading, stop_seq))

headway_times <- headway_times %>%
  mutate(dep_time = current_dep_dt - dt) %>%
  left_join(.,stop_sequence, by=c("from_stop" = "stop_id"))

travel_times <- travel_times %>%
  mutate(dep_time = dep_dt - dt) %>%
  left_join(.,stop_sequence, by=c("from_stop" = "stop_id"))

# Plot of times between trains by time of day, weekday Southbound
ggplot(headway_times %>% filter(is_weekend == FALSE, direction == "0"), aes(x = as.numeric(dep_time, units="hours"), y = time_delta)) +
  geom_point(alpha=0.1) +
  xlab("Hours (24+ is past midnight)") +
  ylab("Seconds from Benchmark") +
  ggtitle("Times between trains by time of day, weekday Southbound")

# Boxplot of times between trains by station, weekday Southbound.
boxplot(time_delta~parent_station_name,
     data = headway_times %>% filter(is_weekend == FALSE, direction == "0"),
     main="times between trains by station, weekday Southbound",
     xlab="Stop Sequence", ylab = "wait time (seconds)", pch=".", ylim=c(-800, 1600))

# Boxplot of times between trains by station, weekday Northbound.
boxplot(time_delta~stop_seq,
        data = headway_times %>% filter(is_weekend == FALSE, direction == "1"),
        main="times between trains by station, weekday Southbound",
        xlab="Stop Sequence", ylab = "wait time (seconds)", pch=".", ylim=c(-800, 1600))

# density plot of times between trains by station.
headway_plots <- list()
headway_plots[[1]] <- ggplot(headway_times %>% filter(direction == "0", is_weekend == FALSE), aes(x=time_delta)) +
  geom_density(aes(group = parent_station_name, colour= parent_station_name, fill= parent_station_name), alpha=0.2) +
  ggtitle("Weekday, Southbound") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key.size=unit(0.3, "cm"),
        legend.text=element_text(size=6),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 10))

headway_plots[[2]] <- ggplot(headway_times %>% filter(direction == "1", is_weekend == FALSE), aes(x=time_delta)) +
  geom_density(aes(group = parent_station_name, colour= parent_station_name, fill= parent_station_name), alpha=0.2) +
  ggtitle("Weekday, Northbound") + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key.size=unit(0.3, "cm"),
        legend.text=element_text(size=6),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 10))

headway_plots[[3]] <- ggplot(headway_times %>% filter(direction == "0", is_weekend == TRUE), aes(x=time_delta)) +
  geom_density(aes(group = parent_station_name, colour= parent_station_name, fill= parent_station_name), alpha=0.2) +
  ggtitle("Weekend, Southbound") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key.size=unit(0.3, "cm"),
        legend.text=element_text(size=6),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 10))

headway_plots[[4]] <- ggplot(headway_times %>% filter(direction == "1", is_weekend == TRUE), aes(x=time_delta)) +
  geom_density(aes(group = parent_station_name, colour= parent_station_name, fill= parent_station_name), alpha=0.2) +
  ggtitle("Weekend, Northbound") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.key.size=unit(0.3, "cm"),
        legend.text=element_text(size=6),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 10))
  
grid.arrange(headway_plots[[1]], headway_plots[[2]],headway_plots[[3]],headway_plots[[4]], ncol=2, left="Type of Day", top="Heading")
grid.rect(gp=gpar(fill=NA, col="gray"))

# And a simple plot of the densities of travel times for Northbound trains of weekdays, all times of day.
ggplot((travel_times %>% filter(direction == 1, is_weekend == FALSE)), aes(x=travel_time_sec)) +
  geom_density(aes(group = parent_station_name, colour= parent_station_name, fill= parent_station_name), alpha=0.2) +
  ggtitle("Travel times for Northbound trains of weekdays, by departing station") +
  xlim(0, 200) + ylim(0, .25)
