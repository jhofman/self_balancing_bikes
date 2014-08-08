library(reshape)
library(data.table)
library(ggplot2)
library(scales)
library(plyr)
library(ggmap)
theme_set(theme_bw())

output_dir <- "plots/"

prettytimes <- c(paste0(c(12, 1:11),"am"), paste0(c(12,1:11),"pm"))

load("data/clean_citibike.RData")

#Figure 1: Number of trips by time of day and weekday/weekend
plotdata <- trips[ , list(num.trips = .N/length(unique(startymd))), by = c("rounded.starttime", "is.weekday")]
plotdata <- transform(plotdata, is.weekday = ifelse(is.weekday, "Weekday", "Weekend"))
p <- ggplot(data = plotdata, aes(x=as.integer(as.factor(rounded.starttime)), y = num.trips)) 
p <- p + geom_line(aes(color=is.weekday))
p <- p + scale_y_continuous("Number of Trips\n(per 15-minute interval)\n")
p <- p +  scale_x_continuous("", 
                             breaks = seq(1,96,by=8), 
                             labels = prettytimes[seq(1,24, by = 2)])
p <- p + theme(legend.position = c(.1,.9), 
               legend.title=element_blank(),
               axis.text.x=element_text(angle=45, hjust=1), 
               legend.background = element_rect(fill = "transparent"))
p

filename = paste0(output_dir,"num_trips_by_interval.pdf") 
unlink(filename)
ggsave(p, file = filename, width = 7, height = 5)


#Figure 3: Trip Length Distribution
trips$rounded.distance <- round_any(trips$distance, .2)
plotdata <- trips[is.weekday == T & rounded.distance > 0, list(num.trips = .N), by = rounded.distance]
meanDistance <- mean(trips$distance[trips$is.weekday & trips$distance > 0])

p <- ggplot(data = plotdata, aes(x=rounded.distance, y = num.trips)) 
p <- p + geom_line()
p <- p + scale_y_continuous("")
p <- p + scale_x_continuous("\nDistance (Km)")
p <- p + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
p <- p + geom_vline(xintercept = meanDistance, linetype = "dashed" )
p

filename = paste0(output_dir,"trip_length_distribution.pdf") 
unlink(filename)
ggsave(p, file = filename, width = 5, height = 5)

#Figure 6 Transports over time 
numDays <- sum(unique(teleportations[is.weekday == T]$startymd) <= "2013-11-30")

plotdata <- teleportations[  is.weekday == T & startymd <= "2013-11-30", 
                             list(num.teleports = .N/numDays), 
                             by=c("rounded.time.reappeared")]
p <- ggplot(data = plotdata, aes(x=as.integer(as.factor(rounded.time.reappeared)), y = num.teleports)) 
p <- p + geom_line()
p <- p + scale_y_continuous("Number of Teleports\n(per 15-minute interval)\n")
p <- p +  scale_x_continuous("", 
                             breaks = seq(1,96,by=8), 
                             labels = prettytimes[seq(1,24, by = 2)])
p <- p + theme(legend.position = c(.1,.9), 
               legend.title=element_blank(),
               axis.text.x=element_text(angle=45, hjust=1), 
               legend.background = element_rect(fill = "transparent"))
p

filename = paste0(output_dir,"num_teleports_by_interval.pdf") 
unlink(filename)
ggsave(p, file = filename, width = 7, height = 5)

#Figure 8 Examples of teleport patterns for different locations
station_list <- c("Broadway & W 41 St", "W 51 St & 6 Ave", "W 41 St & 8 Ave", "Greenwich Ave & 8 Ave")
numDays <- sum(unique(teleportations[is.weekday == T]$startymd) <= "2013-11-30")

bikes_taken <- teleportations[  is.weekday == T & stopymd <= "2013-11-30" , 
                                list(num = .N/numDays, teleport.type = "Picked Up"), 
                                by=c("station.disappeared","hour.lastknown")]

bikes_left <- teleportations[  is.weekday == T & startymd <= "2013-11-30", 
                               list(num = .N/numDays, teleport.type = "Dropped Off"), 
                               by=c("station.reappeared","hour.reappeared")]
setnames(bikes_taken, c("station.disappeared", "hour.lastknown"), c("station", "interval"))
setnames(bikes_left, c("station.reappeared", "hour.reappeared"), c("station", "interval"))
teleports_by_station  <- rbind(bikes_taken,bikes_left)

plot_data = teleports_by_station[station %in% station_list]
plot_data$station <- factor(plot_data$station, levels = station_list)
p <- ggplot(plot_data, aes(x=interval, y = num)) 
p <- p + geom_line(aes(color = teleport.type))
p <- p + facet_wrap(~station)
p <- p + scale_y_continuous("Number of Transports\n(per hour)\n")
p <- p +  scale_x_continuous("", 
                             breaks = seq(0,23,by=4), 
                             labels = prettytimes[seq(1,24, by = 4)])
p <- p + theme(legend.position = c(.1,.95), 
              legend.title=element_blank(),
              axis.text.x=element_text(angle=45, hjust=1), 
              legend.background = element_rect(fill = "transparent"))

filename = paste0(output_dir,"teleports_station_examples.pdf") 
unlink(filename)
ggsave(plot=p, file = filename, width = 7, height = 7)

# Figure 4: Percent of starved and congested stations by time
setkey(availability, station.name)
setkey(stationcap, station.name) 
#setting nomatch to 0 removes those rows
avail_by_time <- stationcap[availability, nomatch = 0]
avail_by_time <- avail_by_time[is.weekday == T & ymd <= "2013-11-31" & ymd >= "2013-07-01"] 
avail_by_time <- avail_by_time[, p.avail := bikes.available/station.capacity]

# p.avail > .8 returns a binary vector and if you sum it you get total times it was true. 
# If you take the mean you get the percentage of trues
status_by_time <- avail_by_time[, list(Congested = mean(p.avail > .8), 
                                      Starved = mean(p.avail < .2)), by = 'rounded.interval' ]
plotdata <- melt(status_by_time, id = "rounded.interval")

p <- ggplot(data = plotdata, aes(x=as.integer(as.factor(rounded.interval)), y = value))
p <- p + geom_line(aes(color=variable))
p <- p + scale_y_continuous("Percentage of stations\n", labels = percent_format(), limits = c(0,.5))
p <- p +  scale_x_continuous("", 
                             breaks = seq(1,96,by=8), 
                             labels = prettytimes[seq(1,24, by = 2)])
p <- p + theme(legend.position = c(.1,.9), 
               legend.title=element_blank(),
               axis.text.x=element_text(angle=45, hjust=1), 
               legend.background = element_rect(fill = "transparent"))
p 

filename = paste0(output_dir,"station_status.pdf") 
unlink(filename)
ggsave(p, file = filename, width = 7, height = 5)

# Figure 5: Examples of the four types of stations
station_status <- avail_by_time[, list(percent.starved = mean(p.avail < .2),
                            percent.congested = mean(p.avail >.8)) , by = "station.name"]
station_status <- station_status[, is.congested := percent.congested > .2]
station_status <- station_status[, is.starved := percent.starved > .2]

sample_stations <- station_status[ , list(station.name = sample(station.name, 1)), by = c("is.congested", "is.starved")]

avail_by_station <- avail_by_time[ , list(p.avail = mean(p.avail)), by = c("station.name", "rounded.interval")]

setkey(sample_stations, "station.name")
setkey(avail_by_station, "station.name")

plotdata <- sample_stations[avail_by_station, nomatch = 0]

p <- ggplot(plotdata, aes(x=as.integer(as.factor(rounded.interval)), y = p.avail)) 
p <- p + geom_line()
p <- p + facet_grid(is.congested~is.starved)
p

p <- p + scale_y_continuous("Number of Transports\n(per hour)\n")
p <- p +  scale_x_continuous("", 
                             breaks = seq(0,23,by=4), 
                             labels = prettytimes[seq(1,24, by = 4)])
p <- p + theme(legend.position = c(.1,.95), 
               legend.title=element_blank(),
               axis.text.x=element_text(angle=45, hjust=1), 
               legend.background = element_rect(fill = "transparent"))

filename = paste0(output_dir,"teleports_station_examples.pdf") 
unlink(filename)
ggsave(plot=p, file = filename, width = 7, height = 7)

# Examples of starved/congested station types


# Heatmap of total teleports
t <- teleportations[, list(lat = station.disappeared.latitude[1],
                           lon = station.disappeared.longitude[1],
                           num = .N), by = "station.disappeared"
                    ]

p <- qmap("Lower Manhattan", zoom = 12, color = "bw")
p <- p + geom_point(data = t, aes(x = lon, y = lat, size = num, color = num )) + 
  scale_color_gradient(low="white", high = "red") + scale_size_area(max_size = 10)

filename = paste0(output_dir,"bikes_pickedup.png") 
unlink(filename)
ggsave(plot=p, file = filename, width = 10, height = 10)

t <- teleportations[, list(lat = station.reappeared.latitude[1],
                           lon = station.reappeared.longitude[1],
                           num = .N), by = "station.reappeared"]

p <- qmap("Lower Manhattan", zoom = 12, color = "bw")
p <- p + geom_point(data = t, aes(x = lon, y = lat, size = num, color = num )) + 
  scale_color_gradient(low="white", high = "red") + scale_size_area(max_size = 10)

filename = paste0(output_dir,"bikes_droppedoff.png") 
unlink(filename)
ggsave(plot=p, file = filename, width = 10, height = 10)

# stationprox
setkey(avail_by_station,station.name)
setkey(stationprox, station.1)
station_prox_with_avail <- avail_by_station[stationprox, 
            allow.cartesian =  T][rounded.interval <= "19:00" & rounded.interval >= "18:00"]

setnames(station_prox_with_avail, c("station.name", "p.avail"), c("station.1", "station.1.avail"))

setkey(station_prox_with_avail, station.2, rounded.interval)
setkey(avail_by_station, station.name, rounded.interval)

station_prox_with_avail <- avail_by_station[station_prox_with_avail][order(station.1, rounded.interval)]
setnames(station_prox_with_avail, c("station.name", "p.avail"), c("station.2", "station.2.avail"))
station_prox_with_avail<- station_prox_with_avail[,list(station.1,
                                                        station.1.avail,
                                                        rounded.interval,
                                                        station.2,
                                                        station.2.avail)]
# v(station_prox_with_avail)
station_prox_with_avail <- station_prox_with_avail[,list(Average_Station_Availability = station.1.avail[1],
                                     Average_Neighbor_Availability = mean(station.2.avail)),
                                     by = c("station.1", "rounded.interval")]

p <- ggplot(station_prox_with_avail, aes(x = Average_Station_Availability, y = Average_Neighbor_Availability))
p <- p + geom_point()
p <- p + scale_y_continuous("Average Neighbors' Availability\n", limits = c(0,1), labels = percent_format())
p <- p +  scale_x_continuous("\nAverage Station Availability", limits = c(0,1), labels = percent_format())
p

filename = paste0(output_dir,"average_neighbor_availability.pdf") 
unlink(filename)
ggsave(plot=p, file = filename, width = 6, height = 6)





###########################################
# load simulation data from clean_sim_data.R
###########################################
load('data/simulation.RData')

# compute fration of available bikes at each point in time
simulation <- simulation[, p.avail := start.available.bikes/start.station.cap]

# filter to weekdays only
simulation <- simulation[, is_weekday := strftime(as.POSIXct(starttime, origin="1970-01-01"),format="%u")]
simulation <- simulation[, is_weekday := ifelse(is_weekday < 6, TRUE, FALSE)]
simulation <- simulation[is_weekday == T]

# compute the average condition in each 15 minute interval for each station
sim_by_station_and_time <- simulation[, list(Congested = mean(p.avail > .8),
                                           Starved = mean(p.avail < .2)), by = c('strategy','rounded.interval','start.station.name') ]

# average across stations for each 15 minute interval
sim_by_time_across_stations <- sim_by_station_and_time[, list(Congested=mean(Congested),
                                                              Starved=mean(Starved)), by=c('strategy','rounded.interval')]

# Figure 9: System health from simulations (No vans, No vans + local re-routing)
alt_prettytimes <- c(paste0(c(12, 1:11),"am"), paste0(c(12,1:11),"pm"))

plotdata <- melt(sim_by_time_across_stations, id = c("rounded.interval","strategy"), measure.vars=c("Congested","Starved"))
plotdata <- transform(plotdata, strategy=revalue(strategy, c(rider="No vans", greedy="No vans + re-routing")))
plotdata <- transform(plotdata, rounded.interval=factor(rounded.interval, levels=unique(rounded.interval)))

p <- ggplot(data = plotdata, aes(x=as.integer(as.factor(rounded.interval)), y = value))
p <- p + geom_line(aes(color=variable))
p <- p + facet_wrap(~ strategy)
p <- p + scale_y_continuous("Percentage of stations\n", labels = percent_format(), limits = c(0,.5))
p <- p +  scale_x_continuous("",
                             breaks = seq(1,96,by=8),
                             labels = alt_prettytimes[c(seq(5,24,by=2),1,3)])
p <- p + theme(legend.position = c(.1,.9),
               legend.title=element_blank(),
               axis.text.x=element_text(angle=45, hjust=1),
               legend.background = element_rect(fill = "transparent"))
p

filename = paste0(output_dir,"station_status_no_vans_reroute_facet.png")
unlink(filename)
ggsave(p, file = filename, width = 8, height = 4)

# measure failure rates
simulation[, mean(is.na(start.station.prox) | is.na(end.station.prox)), by="strategy"]
