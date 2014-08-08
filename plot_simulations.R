library(data.table)
library(ggplot2)
library(scales)
library(reshape)

theme_set(theme_bw())

output_dir <- "plots/"

#prettytimes <- c(paste0(4:11, "am"), paste0(c(12,1:11), "pm"), "12am", paste0(1:3,"am"))
prettytimes <- c(paste0(c(12, 1:11),"am"), paste0(c(12,1:11),"pm"))

# load data from CleanSimData.R
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

plotdata <- melt(sim_by_time_across_stations, id = c("rounded.interval","strategy"), measure.vars=c("Congested","Starved"))
plotdata <- transform(plotdata, strategy=revalue(strategy, c(rider="No vans", greedy="No vans + re-routing")))
plotdata <- transform(plotdata, rounded.interval=factor(rounded.interval, levels=unique(rounded.interval)))

p <- ggplot(data = plotdata, aes(x=as.integer(as.factor(rounded.interval)), y = value))
p <- p + geom_line(aes(color=variable))
p <- p + facet_wrap(~ strategy)
p <- p + scale_y_continuous("Percentage of stations\n", labels = percent_format(), limits = c(0,.5))
p <- p +  scale_x_continuous("",
                             breaks = seq(1,96,by=8),
                             labels = prettytimes[c(seq(5,24,by=2),1,3)])
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
