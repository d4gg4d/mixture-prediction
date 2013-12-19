timeRange <- function(data) {
  range <- as.POSIXct(c(min(data$time), max(data$time)), origin=attr(data, "time.origin"));
  return(list(start=range[1], end=range[2]));
}

plotGisDistribution <- function(data) {
  userId <- data$userid[1];
  locationStatistics <- stat_sum(aes(x=longitude, y=latitude, colour=type, group=type), data=data, alpha=.6);

  ## map around median position
  mapCenter <- sapply( list(data$longitude, data$latitude), FUN=function(x) {return(median(x, na.rm=TRUE))});
  zoomLevel <- 11; ##todo figure out Error in eval(expr, envir, enclos) (from #8) : object 'zoomLevel' not found
  map <- qmap(mapCenter, zoom=11);
  medianPlace <- map + locationStatistics + scale_size(range = c(2, 20)) + ggtitle(paste("Around median position, zoom", zoomLevel));

  ## around lausanne
  zoomLevel <- 9;
  map <- qmap("lausanne", zoom=9);
  overallPlace <- map + locationStatistics + scale_size(range = c(1, 10)) + ggtitle(paste("Around Lausanne, zoom", zoomLevel));

  ## overall time distibution
  range <- timeRange(data);
  title <- paste("Time distribution: ", paste(range$start, range$end, sep="=>"))
  timeHisto <- qplot(as.POSIXct(time, origin=attr(data,"time.origin")), data=data, geom="histogram", xlab="Date") + geom_bar(aes(fill=type)) + ggtitle(title);

  ## arrange produced plots
  grid.arrange(medianPlace, arrangeGrob(overallPlace, timeHisto, ncol=1),
               ncol=2, widths=c(2/3,1/3), main=paste("GIS statistics of a user",userId));
}

timeline.plots <- function(data) {
  base.plot <- ggplot(data, eval(substitute(aes(x=as.POSIXct(time, origin=org)), list(org=attr(data, "time.origin"))))) + xlab("dates")
  var.names <- lapply(c("latitude", "longitude", "altitude", "speed", "heading"), as.name)
  plots <- lapply(var.names, function(variable) {
    return(base.plot + eval(substitute(geom_point(aes(y=var)), list(var=variable))))
  })
  grid.arrange(do.call(arrangeGrob, plots[1:2]), do.call(arrangeGrob, plots[3:5]),
               ncol=2, main="GIS variables in selected time frame")
}
