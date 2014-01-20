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

separate.timeline.plots <- function(data, variables) {
  base.plot <- ggplot(data, eval(substitute(aes(x=as.POSIXct(time, origin=org)), list(org=attr(data, "time.origin"))))) + xlab("dates")
  return(lapply(lapply(variables, as.name), function(variable) {
    return(base.plot + eval(substitute(geom_point(aes(y=var)), list(var=variable))))
  }))
}

timeline.plots <- function(data) {
  plots <- separate.timeline.plots(data, c("longitude", "latitude", "altitude", "speed", "heading"))
  grid.arrange(do.call(arrangeGrob, plots[1:2]), do.call(arrangeGrob, plots[3:5]),
               ncol=2, main="GIS variables in selected time frame")
}

VisualizeExtraction <- function(features, data, t.dist=3*hours, t.window.length=2*days) {
  test.extractions <- features.extraction(features, data, t.dist=t.dist, t.window.length=t.window.length)
  attr(test.extractions, "time.origin") <- attr(data, "time.origin")

  names <- names(test.extractions)
  plots <- separate.timeline.plots(test.extractions, names[!names %in% "time"])
  
  grid.arrange(do.call(arrangeGrob, plots))
}

PlotPredictionDiff <- function(prediction.data, valid.data) {
  base.plot <- ggplot(PredictionError(prediction.data, valid.data))
  time.range <- coord_cartesian(xlim=with(valid.data, as.POSIXct(c(min(time), max(time)), origin="1970-01-01")))
  grid.arrange(arrangeGrob(
    arrangeGrob(
      base.plot + geom_histogram(aes(x=longitude), binwidth=0.01) + xlim(-0.15,0.15),
      base.plot + geom_histogram(aes(x=latitude), binwidth=0.01) + xlim(-0.15,0.15),
      main="Around Center"),
    arrangeGrob(
      base.plot + geom_histogram(aes(x=longitude), binwidth=0.01),
      base.plot + geom_histogram(aes(x=latitude), binwidth=0.01),
      main="Whole Data"),
    ncol=2, main="Prediction Error Histograms"),
               ggplot(valid.data) +
               geom_point(aes(x=as.POSIXct(time, origin="1970-01-01"), y=longitude)) +
               geom_point(data=prediction.data, aes(x=as.POSIXct(time, origin="1970-01-01"), y=longitude, colour="red")) +
               time.range +
               theme(legend.position="none", axis.title.x=element_blank()),
               base.plot +
               geom_point(aes(x=as.POSIXct(time, origin="1970-01-01"), y=longitude)) +
               geom_hline(aes(yintercept=0, colour="red")) +
               time.range +
               theme(axis.title.x=element_blank()),

               ggplot(valid.data) +
               geom_point(aes(x=as.POSIXct(time, origin="1970-01-01"), y=latitude)) +
               geom_point(data=prediction.data, aes(x=as.POSIXct(time, origin="1970-01-01"), y=latitude, colour="red")) +
               time.range +
               theme(legend.position="none", axis.title.x=element_blank()),
               base.plot +
               geom_point(aes(x=as.POSIXct(time, origin="1970-01-01"), y=latitude)) +
               geom_hline(aes(yintercept=0, colour="red")) +
               time.range +
               theme(axis.title.x=element_blank()),

               heights=c(4,2,1,2,1), ncol=1)
}
