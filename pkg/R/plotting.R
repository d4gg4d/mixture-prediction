plotGisDistribution <- function(gisData) {
  userId <- gisData$userid[1];
  locationStatistics <- stat_sum(aes(x=longitude, y=latitude, colour=type, group=type), data=gisData, alpha=.6);

  ## map around median position
  mapCenter <- median( c(gisData$longitude, gisData$latitude), na.rm=TRUE);
  zoomLevel <- 11;
  map <- qmap(mapCenter, zoom=zoomLevel);
  medianPlace <- map + locationStatistics + scale_size(range = c(2, 20)) + ggtitle(paste("Around median position, zoom", zoomLevel));

  ## around lausanne
  zoomLevel <- 9;
  map <- qmap("lausanne", zoom=zoomLevel);
  overallPlace <- map + locationStatistics + scale_size(range = c(1, 10)) + ggtitle(paste("Around Lausanne, zoom", zoomLevel));

  ## overall time distibution
  timeHisto <- qplot(as.POSIXct(time, origin="1970-01-01"), data=gisData, geom="histogram", title="time distibution of observations", xlab="Date") + geom_bar(aes(fill=type)) + ggtitle("time distibution");
  ## arrange produced plots
  grid.arrange(medianPlace, arrangeGrob(overallPlace, timeHisto, ncol=1),
               ncol=2, widths=c(2/3,1/3), main=paste("GIS statistics of a user",userId));
};

