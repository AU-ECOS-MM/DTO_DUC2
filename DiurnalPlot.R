diurnalPlot=function(dataIn,selectLocs) {
  
  data=dataIn[dataIn$PPM==1,]
  
  
  #Get all days in subset
  dates=unique(as.Date(data$datetime, tz='UTC'))
  dates=append(min(dates)-day(1), dates)
  dates=append(max(dates)+day(1), dates)
  
  dates=dates[order(dates)]
  
  #Get the sunrise for those days per station
  solarTimes=list()
  for (s in 1:nrow(selectLocs)){
    sunR=sunriset(
      matrix(c(selectLocs$long[s], selectLocs$lat[s]), nrow = 1),
      as.POSIXct.Date(dates, tz='utc'),
      direction=c( "sunrise"),
      POSIXct.out=TRUE
    )[,2]
    sunS=sunriset(
      matrix(c(selectLocs$long[s], selectLocs$lat[s]), nrow = 1),
      as.POSIXct.Date(dates, tz='utc'),
      direction=c( "sunset"),
      POSIXct.out=TRUE
    )[,2]
    sunR=sunR[order(sunR)]
    sunS=sunS[order(sunS)]
    sTimes=data.frame(sunR, sunS)
    solarTimes[[s]]=sTimes
    
  }
  
  names(solarTimes)= selectLocs$Station
  
  
  allNT=NULL
  for (nt in 1:length(solarTimes)){
    stationData = data[data$Station==names(solarTimes[nt]),]
    if (length(stationData)==0) {next}
    
    xdidc=match(as.Date(stationData$datetime), as.Date(solarTimes[[nt]][,1]))
    # Normalize the time between sunset and sunrise (-1 - 0) and sunrise and sunset (0 - 1) via linear interpolation.
    
    normalizedTime = NULL
    for (dd in 1:length(xdidc)) {
      sunR=solarTimes[[nt]][,1]
      sunS=solarTimes[[nt]][,2]
      
      if (stationData$datetime[dd] < sunR[xdidc[dd]]) {
        # Detection before Sunrise
        dawn = as.numeric(sunR[xdidc[dd]])
        dusk = as.numeric(sunS[xdidc[dd]-1])
        timeSeq = seq(dusk,dawn, by = 1)
        nTx = seq(-1.0,0.0, length.out=length(timeSeq))
        
      } else if (stationData$datetime[dd] > sunR[xdidc[dd]] && stationData$datetime[dd] < sunS[xdidc[dd]]) {
        # Detection after Dawn but before Dusk  
        dawn = as.numeric(sunR[xdidc[dd]])
        dusk = as.numeric(sunS[xdidc[dd]])
        timeSeq = seq(dawn,dusk, by = 1)
        nTx = seq(0.0,1.0, length.out=length(timeSeq))
        
      } else if ( stationData$datetime[dd] > sunS[xdidc[dd]]) {
        #Detection after Dusk
        dawn = as.numeric(sunR[xdidc[dd]+1])
        dusk = as.numeric(sunS[xdidc[dd]])
        timeSeq = seq(dusk,dawn, by = 1)
        nTx = seq(-1.0,0.0, length.out=length(timeSeq))
        
      }
      
      detcTime=as.numeric(stationData$datetime[dd])
      idx=which.min(abs(timeSeq-detcTime))
      
      nT=nTx[idx]
      
      normalizedTime=rbind(normalizedTime,nT)
    }
    
    allNT = append(allNT, normalizedTime)
      
  }
  
  data$normalizedTime=allNT
  
  bins <- 36
  cols <- c("#0072B2", "#253f4b", "#0072B2", "#D55E00", "#F0E442", "#D55E00")
  colGradient <- colorRampPalette(cols)
  cut.cols <- colGradient(bins)
  cuts <- cut
  
  
  diurnalp=ggplot(data, aes(x = normalizedTime)) +
    geom_histogram(
      aes(fill = after_stat(factor(x))),
      bins = bins
    ) +
    
    scale_color_manual(values = cut.cols, labels = levels(cuts)) +
    scale_fill_manual(values = cut.cols, labels = levels(cuts)) +
    scale_x_continuous(
      breaks = c(-1, 0),
      labels = c("Sunset", "Sunrise"),
      limits = c(-1,1)
    ) +
    theme_minimal() +
    theme(
      text=element_text(size=15)
    ) +
    coord_polar(theta = "x", start = 0)+
    xlab('') +
    
    #  facet_wrap(~Station) +
    guides(fill = "none") +
    theme(text=element_text(size=15))
  
  return(diurnalp)
  
}















