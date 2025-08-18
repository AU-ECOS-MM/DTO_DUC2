extractDTstampEMODnetBio = function (df, status=c('start', 'end')) {
  
  #This function is to extract a datetime stamp from the EMODnet database.It requires:
  #   df: The dataframe extracted from the database
  #   status: If you want to extract the 'Start' or the 'End' part of the timestamp.
  
  status=match.arg(status)
  
  allDT=NULL
  for (r in 1:nrow(df)) {
    
    row=df[r,]
  
    if(is.na(row$yearcollected)) {
      var = paste0(status, 'yearcollected')
      year=row[[var]]
    } else {
      year=row$yearcollected
    }
    
    if(is.na(row$monthcollected)) {
      var = paste0(status, 'monthcollected')
      month=row[[var]]
    } else {
      month=row$monthcollected
    }
    
    if(is.na(row$daycollected)) {
      var = paste0(status, 'daycollected')
      day=row[[var]]
    } else {
      day=row$daycollected
    }
    
    if(is.na(row$timeofday)) {
      var = paste0(status, 'timeofday')
      hour=row[[var]]
    } else {
      hour=ifelse(status=='start', row$timeofday, 0)
    }
    
    dt=paste(year,month,day,hour, sep='-')
    
    datetime=as.data.frame(as.POSIXct(dt, '%Y-%m-%d-%H', tz='UTC'))
    allDT=rbind(allDT,datetime)

  }
  names(allDT)='timestamp'
  return(allDT)
}
  