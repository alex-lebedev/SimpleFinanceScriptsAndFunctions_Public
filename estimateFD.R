# estimateFD.R
# Simple function to estimate fractal dimension of a stock time-series for a given period
# relies on 'quantmod' and 'fractaldim'
estimateFD<- function(pricePair='DJI',
                      startDate='2000-01-01',
                      targetDate=Sys.Date()){
  f = getSymbols(pricePair, auto.assign=FALSE, from=startDate, to=targetDate,src='yahoo')
  f <- as.data.frame(f)
  colnames(f) <- pts
  f$pct_change <- (f$open-f$close)/f$open
  f$Date <- as.Date(rownames(f))
  colnames(f)[ptype] <- 'market'
  selDate <- last(f$Date)
  f <- f[complete.cases(f$market),]
  round(fd.estimate(as.numeric(as.vector(f[,'market'])))$fd,2)[1,1]
}
  
