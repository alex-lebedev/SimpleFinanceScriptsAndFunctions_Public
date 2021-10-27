# getFinNews.R
# simple Finviz news scraper with sentiment analysis
ticker = "MNMD"
getFinNews = function(ticker)
{
  Sys.sleep(5)
  url <- paste0("https://finviz.com/quote.ashx?t=",ticker)
  # read finviz news data
  data <- read_html(url)
  # copy xpath
  data = data %>% html_nodes(xpath = "//*[@id='news-table']") %>% html_table()
  tmp = do.call(rbind,data)
  dtime = as.data.frame(tmp[,1])
  # Split Dates & Times
  dtime <- t(as.data.frame(str_split(dtime[,1], pattern = " ")))
  dates <- as.character(dtime[,1])
  tmz   <- as.character(dtime[,2])
  # detect times by using colon ":" & replace with NA
  dates[str_detect(dates,pattern = ":")] <- NA
  dates <- na.locf(dates)
  # combine into a timeStamp
  timeStamp <- as.data.frame(as.POSIXct(paste(dates,tmz), format="%b-%d-%y %I:%M%p"))
  # combine timeStamps with News
  tmp[,1] <- timeStamp
  tmp[,3] <- ticker
  colnames(tmp) <- c("Date","News","Ticker")
  tmp
}

dat <- getFinNews(ticker)
sentiment <- get_sentiments("afinn")

source('/Users/alebedev/GitHub/WorldSentiment/functions/Clean_String.R')

sents <- data.frame(sentiment=as.vector(rep(NA,length(dat$News))), Date=as.Date('2020-01-01'))
for (i in 1:length(dat$News)){
  sents$Date[i] <- (as.Date(as.POSIXct((dat$Date[i]), tz='CEST', format="%Y-%m-%d %H:%M:%OS")))
  sents$sentiment[i] <- sum(sentiment[is.element(sentiment$word,as.vector(Clean_String(dat$News[i]))),]$value)
}

f = as.data.frame(getSymbols(ticker, auto.assign=FALSE, from=startDate, to=targetDate,src='yahoo'))
f$Date <- as.Date(rownames(f))

ff <- merge(f,sents, by='Date',all.x = T)
ff <- subset(ff, ff$Date>=min(dat$Date))

#plot(ff$Date,scale(ff[,5]), type='n',ylim = c(min(scale(cbind(ff[,5],ff$sentiment))),max(scale(cbind(ff[,5],ff$sentiment)))))
#points(ff$Date,scale(ff$sentiment), type='b', col=2,add=T)
#points(ff$Date,scale(ff[,5]), type='l', lwd=3,col=3,add=T)

df1 = data.frame(
  date_id = ff$Date,          
  price = ff[,5], 
  sent = as.numeric(ff$sentiment))
df1$sent <- na_interpolation(df1$sent)
ggplot(df1, aes(x=as.Date(date_id))) +
  geom_smooth( aes(y=scale(sent)), size=1, color='green', span = 0.25)+
  geom_line( aes(y=scale(price)), size=1, color='black', group = 1)+
  theme_bw()+theme(legend.position="top")
