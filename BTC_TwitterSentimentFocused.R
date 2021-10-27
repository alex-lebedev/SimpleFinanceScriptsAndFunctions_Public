# BTC_TwitterSentimentFocused.R
# I played with this script some time ago to explore the impact of Twitter influencers on BTC price action

# START
# Clear workspace:
rm(list=ls())
library(imputeTS)
library(colorednoise)
library(foreach)
library(doParallel)
library(ggplot2)
library(gtrendsR)
library(data.table)
library(dplyr)
library(purrr)
library(twitteR)
library(lubridate)
library(scales)
library(tidytext)
library(stringr)
library(tidyr)
library(quantmod)
library(xlsx)
library(zoo)

# http://varianceexplained.org/r/trump-tweets/
consumer_key <- "CONSUMER-KEY"
consumer_secret <- "CONSUMER-SECRET"
access_token <- "ACCESS-TOKEN"
access_secret <- "ACCESS-SECRET"
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

startDate = "2000-01-01"
targetDate = as.Date(Sys.Date())

pricePair = "BTC-USD"

# List of Twitter influencers:
influencerList = c('chamath','PrestonPysh','Breedlove22',
                   'APompliano','LynAldenContact',
                   'PeterMcCormack','creation247',
                   'naval', '100trillionUSD')

#working: buy_words=c('buy', 'buying','up','upward','rise')
buy_words=c('buy', 'buying','up','upward','rise', 'increase')
sell_words=c('sell', 'selling','downward','bear','bears','bearish')
urgent_words <- c('hold','opportunity','urgent', 'quick', 'quickly', 'ahead', 'prepare','major','big','huge','time')
hold_words <- c('don','hodl','wait','continue')

ptype = 4 #1=open, 2 - high, 3-low, 4-close, 5-Volume, 6-adjusted

f = getSymbols(pricePair, auto.assign=FALSE, from=startDate, src='yahoo')
f <- as.data.frame(f)
f$Date <- as.Date(rownames(f))
dateHistorical <- as.Date(last(f$Date))
mName <- names(f)[ptype] 
f <- f[,c('Date',mName)]
colnames(f)[2] <- 'market'
f_dates <- data.frame(Date=seq.Date(as.Date(range(f$Date)[1]), as.Date(targetDate), by=1)) #
f <- merge(f_dates,f, by='Date', all=T)

twts <- array(NA, c(nrow(f), 4, length(influencerList)));  

for (i in 1:length(influencerList)){
  raw_tweets <- userTimeline(influencerList[i], n = 3200,includeRts=T)
  raw_tweets_df <- tbl_df(map_df(raw_tweets, as.data.frame))
  tweets <- raw_tweets_df %>%
    select(id, statusSource, text, created) %>%
    extract(statusSource, "source", "Twitter for (.*?)<")
  
  reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
  tweet_words <- tweets %>%
    filter(!str_detect(text, '^"')) %>%
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
    unnest_tokens(word, text, token = "regex", pattern = reg) %>%
    filter(!word %in% stop_words$word,
           str_detect(word, "[a-z]"))
  
  
  #BUY:
  wcount_df <- data.frame(Date=as.Date(tweet_words$created), hits=is.element(tweet_words$word,buy_words))
  tmp_avg <-as.data.frame(wcount_df %>% 
                            group_by(Date) %>% 
                            summarise(hits = sum(hits, na.rm=T),
                                      .groups = 'drop'
                            ))
  twts[,1,i] <- merge(f, tmp_avg, by = 'Date', all.x =T)[,'hits']
  #SELL:
  wcount_df <- data.frame(Date=as.Date(tweet_words$created), hits=is.element(tweet_words$word,sell_words))
  #wcount_df <- data.frame(Date=as.Date(tweet_words$created), hits=is.element(tweet_words$word,sell_words)&(tweet_words$word=='gold'))
  
  tmp_avg <-as.data.frame(wcount_df %>% 
                            group_by(Date) %>% 
                            summarise(hits = sum(hits, na.rm=T),
                                      .groups = 'drop'
                            ))
  twts[,2,i] <- merge(f, tmp_avg, by = 'Date', all.x =T)[,'hits']
  #URGENT:
  wcount_df <- data.frame(Date=as.Date(tweet_words$created), hits=is.element(tweet_words$word,urgent_words))
  tmp_avg <-as.data.frame(wcount_df %>% 
                            group_by(Date) %>% 
                            summarise(hits = sum(hits, na.rm=T),
                                      .groups = 'drop'
                            ))
  twts[,3,i] <- merge(f, tmp_avg, by = 'Date', all.x =T)[,'hits']
  
  #HOLD:
  wcount_df <- data.frame(Date=as.Date(tweet_words$created), hits=is.element(tweet_words$word,hold_words))
  tmp_avg <-as.data.frame(wcount_df %>% 
                            group_by(Date) %>% 
                            summarise(hits = sum(hits, na.rm=T),
                                      .groups = 'drop'
                            ))
  twts[,4,i] <- merge(f, tmp_avg, by = 'Date', all.x =T)[,'hits']
}

twts <- apply(twts,c(2,3),na_interpolation)
twts <- as.data.frame(apply(twts,c(1,2),sum))
colnames(twts) <- c('buy','sell', 'urgent','hold')

twts <- as.data.frame(twts)
twts$Date <-as.Date(f$Date)
twts$market <-f$market

# Exclude market data:
twtsFocused <- twts[, c('Date','buy','sell', 'urgent','hold')] 
# Save the data with a time stamp:
save(twtsFocused, file=paste0('/PATH-TO-THE-TARGET-FOLDER/twtsFocused-',Sys.Date(),'.rda'))
