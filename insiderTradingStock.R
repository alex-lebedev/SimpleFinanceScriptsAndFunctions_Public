insiderTradingStock <- function(stockTicker){
  require("rvest")
  formatVOL <- function(x){as.numeric(gsub(",","",x))}
  # **********************************************************************************
  #                INSIDER TRANSACTIONS
  # **********************************************************************************
  url <- paste0("https://finviz.com/quote.ashx?t=",stockTicker)
  TABLE <- read_html(url)
  TABLE <- TABLE  %>% html_nodes("table") %>% .[[33]] %>%html_table(header=TRUE,fill=TRUE)
  #fundamentals:[[8]]; ratnings: [[9-10]]
  colnames(TABLE) <- c('Actor','Relationship','Date','Transaction','Cost','Shares','Value','TotalShares','SECform4')
  
  TABLE$Shares <- as.numeric(gsub(",","",TABLE$Shares))
  TABLE$Value <- as.numeric(gsub(",","",TABLE$Value))
  TABLE$TotalShares <- as.numeric(gsub(",","",TABLE$TotalShares))
  TABLE
}



