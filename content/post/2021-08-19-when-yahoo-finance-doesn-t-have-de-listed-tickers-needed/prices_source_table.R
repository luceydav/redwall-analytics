library(data.table)

prices_source_table <- function() {
  
  path <- "~/Desktop/David/Projects/new_constructs_targets/"
  
  # Load quantmod weekly stock prices previously batch downloaded
  files <- list.files(paste0(path, "data/historical_prices"))
  
  yahoo_prices <-
    rbindlist(lapply(
      paste0(path, "data/historical_prices/", files),
      fread,
      select = c(
        ticker = "character",
        ref.date = "IDate",
        price.adjusted = "numeric"
      )
    ))
  
  # Clean up ticker to standardize for joins and convert price.adjusted to numeric
  yahoo_prices[, `:=`(
    source = "yahoo",
    ticker = trimws(ticker),
    price.adjusted = as.numeric(price.adjusted)
  )]
  
  # Remove extended periods of same price
  yahoo_prices[, id := rleid(price.adjusted), by = ticker]
  yahoo_prices <-
    yahoo_prices[,
                 .SD[
                   !duplicated(id, fromLast = TRUE) & id == 1 |
                     !duplicated(id) & id == max(id) |
                     between(id, 2, max(id) - 1)],
                 ticker]
  yahoo_prices[, id := NULL]
  
  # # Fix names and set key for joins
  setnames(yahoo_prices, "ref.date", "filing_date")
  
  eod_prices <- fread(paste0(path, "data/eod_weekly_prices.csv"))
  eod_prices[, `:=`(
    source = "eod",
    date = as.IDate(date))]
  setnames(eod_prices, c("ticker", "filing_date", "price.adjusted", "source"))
  
  # Load and clean av_prices
  keeps <- c("source", "ticker", "filing_date", "price.adjusted")
  av_prices <- 
    fread(paste0(path, "data/av_historical_prices/av_missing_tickers.csv"))
  av_prices[, source := "av"]
  setnames(av_prices,
           c("ref.date", "price.close"),
           c("filing_date", "price.adjusted"))
  av_prices <- 
    av_prices[, ..keeps]
  
  # Merge yahoo and av prices
  prices <- rbind(eod_prices, av_prices, yahoo_prices)
  
  # Unique prices by ticker, filing_date
  prices <- 
    prices[, .SD[!duplicated(filing_date)], ticker]
  setkeyv(prices, c("ticker", "filing_date"))
  
  price_table <- prices[, .N, source]
  return(price_table)

}