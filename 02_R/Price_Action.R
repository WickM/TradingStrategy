#################################################x
#' Project:
#' Script purpose:
#' Sun Oct 24 11:33:18 2021
#' Author: Manuel Wick-Eckl 
#################################################x
options(stringsAsFactors = FALSE)


library(tidymodels)
library(tidyverse)
library(slider)
library(here)

stock_data <- read_rds(here("01_data/wrangled_stock_data.RDS"))

# Stochastic Oscillator / Stochastic Momentum Index:
#   stoch(HLC, nFastK = 14, nFastD = 3, nSlowD = 3, maType, bounded = TRUE, smooth = 1, ...): Stochastic Oscillator
# SMI(HLC, n = 13, nFast = 2, nSlow = 25, nSig = 9, maType, bounded = TRUE, ...): Stochastic Momentum Index
# 
# Strength buffer will calculate the relative strength of the
# price action compared to the inputted 'Period' amount of past
# candles.
# 
# Overlap buffer will calculate the relative overlapping of the
# current bars compared to the inputted 'Period' amount of past
# candles.
# 
# Reversal buffer  Recent peaks/bottoms of the price
#     compared to the Price Action Strength value
# 
# Continuation buffer This indicates a highly probable
#     move. Continuation/momentum sentiment, which is calculated using the
#     Price Action Strength momentum and value

# strength how many upward (bullish) periods (candles) occurred during the past
# movement vs downward (bearish);
#   bullish candle = close > open
#   bearish candle = close < open
# what is the average bullish candle body size vs bearish;
# how many bullish candles occur in a row on average vs bearish;
# what is the average upper wick (tail) size vs lower; 
# how much do the candles overlap on average.
pai_stregth <- function(data, open = "open", high = "high", low = "low", close = "close", date ="date", time_period) {
  
  fun_data <- data %>% 
    arrange(desc(date))
  
  pai <- map_dfr(seq(1, nrow(fun_data)), ~ {
    ind <- seq(.x, .x + time_period -1)
    
    
    fun_data_temp <- fun_data[ind, c(date, open, close, low, high)] %>% 
      mutate(strength = close - open, 
             candle_type = ifelse(strength < 0, "bearish", "bullish"), 
             candle_body = abs(strength),
             tail_size_up = ifelse(candle_type == "bearish", high - open, high - close),
             tail_size_down = ifelse(candle_type == "bearish", close - low, open - low))
    
    
    fun_data_temp$differing <- 0
    

    
   for (day in seq(1, nrow(fun_data_temp)-1) ){
     
     if(is.na(fun_data_temp$candle_type[day+1]) == FALSE & is.na(fun_data_temp$candle_type[day]) == FALSE ) {
      if(fun_data_temp$candle_type[day] == "bullish") {
        range_stocka <-  round(seq(fun_data_temp$open[day], fun_data_temp$close[day], by = 0.1), digits = 1) } 
    
      if(fun_data_temp$candle_type[day] == "bearish") {
        range_stocka <-  round(seq(fun_data_temp$close[day], fun_data_temp$open[day], by = 0.1), digits = 1) } 
    
      if(fun_data_temp$candle_type[day+1] == "bullish") {
        range_stockb <-  round(seq(fun_data_temp$open[day+1], fun_data_temp$close[day+1], by = 0.1), digits = 1) } 
    
      if(fun_data_temp$candle_type[day+1] == "bearish") {
        range_stockb <-  round(seq(fun_data_temp$close[day+1], fun_data_temp$open[day+1], by = 0.1), digits = 1) }
    
      fun_data_temp$differing[day] <- length( setdiff(range_stocka, range_stockb)) / length(range_stocka)
    } else {fun_data_temp$differing[day] <- 0}
     
   }
    
  
  sum <- fun_data_temp %>% 
    summarise(strength = sum(candle_type == "bullish") /n(), 
              avg_body_diff = mean(differing), 
              diff_candle_tail =mean(tail_size_up) / mean(tail_size_down)
              )
  sum$candle_type <- fun_data_temp$candle_type[1]
  sum$date <- fun_data_temp$date[1]
  return(sum)
})
  
  return(pai)
  
}

###
stock_data <- stock_data %>% 
  group_by(symbol) %>% 
  nest() %>% 
  mutate(pai =  map(data, ~ pai_stregth(., time_period = 5))) %>% 
  unnest(cols = c(data, pai))

saveRDS(stock_data, file = here("01_data/stock_data_pai.RDS"))



