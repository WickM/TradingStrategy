#################################################x
#' Project:
#' Script purpose:
#' Mon Oct 25 22:37:04 2021
#' Author: Manuel Wick-Eckl 
#################################################x
options(stringsAsFactors = FALSE)


library(tidyquant)
library(tidyverse)
library(here)
library(slider)

source("02_R/global_functions.R")

dat_stock <- get_new_stock(path_stck_data = here::here("01_data/stocks.RData"), 
                           user = Sys.getenv('igc_api_user'), 
                           pw = Sys.getenv('igc_api_pw'))

low_freq <- dat_stock %>% 
  group_by(symbol) %>% 
  summarise(anz = n()) %>% 
  filter(anz < 100) %>% 
  pull(symbol)

#----

stk_indikatoren <- dat_stock %>% 
  filter(! symbol %in% low_freq) %>%
  filter(is.na(close) == FALSE) %>% 
  group_by(symbol) %>% 
  #MACD
  tq_mutate(select     = close, 
            mutate_fun = MACD, 
            nFast      = 12, 
            nSlow      = 26, 
            nSig       = 9, 
            maType     = EMA) %>% 
  mutate(madc_diff = macd - signal) %>% 
  #RSI
  tq_mutate(select     = close, 
            mutate_fun = RSI) %>% 
  #ROC
  tq_mutate(select     = close, 
            mutate_fun = ROC,
            n = 10) %>% 
  #BBands
  tq_mutate(select     = c(high, low, close), 
            mutate_fun = BBands) %>% 
  #ADX
  tq_mutate(select = c("high", "low", "close"),
            mutate_fun = ADX
  ) %>% 
  #SMA
  tq_mutate(select = c("close"),
            mutate_fun = SMA,
            n=20, 
            col_rename = "sma_20"
  ) %>% 
  ungroup() %>%
  group_by(symbol) %>% 
  #EMA
  # ungroug und group da ansonsten eine komische Fehlermeldung kommt
  tq_mutate(select = c("close"),
            mutate_fun = EMA,
            n=5,
            col_rename = "ema_5"
  ) %>% 
  arrange(desc(date)) %>% 
  # Crossing ema zu sma
  mutate(ema_diff = ema_5 - sma_20) %>% 
  mutate(cross_sm = slide_lgl(.x = ema_diff, .after = 1, .complete = TRUE, .f = ~ { ! (all(.x > 0) | all(.x < 0)) } )) %>% 
  mutate(cross_madc = slide_lgl(.x = madc_diff, .after = 1, .complete = TRUE, .f = ~ { ! (all(.x > 0) | all(.x < 0)) } )) %>%
  #dailyReturn
  tq_mutate(select = adjusted, dailyReturn) %>% 
  janitor::clean_names()

stk_indikatoren <- stk_indikatoren %>% 
  # % return
  mutate(p_dailyReturn =  round ((adjusted / (adjusted - daily_returns) - 1) *100, digits = 2)) %>% 
  arrange(symbol, desc(date))

ind <- which(stk_indikatoren$cross_sm == TRUE)
# Crossing sma aufwärts oder abwärts
stk_indikatoren$cross_sm[ind] <- ifelse(stk_indikatoren$sma_20[ind] > stk_indikatoren$ema_5[ind], "down", "up") 

ind <- which(stk_indikatoren$cross_madc == TRUE)

# Crossing madc aufwärts oder abwärts
stk_indikatoren$cross_madc[ind] <- ifelse(stk_indikatoren$madc_diff[ind] < 0, "down", "up")

stk_indikatoren <- stk_indikatoren %>% 
  filter(is.na(macd) == FALSE) %>% 
  select(-value)

companies <- stk_indikatoren %>% 
  select(name, symbol) %>% 
  distinct(name, symbol)

#----
saveRDS(stk_indikatoren, file = here("01_data/wrangled_stock_data.RDS"))
saveRDS(companies, file = here("01_data/companies.RDS"))
