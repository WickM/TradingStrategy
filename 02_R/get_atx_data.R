# Skript mit welchen alle ATX Aktien und deren Daten heruntergeladen werden

library(tidyverse)
library(tidyquant)


atx_data  <- read.csv2("01_data/atx.csv")

atx_symbols <- tolower (atx_data$Symbol)

prices <- tq_get(x = atx_symbols,  get  = "stock.prices",
                 from = "1990-01-01",
                 to   = "2021-10-07")
