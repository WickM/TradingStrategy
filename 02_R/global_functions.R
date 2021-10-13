


get_new_stock <- function(path_stck_data, user, pw) {
  
  require(tidyquant)
  require(tidyverse)
  require(glue)
  
  require(curl)
  require(rlang)
  
  #setProxy
  
  new_handle_plain = curl::new_handle
  
  new_handle_ntlm = function(){
    handle = new_handle_plain()
    handle_setopt(handle, .list = list(PROXYUSERPWD = glue("{user}:{pw}")))
    return(handle)
  }
  
  rlang::env_unlock(env = asNamespace('curl'))
  rlang::env_binding_unlock(env = asNamespace('curl'))
  assign('new_handle', new_handle_ntlm, envir = asNamespace('curl'))
  rlang::env_binding_lock(env = asNamespace('curl'))
  rlang::env_lock(asNamespace('curl'))
  
  Sys.setenv("http_proxy" = curl::ie_get_proxy_for_url("http://orf.at"))
  Sys.setenv("https_proxy" = curl::ie_get_proxy_for_url("http://orf.at"))
  
  #get stocks
  load(path_stck_data)
  
  symbols <- map_chr(companies, ~ pluck(.x, "symbols", 1, "yahoo", .default = NA))
  from_date <- max(dadat_stock$date)
  
  dadat_stock_new <- tidyquant::tq_get(symbols,
                                       get  = "stock.prices",
                                       from =  from_date,
                                       to   = Sys.Date())
  
  dadat_stock <- bind_rows(dadat_stock, dadat_stock_new)
  
  stock_name <-tibble(name = map_chr(companies, ~ pluck(.x, "name", .default = NA)),
                      symbol =  map_chr(companies, ~ pluck(.x, "symbols", 1, "yahoo", .default = NA),),
                      country =  map_chr(companies, ~ pluck(.x, "country", .default = NA))
  )
  
  dadat_stock$name <- stock_name$name [match(dadat_stock$symbol, stock_name$symbol)]
  dadat_stock$country <- stock_name$country [match(dadat_stock$symbol, stock_name$symbol)]
  
  save(dadat_stock, companies, file = path_stck_data)
  
  return(dadat_stock)
}
#dat_stock <<- get_new_stock(path_stck_data = here::here("01_data/stocks.RData"), user = Sys.getenv('igc_api_user'), pw = Sys.getenv('igc_api_pw'))
