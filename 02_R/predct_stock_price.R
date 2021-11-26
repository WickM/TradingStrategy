#################################################x
#' Project:
#' Script purpose: 
#' Sun Oct 24 11:33:18 2021
#' Author: Manuel Wick-Eckl 
#################################################x
options(stringsAsFactors = FALSE)


library(tidymodels)
library(tidyverse)
library(here)
library(corrr)
library(kernlab)
library(vip)

set.seed(123)

stock_data_raw <- read_rds(here("01_data/stock_data_pai.RDS")) %>% 
  select(-stock_prices, -date1)

stock_data <- stock_data_raw %>% 
  group_by(symbol) %>% 
  arrange(symbol, desc(date) ) %>% 
  mutate(stock_pred = lag(close, n = 1), 
         stock_pred = if_else(stock_pred > close, "up", "down")) %>% 
  ungroup() %>% 
  filter(is.na(stock_pred) == FALSE)

stock_split <- stock_data %>%
  initial_split(strata = stock_pred)

stock_train <- training(stock_split)
stock_test <- testing(stock_split)
stock_metrics <- metric_set(accuracy, roc_auc)


stock_folds <- vfold_cv(stock_train, strata = stock_pred)
stock_folds
names(stock_test)

stock_rec <- recipe(stock_pred ~ strength
                    + candle_type 
                    + avg_body_diff
                    + rsi 
                    + roc,
                    data = stock_train) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_naomit(all_predictors(), skip = TRUE)
  
prep(stock_rec)

xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), 
  min_n = tune(), 
  loss_reduction = tune(),          
  sample_size = tune(), 
  mtry = tune(),         
  learn_rate = tune(),
  stop_iter = tune()
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), stock_train),
  learn_rate(),
  stop_iter(range = c(10L, 50L)),
  size = 10
)
xgb_grid

stock_wf <- workflow(stock_rec, 
                     xgb_spec)

doParallel::registerDoParallel(cores = 4)

stock_rs <- tune_grid(
  stock_wf,
  stock_folds,
  grid = xgb_grid,
  metrics = stock_metrics
)
stock_rs

autoplot(stock_rs)
show_best(stopping_rs)

best_auc <- select_best(stock_rs, "roc_auc")
best_auc$stop_iter <- 1000

final_stock <- finalize_workflow(
  stock_wf,
  best_auc
)


stock_res <-  final_stock %>%
  fit_resamples(stock_folds)

collect_metrics(stock_res)

stock_res %>% 
  pull_workflow_fit() %>%
  vip(geom = "point")

final_res <- last_fit(stock_res, split = stock_split)
collect_metrics(final_res)
