# данные по России
library(tidyverse)

source("scripts\\misc.R")

russia_load <- function(){
  
  first_date <- as.Date("6/03/20", format = "%d/%m/%y")
  reported_cases <- c(
    14, #7
    17, #8
    20, #9
    20, #10 
    28,
    34,
    45,
    59,
    63,
    93,
    114,
    147,
    199,
    253,
    301,
    367,
    438,
    495,
    658,
    840,
    1036,
    1264,
    1534,
    1836
  )
  new_cases <- reported_cases - c(14,reported_cases[1:length(reported_cases)-1])
  
  report_dates <- first_date + 1:length(reported_cases)
  
  predict_days <- 0
  
  base_model2 <- nls(reported_cases ~ start_cases*case_mult**(as.integer(report_dates-report_dates[1])), 
                     start = list(start_cases = 14, case_mult = 1.1))
  
  sum_base_model <- summary(base_model2)
  
  start_cases <- sum_base_model$coefficients["start_cases","Estimate"]
  
  case_mult <- sum_base_model$coefficients["case_mult","Estimate"]
  
  model_text <- paste("Модель: ", round(start_cases,1), 
                      " * ", round(case_mult,2), "^(число дней с начала)", sep = "")
  
  cases_data <- tibble(
    dates = report_dates,
    cases = reported_cases,
    new_cases = new_cases
  )
  
  model_cases <- start_cases*case_mult**(0:(length(reported_cases)-1+predict_days))
  model_dates <- first_date + 1:length(model_cases)
  
  model_data <- tibble(
    dates = model_dates,
    cases = model_cases
  )
  
  russia_data <- cases_data %>%  mutate(type = "Доклад") %>%
    bind_rows(model_data %>%
                mutate(type = "Модель")) %>% add_daily_percent()
  return(list(russia_data, model_text))
  
}

temp <- russia_load()

russia_data <- temp[[1]]

russia_model_text <- temp[[2]]

rm(russia_load, temp)