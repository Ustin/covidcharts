library(tidyverse)
source("scripts\\misc.R")

moscow_load <- function(){
  first_date <- as.Date("7/03/20", format = "%d/%m/%y")
  reported_cases_new_wiki <- c(
    5, #8
    3, #9
    0,
    6,
    4,
    5,
    9,
    0,
    20,
    4,
    31,
    12,
    33,
    6,
    54,
    71,
    28,
    120,
    136,
    157,
    114,
    197,
    212
  )
  
  reported_cases_wiki <-cumsum(reported_cases_new_wiki)
  
  reported_cases <- reported_cases_wiki
  
  new_cases <- reported_cases_new_wiki
  
  predict_days <- 0
  
  report_dates <- first_date + 1:length(reported_cases)
  
  # модель для экспоненциального роста
  #base_model <- nls(reported_cases ~ offset_cases + start_cases*case_mult**(as.integer(report_dates-report_dates[1])), 
  #                  start = list(offset_cases = 0, start_cases = 14, case_mult = 1.1))
  
  base_model2 <- nls(reported_cases ~ start_cases*case_mult**(as.integer(report_dates-report_dates[1])), 
                     start = list(start_cases = 14, case_mult = 1.1))
  
  
  sum_base_model <- summary(base_model2)
  
  #offset_cases <- sum_base_model$coefficients["offset_cases","Estimate"]
  
  start_cases <- sum_base_model$coefficients["start_cases","Estimate"]
  
  case_mult <- sum_base_model$coefficients["case_mult","Estimate"]
  
  model_text <- paste("Модель:", round(start_cases,1), 
                      "*", round(case_mult,2), "^(число дней с начала)")
  
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
  
  moscow_data <- cases_data %>%  mutate(type = "Доклад") %>%
    bind_rows(model_data %>%
                mutate(type = "Модель")) %>% add_daily_percent()
  
  return(list(moscow_data, model_text))
  
}

temp <- moscow_load()

moscow_data <- temp[[1]]

moscow_model_text <- temp[[2]]

rm(moscow_load, temp)