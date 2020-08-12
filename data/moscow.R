library(tidyverse)
source(file.path("scripts/misc.R"), encoding = "UTF-8")

moscow_load <- function(){
  first_date <- as.Date("5/03/20", format = "%d/%m/%y")
  reported_cases_new_wiki <- c(
    5,
    0,
    0, #8
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
    212,
    387,
    267,
    595,
    448,
    434,
    536,
    591,
    697,
    660,
    857,
    1124,
    1030,
    1306,
    1355,
    1489,
    1774,
    1370,
    1959,
    2649,
    3570,
    2026,
    3083,
    2548,
    1959,
    2957,
    2612,
    2971,
    2871,
    3075,
    2220, 
    3093,
    3561,
    5358,
    5948,
    5795,
    5714,
    5858,
    6703,
    5846,
    5667,
    5551,
    6169,
    5392,
    4703,
    4712,
    4748,
    3505,
    3855,
    3238,
    3545,
    2699,
    2913,
    2988,
    3190,
    2516,
    2560,
    2830,
    2140,
    2054,
    2332,
    2367,
    2596, 
    2297,
    2286,
    1842,
    1998,
    1855,
    1992,
    1956,
    2001,
    1572,
    1195,
    1436,
    1714,
    1493,
    1477,
    1359,
    1416,
    1065,
    1040,
    1136,
    1057,
    968,
    1068,
    1081,
    811,
    885,
    813,
    750,
    717,
    782,
    745,
    611,
    662,
    659,
    680,
    650,
    685,
    629,
    621,
    568,
    637,
    678,
    679,
    672,
    613,
    628,
    531,
    575,
    578,
    591,
    578,
    602,
    638,
    608,
    645,
    648,
    683,
    694,
    674,
    671,
    678,
    695,
    690,
    664,
    693,
    691,
    687,
    684,
    686,
    691,
    689,
    694,
    694,
    689
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
  
  #cases_data <- tibble(
  #  dates = report_dates,
  #  cases = reported_cases,
  #  new_cases = new_cases
  #)
  
  model_cases <- start_cases*case_mult**(0:(length(reported_cases)-1+predict_days))
  model_dates <- first_date + 1:length(model_cases)
  
  #model_data <- tibble(
  #  dates = model_dates,
  #  cases = model_cases
  #)
  
  cases_data <- tibble(
    dates = model_dates,
    cases = c(reported_cases, rep(NA,predict_days)),
    new_cases = c(new_cases, rep(NA,predict_days)),
    model_cases = model_cases
  )
  
  moscow_data <- cases_data %>%  
    add_daily_percent() #%>% add_25_percent()
  
  return(list(moscow_data, model_text))
  
}

temp <- moscow_load()

moscow_data <- temp[[1]]

moscow_model_text <- temp[[2]]

rm(moscow_load, temp)