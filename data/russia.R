# данные по России
library(tidyverse)

source(file.path("scripts/misc.R"), encoding = "UTF-8")

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
    1836,
    2337,
    2777,
    3548,
    4149,
    4731,
    5389,
    6343,
    7497,
    8672,
    10131,
    11917,
    13584,
    15770,
    18328,
    21102,
    24490,
    27938,
    32008,
    36793,
    42853,
    47121,
    52763,
    57999,
    62773,
    68622,
    74588,
    80949,
    87147,
    93558,
    99399,
    106498,
    114431,
    124054,
    134687,
    145268,
    155370,
    165929,
    177160,
    187859,
    198676,
    209688,
    221344,
    232243,
    242271,
    252245,
    262843,
    272043,
    281752,
    290678,
    299941,
    308705,
    317554,
    326448,
    335882,
    344481,
    353427,
    362342,
    370680,
    379051, 
    387623,
    396575, 
    405843,
    414878,
    423741,
    432277,
    441108,
    449834,
    458689,
    467673,
    476658,
    485253,
    493657,
    502436,
    511423,
    520129,
    528964,
    537210,
    545458,
    553301,
    561091,
    569063,
    576952,
    584680,
    592280,
    599705,
    606881,
    613994,
    620794,
    627646,
    634437,
    641156,
    647849,
    654405,
    661165,
    667883,
    674515,
    681251,
    687862,
    694230,
    700792,
    707301,
    713936,
    720547,
    727162,
    733699,
    739947,
    746369,
    752797,
    759203,
    765437,
    771546,
    777486,
    783328,
    789190,
    795038,
    800849,
    806720,
    812485,
    818120,
    823515 
  )
  predict_days <- 0
  
  
  new_cases <- reported_cases - c(14,reported_cases[1:length(reported_cases)-1])
  
  report_dates <- first_date + 1:(length(reported_cases))
  
  reported_cases_temp <- reported_cases[1:10]
  report_dates_temp <- report_dates[1:10]
  report_dates_first <- report_dates[1]
  base_model2 <- nls(reported_cases_temp ~ start_cases*case_mult**(as.integer(report_dates_temp-report_dates_first)), 
                     start = list(start_cases = 14, case_mult = 1.1))
  
  sum_base_model <- summary(base_model2)
  
  start_cases <- sum_base_model$coefficients["start_cases","Estimate"]
  
  case_mult <- sum_base_model$coefficients["case_mult","Estimate"]
  
  model_text <- paste("Модель: ", round(start_cases,1), 
                      " * ", round(case_mult,2), "^(число дней с начала)", sep = "")
  
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
    model_cases = c(model_cases)
  )
  
  data_model3 <- filter(cases_data, dates > as.Date("02/04/20", format = "%d/%m/%y")) %>% drop_na()
  
  cases3 <- data_model3$cases
  
  dates3 <- as.integer(data_model3$dates-data_model3$dates[1])
  
  base_model3 <- nls(cases3 ~ start_cases3*case_mult3**(dates3), 
                     start = list(start_cases3 = 4000, case_mult3 = 1.1))
  
  sum_base_model3 <- summary(base_model3)
  
  start_cases3 <- sum_base_model3$coefficients["start_cases3","Estimate"]
  
  case_mult3 <- sum_base_model3$coefficients["case_mult3","Estimate"]
  
  model_cases3 <- start_cases3*case_mult3**(0:(length(cases3)-1+predict_days))
  model_dates3 <- data_model3$dates[1] + 1:length(cases3)
  
  cases_data2 <- tibble(
    dates = model_dates,
    cases = c(reported_cases, rep(NA,predict_days)),
    new_cases = c(new_cases, rep(NA,predict_days)),
    model_cases = c(model_cases),
    model_cases_new = c(rep(NA, (length(model_dates)-length(model_cases3))),model_cases3)
  )
  
  russia_data <- cases_data2 %>%  #mutate(type = "Доклад") %>%
    #bind_rows(model_data %>%
    #            mutate(type = "Модель")) %>% 
    add_daily_percent()# %>% add_25_percent()
  return(list(russia_data, model_text))
  
}

temp <- russia_load()

russia_data <- temp[[1]]

russia_model_text <- temp[[2]]

rm(russia_load, temp)