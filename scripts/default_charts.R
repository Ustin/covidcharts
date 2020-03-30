library(tidyverse)

source("data\\russia.R", encoding = "UTF-8")
source("data\\moscow.R", encoding = "UTF-8")
source("graphs\\simple_graph.R", encoding = "UTF-8")

russia_data %>% log_cases_graph("России", russia_model_text,2)
moscow_data %>% log_cases_graph("Москве", moscow_model_text,2)

moscow_data %>% daily_cases_graph("Москве")

