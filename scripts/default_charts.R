library(tidyverse)

source("data\\russia.R", encoding = "UTF-8")
source("data\\moscow.R", encoding = "UTF-8")
source("graphs\\simple_graph.R", encoding = "UTF-8")

russia_data <- russia_data %>% filter(dates > as.Date("13/03/20", format = "%d/%m/%y"))
moscow_data <- moscow_data %>% filter(dates > as.Date("13/03/20", format = "%d/%m/%y"))

moscow_data %>% log_cases_graph("Москве", moscow_model_text,2)
russia_data %>% log_cases_graph("России", russia_model_text,2)

moscow_data %>% daily_cases_graph("Москве")
russia_data %>% daily_cases_graph("России")

moscow_data %>% daily_percent_graph("Москве")
russia_data %>% daily_percent_graph("России")
