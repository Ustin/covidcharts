library(tidyverse)

source("data\\russia.R", encoding = "UTF-8")
source("data\\moscow.R", encoding = "UTF-8")
source("graphs\\simple_graph.R", encoding = "UTF-8")
source("scripts\\misc.R", encoding = "UTF-8")

moscow_data <- moscow_data %>% #filter(dates > as.Date("14/03/20", format = "%d/%m/%y")) %>% 
               add_25_percent()
russia_data <- russia_data %>% #filter(dates > as.Date("14/03/20", format = "%d/%m/%y")) %>% 
               add_25_percent()

moscow_data %>% filter(dates > as.Date("25/03/20", format = "%d/%m/%y"))%>% log_cases_graph("Москве", moscow_model_text,7)
russia_data %>% filter(dates > as.Date("25/03/20", format = "%d/%m/%y")) %>% log_cases_graph("России", russia_model_text,7)

#moscow_data %>% daily_cases_graph("Москве")
#russia_data %>% daily_cases_graph("России")

russia_data %>% drop_na(cases) %>% select(-model_cases_new) %>% combine_cases(moscow_data) %>% filter(dates >= as.Date("10/04/20", format = "%d/%m/%y"))  %>% combined_daily_cases_graph(dates_div = 7)  

russia_data %>% filter(dates > as.Date("05/04/20", format = "%d/%m/%y")) %>% daily_percent_graph("России")
moscow_data %>% filter(dates > as.Date("30/03/20", format = "%d/%m/%y")) %>% daily_percent_graph("Москве")

russia_data %>% filter(dates > as.Date("15/03/20", format = "%d/%m/%y")) %>% log_cases_graph_new(subtitle = "И продолжение тренда на десять дней")

ggsave("test.png")
