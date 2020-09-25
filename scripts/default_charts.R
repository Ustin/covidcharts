library(tidyverse)

source(file.path("data/russia.R"), encoding = "UTF-8")
source(file.path("data/moscow.R"), encoding = "UTF-8")
source(file.path("graphs/simple_graph.R"), encoding = "UTF-8")
source(file.path("scripts/misc.R"), encoding = "UTF-8")

moscow_data <- moscow_data %>% #filter(dates > as.Date("14/03/20", format = "%d/%m/%y")) %>% 
               add_25_percent()
russia_data <- russia_data %>% #filter(dates > as.Date("14/03/20", format = "%d/%m/%y")) %>% 
               add_25_percent()

#moscow_data %>% filter(dates > as.Date("25/03/20", format = "%d/%m/%y"))%>% log_cases_graph("Москве", moscow_model_text,7)
#russia_data %>% filter(dates > as.Date("25/03/20", format = "%d/%m/%y")) %>% log_cases_graph("России", russia_model_text,7)

moscow_data %>% daily_cases_graph("Москве",14)
#russia_data %>% daily_cases_graph("России")
ggsave("moscow.png", width = 12, height = 8)

russia_data %>% drop_na(cases) %>% select(-model_cases_new) %>% combine_cases(moscow_data) %>% filter(dates >= as.Date("10/04/20", format = "%d/%m/%y"))  %>% combined_daily_cases_graph(dates_div = 30)  

#russia_data %>% filter(dates > as.Date("05/04/20", format = "%d/%m/%y")) %>% daily_percent_graph("России")
#moscow_data %>% filter(dates > as.Date("30/03/20", format = "%d/%m/%y")) %>% daily_percent_graph("Москве")

#moscow_data %>% filter(dates > as.Date("15/03/20", format = "%d/%m/%y")) %>% log_cases_graph_new(subtitle = "И продолжение тренда на десять дней")

ggsave("russia.png", width = 12, height = 8)
