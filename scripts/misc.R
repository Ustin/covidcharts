library(tidyverse)

add_daily_percent <- function(data) {
  
  return(data %>% mutate(daily_percent = ifelse(cases != new_cases, new_cases/(cases - new_cases)*100,0) ) )
}

add_25_percent <- function(data){
  min_date <- data %>% select(dates) %>% .[[1]] %>% min()
  min_num <- data %>% select(cases) %>% .[[1]] %>% min()
  return(data %>% mutate(p25 = min_num*1.25**(as.numeric(dates-min_date))))
}

combine_cases <- function(russia_data, moscow_data){
  out_data <- bind_rows(list(russia_data, moscow_data), .id = "type") %>% 
    mutate(name = ifelse(type == "2", "Москва", "Россия_full")) %>% 
    filter(dates > as.Date("13/03/20", format = "%d/%m/%y")) %>%
    select(-cases,-model_cases,-daily_percent, -type, -p25) %>%
    pivot_wider(names_from = name,values_from = new_cases) %>%
    mutate(Россия = Россия_full - Москва) %>% select(-Россия_full) %>% select(dates,Россия, Москва) %>%
    pivot_longer(-dates,names_to = "type", values_to = "cases")
  return(out_data)
}

