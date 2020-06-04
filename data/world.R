library(rio)
library(tidyverse)

gen_world_data100 <- function(){
  source("data\\russia.R", encoding = "UTF-8")
  
  world_data <- import("data\\COVID-19\\data\\cases_time.csv") %>% tibble() %>% .[[1]] %>%
    dplyr::rename(country = Country_Region, cases = Confirmed, new_cases = Delta_Confirmed ) %>%
    mutate(dates = as.Date(Last_Update, format = "%m/%d/%y")) %>% 
    replace_na(list(Cases = 0, Recovered = 0)) %>%
    select(c(-Active,-Delta_Recovered, -Last_Update)) %>% 
    select(dates, everything()) %>% filter(Province_State == "")
  
  
  russia_date100 <- as.Date("16/03/20", format = "%d/%m/%y")
  
  russia_data100 <- russia_data %>% mutate(country = "Russia")  %>% select(country, dates, cases) %>%
    mutate(date100 = russia_date100, date_adj = dates, dates_diff = russia_date100 - russia_date100) %>%
    filter(cases > 100)
  
  russia_datemax <- russia_data100 %>% slice(dates,n()) %>% select(dates) %>% .[[1]]
  
  first_date <- world_data  %>% filter(country != "Russia") %>% select(country, dates, cases) %>%
    filter(cases > 100) %>% group_by(country) %>% slice(dates,1) %>% ungroup() %>% 
    select(-cases) %>% rename(date100 = dates)
  
  world_data100 <- world_data %>% inner_join(first_date, by = "country") %>% 
    mutate(date_adj = dates - date100 + russia_date100 + 1) %>% 
    mutate(dates_diff = date100 - russia_date100) %>% 
    filter(date_adj > russia_date100, date_adj < russia_datemax + 20) %>%
    select(dates, country, cases, date100, date_adj,dates_diff)
  
  
  full_data100 <- bind_rows(russia_data100, world_data100)
  
  return(full_data100)
}

world_data100 <- gen_world_data100()
