library(tidyverse)

add_daily_percent <- function(data) {
  
  return(data %>% mutate(daily_percent = ifelse(cases != new_cases, new_cases/(cases - new_cases)*100,0) ) )
}

