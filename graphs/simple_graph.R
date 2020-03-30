library(tidyverse)
library(scales)

#source("data\\russia.R", encoding = "UTF-8")

#data <- russia_data

log_cases_graph <- function(data, name = "России", subtitle = "В логарифмическом масштабе", dates_div = 2, model = T){
  my_breaks <-c(10**(1:6),5*10**(0:5))
  my_minor_breaks <- rep(1:10,4)*c(rep(10,10),rep(100,10),rep(1000,10),rep(10000,10))
  
  model_dates_num <-  data %>%  filter(type == "Модель") %>% select(dates) %>% .[[1]] %>% length()
  max_date <- data %>%  filter(type == "Доклад") %>% select(dates) %>% .[[1]] %>% max()
  
  gg_points <- data %>%  filter(type == "Доклад") %>% 
    ggplot(aes(x=dates, y=cases, color = type), log = "y") + 
    scale_y_log10(limits = c(min(data$cases)/1.33,max(data$cases)*1.33), breaks = my_breaks, minor_breaks = my_minor_breaks, labels = function(x) format(x, scientific = F)) +
    scale_x_date(breaks=pretty_breaks(n = model_dates_num/2)) + 
    annotation_logticks(scaled = T, base = 10,  short = unit(.5,"mm"),
                        mid = unit(3,"mm"),
                        long = unit(4,"mm")) +
    geom_point(size = 4) + geom_line(size = 2) +
    labs(
      y="Случаи", 
      x="День", 
      title= paste("Подтвержденные случаи заражения COVID-19 в", name),
      subtitle = subtitle,
      caption = paste("tg: @covidcharts. Обновление за", max_date)) + 
    theme(legend.title=element_blank()) +  theme(legend.position='none') 
  
  #(gg_points)
  
  if (model){
    gg_line <- data %>% filter(type == "Модель") %>% 
      geom_line(mapping = aes(x=dates, y=cases, color = type),size = 1.5)  
    
    gg_plot <- gg_points + gg_line + theme_light() + 
      theme(legend.title = element_blank()) + 
      theme(legend.position = c(0.8, 0.3)) + 
      theme(legend.background = element_rect(fill="lightblue", 
                                             size=0.5, linetype="solid"))
    
  } else {
    gg_plot <- gg_points + theme_light() + 
      theme(legend.position = element_blank()) 
  }
  return(gg_plot)
  
}

#log_cases_graph(russia_data)

daily_cases_graph <- function(data, name = "Москве", dates_div = 2){
  max_date <- data %>% filter(type == "Доклад") %>% select(dates) %>% .[[1]] %>% max()
  length_dates <- data %>% filter(type == "Доклад") %>% select(dates) %>% .[[1]] %>% length()
  
  data %>% filter(type == "Доклад") %>%
    ggplot(aes(x = dates, y = new_cases)) + geom_col() + 
    scale_x_date(breaks=pretty_breaks(n = length_dates/dates_div)) + 
    labs(
      y="Новые случаи", 
      x="День", 
      title=paste("Новые случаи заражения COVID-19 в", name),
      caption = paste("tg: @covidcharts.", max(max_date))) + theme_light()
}


