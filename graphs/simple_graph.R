library(tidyverse)
library(scales)
library(viridis)

#source("data\\russia.R", encoding = "UTF-8")

#data <- russia_data

log_cases_graph <- function(data, name = "России", subtitle = "В логарифмическом масштабе", dates_div = 2, model = T){
  my_breaks <-c(10**(1:6),5*10**(0:5))
  my_minor_breaks <- rep(1:10,4)*c(rep(10,10),rep(100,10),rep(1000,10),rep(10000,10))
  
  model_dates_num <-  data %>%  select(dates) %>% .[[1]] %>% length()
  max_date <- data %>% select(dates) %>% .[[1]] %>% max()
  
  gg_points <- data %>%  
    ggplot(aes(x=dates, y=cases, color = "Данные"), log = "y") + 
    scale_y_log10(limits = c(min(data$cases)/1.33,max(data$cases)*2), breaks = my_breaks, minor_breaks = my_minor_breaks, labels = function(x) format(x, scientific = F)) +
    scale_x_date(breaks=pretty_breaks(n = model_dates_num/dates_div)) + 
    annotation_logticks(scaled = T, base = 10,  short = unit(.5,"mm"),
                        mid = unit(3,"mm"),
                        long = unit(4,"mm")) +
    geom_point(size = 5) + geom_line(size = 2.5) +
    labs(
      y="Случаи", 
      x="День", 
      title= paste("Подтвержденные случаи заражения COVID-19 в", name),
      #subtitle = subtitle,
      caption = paste("tg: @covidcharts. Обновление за", max_date)) + 
    theme(legend.title=element_blank()) +  theme(legend.position='none') 
  
  #(gg_points)
  
  #if (model){
    #gg_line <- data %>% #filter(type == "Модель") %>% 
    #  geom_line(mapping = aes(x=dates, y=model_cases, color = "Модель"),size = 1.5)  
    
    gg_line_25 <- data %>% #
      geom_line(mapping = aes(x=dates, y=p25, color = "25% увеличение"),size = 1.5)  
    
    gg_plot <- gg_points+ gg_line_25 +# gg_line  + 
      theme_light() + scale_color_manual(values=c("#00BFC4","#F8766D",  "#003d3f"))+
      theme(legend.title = element_blank()) + 
      theme(legend.position = c(0.8, 0.3)) + 
      theme(legend.background = element_rect(fill="lightblue", 
                                             size=0.5, linetype="solid"))
    
    
  #} else {
  #  gg_plot <- gg_points + theme_light() + 
  #    theme(legend.position = element_blank()) 
  #}
  return(gg_plot)
  
}

#log_cases_graph(russia_data)

daily_cases_graph <- function(data, name = "Москве", dates_div = 2){
  max_date <- data %>% select(dates) %>% .[[1]] %>% max()
  length_dates <- data %>% select(dates) %>% .[[1]] %>% length()
  
  data %>% #filter(type == "Доклад") %>%
    ggplot(aes(x = dates, y = new_cases, color)) + geom_col() + 
    scale_x_date(breaks=pretty_breaks(n = length_dates/dates_div)) + 
    labs(
      y="Новые случаи", 
      x="День", 
      title=paste("Новые случаи заражения COVID-19 в", name),
      caption = paste("tg: @covidcharts.", max(max_date))) + theme_light()
}


daily_percent_graph <- function(data, name = "Москве", dates_div = 2){
  max_date <- data %>% select(dates) %>% .[[1]] %>% max()
  length_dates <- data %>% select(dates) %>% .[[1]] %>% length()
  
  data %>% #filter(type == "Доклад") %>%
    ggplot(aes(x = dates, y = daily_percent, color)) + geom_col() + 
    scale_x_date(breaks=pretty_breaks(n = length_dates/dates_div)) + 
    geom_text(aes(label=paste(round(daily_percent,1),"",sep = "")), vjust=-0.5, color="black")+
    labs(
      y="Процент новых случаев от существущих", 
      x="День", 
      title=paste("Процент новых случаев заражения от существующих случаев в", name),
      caption = paste("tg: @covidcharts.", max(max_date))) + theme_light()
}

combined_daily_cases_graph <- function(data, dates_div = 2){
  max_date <- data %>% select(dates) %>% .[[1]] %>% max()
  length_dates <- data %>% select(dates) %>% .[[1]] %>% length()
  
  data %>% 
    ggplot(aes(x = dates, y = cases, fill = factor(type, levels = c("Москва", "Россия")))) + 
    geom_bar(stat="identity", position = "stack") + 
    scale_x_date(breaks=pretty_breaks(n = length_dates/dates_div)) + 
    #geom_text(aes(y = cases, label=cases), vjust=-0.5, color="black")+
    labs(
      y="Новые случаи", 
      x="День", 
      title=paste("Новые случаи заражения COVID-19 в Москве и остальной России"),
      caption = paste("tg: @covidcharts.", max(max_date))) + theme_light() +
    theme(legend.title = element_blank())
}

log_cases_graph_new <- function(data, name = "России", subtitle = "В логарифмическом масштабе", dates_div = 2){
  my_breaks <-c(10**(1:6),5*10**(0:5))
  my_minor_breaks <- rep(1:10,4)*c(rep(10,10),rep(100,10),rep(1000,10),rep(10000,10))
  
  model_dates_num <-  data %>%  select(dates) %>% .[[1]] %>% length()
  max_date <- data %>% select(dates) %>% .[[1]] %>% max()
  min_date <- data %>% select(dates) %>% .[[1]] %>% min()
  
  gg_points <- data %>%  
    ggplot(aes(x=dates, y=cases, color = "Данные"), log = "y") + 
    scale_y_log10(limits = c(min(data$cases)/1.33,max(data$cases)*2), breaks = my_breaks, minor_breaks = my_minor_breaks, labels = function(x) format(x, scientific = F)) +
    scale_x_date(breaks=pretty_breaks(n = model_dates_num/2), limits = c(min_date, max_date)) + 
    annotation_logticks(scaled = T, base = 10,  short = unit(.5,"mm"),
                        mid = unit(3,"mm"),
                        long = unit(4,"mm")) +
    geom_point(size = 5) + geom_line(size = 2.5) +
    labs(
      y="Случаи", 
      x="День", 
      title= paste("Подтвержденные случаи заражения COVID-19 в", name),
      subtitle = subtitle,
      caption = paste("tg: @covidcharts. Обновление за", max_date)) + 
    theme(legend.title=element_blank()) +  theme(legend.position='none') 
  
  #(gg_points)
  
  #if (model){
  gg_line <- data %>% #filter(type == "Модель") %>% 
    geom_line(mapping = aes(x=dates, y=model_cases_new, color = "Модель"),size = 1.5)  
  
  #gg_line_25 <- data %>% #
  #  geom_line(mapping = aes(x=dates, y=p25, color = "25% увеличение"),size = 1.5)  
  
  gg_plot <- gg_points + gg_line  + theme_light() + scale_color_manual(values=c("#00BFC4","#F8766D",  "#003d3f"))+
    theme(legend.title = element_blank()) + 
    theme(legend.position = c(0.8, 0.3)) + 
    theme(legend.background = element_rect(fill="lightblue", 
                                           size=0.5, linetype="solid"))
  
  
  #} else {
  #  gg_plot <- gg_points + theme_light() + 
  #    theme(legend.position = element_blank()) 
  #}
  return(gg_plot)
  
}
