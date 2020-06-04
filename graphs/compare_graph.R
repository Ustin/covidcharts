library(tidyverse)
library(scales)
library(ggrepel)

source("data\\world.R", encoding = "UTF-8")

my_breaks <-c(10**(1:6),5*10**(0:5))
my_minor_breaks <- rep(1:10,4)*c(rep(10,10),rep(100,10),rep(1000,10),rep(10000,10))

max_date <- world_data100 %>% filter(country == "Japan") %>% slice(date_adj,n()) %>% select(date_adj) %>% .[[1]]
min_date <- world_data100 %>% filter(country == "Russia") %>% slice(date_adj,1) %>% select(date_adj) %>% .[[1]]

max_date_russia <- world_data100 %>% filter(country == "Russia") %>% slice(date_adj,n()) %>% select(date_adj) %>% .[[1]]

show_for <- c("Russia", "United Kingdom", 
              "Finland", "Iran",
              
              "Netherlands", "US", 
              "Italy", "Spain",
              #"Japan", 
              "Germany", "France", 
              "Israel",
              "Sweden", "Ukraine",#, 
              "Belarus"
              #"Turkey"#,
              #"China"
              )
  
#show_for <- c("Sweden", "Finland", "Norway", "Russia")
label_data_end <- world_data100 %>% filter(country %in% show_for) %>% group_by(country) %>% 
  slice(date_adj,n()) %>% ungroup()

world_data100_russia <- world_data100 %>% filter(country == "Russia")
world_data100_others <- world_data100 %>% filter(country %in% show_for)


world_data100 %>% filter(country %in% show_for) %>% 
  ggplot(aes(x=date_adj, y=cases, color = country)) + 
  scale_y_log10(limits = c(100,max(world_data100_others$cases)*1.33), 
                breaks = my_breaks, minor_breaks = my_minor_breaks,
                labels = function(x) format(x, scientific = F)) +
  annotation_logticks(scaled = T,base = 10,  
                      short = unit(.5,"mm"),
                      mid = unit(3,"mm"),
                      long = unit(4,"mm")) +
  geom_line(data = world_data100_russia, size = 2) + geom_point(data =world_data100_russia, size = 4) +
  geom_line(data = world_data100_others, size = 1) + geom_point(data = world_data100_others, size = 1) +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(data = label_data_end, aes(label = paste(country, -dates_diff, "days")), 
                   direction = "both", nudge_y = 0.2, nudge_x = 1, 
                   fontface = "bold") + 
  labs(y="Случаи", 
       x="Даты для России", 
       title= paste("Сравнение развития эпидемии COVID-19 в России и других странах"),
       subtitle = "Начиная с первого дня с >100 случаев. Даты для России, остальные страны смещены на указанное число дней",
       caption = paste("telegram: @covidcharts. Данные за", max_date_russia)) +theme_light()+
  scale_x_date(breaks=pretty_breaks(n = as.numeric(max_date-min_date)/4), 
               limits = c(min_date, max_date + 3.5))
