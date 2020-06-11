library(rio)
library(tidyverse)
library(wesanderson)

moscow_mortality_raw <- import("data\\data-6267-2020-05-08.csv")

moscow_mortality <- moscow_mortality_raw %>% select(Year, Month, StateRegistrationOfDeath) %>% 
  rename(Deaths = StateRegistrationOfDeath) %>% add_row(Year = 2020, Month = "Май", Deaths = 15713) 

mounth_names <-  c("Январь", "Февраль",  "Март", "Апрель", "Май", "Июнь", 
                   "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь", "Декабрь")

temp <- factor(moscow_mortality$Month, levels = mounth_names)

moscow_mortality$Month <- temp

temp <- factor(moscow_mortality$Year)

moscow_mortality$Year <- temp

mycolors <- c("#c64c71",
              "#5cb54e",
              "#b854bb",
              "#b0b343",
              "#6c6cc9",
              "#c48a42",
              "#5b9ed4",
              "#52b38e",
              "#c57eb5",
              "#647b37",
              "#ca593f")

moscow_mortality %>% ggplot(aes(x = Month, y = Deaths, group = Year, color = Year)) +
  geom_line(size = c(rep(1.5,120),rep(3,5))) + ylim(c(7500,16000)) + 
  theme_minimal() + scale_color_manual(values = mycolors)+
  geom_point(size = 4) + theme(plot.title = element_text(size=25), axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 12)) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 12))+ 
  theme(panel.grid.major = element_line(color = "#C0C0C0")) +
  theme(plot.caption = element_text(size = 12))+
  labs(x = "", y = "Смерти от всех причин",
       caption = "telegram: @covidcharts\n\nИсточники: Портал открытых данных правительства Москвы, Депздрав Москвы",
       title = "Смертность в Москве от всех причин")+ labs(color = "Год")

ggsave("moscowdeaths.png")