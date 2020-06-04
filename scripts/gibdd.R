library(tidyverse)
library(ggTimeSeries)

last_date <- as.Date("10/05/20", format = "%d/%m/%y")


dtp_death_raw <- c(
  33,
  35,
  24,
  26,#7
  30,
  25,
  23,
  34,
  26,
  21,#1.05
  37,#30.04
  12,
  15,
  15,
  36,
  25,
  19,
  13,
  16,
  13,
  34,#20
  20,
  17,
  16,
  20,
  18,#15
  12,
  25,
  22,
  27,
  NA,
  14,#9
  22,
  14,
  19,
  12,
  19,
  19,
  12,
  20,#1.04
  NA,#31
  14,#30
  33,
  30,
  25,
  17,
  12,#25
  17,
  25,
  28,
  27,
  14,
  17,
  29,
  18,
  20,
  33,
  39,
  24,
  NA,#12
  29,#11
  26,
  25,
  29,
  NA,#7
  18,#6
  23,
  24,
  18,
  19,
  24,#1.03
  23,#29
  40,
  35,
  19,
  NA,#25
  46,#24
  38,
  39,
  37,
  23,#20
  32,
  28,
  31,
  40,
  32,
  42,
  35,
  33,
  33,
  21,#10
  38,#9
  35,
  23,
  26,
  18,
  27,
  29,
  42,#2
  27, #1.2
  38,
  20,
  24,
  31,
  29,
  36,
  39,
  39,
  29,
  41
)

dates <- last_date - 0:(length(dtp_death_raw)-1)

data <- tibble(dates = rev(dates),
               deaths = rev(dtp_death_raw))

#source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
#calendarHeat(rev(dates),rev(dtp_death_raw), color = "g2r")

library(wesanderson)
pal <- wes_palette("Zissou1", 100, type = "continuous")

ggplot_calendar_heatmap_ru(data, "dates", "deaths")+ 
  #xlab("Месяц") + ylab(NULL) + 
  #scale_fill_continuous(type = "gradient",low="#1fed11", high="black") + 
  scale_fill_gradientn(colours = pal) + 
  labs(y=NULL, 
       x=NULL, 
       title= paste("Смертность в ДТП на дорогах России во время эпидемии COVID-19"),
       caption = paste("tg: @covidcharts. На основе данных twitter.com/dtp_stat"),
       fill = "Смерти")+
  theme( #axis.text = element_blank(), 
         #axis.text.x = element_text(c("Январь", "Февраль", "Март","Апрель")),
         axis.ticks = element_blank(), 
         #legend.position = 'none', 
         strip.background = element_blank(), 
         # strip.text = element_blank(), # useful if only one year of data 
         #plot.background = element_blank(), #panel.border = element_blank(), 
         panel.background = element_blank(), 
         #panel.grid = element_blank(), 
         #panel.border = element_blank() 
         ) 
