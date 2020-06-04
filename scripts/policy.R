library(tidyverse)
library(rio)
library(magrittr)
  policy_data <- import("data\\policy\\data\\OxCGRT_latest.csv")
  
  policy_data %<>% mutate(Date = as.Date(as.character(Date), format = "%Y%m%d")) %>% 
    filter(Date == as.Date("20200423", format = "%Y%m%d")) #%>% filter(CountryCode != "USA")
  
  #policy_data %>% ggplot(aes(y = factor(`C6_Stay at home requirements`), x = ConfirmedDeaths)) +
  #  geom_violin() + scale_x_log10() + theme_minimal()
  
library(wbstats)

  pop_data <- wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018)  %>% 
    as_tibble() %>% select(iso3c, value) %>% rename(code = iso3c)
  
  policy_pop <- left_join(policy_data, pop_data, 
                             by = c("CountryCode" = "code")) %>% 
    rename(Population = value)
  
  #policy_pop %>% select(CountryName, Population) %>% View()
 
library(ggrepel)
   
  policy_pop %<>% mutate(DeathPerMillion = ConfirmedDeaths/Population*1e6)
  
  policy_pop %>% filter(Population > 1e7) %>% ggplot(aes(x = StringencyIndexForDisplay, y = DeathPerMillion)) +
    geom_point(aes(color = factor(`C6_Stay at home requirements`))) +# geom_smooth(method = "loess") + 
    theme_minimal() + 
    scale_y_log10()+
    labs(
      y="Число подтвержденных смертей на миллион жителей", 
      x="Stringency Index, индекс строгости принятых мер", 
      title= paste("Зависимость смертности от строгости принятых мер"),
      caption = paste("tg: @covidcharts. Данные за 23/04/2020")) + theme(legend.title=element_blank())
  
  policy_pop %>% filter(Population > 1e7) %>% ggplot(aes(x = factor(`C6_Stay at home requirements`), y = DeathPerMillion)) +
    geom_violin() + #scale_y_log10() + 
    theme_minimal()