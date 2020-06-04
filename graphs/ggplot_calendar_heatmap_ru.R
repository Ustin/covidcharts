library(data.table)
ggplot_calendar_heatmap_ru <- function (dtDateValue, cDateColumnName = "", cValueColumnName = "", 
          vcGroupingColumnNames = "Year", dayBorderSize = 0.25, 
          dayBorderColour = "black", monthBorderSize = 2, monthBorderColour = "black", 
          monthBorderLineEnd = "round") 
{
  Year <- ""
  MonthOfYear <- ""
  WeekOfYear <- ""
  DayOfWeek <- ""
  as.formula <- ""
  MonthChange <- ""
  meanWeekOfYear <- ""
  dtDateValue <- copy(data.table(dtDateValue))
  dtDateValue[, `:=`(Year, as.integer(strftime(get(cDateColumnName), 
                                               "%Y")))]
  vcGroupingColumnNames <- unique(c(vcGroupingColumnNames, 
                                    "Year"))
  dtDateValue <- merge(dtDateValue, setnames(dtDateValue[, 
                                                         list(DateCol = seq(min(get(cDateColumnName)), max(get(cDateColumnName)), 
                                                                            "days")), vcGroupingColumnNames], "DateCol", 
                                             cDateColumnName), c(vcGroupingColumnNames, cDateColumnName), 
                       all = T)
  dtDateValue[, `:=`(MonthOfYear, as.integer(strftime(get(cDateColumnName), 
                                                      "%m")))]
  dtDateValue[, `:=`(WeekOfYear, 1 + as.integer(strftime(get(cDateColumnName), 
                                                         "%W")))]
  dtDateValue[, `:=`(DayOfWeek, as.integer(strftime(get(cDateColumnName), 
                                                    "%w")))]
  dtDateValue[DayOfWeek == 0L, `:=`(DayOfWeek, 7L)]
  ggplotcalendar_heatmap <- ggplot(data = dtDateValue[, list(WeekOfYear, 
                                                             DayOfWeek)], aes(x = WeekOfYear, y = DayOfWeek)) + geom_tile(data = dtDateValue, 
                                                                                                                          aes_string(fill = cValueColumnName), color = dayBorderColour, 
                                                                                                                          size = dayBorderSize) + coord_fixed() + xlab("Month") + 
    ylab("DoW") + facet_wrap(as.formula(paste("~", 
                                              paste(vcGroupingColumnNames, collapse = "+"))))
  setkeyv(dtDateValue, c(vcGroupingColumnNames, "DayOfWeek", 
                         "WeekOfYear", "MonthOfYear"))
  dtDateValue[, `:=`(MonthChange, c(1, diff(MonthOfYear))), 
              c(vcGroupingColumnNames, "DayOfWeek")]
  dtMonthChangeDatasetBetweenWeeks <- dtDateValue[MonthChange == 
                                                    1]
  dtMonthChangeDatasetBetweenWeeks[, `:=`(WeekOfYear, 
                                          WeekOfYear - 0.5)]
  dtMonthChangeDatasetBetweenWeeks <- rbind(dtMonthChangeDatasetBetweenWeeks[, 
                                                                             c("DayOfWeek", "WeekOfYear", vcGroupingColumnNames), 
                                                                             with = F], dtDateValue[, list(WeekOfYear = 0.5 + max(WeekOfYear)), 
                                                                                                    c(vcGroupingColumnNames, "DayOfWeek")])
  if (nrow(dtMonthChangeDatasetBetweenWeeks) > 0) {
    ggplotcalendar_heatmap <- ggplotcalendar_heatmap + geom_segment(data = dtMonthChangeDatasetBetweenWeeks, 
                                                                    aes(x = WeekOfYear, xend = WeekOfYear, y = DayOfWeek - 
                                                                          0.5, yend = DayOfWeek + 0.5), size = monthBorderSize, 
                                                                    colour = monthBorderColour, lineend = monthBorderLineEnd)
  }
  setkeyv(dtDateValue, c(vcGroupingColumnNames, "WeekOfYear", 
                         "DayOfWeek", "MonthOfYear"))
  dtDateValue[, `:=`(MonthChange, c(1, diff(MonthOfYear))), 
              vcGroupingColumnNames]
  MonthChangeDatasetWithinWeek <- dtDateValue[MonthChange == 
                                                1 & (DayOfWeek != 1)]
  MonthChangeDatasetWithinWeek[, `:=`(DayOfWeek, DayOfWeek - 
                                        0.5)]
  MonthChangeDatasetWithinWeek <- rbind(MonthChangeDatasetWithinWeek[, 
                                                                     c("DayOfWeek", "WeekOfYear", vcGroupingColumnNames), 
                                                                     with = F], dtDateValue[, list(DayOfWeek = c(min(DayOfWeek) - 
                                                                                                                   0.5, max(DayOfWeek) + 0.5)), c(vcGroupingColumnNames, 
                                                                                                                                                  "WeekOfYear")])
  if (nrow(MonthChangeDatasetWithinWeek) > 0) {
    ggplotcalendar_heatmap <- ggplotcalendar_heatmap + geom_segment(data = MonthChangeDatasetWithinWeek, 
                                                                    aes(x = WeekOfYear - 0.5, xend = WeekOfYear + 0.5, 
                                                                        y = DayOfWeek, yend = DayOfWeek), size = monthBorderSize, 
                                                                    colour = monthBorderColour, lineend = monthBorderLineEnd)
  }
  dtMonthLabels <- dtDateValue[, list(meanWeekOfYear = mean(WeekOfYear)), 
                               by = c("MonthOfYear")]
  
  dtMonthLabels[, `:=`(MonthOfYear, month.abb[MonthOfYear])]
  ggplotcalendar_heatmap <- ggplotcalendar_heatmap + scale_x_continuous(breaks = dtMonthLabels[, 
                                                                                               meanWeekOfYear], labels = c("Январь", "Февраль", "Март", "Апрель", "Май"),#dtMonthLabels[, MonthOfYear], 
                                                                        expand = c(0, 0)) + scale_y_continuous(trans = "reverse", 
                                                                                                               breaks = c(1:7), labels = c("Пн", "Вт", 
                                                                                                                                           "Ср", "Чт", "Пт", "Сб", 
                                                                                                                                           "Вс"), expand = c(0, 0))
  return(ggplotcalendar_heatmap)
}