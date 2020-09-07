# BOER does well in years with good June-August precip

library(dplyr)

ppt = read.csv('../JRN_quadrat_datapaper/Climate/HeadquartersMonthlyPrecip.csv', stringsAsFactors = F)

summer = dplyr::filter(ppt, month %in% c(6,7,8), year>=1995) %>%
  group_by(year) %>%
  summarize(summer_ppt = sum(mean_precip))

ggplot(summer, aes(x=year, y=summer_ppt)) +
  geom_line()
