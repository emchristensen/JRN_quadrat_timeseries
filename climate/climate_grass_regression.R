# look for correlations between grass and precip. Does the relationship change once shrubs have arrived?
# EMC 2/5/21

library(dplyr)
library(ggplot2)

veg = read.csv('data/quadrat_veg.csv', stringsAsFactors = F)
veg$date = as.Date(paste(veg$year, veg$month, veg$day, sep='-'))
ppt = read.csv('climate/ppt_6mo_year_sums.csv', stringsAsFactors = F)

# merge data together
veg_ppt = merge(veg, ppt, by=c('year','month'))

veg_ppt_grass = dplyr::filter(veg_ppt, total_grass>0.1)

ggplot(veg_ppt_grass, aes(x=ppt_yr, y=total_grass)) +
  geom_point()

veg_ppt_early = dplyr::filter(veg_ppt_grass, year<1945, month %in% c(9,10,11))

ggplot(veg_ppt_early, aes(x=ppt_yr, y=total_grass)) +
  geom_point()
