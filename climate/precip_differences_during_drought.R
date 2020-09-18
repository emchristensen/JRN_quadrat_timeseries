#' Look at spatial differences in precip during major droughts
#' EMC 9/1/20

library(dplyr)
library(ggplot2)
library(sf)

datarepo = '../JRN_quadrat_datapaper/'
raingauges = read.csv(paste0(datarepo,'Climate/JER_standard_raingauge_network_monthly_precip.csv'), stringsAsFactors = F)
# remove gauges up in mountains
raingauges = dplyr::filter(raingauges, !(name %in% c('SAN ANDRACITO','LEAD CAMP','ROPES SPRINGS','ASH CANYON','SAN NICHOLAS','GOAT MOUNTAIN')))
raingauges$prec_in = as.numeric(raingauges$prec_in)
# create season column
raingauges$season = NA
raingauges$season[raingauges$month %in% c(1,2,3)] <- 'winter'
raingauges$season[raingauges$month %in% c(4,5,6)] <- 'spring'
raingauges$season[raingauges$month %in% c(7,8,9)] <- 'summer'
raingauges$season[raingauges$month %in% c(10,11,12)] <- 'fall'


# get data for mapping
# Get weather station locations
ws = read.csv('RawFiles/RG_latlong.csv', stringsAsFactors = F)
# Import JER boundary shapefile
JER.border <- read_sf(dsn = "R:/Quadrat/Location_conflicts_Adler", layer = "jer_boundary")



# 1935 drought ----
# highest/lowest/mean: yearly
rg1935_yearly = dplyr::filter(raingauges, year %in% 1934:1935) %>%
  group_by(name, year) %>%
  summarize(yearly_ppt=sum(prec_in)) %>%
  ungroup() %>%
  group_by(name) %>%
  summarize(mean_yearly=mean(yearly_ppt))
mean(rg1935_yearly$mean_yearly)
sd(rg1935_yearly$mean_yearly)

# highest/lowest/mean: just summer precip
rg1935_summer = dplyr::filter(raingauges, year %in% 1934:1935, season=='summer') %>%
  group_by(name, year) %>%
  summarize(summer_ppt=sum(prec_in)) %>%
  ungroup() %>%
  group_by(name) %>%
  summarize(mean_summer=mean(summer_ppt))
mean(rg1935_summer$mean_summer)
sd(rg1935_summer$mean_summer)

# map
rg1935mean = rg1935_yearly %>%
  merge(ws, by='name', all.x=T)
rgmap35 = ggplot() +
  geom_sf(data = JER.border) +
  geom_point(data = rg1935mean, aes(x=Xcoord, y=Ycoord, color=mean_yearly), size=3) +
  theme_bw() +
  ggtitle('1934-35 Mean Yearly Precip (in)') +
  ylab('') +
  xlab('') +
  theme(axis.text.x=element_text(angle=90))
rgmap35
ggsave('C:/Users/echriste/Desktop/git/JRN_quadrat_timeseries/Figures/totalppt1935.png', plot=rgmap35, width=4, height=4)

# 1950-56 drought ----
# highest/lowest/mean: yearly
rg1950_yearly = dplyr::filter(raingauges, year %in% 1950:1956) %>%
  group_by(name, year) %>%
  summarize(yearly_ppt=sum(prec_in)) %>%
  ungroup() %>%
  group_by(name) %>%
  summarize(mean_yearly=mean(yearly_ppt))
mean(rg1950_yearly$mean_yearly)
sd(rg1950_yearly$mean_yearly)

# highest/lowest/mean: just summer precip
rg1950_summer = dplyr::filter(raingauges, year %in% 1950:1956, season=='summer') %>%
  group_by(name, year) %>%
  summarize(summer_ppt=sum(prec_in)) %>%
  ungroup() %>%
  group_by(name) %>%
  summarize(mean_summer=mean(summer_ppt))
mean(rg1950_summer$mean_summer)
sd(rg1950_summer$mean_summer)

# map
rg1950mean = rg1950_yearly %>%
  merge(ws, by='name', all.x=T)
rgmap50 = ggplot() +
  geom_sf(data = JER.border) +
  geom_point(data = rg1950mean, aes(x=Xcoord, y=Ycoord, color=mean_yearly), size=3) +
  theme_bw() +
  ggtitle('1950-56 Mean Yearly Precip (in)') +
  ylab('') +
  xlab('') +
  theme(axis.text.x=element_text(angle=90))
rgmap50
ggsave('C:/Users/echriste/Desktop/git/JRN_quadrat_timeseries/Figures/totalppt1950.png', plot=rgmap50, width=4, height=4)

# 2011 drought ----
# highest/lowest/mean: yearly
rg2011_yearly = dplyr::filter(raingauges, year %in% 2010:2012) %>%
  group_by(name, year) %>%
  summarize(yearly_ppt=sum(prec_in)) %>%
  ungroup() %>%
  group_by(name) %>%
  summarize(mean_yearly=mean(yearly_ppt))
mean(rg2011_yearly$mean_yearly)
sd(rg2011_yearly$mean_yearly)

# highest/lowest/mean: just summer precip
rg2011_summer = dplyr::filter(raingauges, year %in% 2010:2012, season=='summer') %>%
  group_by(name, year) %>%
  summarize(summer_ppt=sum(prec_in)) %>%
  ungroup() %>%
  group_by(name) %>%
  summarize(mean_summer=mean(summer_ppt))
mean(rg2011_summer$mean_summer)
sd(rg2011_summer$mean_summer)

# map
rg2011mean = rg2011_yearly %>%
  merge(ws, by='name', all.x=T)
rgmap11 = ggplot() +
  geom_sf(data = JER.border) +
  geom_point(data = rg2011mean, aes(x=Xcoord, y=Ycoord, color=mean_yearly), size=3) +
  theme_bw() +
  ggtitle('2010-2012 Mean Yearly Precip (in)') +
  ylab('') +
  xlab('') +
  theme(axis.text.x=element_text(angle=90))
rgmap11
ggsave('C:/Users/echriste/Desktop/git/JRN_quadrat_timeseries/Figures/totalppt2011.png', plot=rgmap11, width=4, height=4)
