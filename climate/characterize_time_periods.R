#' Script characterizing time periods by climate variables
#' ppt and temp data downloaded from https://doi.org/10.6073/pasta/f09060a9ceb136a1d40b5323aaa0d9a6 
#' EMC 9/18/20

library(dplyr)
library(ggplot2)
library(lubridate)

datarepo = '../JRN_quadrat_datapaper/'
pptdat = read.csv(paste0(datarepo, 'Climate/JRN_379001_NOAA_JER_HQ_daily_climate_data.csv'), stringsAsFactors = F)
prismdat = read.csv(paste0(datarepo, 'Climate/PRISM_ppt_tmean_vpdmax_stable_4km_191501_201701_Headquarters.csv'), stringsAsFactors = F, skip=10)

# deal with dates 
pptdat$Date = as.Date(pptdat$date)
pptdat$year = lubridate::year(pptdat$Date)
pptdat$month = lubridate::month(pptdat$Date)
prismdat$year = as.numeric(substr(prismdat$Date, 1,4))
prismdat$month = as.numeric(substr(prismdat$Date, 6,7))

# get total ppt and mean T by year and summer
yearlyppt = pptdat %>%
  group_by(year) %>%
  summarize(yearly_ppt_mm = sum(prec_mm, na.rm=T), yearly_mean_max_T = mean(tmax_c, na.rm=T),
            yearly_ppt_nas = sum(is.na(prec_mm)), yearly_tmax_nas = sum(is.na(tmax_c)))

summerppt = pptdat %>%
  dplyr::filter(month %in% c(6,7,8,9)) %>%
  group_by(year) %>%
  summarize(summer_ppt_mm = sum(prec_mm, na.rm=T), summer_mean_max_T = mean(tmax_c, na.rm=T),
            summer_ppt_nas = sum(is.na(prec_mm)), summer_tmax_nas = sum(is.na(tmax_c)))

prismyear = prismdat %>%
  group_by(year) %>%
  summarize(yearly_ppt_in = sum(ppt..inches.), yearly_temp_prism = mean(tmean..degrees.F.),
            yearly_vpd = mean(vpdmax..hPa.)) %>%
  mutate(yearly_ppt_prism = yearly_ppt_in*25.4, yearly_temp_prism = (yearly_temp_prism-32) * 5/9)

prismsummer = prismdat %>%
  dplyr::filter(month %in% c(6,7,8,9)) %>%
  group_by(year) %>%
  summarize(summer_ppt_in = sum(ppt..inches.), summer_temp_prism = mean(tmean..degrees.F.),
            summer_vpd = mean(vpdmax..hPa.)) %>%
  mutate(summer_ppt_prism = summer_ppt_in*25.4, summer_temp_prism = (summer_temp_prism -32) * 5/9)

pptsummary = merge(yearlyppt, summerppt, all=T) %>%
  merge(prismyear) %>%
  merge(prismsummer) %>%
  dplyr::select(year, yearly_ppt_mm, yearly_ppt_nas, yearly_ppt_prism, 
                yearly_mean_max_T, yearly_tmax_nas, yearly_temp_prism, yearly_vpd,
                summer_ppt_mm, summer_ppt_nas, summer_ppt_prism,
                summer_mean_max_T, summer_tmax_nas, summer_temp_prism, summer_vpd)

# compare data sources
ggplot(pptsummary, aes(x=yearly_ppt_mm, y=yearly_ppt_prism)) +
  geom_point()
ggplot(pptsummary, aes(x=summer_ppt_mm, y=summer_ppt_prism)) +
  geom_point()
ggplot(pptsummary, aes(x=yearly_mean_max_T, y=yearly_temp_prism)) +
  geom_point()
ggplot(pptsummary, aes(x=summer_mean_max_T, y=summer_temp_prism)) +
  geom_point()

# ppt lines up well, temperature does not

write.csv(pptsummary, 'climate/climate_variables.csv', row.names=F)
