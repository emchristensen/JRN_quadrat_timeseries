#' Script characterizing time periods by climate variables
#' ppt and temp data downloaded from https://doi.org/10.6073/pasta/f09060a9ceb136a1d40b5323aaa0d9a6 
#' EMC 9/18/20

library(dplyr)
library(ggplot2)
library(lubridate)


# ==============================================================
# munge raw data ----

datarepo = '../JRN_quadrat_datapaper/'
pptdat = read.csv(paste0(datarepo, 'Climate/JRN_379001_NOAA_JER_HQ_daily_climate_data.csv'), stringsAsFactors = F)
prismdat = read.csv(paste0(datarepo, 'Climate/PRISM_ppt_tmean_vpdmax_stable_4km_191501_201701_Headquarters.csv'), stringsAsFactors = F, skip=10)

# deal with dates 
pptdat$Date = as.Date(pptdat$date)
pptdat$year = lubridate::year(pptdat$Date)
pptdat$month = lubridate::month(pptdat$Date)
prismdat$year = as.numeric(substr(prismdat$Date, 1,4))
prismdat$month = as.numeric(substr(prismdat$Date, 6,7))

# get monthly ppt
monthlyppt = pptdat %>%
  group_by(year, month) %>%
  summarize(monthly_ppt_mm= sum(prec_mm, na.rm=T), monthly_ppt_nas = sum(is.na(prec_mm)),
            monthly_maxt = mean(tmax_c, na.rm=T), monthly_temp_nas = sum(is.na(tmax_c)),
            monthly_mint = mean(tmin_c, na.rm=T), monthly_tmin_nas = sum(is.na(tmin_c)))

monthlyprism = prismdat %>%
  group_by(year, month) %>%
  mutate(monthly_ppt_prism = sum(ppt..inches.)*25.4) %>%
  dplyr::select(year, month, monthly_ppt_prism, monthly_vpd = vpdmax..hPa.)

# if our ppt measurements are missing more than 7 days in a month, fill with prism data
#   this only affects 6 vaues: 5/1915, 12/1977, 1/2009, 9/1979, 7/2002, 8/1948
monthly = merge(monthlyppt, monthlyprism)
monthly$monthly_ppt_filled = monthly$monthly_ppt_mm
monthly$monthly_ppt_filled[monthly$monthly_ppt_nas>7] <- monthly$monthly_ppt_prism[monthly$monthly_ppt_nas>7]

# aggregate by year and summer
yearlyppt = monthly %>%
  group_by(year) %>%
  summarize(yearly_ppt_mm = sum(monthly_ppt_filled),
            yearly_maxt = mean(monthly_maxt, na.rm = T), yearly_maxt_nas = sum(is.na(monthly_maxt)),
            yearly_mint = mean(monthly_mint, na.rm = T), yearly_mint_nas = sum(is.na(monthly_mint)),
            yearly_vpd = mean(monthly_vpd))
# if there is more than one entire month missing from temp, replace with NA (not a reliable avg)
#   this affects 9 values for max, 11 for min
yearlyppt$yearly_maxt[yearlyppt$yearly_maxt_nas>1] <- NA
yearlyppt$yearly_mint[yearlyppt$yearly_mint_nas>1] <- NA

summerppt = monthly %>%
  dplyr::filter(month %in% c(6,7,8,9)) %>%
  group_by(year) %>%
  summarize(summer_ppt_mm = sum(monthly_ppt_filled),
            summer_maxt = mean(monthly_maxt, na.rm=T), summer_maxt_nas = sum(is.na(monthly_maxt)),
            summer_vpd = mean(monthly_vpd))
# if there is more than one entire month missing from summer temp, replace with NA (not a reliable avg)
#   this only affects 4 values
summerppt$summer_maxt[summerppt$summer_maxt_nas>1] <- NA

# merge into one data frame
climatesummary = merge(yearlyppt, summerppt) %>%
  dplyr::select(year, yearly_ppt_mm, yearly_maxt, yearly_mint, yearly_vpd, 
                summer_ppt_mm, summer_maxt, summer_vpd)

write.csv(climatesummary, 'climate/climate_variables.csv', row.names=F)

# ========================================================================
# characterize time periods ----
climatesummary = read.csv('climate/climate_variables.csv', stringsAsFactors = F)
stocking = read.csv(paste0(datarepo,'SiteAndMethods/JERLivestockStockingRates.csv'), stringsAsFactors = F)

# save timeseries plots
vpd = ggplot(climatesummary, aes(x=year, y=yearly_vpd)) +
  geom_line() +
  geom_line(aes(x=year, y=summer_vpd), color='blue') +
  xlab('') +
  ylab('max VPD (hPa)') +
  theme_bw()
vpd
ggsave(plot=vpd, 'Figures/VPD_timeseries.png', width=4, height=3)

ppt = ggplot(climatesummary, aes(x=year, y=yearly_ppt_mm)) +
  geom_line() +
  geom_line(aes(x=year, y=summer_ppt_mm), color='blue') +
  #geom_line(aes(x=year, y=yearly_ppt_prism), color='red') +
  xlab('') +
  ylab('total precip (mm)') +
  theme_bw()
ppt
ggsave(plot=ppt, 'Figures/ppt_timeseries.png', width=4, height=3)

temp = ggplot(climatesummary, aes(x=year, y=yearly_maxt)) +
  geom_line() +
  geom_line(aes(x=year, y=summer_maxt), color='blue') +
  #geom_line(aes(x=year, y=yearly_ppt_prism), color='red') +
  xlab('') +
  ylab('max daily temp (C)') +
  theme_bw()
temp
ggsave(plot=temp, 'Figures/temp_timeseries.png', width=4, height=3)


stock = ggplot(stocking, aes(x=year, y=AUM, color=animal)) +
  geom_line() +
  xlab('') +
  ylab('Stocking rate: AUM') +
  theme_bw()
stock
ggsave(plot=stock, 'Figures/stock_timeseries.png', width=4, height=3)

# =====================================
# time period summary ----
time1 = dplyr::filter(climatesummary, year>=1945, year<=1956) %>%
  summarize(mean_ppt = mean(yearly_ppt_mm), sd_ppt = sd(yearly_ppt_mm),
            summer_ppt = mean(summer_ppt_mm), summerpptsd = sd(summer_ppt_mm),
            maxt = mean(yearly_maxt, na.rm=T), maxtsd = sd(yearly_maxt, na.rm=T),
            mint = mean(yearly_mint, na.rm=T), mintsd = sd(yearly_mint, na.rm=T),
            summermaxt = mean(summer_maxt), sdmaxt = sd(summer_maxt),
            vpd = mean(yearly_vpd), vpdsd = sd(yearly_vpd),
            summervpd = mean(summer_vpd), summervpdsd = sd(summer_vpd))
time1
time2 = dplyr::filter(climatesummary, year>=1955, year<=1980) %>%
  summarize(mean_ppt = mean(yearly_ppt_mm), sd_ppt = sd(yearly_ppt_mm),
            summer_ppt = mean(summer_ppt_mm), summerpptsd = sd(summer_ppt_mm),
            maxt = mean(yearly_maxt, na.rm=T), maxtsd = sd(yearly_maxt, na.rm=T),
            mint = mean(yearly_mint, na.rm=T), mintsd = sd(yearly_mint, na.rm=T),
            summermaxt = mean(summer_maxt), sdmaxt = sd(summer_maxt),
            vpd = mean(yearly_vpd), vpdsd = sd(yearly_vpd),
            summervpd = mean(summer_vpd), summervpdsd = sd(summer_vpd))
time2
time3 = dplyr::filter(climatesummary, year>=1995, year<=2016) %>%
  summarize(mean_ppt = mean(yearly_ppt_mm), sd_ppt = sd(yearly_ppt_mm),
            summer_ppt = mean(summer_ppt_mm), summerpptsd = sd(summer_ppt_mm),
            maxt = mean(yearly_maxt, na.rm=T), maxtsd = sd(yearly_maxt, na.rm=T),
            mint = mean(yearly_mint, na.rm=T), mintsd = sd(yearly_mint, na.rm=T),
            summermaxt = mean(summer_maxt), sdmaxt = sd(summer_maxt),
            vpd = mean(yearly_vpd), vpdsd = sd(yearly_vpd),
            summervpd = mean(summer_vpd), summervpdsd = sd(summer_vpd))
time3

stockingrate = stocking %>% dplyr::filter(animal=='Cattle')
stockingrate %>% dplyr::filter(year>=1915, year<=1945) %>% mutate(aum = mean(AUM))
stockingrate %>% dplyr::filter(year>=1945, year<=1956) %>% mutate(aum = mean(AUM))
stockingrate %>% dplyr::filter(year>=1955, year<=1980) %>% mutate(aum = mean(AUM))
stockingrate %>% dplyr::filter(year>=1995, year<=2016) %>% mutate(aum = mean(AUM))
