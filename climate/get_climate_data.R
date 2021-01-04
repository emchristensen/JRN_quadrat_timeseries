# get raw climate data and create monthly timeseries
# EMC
# last run: 1/4/21

library(dplyr)
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
