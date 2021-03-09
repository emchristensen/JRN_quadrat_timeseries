#' Script characterizing time periods by climate variables
#' ppt and temp data downloaded from https://doi.org/10.6073/pasta/f09060a9ceb136a1d40b5323aaa0d9a6 
#' EMC 9/18/20

library(dplyr)
library(ggplot2)
library(lubridate)



# ========================================================================
# characterize time periods ----
climatesummary = read.csv('climate/climate_variables.csv', stringsAsFactors = F)
datarepo = '../JRN_quadrat_datapaper/'
stocking = read.csv(paste0(datarepo,'SiteAndMethods/JERLivestockStockingRates.csv'), stringsAsFactors = F)

# yearly precip
ppt = ggplot(climatesummary, aes(x=year, y=yearly_ppt_mm)) +
  #geom_rect(aes(xmin=1936, xmax=1945, ymin=0, ymax=600), alpha=.008) +
  #geom_rect(aes(xmin=1946, xmax=1955, ymin=0, ymax=600), alpha=.008) +
  #geom_rect(aes(xmin=1957, xmax=1979, ymin=0, ymax=600), alpha=.008) +
  #geom_rect(aes(xmin=1995, xmax=2016, ymin=0, ymax=600), alpha=.008) +
  geom_line() +
  #geom_vline(xintercept=1945) +
  geom_hline(yintercept=mean(climatesummary$yearly_ppt_mm), linetype='dashed') +
  #geom_line(aes(x=year, y=summer_ppt_mm), color='blue') +
  #geom_line(aes(x=year, y=yearly_ppt_prism), color='red') +
  xlab('') +
  ylab('yearly precip (mm)') +
  #ylim(0,550)+
  theme_bw()
ppt
ggsave(plot=ppt, 'Figures/ppt_timeseries.png', width=4, height=3)

# get mean precip values for 3 selected time periods and long-term mean
climatesummary %>% summarize(mean=mean(yearly_ppt_mm), sd=sd(yearly_ppt_mm))
dplyr::filter(climatesummary, year >= 1945, year <= 1956) %>% summarize(mean=mean(yearly_ppt_mm), sd=sd(yearly_ppt_mm))
dplyr::filter(climatesummary, year >= 1957, year <= 1979) %>% summarize(mean=mean(yearly_ppt_mm), sd=sd(yearly_ppt_mm))
dplyr::filter(climatesummary, year >= 1995, year <= 2016) %>% summarize(mean=mean(yearly_ppt_mm), sd=sd(yearly_ppt_mm))


# timeseries plots
vpd = ggplot(climatesummary, aes(x=year, y=yearly_vpd)) +
  geom_line() +
  #geom_line(aes(x=year, y=summer_vpd), color='blue') +
  xlab('') +
  ylab('max VPD (hPa)') +
  theme_bw()
vpd
#ggsave(plot=vpd, 'Figures/VPD_timeseries.png', width=4, height=3)



temp = ggplot(climatesummary, aes(x=year, y=yearly_maxt)) +
  geom_line() +
  #geom_line(aes(x=year, y=summer_maxt), color='blue') +
  #geom_line(aes(x=year, y=yearly_ppt_prism), color='red') +
  xlab('') +
  ylab('max daily temp (C)') +
  theme_bw()
temp
#ggsave(plot=temp, 'Figures/temp_timeseries.png', width=4, height=3)

stock = ggplot(stocking, aes(x=year, y=AUM, color=animal)) +
  geom_line() +
  xlab('') +
  ylab('Stocking rate: AUM') +
  theme_bw()
stock
#ggsave(plot=stock, 'Figures/stock_timeseries.png', width=4, height=3)


# boxplot of mean precip values for 3 time periods and long-term mean
pptbox1 = climatesummary %>%
  mutate(timesection=rep('All years'))
pptbox2 = climatesummary %>%
  mutate(timesection=rep(NA))
pptbox2$timesection[pptbox2$year %in% 1945:1956] <- '1945-1956'
pptbox2$timesection[pptbox2$year %in% 1956:1980] <- '1956-1979'
pptbox2$timesection[pptbox2$year %in% 1995:2016] <- '1995-2016'
pptbox2$timesection[pptbox2$year %in% 1936:1945] <- '1936-1945'

pptbox = rbind(pptbox1, pptbox2[!is.na(pptbox2$timesection),])


precipboxplot = ggplot(pptbox, aes(x=timesection, y=yearly_ppt_mm)) +
  geom_boxplot() +
  xlab('') +
  ylab('yearly precip. (mm)') +
  theme_bw()
precipboxplot
ggsave(precipboxplot, filename='Figures/ppt_boxplot.png', width=4, height=3)


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

