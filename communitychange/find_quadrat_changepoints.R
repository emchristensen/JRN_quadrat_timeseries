#' Find temporal cutpoints (biggest bray-curtis difference) 
#' 
#' EMC 9/29/20

library(dplyr)
library(cluster)
library(vegan)
library(ggplot2)
set.seed(123)

veg = read.csv('data/all_species_counts_cover.csv', stringsAsFactors = F) %>%
  dplyr::filter(!(species %in% c('UNKCA','UNKF','UNKG','UNKN','UNKP','UNKP1','UNKP2','UNKP3','UNKPF','UNKPF1',
                                 'UNKPF2','UNKPF3','UNKPG')))
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)
veg$date = as.Date(paste(veg$year, veg$month, '15',sep='-'))


# find 2-cluster for all quadrats
inflectiondates = c()
for (quad in unique(dates$quadrat)) {
  # create wide data frame
  quaddat = dplyr::filter(veg, quadrat==quad, year<1995) %>% 
    dplyr::select(-cover) %>%
    tidyr::pivot_wider(names_from=species, values_from=count)
  # fill in missing with zeros
  quaddat[is.na(quaddat)] <- 0
  
  # get baseline: avg first 5 points
  baseline = quaddat %>%
    slice(1:5) %>%
    dplyr::select(-quadrat, -project_year, -year, -month, -date) %>%
    colMeans()
  distdat_baseline = rbind(baseline, select(quaddat,-quadrat,-project_year,-year,-month,-date))
  # get bray-curtis distance: every date compared to baseline
  quaddist_baseline = data.frame()
  for (n in 2:dim(distdat_baseline)[1]) {
    ndist = vegdist(distdat_baseline[c(1,n),], 'bray')
    quaddist_baseline = rbind(quaddist_baseline, data.frame(bray=as.numeric(ndist)))
  }
  quaddist_baseline$date = quaddat$date
  
  # just veg columns to be used for distances
  distdat = dplyr::select(quaddat, -quadrat, -project_year, -year, -month, -date)
  
  # do clustering; k=2
  cluster = pam(vegdist(distdat, 'bray'), k=2)
  quaddat$cluster = cluster$clustering-1
  
  # are clusters perfectly in order?
  if (all(diff(quaddat$cluster)>=0)) {
    # find date where quad switches to other cluster
    quantile.low = quaddat$date[which(quaddat$cluster==1)[1]-1]
    quantile.upper = quaddat$date[which(quaddat$cluster==1)[1]]
    inflectiondate_estimate = mean(c(inflectiondate_le, inflectiondate_ue))
    
    # plot
    changeplot = ggplot(quaddat, aes(x=date, y=cluster)) +
      geom_point() +
      geom_vline(xintercept=inflectiondate) +
      geom_point(data=quaddist_baseline, aes(x=date, y=bray), alpha=.3) +
      geom_line(data=quaddist_baseline, aes(x=date, y=bray), alpha=.3) +
      ggtitle(quad) +
      xlab('') +
      ylab('Bray-Curtis dissimilarity') +
      theme_bw()
    #changeplot
    ggsave(plot=changeplot, filename=paste0('Figures/quad_changepoints/',quad,'.png'), width=3, height=3)
    } else {
    # fit a logistic model
    glm.fit = glm(cluster ~ date, data=quaddat, family=binomial)
    
    # predicted values
    predict.on = data.frame(date=seq.Date(from = min(quaddat$date), to=max(quaddat$date), by='quarter'))
    predictedvals = data.frame(date=predict.on$date,
                               predicted=predict.glm(glm.fit, newdata=predict.on, type='response'))
    #predictedvals = data.frame(date=quaddat$date, predicted=glm.fit$fitted.values)
    
    # inflection point
    inflectiondate_estimate = predictedvals$date[which(predictedvals$predicted>.5)[1]]
    quantile.low = predictedvals$date[which(predictedvals$predicted>.25)[1]]
    quantile.upper = predictedvals$date[which(predictedvals$predicted>.75)[1]]
    
    # logplot
    logplot = ggplot(quaddat, aes(x=date, y=cluster)) +
      geom_point() +
      geom_line(data=predictedvals, aes(x=date, y=predicted)) +
      geom_vline(xintercept=inflectiondate) +
      geom_point(data=quaddist_baseline, aes(x=date, y=bray), alpha=.3) +
      geom_line(data=quaddist_baseline, aes(x=date, y=bray), alpha=.3) +
      ggtitle(quad) +
      xlab('') +
      ylab('Bray-Curtis dissimilarity') +
      theme_bw()
    #logplot
    ggsave(plot=logplot, filename=paste0('Figures/quad_changepoints/',quad,'.png'), width=3, height=3)
    
  }
  inflectiondates = rbind(inflectiondates, data.frame(quadrat=quad,
                                                      date_estimate=inflectiondate_estimate,
                                                      date_lowerbound = quantile.low,
                                                      date_upperbound = quantile.upper))
  
}
write.csv(inflectiondates, 'communitychange/quadrat_count_changepoints.csv', row.names=F)
# =======================================
# histogram of changepoints
# analysis using counts, not cover
# these are the quadrats where the model estimate looks wrong:
badquads = c('A3','AR4','B3','B5','G1','I1','I3','K2','K4','L3','L4','L5','M6','P2','P3','P4','P5',
             'T1','T2','T3','T4','T5','T6','T7','T8','T9','T10','T11','U4','V1','V4','Y3','Y7')
# these might be bad because of outliers late in timeseries
outliers = c('A3','AR4','B3','B5','L5','M6','U4','V1','V4')
# these never got very far from initial conditions
nochange = c('I3','L3','L4','P5','Y3','Y7')
# these are missing too many points around the change
notenoughpts = c('K2','P2','P3','P4','T1','T2','T3','T4','T5','T6','T7','T8','T9','T10','T11')
# I don't know why these are bad
bad = c('G1','I1','K4')


inflectionhist = dplyr::filter(inflectiondates, !quadrat %in% badquads)
vectorofnumbers = lubridate::year(inflectionhist$date_estimate)

hist(vectorofnumbers, breaks = seq(1920, 1980,5), xlab='', main='Histogram: year of greatest community change')
