#' Find temporal cutpoints 
#' 
#' EMC 9/29/20

library(dplyr)
library(cluster)
library(vegan)
library(ggplot2)
library(cowplot)
set.seed(123)

veg = read.csv('data/all_species_counts_cover.csv', stringsAsFactors = F) %>%
  dplyr::filter(!(species %in% c('UNKCA','UNKF','UNKG','UNKN','UNKP','UNKP1','UNKP2','UNKP3','UNKPF','UNKPF1',
                                 'UNKPF2','UNKPF3','UNKPG')))
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)
splist = read.csv('../JRN_quadrat_datapaper/Plants/Jornada_quadrat_species_list_WIP.csv', stringsAsFactors = F)

# restrict to dates in dates file and non-shrub species
veg_use = merge(veg, splist[,c('species_code','form')], by.x='species',by.y='species_code') %>%
  dplyr::filter(form !='SHRUB') %>%
  dplyr::select(-form) %>%
  merge(dates)
veg_use$date = as.Date(paste(veg_use$year, veg_use$month, '15',sep='-'))


# distance metric
distance_metric = 'jaccard'

#' @title prepare quad data
#' @description prepare quadrat data for various analyses below. cuts off at a max date if desired, removes
#' species that were only observed on the quadrat once
#'
#'
prepare_quad_data = function(veg_use, quad, max_year=2016) {
  # create wide data frame
  quaddat = dplyr::filter(veg_use, quadrat==quad, year<=max_year) %>% 
    dplyr::select(-count) %>%
    tidyr::pivot_wider(names_from=species, values_from=cover) 
  # fill in missing with zeros
  quaddat[is.na(quaddat)] <- 0
  # find any species with only one observation
  test = dplyr::select(quaddat, -quadrat, -project_year, -year, -month, -day, -date) 
  keepspecies = names(test[colSums(test !=0)>1])
  # keep only species that have been observed more than once; remove any empty rows
  quaddat2 = dplyr::select(quaddat, quadrat, project_year, year, month, date, all_of(keepspecies))
  quaddat3 = quaddat2[rowSums(quaddat2[,6:ncol(quaddat2)])>0,]
  
  # find dates where quad was empty (for plot later)
  quaddates = dplyr::filter(dates, quadrat==quad, year<2020) %>%
    mutate(date = as.Date(paste(year, month, '15', sep='-')))
  quaddates_empty = data.frame(date=quaddates$date[!quaddates$date %in% quaddat3$date])
  
  # get baseline: avg first 5 points
  baseline = quaddat3 %>%
    slice(1:5) %>%
    dplyr::select(-quadrat, -project_year, -year, -month, -date) %>%
    colMeans()
  distdat_baseline = rbind(baseline, select(quaddat3,-quadrat,-project_year,-year,-month, -date))
  # get bray-curtis distance: every date compared to baseline
  quaddist_baseline = data.frame()
  for (n in 2:dim(distdat_baseline)[1]) {
    ndist = vegdist(distdat_baseline[c(1,n),], method=distance_metric, binary=T)
    quaddist_baseline = rbind(quaddist_baseline, data.frame(bray=as.numeric(ndist)))
  }
  quaddist_baseline$date = quaddat3$date
  
  return(list(quaddat3, quaddates_empty, quaddist_baseline))
}

# first, look at bray-curtis compared to baseline to determine if change took place
firstlasttest = c()
for (quad in unique(dates$quadrat)) {
  # prepare data
  quaddat_prepared = prepare_quad_data(veg_use, quad, max_year=2016)
  
  quaddates_empty = quaddat_prepared[[2]]
  quaddist_baseline = quaddat_prepared[[3]]
  quaddat3 = quaddat_prepared[[1]]
  
  # just veg columns to be used for distances
  distdat = dplyr::select(quaddat3, -quadrat, -project_year, -year, -month, -date)
  nspecies = ncol(distdat)
  
  # logplot
  brayplot = ggplot(quaddist_baseline, aes(x=date, y=bray)) +
    geom_point() +
    geom_line() +
    ggtitle(quad) +
    xlab('') +
    xlim(as.Date('1915-06-15'), as.Date('2017-01-15')) +
    ylab('Bray-Curtis dissimilarity') +
    theme_bw()
  
  if (nrow(quaddates_empty)>0) {
    brayplot = brayplot +geom_point(data=quaddates_empty, aes(x=date, y=rep(0)), color='red', shape=23) 
  }
  #brayplot
  ggsave(plot=brayplot, filename=paste0('Figures/bray_curtis/',quad,'.png'), width=3, height=3)
  
  # are last 5 points different from first 5 points?
  testdata = arrange(quaddat3, date) %>%
    slice(c(1:5, (nrow(quaddat3)-4):nrow(quaddat3))) %>%
    dplyr::select(-quadrat, -project_year, -year, -month, -date) %>%
    select_if(colSums(.) != 0) %>%
    as.matrix()
  groups = factor(c(rep('A',5), rep("B",5)))
  
  # test using cmpoutput from micompr package: 
  #cmp = cmpoutput("firstlast", 0.9, data=testdata, obs_lvls=groups)
  
  # use adonis from vegan package
  adonistest = adonis(testdata ~groups, permutations=999, method=distance_metric)
  firstlasttest = rbind(firstlasttest, data.frame(quadrat=quad,
                                                  p.adonis = adonistest$aov.tab[1,6]))
  # firstlasttest = rbind(firstlasttest, data.frame(quadrat=quad,
  #                                                 npcs = cmp$npcs,
  #                                                 p.manova = cmp$p.values[1],
  #                                                 p.ttest = cmp$p.values[[2]][1],
  #                                                 p.MWU = cmp$p.values[[3]][1],
  #                                                 p.adjttest = cmp$p.values[[4]][1],
  #                                                 p.adjMWU = cmp$p.values[[5]][1],
  #                                                 p.adonis = adonistest$aov.tab[1,6]))
}

# no transitions ----
# these quadrats did not have a significant test between the first 5 and last 5 points
notransition = firstlasttest$quadrat[firstlasttest$p.adonis>.05]
b2a = prepare_quad_data(veg_use, quad='B2A')
brayb2a = ggplot(b2a[[3]], aes(x=date, y=bray)) +
  geom_point() +
  geom_line() +
  ggtitle('B2A') +
  xlab('') +
  xlim(as.Date('1915-06-15'), as.Date('2017-01-15')) +
  ylab('Bray-Curtis dissimilarity') +
  theme_bw()
l4 = prepare_quad_data(veg_use, quad='L4')
brayl4 = ggplot(l4[[3]], aes(x=date, y=bray)) +
  geom_point() +
  geom_line() +
  ggtitle('L4') +
  xlab('') +
  xlim(as.Date('1915-06-15'), as.Date('2017-01-15')) +
  ylab('') +
  theme_bw()
i3 = prepare_quad_data(veg_use, quad='I3')
brayi3 = ggplot(i3[[3]], aes(x=date, y=bray)) +
  geom_point() +
  geom_line() +
  ggtitle('I3') +
  xlab('') +
  xlim(as.Date('1915-06-15'), as.Date('2017-01-15')) +
  ylab('') +
  theme_bw()
n1 = prepare_quad_data(veg_use, quad='N1')
brayn1 = ggplot(n1[[3]], aes(x=date, y=bray)) +
  geom_point() +
  geom_line() +
  ggtitle('N1') +
  xlab('') +
  xlim(as.Date('1915-06-15'), as.Date('2017-01-15')) +
  ylab('Bray-Curtis dissimilarity') +
  theme_bw()
p1 = prepare_quad_data(veg_use, quad='P1')
brayp1 = ggplot(p1[[3]], aes(x=date, y=bray)) +
  geom_point() +
  geom_line() +
  ggtitle('P1') +
  xlab('') +
  xlim(as.Date('1915-06-15'), as.Date('2017-01-15')) +
  ylab('') +
  theme_bw()
p4 = prepare_quad_data(veg_use, quad='P4')
brayp4 = ggplot(p4[[3]], aes(x=date, y=bray)) +
  geom_point() +
  geom_line() +
  ggtitle('P4') +
  xlab('') +
  xlim(as.Date('1915-06-15'), as.Date('2017-01-15')) +
  ylab('') +
  theme_bw()
r1 = prepare_quad_data(veg_use, quad='R1')
brayr1 = ggplot(r1[[3]], aes(x=date, y=bray)) +
  geom_point() +
  geom_line() +
  ggtitle('R1') +
  xlab('') +
  xlim(as.Date('1915-06-15'), as.Date('2017-01-15')) +
  ylab('Bray-Curtis dissimilarity') +
  theme_bw()
t3 = prepare_quad_data(veg_use, quad='T3')
brayt3 = ggplot(t3[[3]], aes(x=date, y=bray)) +
  geom_point() +
  geom_line() +
  ggtitle('T3') +
  xlab('') +
  xlim(as.Date('1915-06-15'), as.Date('2017-01-15')) +
  ylab('') +
  theme_bw()
t7 = prepare_quad_data(veg_use, quad='T7')
brayt7 = ggplot(t7[[3]], aes(x=date, y=bray)) +
  geom_point() +
  geom_line() +
  ggtitle('T7') +
  xlab('') +
  xlim(as.Date('1915-06-15'), as.Date('2017-01-15')) +
  ylab('') +
  theme_bw()
t8 = prepare_quad_data(veg_use, quad='T8')
brayt8 = ggplot(t8[[3]], aes(x=date, y=bray)) +
  geom_point() +
  geom_line() +
  ggtitle('T8') +
  xlab('') +
  xlim(as.Date('1915-06-15'), as.Date('2017-01-15')) +
  ylab('Bray-Curtis dissimilarity') +
  theme_bw()
t9 = prepare_quad_data(veg_use, quad='T9')
brayt9 = ggplot(t9[[3]], aes(x=date, y=bray)) +
  geom_point() +
  geom_line() +
  ggtitle('T9') +
  xlab('') +
  xlim(as.Date('1915-06-15'), as.Date('2017-01-15')) +
  ylab('') +
  theme_bw()
notransitionplot = plot_grid(brayb2a, brayi3, brayl4, brayn1, brayp1, brayp4, brayr1, brayt3, brayt7,
                             brayt8, brayt9, ncol=3)
#notransitionplot
ggsave(plot=notransitionplot, filename = 'Figures/quads_with_no_transition.png', width=8, height=8)



# transitions ----
# find 2-cluster for all quadrats that did have a transition
transitionquads = firstlasttest$quadrat[firstlasttest$p.adonis<=.05]
inflectiondates = c()
for (quad in transitionquads) {
  # prepare data
  quaddat_prepared = prepare_quad_data(veg_use, quad, max_year=2016)
  
  quaddates_empty = quaddat_prepared[[2]]
  quaddist_baseline = quaddat_prepared[[3]]
  quaddat3 = quaddat_prepared[[1]]
  
  # just veg columns to be used for distances
  distdat = dplyr::select(quaddat3, -quadrat, -project_year, -year, -month, -date)
  
  # do clustering; k=2
  cluster = pam(vegdist(distdat, method=distance_metric), k=2)
  quaddat3$cluster = cluster$clustering-1
  
  # are clusters perfectly in order?
  if (all(diff(quaddat3$cluster)>=0)) {
    # find date where quad switches to other cluster
    quantile.low = quaddat3$date[which(quaddat3$cluster==1)[1]-1]
    quantile.upper = quaddat3$date[which(quaddat3$cluster==1)[1]]
    inflectiondate_estimate = mean(c(quantile.low, quantile.upper))
    
    # logplot
    logplot = ggplot(quaddat3, aes(x=date, y=cluster)) +
      geom_point() +
      geom_vline(xintercept=inflectiondate_estimate) +
      geom_point(data=quaddist_baseline, aes(x=date, y=bray), alpha=.3) +
      geom_line(data=quaddist_baseline, aes(x=date, y=bray), alpha=.3) +
      ggtitle(quad) +
      xlab('') +
      xlim(as.Date('1915-06-15'), as.Date('2017-01-15')) +
      ylab('Bray-Curtis dissimilarity') +
      theme_bw()

    } else {
    # fit a logistic model
    glm.fit = glm(cluster ~ date, data=quaddat3, family=binomial)
    
    # predicted values
    predict.on = data.frame(date=seq.Date(from = min(quaddat3$date), to=max(quaddat3$date), by='quarter'))
    predictedvals = data.frame(date=predict.on$date,
                               predicted=predict.glm(glm.fit, newdata=predict.on, type='response'))
    #predictedvals = data.frame(date=quaddat$date, predicted=glm.fit$fitted.values)
    
    # inflection point
    inflectiondate_estimate = predictedvals$date[which(predictedvals$predicted>.5)[1]]
    quantile.low = predictedvals$date[which(predictedvals$predicted>.25)[1]]
    quantile.upper = predictedvals$date[which(predictedvals$predicted>.75)[1]]
    
    # logplot
    logplot = ggplot(quaddat3, aes(x=date, y=cluster)) +
      geom_point() +
      geom_line(data=predictedvals, aes(x=date, y=predicted)) +
      geom_vline(xintercept=inflectiondate_estimate) +
      geom_point(data=quaddist_baseline, aes(x=date, y=bray), alpha=.3) +
      geom_line(data=quaddist_baseline, aes(x=date, y=bray), alpha=.3) +
      ggtitle(quad) +
      xlab('') +
      xlim(as.Date('1915-06-15'), as.Date('2017-01-15')) +
      ylab('Bray-Curtis dissimilarity') +
      theme_bw()
    }
  
  if (nrow(quaddates_empty)>0) {
    logplot = logplot +geom_point(data=quaddates_empty, aes(x=date, y=rep(0)), color='red', shape=23) 
  }
  #logplot
  ggsave(plot=logplot, filename=paste0('Figures/quad_changepoints/',quad,'.png'), width=3, height=3)
  inflectiondates = rbind(inflectiondates, data.frame(quadrat=quad,
                                                      date_estimate=inflectiondate_estimate,
                                                      date_lowerbound = quantile.low,
                                                      date_upperbound = quantile.upper))
  
}
write.csv(inflectiondates, 'communitychange/quadrat_count_changepoints.csv', row.names=F)
# =======================================
# histogram of changepoints
# analysis using counts, not cover

# types of change experienced by quadrats:
# these have too few samples to pinpoint timing of change (within 5-10 years)
notenoughpts = c('K2','K4','L2','L5','P2','P3')

# these might be bad because of outliers late in timeseries
outliers = c('B5','I4','P4')

# these never got very far from initial conditions
nochange = c('A3','A4','G1','I3','L3','L4')

# bad for different reason
bad = c('AR4','AR5','L1')


# many totally blank samples later in timeseries
tobare = c('AR1','AR2','G5','J9','L5','M5','M6')

inflectionhist = dplyr::filter(inflectiondates, !quadrat %in% badquads)
vectorofnumbers = lubridate::year(inflectionhist$date_estimate)

hist(vectorofnumbers, breaks = seq(1920, 1980,5), xlab='', main='Histogram: year of greatest community change')

# =====================================
# extras
# NMDS to see if clustering is appropriate
for (quad in unique(dates$quadrat)) {
  # prepare data
  quaddat_prepared = prepare_quad_data(veg_use, quad, max_year=2016)
  
  quaddates_empty = quaddat_prepared[[2]]
  quaddist_baseline = quaddat_prepared[[3]]
  quaddat3 = quaddat_prepared[[1]]
  
  # just veg columns to be used for nmds
  nmdsdat = dplyr::select(quaddat3, -quadrat, -project_year, -year, -month, -date)
  
  quad.center = colMeans(nmdsdat)
  quad.cov = cov(nmdsdat)
  distances = mahalanobis(x=nmdsdat, center=quad.center, cov=quad.cov)
  cutoff = qchisq(p=.99, df = ncol(nmdsdat))
  quaddat3[distances > cutoff,]
  
  
  # do NMDS
  nmds = metaMDS(nmdsdat, distance='bray')
  data.scores = as.data.frame(scores(nmds))
  data.scores$project_year = quaddat3$project_year
  
  nmdsplot = ggplot(data.scores, aes(x=NMDS1, y=NMDS2)) + 
    geom_point() +
    geom_text(label=data.scores$project_year) +
    geom_path() +
    ggtitle(quad)
  theme_bw()
  ggsave(plot=nmdsplot, filename=paste0('Figures/nmds/',quad,'.png'), width=3, height=3)
}
