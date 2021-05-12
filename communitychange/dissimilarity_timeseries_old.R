#' Dissimilarity timeseries
#' EMC 8/21/20

library(dplyr)
library(ggplot2)
library(vegan)

# read in veg data and remove unknowns
veg_noblank = read.csv('data/all_species_counts_cover.csv', stringsAsFactors = F) 
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)
# merge with dates to get only one sample per quadrat per year
veg_selected_noblank = merge(dates, veg_noblank)
# merge with dates so empty quadrats are included
veg = merge(dates, veg_noblank, all.x=T)

# get species list and remove unknowns, keeping genus-only codes
splist = read.csv('../JRN_quadrat_datapaper/Plants/Jornada_quadrat_species_list_WIP.csv', stringsAsFactors = F)
knownspecies = splist %>%
  dplyr::filter(!is.na(species), species !='', habit !='A')

# data frame to use: no blank charts included, only known species, only one chart per quad per year
veg_nounkn = dplyr::filter(veg_selected_noblank, species %in% c(knownspecies$species_code, NA)) %>%
  dplyr::select(-form, -category, -day)
veg_nounkn$date = as.Date(paste(veg_nounkn$year, veg_nounkn$month, '15',sep='-')) 

# ==============================================
# create wide data frame
quaddat = dplyr::filter(veg_nounkn, quadrat=='A1') %>% 
  dplyr::select(-count) %>%
  tidyr::pivot_wider(names_from=species, values_from=cover)
# fill in missing with zeros
quaddat[is.na(quaddat)] <- 0

# get baseline: first 5 years avg
baseline = quaddat %>%
  slice(1:5) %>%
  dplyr::select(-quadrat, -project_year, -year, -month, -date) %>%
  colMeans()

distdat = rbind(baseline, select(quaddat,-quadrat,-project_year,-year,-month,-date))

# ==============================================================
# get bray-curtis distance: every date compared to baseline
quaddist = data.frame()
for (n in 2:dim(distdat)[1]) {
  ndist = vegdist(distdat[c(1,n),], 'bray')
  
  quaddist = rbind(quaddist, data.frame(bray=as.numeric(ndist)))
}
quaddist$date = quaddat$date

ggplot(quaddist, aes(x=date, y=bray)) +
  geom_line() +
  ylab('Bray-Curtis dissimilarity') +
  ylim(0,1) +
  ggtitle('A1') +
  geom_point()

# ==================================================================
# get Bray-curtis distance: pairs of adjacent points
quaddist2 = data.frame()
for (n in 2:dim(distdat)[1]) {
  ndist = vegdist(distdat[c(n-1,n),], 'bray')

  quaddist2 = rbind(quaddist2, data.frame(bray=as.numeric(ndist)))
}
quaddist2$date = quaddat$date
quaddist2$project_year = quaddat$project_year
quaddist2$yeardiff = c(NA,diff(quaddist2$project_year))
quaddist2$yeardiff[quaddist2$yeardiff==0] <- 1
quaddist2$bray_normalized = quaddist2$bray/quaddist2$yeardiff

ggplot(quaddist2, aes(x=date, y=bray)) +
  geom_line() +
  ylab('Bray-Curtis dissimilarity') +
  ylim(0,1) +
  ggtitle('A1') +
  geom_point()

# ==============================
# what about nmds
set.seed(123)

# try looking at 2 quads at once
a1n3 = dplyr::filter(veg, quadrat %in% c('A2')) %>% 
  dplyr::select(-cover) %>%
  tidyr::pivot_wider(names_from=species, values_from=count)
a1n3[is.na(a1n3)] <- 0

nmdsdat = dplyr::select(a1n3, -quadrat, -project_year, -year, -month, -date)

nmds = metaMDS(nmdsdat, distance='bray')
nmds
plot(nmds)
data.scores = as.data.frame(scores(nmds))
data.scores$quadrat = a1n3$quadrat
data.scores$project_year = a1n3$project_year

ggplot(data.scores, aes(x=NMDS1, y=NMDS2, color=quadrat)) + 
  geom_point() +
  geom_text(label=data.scores$project_year) +
  geom_path()
