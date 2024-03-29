#' Dissimilarity timeseries
#' EMC 8/21/20

library(dplyr)
library(ggplot2)
library(vegan)

veg_herbaceous = read.csv('communitychange/veg_herbaceous_all.csv')
richness = read.csv('communitychange/richness_comparison_1928_2016.csv')


# ==============================================
# get average distance in 1928 and 2016

# create wide data frame
quaddat = dplyr::filter(veg_herbaceous, quadrat %in% richness$quadrat) %>% 
  dplyr::select(-cover, -year, -month, -day, -form, -category, -present) %>%
  tidyr::pivot_wider(names_from=species, values_from=count)
# fill in missing with zeros
quaddat[is.na(quaddat)] <- 0

# get samples from 1928
quaddat1928 = dplyr::filter(quaddat, project_year %in% c(1928)) 
# remove columns that are zeros
quaddat1928 = quaddat1928[, colSums(quaddat1928 != 0) >0]
# remove rows that are all zeros
quaddat1928_nozeros = quaddat1928[rowSums(quaddat1928[,-c(1,2)])>0,]
quaddat1928_zeros = quaddat1928[rowSums(quaddat1928[,-c(1,2)])==0,]

# mean pairwise distance in 1928
dist1928 = vegdist(quaddat1928_nozeros[,-c(1,2)], method = 'jaccard') %>% mean()

# get samples from 2016
quaddat2016 = dplyr::filter(quaddat, project_year %in% c(2016))
quaddat2016 = quaddat2016[, colSums(quaddat2016 != 0) >0]
quaddat2016_nozeros = quaddat2016[rowSums(quaddat2016[,-c(1,2)])>0,]

# mean pairwise distance in 2016
dist2016 = vegdist(quaddat2016_nozeros[,-c(1,2)], method='jaccard') %>% mean()

# ==============================================
# nmds plot 
nmdsdat = dplyr::select(quaddat2016_nozeros, -quadrat, -project_year)



nmdsdat[nmdsdat>0] <- 1

nmds = metaMDS(nmdsdat, distance='jaccard')
nmds
plot(nmds)
data.scores = as.data.frame(scores(nmds))
data.scores$quadrat = quaddat2016_nozeros$quadrat
data.scores$project_year = quaddat2016_nozeros$project_year

ggplot(data.scores, aes(x=NMDS1, y=NMDS2, color=project_year)) + 
  geom_point() +
  geom_text(label=data.scores$quadrat) +
  geom_path() +
  theme_bw()






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
