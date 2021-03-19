# look at remotely sensed shrub cover
# Shrub cover data from Wenji Ji
# EMC 9/1/20

library(dplyr)
library(ggplot2)

# remote sensed data from Wenji
shrub = read.csv('shrub/quadrat_cover.csv', stringsAsFactors = F)

# shrub cover from quadrat data
veg = read.csv('data/quadrat_veg.csv', stringsAsFactors = F)


# get avg quadrat shrub cover 2006-2016
veg_2006 = dplyr::filter(veg, project_year>=2006) %>%
  group_by(quadrat) %>%
  summarize(avg_shrub = mean(total_shrub),
            avg_grass = mean(total_grass),
            max_grass = max(total_grass))

# merge with remote sense
shrub_combined = merge(shrub, veg_2006, by='quadrat')


# ---------------
# linear model
quadlm = lm(mean ~ avg_shrub-1, data=shrub_combined)
summary(quadlm)

# plot quad cover vs remote sensing
veg_remote = ggplot(shrub_combined, aes(x=avg_shrub, y=mean)) +
  geom_point() +
  xlab('quadrat shrub cover') +
  ylab('remote shrub cover') +
  geom_abline(aes(slope=quadlm$coefficients[1], intercept=0)) +
  xlim(0,1) +
  ylim(0,1)
veg_remote
ggsave('Figures/shrub_compare_quad_remote.png', plot=veg_remote, width=4, height=3)

# clearly the quadrat shrub cover is not well correlated to the remotely sensed data

# look at DK data collected in 2001
dk = read.csv('../JRN_quadrat_datapaper/PatchVeg/Jornada_quadrat_patch_vegetation.csv', stringsAsFactors = F)
dk_splist = read.csv('../JRN_quadrat_datapaper/PatchVeg/Jornada_quadrat_patch_species_list.csv', stringsAsFactors = F)
dk_shrub = merge(dk, dk_splist) %>%
  dplyr::filter(form %in% c('SHRUB','S-SHR','TREE')) %>%
  group_by(quadrat) %>%
  summarize(dk_shrub = sum(pct_cover)/20/100)

# if quadrats are missing from this df but are in the original data, they have no shrub
dk_bare = data.frame(quadrat = setdiff(unique(dk$quadrat),unique(dk_shrub$quadrat)),
                     dk_shrub = rep(0))
dk_shrub = rbind(dk_shrub, dk_bare)
dk_shrub$quadrat[dk_shrub$quadrat == 'AR5-6'] <- 'AR5'

# merge with other data
shrub_all = merge(shrub_combined, dk_shrub, all.x=T)

# linear model
dkmodel = lm(mean ~ dk_shrub -1, data=shrub_all)
summary(dkmodel)

DK_remote = ggplot(shrub_all, aes(x=dk_shrub, y=mean)) +
  geom_point() +
  xlab('2001 DK shrub cover') +
  ylab('remote shrub cover (mean)') +
  geom_abline(aes(slope=dkmodel$coefficients[1], intercept=0)) +
  ylim(0,1) +
  xlim(0,1)
DK_remote
ggsave('Figures/shrub_compare_dk_remote.png', plot=DK_remote, width=4, height=3)

# just for kicks, what is the correlation between DK and quadrat shrub values
dkquad = lm(avg_shrub ~ dk_shrub-1, data=shrub_all)
summary(dkquad)

DK_quad = ggplot(shrub_all, aes(x=dk_shrub, y=avg_shrub)) +
  geom_point() +
  xlab('2001 DK shrub cover') +
  ylab('quadrat shrub cover') +
  geom_abline(aes(slope=dkquad$coefficients[1], intercept=0)) +
  ylim(0,1) +
  xlim(0,1)
DK_quad
ggsave('Figures/shrub_compare_dk_quadrat.png', plot=DK_quad, width=4, height=3)

# not great!

# ============================================
# How does remotely sensed shrub neigborhood correlate to grass cover?

# linear model
quadlm_grass = lm(mean ~ max_grass-1, data=shrub_combined)
summary(quadlm_grass)

# plot quad cover vs remote sensing
grass_remote = ggplot(shrub_combined, aes(x=max_grass, y=mean)) +
  geom_point() +
  xlab('quadrat grass cover') +
  ylab('remote-sensing shrub cover') +
  #geom_abline(aes(slope=quadlm_grass$coefficients[1], intercept=0)) +
  xlim(0,1) +
  ylim(0,1)
grass_remote
ggsave('Figures/remote_shrub_vs_grass_2006_2016.png', plot=grass_remote, width=4, height=3)
