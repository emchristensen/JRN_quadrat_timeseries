#' Code to compare species richness on quadrats between 1928 (before shrub encroachment) and 2006-2016 (after shrub encroachment)
#' Darren suggested subtracting 1928 richness form 2016 for each quadrat then one t-test
#' EMC 4/5/21

library(dplyr)
library(ggplot2)

# colors for figures
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# ===================
# read data files and proces data
veg_selected_noblank = read.csv('data/all_species_counts_cover.csv', stringsAsFactors = F)
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)
splist = read.csv('../JRN_quadrat_datapaper/Plants/Jornada_quadrat_species_list.csv', stringsAsFactors = F)
#crosstab = read.csv('data/crosstab_subset_quads_and_years.csv', stringsAsFactors = F)
#categories = read.csv('data/quadrat_veg_categories.csv', stringsAsFactors = F)

usedspecies = splist %>% dplyr::filter(species_code %in% veg_selected_noblank$species)
# just "known" species -- includes codes identified to genus (e.g. SPORO) but not total unknowns (e.g. UNKPG)
knownspecies = usedspecies %>% dplyr::filter(species !='')

# remove unknowns, shrub and subshrub species
veg_selected_noblank_noshrub = dplyr::filter(veg_selected_noblank, !form %in% c('SHRUB','S-SHR'), species %in% knownspecies$species_code)

# combine veg with dates so blank quadrats will be included
veg_selected_noshrub = veg_selected_noblank_noshrub %>%
  mutate(present = rep(1)) %>%
  merge(dates, all=T)
veg_selected_noshrub$present[is.na(veg_selected_noshrub$present)] <- 0
write.csv(veg_selected_noshrub, 'communitychange/veg_herbaceous_all.csv', row.names=F)

# calculate average richness per quadrat (including blank quadrats)
quadrichness = veg_selected_noshrub %>%
  group_by(quadrat, project_year) %>%
  summarize(nsp_quad = sum(present)) %>%
  ungroup() 


# =================================================================
# analysis: changes in richness for quads that were grassland in 1928 and shrubland in 2006-2016

# read in historical veg type data
shrub1928 = read.csv('data/quadrats_vegtype_1915_1928_1998.csv')

# get quadrats that were non-shrub (0%) or sparse shrub (0-15%) in 1928 (101 quadrats)
grassquads1928 = dplyr::filter(shrub1928, shrub_class_1928 %in% c(0,1))

# read in remote-sensed shrub cover (avg of 2005, 2011, and 2016 estimates -- better estimate than a single value)
shrub_remote = read.csv('shrub/quadrat_cover.csv') %>% dplyr::select(quadrat, mean)
# create column for shrubland/not shrubland (cutoff at 15% shrub to match historical category)
shrub_remote$shrubland = rep('shrub_encroached')
shrub_remote$shrubland[shrub_remote$mean<=.15] <- 'not_shrub_encroached'

# get species richness for each quadrat in 1928 sample (86 quads have a sample in this year)
richness_1928 = quadrichness %>%
  dplyr::filter(project_year == 1928) %>%
  rename(richness_1928=nsp_quad)

# get species richness for each quadrat in 2016 (91 quadrats)
richness_2016 = quadrichness %>%
  dplyr::filter(project_year == 2016) %>%
  rename(richness_2016=nsp_quad)

# combine 1928 and 2016 into single data frame and merge with shrub
#   also restrict to quadrats that were no or sparse shrub in 1928, and there is a 1928 sample
richness_comparison = merge(richness_1928, richness_2016, by='quadrat') %>%
  left_join(shrub_remote) %>%
  dplyr::filter(quadrat %in% grassquads1928$quadrat) %>%
  dplyr::mutate(sprich_change = richness_2016-richness_1928)
  
# write to csv 
write.csv(richness_comparison, 'communitychange/richness_comparison_1928_2016.csv', row.names=F)

# how many in each category
table(richness_comparison$shrubland)

# boxplot: richness comparing 1928 to 2016 and transitioned to shrub/ did not transition to shrub
box_richness_shrubtransition = ggplot(richness_comparison, aes(x=shrubland, y=sprich_change, group=shrubland, color=as.factor(shrubland))) +
  scale_color_manual(values=cbPalette[c(2,6)]) +
  geom_boxplot() +
  geom_jitter(width=.1, height=.1, alpha=.1) +
  ylab('# species gained/lost') +
  xlab('') +
  labs(color='Year') +
  scale_x_discrete(labels=c('not shrub\n encroached','shrub\n encroached')) +
  ggtitle('Species richness change: 1928 to 2016') +
  theme_bw()
box_richness_shrubtransition
ggsave(box_richness_shrubtransition, filename='Figures/richness/species_richness_comparison_1928_2016.png', width=4, height=3)

# test for significant difference
oneway.test(sprich_change ~ shrubland, data = richness_comparison)
kruskal.test(sprich_change ~ shrubland, data = richness_comparison)
# yes to both
# pairwise t-test to see which pairs differ (no assumption of equal variances). bonferroni is very conservative
pairwise.t.test(richness_comparison$sprich_change, richness_comparison$shrubland, 
                p.adjust.method = 'bonferroni', pool.sd=F)
# shrub_encroached is significantly different from not encroached


# what are the average values in 2016 and 1928?
richness_comparison %>%
  group_by(shrubland) %>%
  summarize(mean=mean(sprich_change),
            sd=sd(sprich_change))
# average loss for not shrub encroached was 0.64, shrub encroached was 2.05

# ======================================================
# what was site-wide species richness

site_richness_1928 = veg_selected_noshrub %>%
  dplyr::filter(project_year==1928, quadrat %in% richness_comparison$quadrat) %>%
  dplyr::select(species, form, category) %>%
  unique()
# 31 species: 12 grass and 19 forb

site_richness_2016 = veg_selected_noshrub %>%
  dplyr::filter(project_year==2016, quadrat %in% richness_comparison$quadrat) %>%
  dplyr::select(species, form, category) %>%
  unique()
# 26 species: 11 grass and 15 forb