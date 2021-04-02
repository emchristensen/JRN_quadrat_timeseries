#' Code to compare species richness on quadrats between 1928 (before shrub encroachment) and 2006-2016 (after shrub encroachment)
#' EMC 3/30/21

library(dplyr)
library(ggplot2)

# colors for figures
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# read data files
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
  rename(richness=nsp_quad)

# get species richness for each quadrat in 2016 (91 quadrats)
richness_2016 = quadrichness %>%
  dplyr::filter(project_year == 2016) %>%
  rename(richness=nsp_quad)

# combine 1928 and 2016 into single data frame and merge with shrub
#   also restrict to quadrats that were no or sparse shrub in 1928, and there is a 1928 sample
richness_comparison = rbind(richness_1928, richness_2016) %>%
  left_join(shrub_remote) %>%
  dplyr::filter(quadrat %in% grassquads1928$quadrat, quadrat %in% richness_1928$quadrat) %>%
  mutate(grouping = paste(shrubland, project_year, sep='_'))

# write to csv 
write.csv(richness_comparison, 'communitychange/richness_comparison_1928_2016.csv', row.names=F)

# how many of each category
table(richness_comparison$grouping)

# boxplot: richness comparing 1928 to 2016 and transitioned to shrub/ did not transition to shrub
box_richness_shrubtransition = ggplot(richness_comparison, aes(x=grouping, y=richness, group=grouping, color=as.factor(project_year))) +
  scale_color_manual(values=cbPalette[c(2,6)]) +
  geom_boxplot() +
  geom_jitter(width=.1, height=.1, alpha=.1) +
  ylab('# species per quadrat') +
  xlab('') +
  labs(color='Year') +
  scale_x_discrete(labels=c('not shrub\n encroached','','shrub encroached','')) +
  ggtitle('Species richness: 1928 vs. 2016') +
  theme_bw()
box_richness_shrubtransition
ggsave(box_richness_shrubtransition, filename='Figures/richness/species_richness_comparison_1928_2016.png', width=4, height=3)

# test for significant difference
oneway.test(richness ~ grouping, data = richness_comparison)
kruskal.test(richness ~ grouping, data = richness_comparison)
# yes to both
# pairwise t-test to see which pairs differ (no assumption of equal variances). bonferroni is very conservative
pairwise.t.test(richness_comparison$richness, richness_comparison$grouping, 
                p.adjust.method = 'bonferroni', pool.sd=F)
# shrub_encroached_2016 is significantly different from all other groups