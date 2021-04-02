# look at changes/differences in species richness through time
# EMC 10.29.20
# last update: 2/18/21

library(dplyr)
library(ggplot2)
library(vegan)

# read data files
veg_selected_noblank = read.csv('data/all_species_counts_cover.csv', stringsAsFactors = F)
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)
splist = read.csv('../JRN_quadrat_datapaper/Plants/Jornada_quadrat_species_list.csv', stringsAsFactors = F)
crosstab = read.csv('data/crosstab_subset_quads_and_years.csv', stringsAsFactors = F)
#categories = read.csv('data/quadrat_veg_categories.csv', stringsAsFactors = F)


# if only using the 44 quads in 29 years
subsetofyears =  names(crosstab)[grepl('[[:digit:]]', names(crosstab))] %>%
  readr::parse_number()
subsetofquads = crosstab$quadrat

# make species lists by type
usedspecies = splist %>% dplyr::filter(species_code %in% veg_selected_noblank$species)
grassspecies = usedspecies %>% dplyr::filter(form=='GRASS', species !='')
shrubspecies = usedspecies %>% dplyr::filter(form=='SHRUB', species !='')
sshrubspecies = usedspecies %>% dplyr::filter(form=='S-SHR', species !='')
forbspecies = usedspecies %>% dplyr::filter(form=='FORB', species !='')
otherspecies = usedspecies %>% dplyr::filter(form=='', species !='')
# most of the otherspecies are probably forbs

# just "known" species -- includes codes identified to genus (e.g. SPORO) but not total unknowns (e.g. UNKPG)
knownspecies = usedspecies %>% dplyr::filter(species !='')
# "complete" species -- does not include identified to genus
completespecies = knownspecies %>% dplyr::filter(species != 'sp.')

# remove shrub and subshrub species
veg_selected_noblank_noshrub = dplyr::filter(veg_selected_noblank, !form %in% c('SHRUB','S-SHR'), species %in% knownspecies$species_code)

# combine veg with dates so blank quadrats will be included
veg_selected_noshrub = veg_selected_noblank_noshrub %>%
  mutate(present = rep(1)) %>%
  merge(dates, all=T)
veg_selected_noshrub$present[is.na(veg_selected_noshrub$present)] <- 0

# calculate average richness per quadrat (including blank quadrats)
quadrichness = veg_selected_noshrub %>%
  #dplyr::filter(project_year<=1945) %>%
  #dplyr::filter(quadrat %in% subsetofquads$quadrat, project_year %in% subsetofyears) %>%
  #dplyr::filter(!is.na(species)) %>%
  group_by(quadrat, project_year) %>%
  summarize(nsp_quad = sum(present)) %>%
  ungroup() 

# filter out quadrats 
#quadrichness = dplyr::filter(quadrichness, !quadrat %in% c('AR6','R2','K2'))
# just use 44 quads in 29 years
quadrichness_subset = dplyr::filter(quadrichness, quadrat %in% subsetofquads, project_year %in% subsetofyears)

# ====================================================
# richness time series
# get average per-quad richness in each project year
quadrichness_mean = quadrichness_subset %>%
  group_by(project_year) %>%
  summarize(nquads = length(unique(quadrat)),
            mean_r = mean(nsp_quad),
            sd = sd(nsp_quad),
            sp_min = min(nsp_quad),
            sp_max = max(nsp_quad))
# take only values from years where at least 30 quads included (out of 90 total)
#quadrichness_mean = dplyr::filter(quadrichness_mean, nquads>=78)

richness_per_quadrat = ggplot(quadrichness_mean, aes(x=project_year, y=mean_r)) +
  geom_ribbon(aes(x=project_year, ymin=mean_r-sd, ymax=mean_r+sd), alpha=.2) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('# species/quadrat') +
  ggtitle('Species richness per quadrat') +
  theme_bw()
richness_per_quadrat
ggsave(richness_per_quadrat, filename='Figures/richness/species_richness_per_quadrat_44quads_29years.png', width=4, height=3)
#write.csv(quadrichness, 'communitychange/species_richness_per_quadrat.csv', row.names=F)

# average species richness 1915-1945
quadrichness_mean %>% dplyr::filter(project_year <1946) %>% summarize(mean(mean_r))
# average species richness 19995 - 2016
quadrichness_mean %>% dplyr::filter(project_year >=1995) %>% summarize(mean(mean_r))

# ==================================================================
# species richness across all quads by year (i.e. what is happening in 1935-38?)
unique(veg_selected_noshrub$species[veg_selected_noshrub$project_year==1935]) %>% sort()
unique(veg_selected_noshrub$species[veg_selected_noshrub$project_year==1934]) %>% sort()


# ==================================================================
# total site richness (for 44 quads in 29 years)

# get list of species and year
sitespecies = veg_selected_noshrub %>%
  dplyr::select(project_year, species) %>%
  dplyr::filter(!is.na(species)) %>%
  unique()

# site-level richness
siterichness = sitespecies %>%
  group_by(project_year) %>%
  summarize(n_species=n_distinct(species))

sitespeciesrich = ggplot(siterichness, aes(x=project_year, y=n_species)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('# species') +
  ggtitle('Site-wide species richness') +
  theme_bw() 
sitespeciesrich
ggsave(sitespeciesrich, filename='Figures/richness/species_richness_site_44quads_29years.png', width=4, height=3)




# ==================================================================
# look at 1995-2016 richness compared to remote sensed shrub (all 120 quadrats, not just 44)
# read in remote sensed shrub data
shrub_remote = read.csv('shrub/quadrat_cover.csv', stringsAsFactors = F) %>%
  dplyr::filter(quadrat %in% dates$quadrat)

# read in total grass and total shrub per quadrat
veg = read.csv('data/quadrat_veg.csv', stringsAsFactors = F)

# merge richness 2006-2016 with total grass shrub cover; get average valuesfor 2006-2016 period
richness_2006 = quadrichness %>% dplyr::filter(project_year>=2006) %>%
  left_join(veg) %>%
  dplyr::select(quadrat, project_year, nsp_quad, month, total_shrub, total_grass) %>%
  group_by(quadrat) %>%
  summarize(mean_richness=mean(nsp_quad),
            mean_shrub=mean(total_shrub),
            mean_grass=mean(total_grass))


# combine shrub data with richness data
richness_shrub = richness_2006 %>%
  merge(shrub_remote[,c('quadrat','mean')], by='quadrat', all=T) %>%
  dplyr::rename(remote_shrub=mean)

# remote sensed shrub vs sp richness
ggplot(richness_shrub, aes(x=remote_shrub, y=mean_richness)) +
  geom_jitter(height=.1) +
  xlab('neighborhood shrub cover (%)') +
  ylab('species richness') +
  ggtitle('Species richness vs. neighborhood shrub cover 2006-2016')

# local shrub vs sp richness
ggplot(richness_shrub, aes(x=mean_shrub, y=mean_richness)) +
  geom_jitter(height=.1) +
  xlab('local shrub cover (%)') +
  ylab('species richness') +
  ggtitle('Species richness vs. local shrub cover 2006-2016')

# create categories for shrub present/absent
richness_shrub$shrub_presence = 0
richness_shrub$shrub_presence[richness_shrub$mean_shrub>0] <-1
richness_shrub$shrub_neighborhood_presence = 0
richness_shrub$shrub_neighborhood_presence[richness_shrub$remote_shrub>.1] <- 1

# boxplot: shrubs present
box_shrubpresent = ggplot(richness_shrub, aes(x=local_category, y=mean_richness, group=local_category)) +
  geom_boxplot() +
  geom_jitter(width=.1, height=.1, alpha=.1) +
  ylab('# species per quadrat') +
  #xlab('Vegetation Type') +
  #ggtitle('Species richness by vegetation category') +
  theme_bw()
box_shrubpresent

# test for significant difference
oneway.test(mean_richness ~ shrub_presence, data = richness_shrub) # p=.11
kruskal.test(mean_richness ~ shrub_presence, data = richness_shrub) # p=.059
# not significant



# # boxplot: grass present
# box_grasspresent = ggplot(richness_shrub, aes(x=grass_presence, y=nsp_quad, group=grass_presence)) +
#   geom_boxplot() +
#   geom_jitter(width=.1, height=.1, alpha=.1) +
#   ylab('# species per quadrat') +
#   #xlab('Vegetation Type') +
#   #ggtitle('Species richness by vegetation category') +
#   theme_bw()
# box_grasspresent

# =================================================================
# look for changes in richness for quads that were grassland in 1928 and shrubland in 2006-2016

# read in 1928 veg type data
shrub1928 = read.csv('data/quadrats_vegtype_1915_1928_1998.csv')
# get only non-shrub quadrats
grassquads1928 = dplyr::filter(shrub1928, Type_Name_1928=='OTHER')
# get non-shrub and sparse shrub quadrats (cutoff 15%)
grassquads1928 = dplyr::filter(shrub1928, shrub_class_1928 %in% c(0,1))

# get avg species richness for 1928
richness_1928 = quadrichness %>%
  dplyr::filter(project_year == 1928) %>%
  rename(richness_1928=nsp_quad)

# combine with modern data
richness_historical_comparison = merge(grassquads1928, richness_shrub) %>%
  dplyr::select(quadrat, mean_richness_modern=mean_richness, remote_shrub, Type_Name_1928) %>%
  merge(richness_1928)
richness_historical_comparison$moderntype = rep('shrub')
richness_historical_comparison$moderntype[richness_historical_comparison$remote_shrub<.15]<- 'not shrubland'

# get into long format
richness_historical_comparison_long1 = dplyr::select(richness_historical_comparison, quadrat, richness=mean_richness_modern, moderntype) %>%
  mutate(period=rep('after'))
richness_historical_comparison_long2 = dplyr::select(richness_historical_comparison, quadrat, richness=richness_1928, moderntype) %>% 
  mutate(period=rep('before'))

richness_historical_comparison_long = rbind(richness_historical_comparison_long1, richness_historical_comparison_long2) %>%
  mutate(grouping = paste(moderntype, period, sep='_'))

# how many of each category
table(richness_historical_comparison_long$grouping)

# boxplot: richness comparing 1928 to 2016 and transitioned to shrub/ did not transition to shrub
box_richness_shrubtransition = ggplot(richness_historical_comparison_long, aes(x=grouping, y=richness, group=grouping)) +
  geom_boxplot() +
  geom_jitter(width=.1, height=.1, alpha=.1) +
  ylab('# species per quadrat') +
  #xlab('Vegetation Type') +
  #ggtitle('Species richness by vegetation category') +
  theme_bw()
box_richness_shrubtransition

# test for significant difference
oneway.test(richness ~ grouping, data = richness_historical_comparison_long)
kruskal.test(richness ~ grouping, data = richness_historical_comparison_long)
# yes to both
# pairwise t-test to see which pairs differ (no assumption of equal variances). bonferroni is very conservative
pairwise.t.test(richness_historical_comparison_long$richness, richness_historical_comparison_long$grouping, 
                p.adjust.method = 'bonferroni', pool.sd=F)
# shrub_after is significantly different from all other groups!

# ==================================================================
# compare richness 
# are there fewer species on shrub-dominated quadrats? (filter out totally bare)
richness_categories = merge(quadrichness, categories, all.x=T) %>%
  dplyr::filter(veg_type != 'bare')

# boxplot: all veg type categories
box_vegtype = ggplot(richness_categories, aes(x=veg_type, y=nsp_quad)) +
  geom_boxplot() +
  geom_jitter(width=.1, height=.1, alpha=.1) +
  ylab('# species per quadrat') +
  xlab('Vegetation Type') +
  ggtitle('Species richness by vegetation category') +
  theme_bw()
box_vegtype

# test for significant difference
oneway.test(nsp_quad ~ veg_type, data = richness_categories)
kruskal.test(nsp_quad ~ veg_type, data = richness_categories)
# yes
# pairwise t-test to see which pairs differ (no assumption of equal variances). bonferroni is very conservative
pairwise.t.test(richness_categories$nsp_quad, richness_categories$veg_type, p.adjust.method = 'bonferroni', pool.sd=F)
# all pairs of categories differ significantly except:
#    bare_with_forbs vs sparse_shrub
#    high_grass vs high_grass_and_shrub vs high_shrub

# boxplot: shrub/grass/mixed categories
box_category = ggplot(richness_categories, aes(x=category, y=nsp_quad)) +
  geom_boxplot() +
  geom_jitter(width=.1, height=.1, alpha=.1) +
  ylab('# species per quadrat') +
  xlab('') +
  ggtitle('Species richness by cover type') +
  scale_x_discrete(breaks=c('Bare','Grass','Mixed','Shrub'), labels=c('Forbs only','Grass','Mixed','Shrub')) +
  theme_bw()
box_category
ggsave(box_category,filename='Figures/richness/boxplot_richness_by_category_44quads_29years.png', width=4, height=3)

# test for significant difference
oneway.test(nsp_quad ~ category, data = richness_categories, var.equal = TRUE)
kruskal.test(nsp_quad ~ category, data = richness_categories)
# yes
# pairwise t-test to see which pairs differ (no assumption of equal variances). bonferroni is very conservative
pairwise.t.test(richness_categories$nsp_quad, richness_categories$category, p.adjust.method = 'bonferroni', pool.sd=F)
# all pairs are significantly different


# boxplot: compare grass levels
grassplotdata = richness_categories[richness_categories$veg_type %in% c('sparse_grass','high_grass','moderate_grass'),]
box_grasslevels = ggplot(grassplotdata,
                         aes(x=veg_type, y=nsp_quad)) +
  geom_boxplot() +
  geom_jitter(width=.1, height=.1, alpha=.1) +
  ylab('# species per quadrat') +
  xlab('') +
  ggtitle('Species richness by grass % cover') +
  scale_x_discrete(breaks=c('high_grass','moderate_grass','sparse_grass'), labels=c('High (>10%)','Moderate (1-10%)','Sparse (<1%)')) +
  theme_bw()
box_grasslevels
ggsave(box_grasslevels,filename='Figures/richness/boxplot_richness_by_grasspercent_44quads_29years.png', width=4, height=3)

# test for significant difference
oneway.test(nsp_quad ~ veg_type, data = grassplotdata, var.equal = TRUE)
kruskal.test(nsp_quad ~ veg_type, data = grassplotdata)
# yes
# pairwise t-test to see which pairs differ (no assumption of equal variances). bonferroni is very conservative
pairwise.t.test(grassplotdata$nsp_quad, grassplotdata$veg_type, p.adjust.method = 'bonferroni', pool.sd=F)
# all pairs are significantly different

# boxplot: compare shrub levels
shrubplotdata = richness_categories[richness_categories$veg_type %in% c('sparse_shrub','high_shrub','moderate_shrub'),]
box_shrublevels = ggplot(shrubplotdata,
                         aes(x=veg_type, y=nsp_quad)) +
  geom_boxplot() +
  geom_jitter(width=.1, height=.1, alpha=.1) +
  ylab('# species per quadrat') +
  xlab('') +
  ggtitle('Species richness by shrub % cover') +
  scale_x_discrete(breaks=c('high_shrub','moderate_shrub','sparse_shrub'), labels=c('High (>10%)','Moderate (1-10%)','Sparse (<1%)')) +
  theme_bw()
box_shrublevels
#ggsave(box_shrublevels,filename='Figures/richness/boxplot_richness_by_shrubpercent.png', width=4, height=3)

# test for significant difference
oneway.test(nsp_quad ~ veg_type, data = shrubplotdata, var.equal = TRUE)
kruskal.test(nsp_quad ~ veg_type, data = shrubplotdata)
# yes
# pairwise t-test to see which pairs differ (no assumption of equal variances). bonferroni is very conservative
pairwise.t.test(shrubplotdata$nsp_quad, shrubplotdata$veg_type, p.adjust.method = 'bonferroni', pool.sd=F)
# sparse is different from the other two, mod and high are not different from each other


