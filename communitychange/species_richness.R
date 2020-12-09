# look at changes/differences in species richness through time
# EMC 10.29.20
# last update: 11/30/20

library(dplyr)
library(ggplot2)
library(vegan)

# read data files
veg_selected_noblank = read.csv('data/all_species_counts_cover.csv', stringsAsFactors = F)
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)
splist = read.csv('../JRN_quadrat_datapaper/Plants/Jornada_quadrat_species_list_WIP.csv', stringsAsFactors = F)
crosstab = read.csv('data/crosstab_subset_quads_and_years.csv', stringsAsFactors = F)


months = c(8,9,10,11,12,1,2,3)

# make species lists by type
usedspecies = splist %>% dplyr::filter(species_code %in% veg_selected_noblank$species)
grassspecies = usedspecies %>% dplyr::filter(form=='GRASS', species !='')
shrubspecies = usedspecies %>% dplyr::filter(form=='SHRUB', species !='')
sshrubspecies = usedspecies %>% dplyr::filter(form=='S-SHR', species !='')
forbspecies = usedspecies %>% dplyr::filter(form=='FORB', species !='')
otherspecies = usedspecies %>% dplyr::filter(form=='', species !='')
# most of the otherspecies are probably forbs

knownspecies = usedspecies %>% dplyr::filter(species !='')
# ===========================================================
# 
# # merge with dates so empty quadrats are included
# veg = merge(dates, veg_selected_noblank, all.x=T) %>% dplyr::filter(month %in% months)
# 
# # find out how many quads each species was found on for each year
# species_richness = veg %>%
#   group_by(project_year) %>%
#   summarize(nquads = length(unique(quadrat)),
#             nspecies = length(unique(species)))
# grass_richness = veg %>%
#   dplyr::filter(species %in% grassspecies$species_code) %>%
#   group_by(project_year) %>%
#   summarize(nsp_grass = length(unique(species)))
# shrub_richness = veg %>%
#   dplyr::filter(species %in% shrubspecies$species_code) %>%
#   group_by(project_year) %>%
#   summarize(nsp_shrub = length(unique(species)))
# subshrub_richness = veg %>%
#   dplyr::filter(species %in% sshrubspecies$species_code) %>%
#   group_by(project_year) %>%
#   summarize(nsp_subshrub = length(unique(species)))
# forb_richness = veg %>%
#   dplyr::filter(species %in% c(forbspecies$species_code, otherspecies$species_code)) %>%
#   group_by(project_year) %>%
#   summarize(nsp_forb = length(unique(species)))
# 
# species_richness = merge(species_richness, grass_richness, all=T) %>%
#   merge(shrub_richness, all=T) %>%
#   merge(subshrub_richness, all=T) %>%
#   merge(forb_richness, all=T) 
# 
# # fill in blanks with 0
# species_richness[is.na(species_richness)] <- 0
# #species_richness$allforb_richness = species_richness$nsp_forb + species_richness$nsp_other
# 
# plotdata = tidyr::pivot_longer(species_richness, cols=c('nspecies','nsp_grass','nsp_shrub','nsp_subshrub','nsp_forb'), 
#                                names_to='veg_type') %>%
#   dplyr::filter(nquads>50)
# sprichness = ggplot(plotdata, aes(x=project_year, y=value, color=veg_type)) +
#   geom_line() +
#   geom_point() +
#   scale_color_discrete(name='Plant type', labels=c('Forbs','Grasses','Shrubs','Subshrubs','All Species')) +
#   xlab('') +
#   ylab('# species') +
#   ggtitle('Species richness') +
#   theme_bw()
# sprichness
# ggsave(sprichness, filename = 'Figures/species_richness_wholeJornada.png', width=5, height=3)
# 
# # calculate average richness per quadrat
# plotdata_perquad = plotdata
# plotdata_perquad$avg_richness = plotdata_perquad$value/plotdata_perquad$nquads
# ggplot(plotdata_perquad, aes(x=project_year, y=avg_richness, color=veg_type)) +
#   geom_line() +
#   geom_point() +
#   scale_color_discrete(name='Plant type', labels=c('Forbs','Grasses','Shrubs','Subshrubs','All Species')) +
#   xlab('') +
#   ylab('# species/quadrat') +
#   ggtitle('Species richness per quadrat') +
#   theme_bw()

# =================================================
# use only the subset of 44 quads and 29 years where all quads were sampled in all years
selected_project_years =  names(crosstab)[grepl('[[:digit:]]', names(crosstab))] %>%
  readr::parse_number()
veg_selectedquads = dplyr::filter(veg_selected_noblank, quadrat %in% crosstab$quadrat, project_year %in% selected_project_years)

# find richness of groups by year
species_richness = veg_selectedquads %>%
  group_by(project_year) %>%
  summarize(nquads = length(unique(quadrat)),
            nspecies = length(unique(species)))
grass_richness = veg_selectedquads %>%
  dplyr::filter(species %in% grassspecies$species_code) %>%
  group_by(project_year) %>%
  summarize(nsp_grass = length(unique(species)))
shrub_richness = veg_selectedquads %>%
  dplyr::filter(species %in% shrubspecies$species_code) %>%
  group_by(project_year) %>%
  summarize(nsp_shrub = length(unique(species)))
subshrub_richness = veg_selectedquads %>%
  dplyr::filter(species %in% sshrubspecies$species_code) %>%
  group_by(project_year) %>%
  summarize(nsp_subshrub = length(unique(species)))
forb_richness = veg_selectedquads %>%
  dplyr::filter(species %in% c(forbspecies$species_code, otherspecies$species_code)) %>%
  group_by(project_year) %>%
  summarize(nsp_forb = length(unique(species)))

species_richness = merge(species_richness, grass_richness, all=T) %>%
  merge(shrub_richness, all=T) %>%
  merge(subshrub_richness, all=T) %>%
  merge(forb_richness, all=T) 

# fill in blanks with 0
species_richness[is.na(species_richness)] <- 0

plotdata = tidyr::pivot_longer(species_richness, cols=c('nspecies','nsp_grass','nsp_shrub','nsp_subshrub','nsp_forb'), 
                               names_to='veg_type') 
sprichness = ggplot(plotdata, aes(x=project_year, y=value, color=veg_type)) +
  geom_line() +
  geom_point() +
  scale_color_discrete(name='Plant type', labels=c('Forbs','Grasses','Shrubs','Subshrubs','All Species')) +
  xlab('') +
  ylab('# species') +
  ggtitle('Species richness') +
  theme_bw()
sprichness
ggsave(sprichness, filename = 'Figures/richness/species_richness_wholeJornada_44quads_29years.png', width=5, height=3)

# ===================================
# use rarefaction to account for different sample sizes
# tutorial: https://peat-clark.github.io/BIO381/veganTutorial.html

# load crosstab data
veg_crosstab = read.csv('data/all_veg_count_crosstab.csv', stringsAsFactors = F) %>%
  dplyr::filter(quadrat %in% crosstab$quadrat, project_year %in% selected_project_years)


# select just years that have over 50 quadrats, combine all quadrats for species richness
richnessmatrix = veg_crosstab %>%
  dplyr::select(-month, -quadrat, -year) %>%
  group_by(project_year) %>%
  summarize_each(list(sum))
# rarify based on number of individuals
sRare_all <- rarefy(richnessmatrix[,-1], min(rowSums(richnessmatrix[,-1])))
sRare_all
rarecurve(richnessmatrix[,-1])

# grass species
grassmatrix = veg_crosstab %>%
  dplyr::select(project_year, grassspecies$species_code) %>%
  group_by(project_year) %>%
  summarize_each(list(sum))
grassmatrix = grassmatrix[rowSums(grassmatrix[,-1])>5,]
# rarefy
sRare_grass <- rarefy(grassmatrix[,-1], min(rowSums(grassmatrix[,-1])))
rarecurve(grassmatrix[,-1])

# forb species
forbmatrix = veg_crosstab %>%
  dplyr::select(project_year, forbspecies$species_code) %>%
  group_by(project_year) %>%
  summarize_each(list(sum)) 
forbmatrix = forbmatrix[rowSums(forbmatrix[,-1])>5,]
# rarefy
sRare_forb <- rarefy(forbmatrix[,-1], min(rowSums(forbmatrix[,-1])))
rarecurve(forbmatrix[,-1])

# shrub species
shrubmatrix = veg_crosstab %>%
  dplyr::select(project_year, shrubspecies$species_code) %>%
  group_by(project_year) %>%
  summarize_each(list(sum))
shrubmatrix = shrubmatrix[rowSums(shrubmatrix[,-1])>5,]
# rarefy
sRare_shrub <- rarefy(shrubmatrix[,-1], min(rowSums(shrubmatrix[,-1])))
rarecurve(shrubmatrix[,-1])

# subshrub species
sshrubmatrix = veg_crosstab %>%
  dplyr::select(project_year, sshrubspecies$species_code) %>%
  group_by(project_year) %>%
  summarize_each(list(sum))
sshrubmatrix = sshrubmatrix[rowSums(sshrubmatrix[,-1])>5,]
# rarefy
sRare_sshrub <- rarefy(sshrubmatrix[,-1], min(rowSums(sshrubmatrix[,-1])))
rarecurve(sshrubmatrix[,-1])

# plot rarefied species richness
rarefied_richness = data.frame(project_year=richnessmatrix$project_year, nsp=sRare_all) %>%
  merge(data.frame(project_year=grassmatrix$project_year, nsp_grass=sRare_grass), all=T) %>%
  merge(data.frame(project_year=shrubmatrix$project_year, nsp_shrub=sRare_shrub), all=T) %>%
  merge(data.frame(project_year=sshrubmatrix$project_year, nsp_sshrub=sRare_sshrub), all=T) %>%
  merge(data.frame(project_year=forbmatrix$project_year, nsp_forb=sRare_forb), all=T) %>%
  tidyr::pivot_longer(cols=c(nsp, nsp_grass, nsp_shrub, nsp_forb, nsp_sshrub), names_to='veg_type')
rarerich = ggplot(rarefied_richness, aes(x=project_year, y=value, color=veg_type)) +
  geom_line() +
  geom_point() +
  scale_color_discrete(name='Plant type', labels=c('All Species','Forbs','Grasses','Shrubs','Subshrubs')) +
  xlab('') +
  ylab('# species') +
  ggtitle('Rarefied Species richness') +
  theme_bw()
rarerich
ggsave(rarerich, filename='Figures/richness/species_richness_rarefied.png', width=5, height=3)




# ===================================
# calculate avg bray-curtis and jaccard similarity between quads

#veg_noblank_nounkn = dplyr::filter(veg_selected_noblank, species %in% c(knownspecies$species_code, NA))
veg_noblank_nounkn = dplyr::filter(veg_selectedquads, species %in% (knownspecies$species_code))


mean_dissimilarity = c()
for (y in unique(veg_noblank_nounkn$project_year)) {
  yeardat = dplyr::filter(veg_noblank_nounkn, project_year==y, form !='SHRUB')
  veg_wide = tidyr::pivot_wider(yeardat, id_cols = c(quadrat), names_from = species, values_from=count)
  # fill with 0s and 1s
  veg_wide[is.na(veg_wide)] <- 0
  pres_abs = veg_wide[,-1]
  pres_abs[pres_abs>0] <- 1
  # calculate jaccard
  jaccard = vegdist(pres_abs, method='jaccard')
  bray = vegdist(veg_wide[,-1], method='bray')
  mean_dissimilarity = rbind(mean_dissimilarity, data.frame(project_year=y, jaccard=mean(jaccard), bray=mean(bray)))
  
}
bray_plot = ggplot(mean_dissimilarity, aes(x=project_year, y=bray)) +
  geom_line() +
  ylab('Bray-Curtis dissimilarity') +
  xlab('') +
  ylim(0,1) +
  theme_bw()
bray_plot
ggsave(bray_plot, filename='Figures/bray_curtis_pairwise_dissimilarity.png', width=3, height=3)


jaccard_plot = ggplot(mean_dissimilarity, aes(x=project_year, y=jaccard)) +
  geom_line() +
  ylab('Jaccard dissimilarity') +
  xlab('') +
  ylim(0,1) +
  theme_bw()
jaccard_plot
# ==========================================================
# count number of quads each species is found on in each year
species_quad_count = veg_nounkn %>%
  group_by(project_year, species) %>%
  summarize(nquads = length(unique(quadrat)))

plotyear = dplyr::filter(species_quad_count, project_year == 1995) %>%
  arrange(-nquads) %>%
  slice(1:8)

ggplot(plotyear, aes(x=species, y=nquads))  +
  geom_bar(stat='identity')
