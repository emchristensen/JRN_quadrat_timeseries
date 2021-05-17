#' interpolated time series of grass species
#' EMC 5/14/21

library(dplyr)
library(ggplot2)

# colors for figures
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

veg = read.csv('data/all_species_counts_cover.csv')
grass_timeseries = read.csv('data/grass_shrub_timeseries_imputed.csv')
quadrat_veg = read.csv('data/quadrat_veg.csv')

# create grid of years and species
selectedyears = data.frame(project_year = 1916:1979)

# get total of grass species by quadrat and year
grass_species_totals = veg %>%
  dplyr::filter(quadrat %in% grass_timeseries$quadrat, form=='GRASS') %>%
  group_by(quadrat, project_year, species) %>%
  summarize(total_cover=sum(cover))

# what are the most prevalent grasses 
grass_species_totals %>%
  mutate(quadyear = paste(quadrat, project_year)) %>%
  group_by(species) %>%
  summarize(total_cover=sum(total_cover),
            nsamples = n_distinct(quadyear)) %>%
  arrange(-total_cover)

# make an "other" category for the rare or unknown species
grass_species_totals_other = grass_species_totals
grass_species_totals_other$species[grass_species_totals_other$species %in% 
                                     c('UNKG','ERLE','ARTE3','ENDE','MUPO2','CYPER','PAOB','PAHA','BOUTE','SELE6')] <- 'OTHER'
# combine MUAR and MUAR2
grass_species_totals_other$species[grass_species_totals_other$species %in%
                                     c('MUAR','MUAR2')] <- 'MUHLE'

grass_species_totals_other= grass_species_totals_other %>%
  group_by(quadrat, project_year, species) %>%
  summarize(total_cover=sum(total_cover))

# list of species
grass_species_list = unique(grass_species_totals_other$species)

# for each quadrat, create time series of each species and impute
#quad = 'A1'
allquadsallspecies = c()
for (quad in unique(grass_species_totals_other$quadrat)) {
  quaddata = grass_species_totals_other %>% dplyr::filter(quadrat==quad)
  
  #sp = 'BOER4'
  quadratspeciesdata = c()
  for (sp in grass_species_list) {
    
    quadsp = dplyr::filter(quaddata, species==sp)
    
    # merge with quadrat_veg and fill in true zeros
    quadseries = dplyr::filter(quadrat_veg, quadrat==quad) %>%
      dplyr::select(quadrat, project_year) %>%
      merge(quadsp, all=T) %>%
      dplyr::select(-species, -quadrat)
    quadseries$total_cover[is.na(quadseries$total_cover)] <- 0
    
    # merge with selectedyears and impute NAs
    series_imputed = merge(quadseries, selectedyears, all.y=T) %>%
      imputeTS::na_interpolation(option='linear') %>%
      mutate(quadrat=quad, species=sp)
    
    # append to quadrat species data
    quadratspeciesdata = rbind(quadratspeciesdata, series_imputed)
  }
  # append to all quadrats all species data frame
  allquadsallspecies = rbind(allquadsallspecies, quadratspeciesdata)
}

# plot one quadrat's data
ggplot(quadratspeciesdata, aes(x=project_year, y=total_cover, color=species, fill=species)) +
  geom_area()


# get modern values and add to imputed values
modern_grass_sp = expand.grid(quadrat = unique(allquadsallspecies$quadrat),
                              project_year =c(1995,2001,2006,2011,2016),
                              species = unique(allquadsallspecies$species)) %>%
  merge(grass_species_totals_other, all.x=T) %>%
  dplyr::select(project_year, total_cover, quadrat, species)
modern_grass_sp$total_cover[is.na(modern_grass_sp$total_cover)] <- 0

all_grass_species = rbind(allquadsallspecies, modern_grass_sp)

# save imputed timeseries to file
write.csv(all_grass_species, 'data/grass_species_timeseries_imputed.csv', row.names=F)

# get yearly mean for plotting
yearly_mean_grass_species = all_grass_species %>%
  group_by(project_year, species) %>%
  summarize(mean_cover=mean(total_cover))

# Plot: mean grass species cover per quadrat per year
ts1 = dplyr::filter(yearly_mean_grass_species, project_year<1995)
ts2 = dplyr::filter(yearly_mean_grass_species, project_year>=1995)
grassspecies <- ggplot(ts1, aes(x=project_year, fill=species)) +
  geom_area(aes(y=mean_cover)) +
  xlim(1915,2020) +
  geom_area(data=ts2, aes(x=project_year, y=mean_cover, fill=species)) +
  labs(x = '',
       y='Avg. cover per quadrat (m^2)',
       fill='Species',
       title='Perennial grass species through time') +
    ylim(0,.2) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=cbPalette) +
  theme_bw()
grassspecies
ggsave('Figures/communitychange/grass_species_through_time_31quads.png', plot=grassspecies, width=6, height=4)
