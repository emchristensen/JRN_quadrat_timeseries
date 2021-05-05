# find dominant species on quads through time
# EMC 11/23/20

library(dplyr)
library(ggplot2)

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
veg_nounkn = mutate(veg_nounkn, date=as.Date(paste(veg_nounkn$year, veg_nounkn$month, '15',sep='-')))


# find dominant species in each sampling
alldominants = c()
for (quad in unique(veg_nounkn$quadrat)) {
  quaddat = dplyr::filter(veg_nounkn, quadrat==quad)
  for (date1 in unique(quaddat$date)) {
    quaddatedat = dplyr::filter(quaddat, date==date1) %>%
      arrange(-cover)
    # get first 2 dominant species
    dominant1 = slice(quaddatedat,1)
    if (nrow(quaddatedat)>1) {
    dominant2 = slice(quaddatedat,2)
    } else {dominant2 = data.frame(species=NA, cover=NA)}
    # get total of all species
    totalcover = sum(quaddatedat$cover)
    # create dataframe
    alldominants = rbind(alldominants, data.frame(quadrat=quad, year=dominant1$year, month=dominant1$month, totalcover=totalcover,
                                                  dominant1_sp=dominant1$species, dominant1_cover=dominant1$cover, 
                                                  dominant1_pct=dominant1$cover/totalcover,
                                                  dominant2_sp=dominant2$species, dominant2_cover=dominant2$cover,
                                                  dominant2_pct=dominant2$cover/totalcover))
  }

  
}

# =========================================
# begin dividing samples into categories

# first divide: is total cover > 1%
sparse = dplyr::filter(alldominants, totalcover<0.01)
notsparse = dplyr::filter(alldominants, totalcover>=0.01)

# second divide: what is the dominant species
arist = dplyr::filter(notsparse, dominant1_sp=='ARIST')
boer = dplyr::filter(notsparse, dominant1_sp=='BOER4')
prgl = dplyr::filter(notsparse, dominant1_sp=='PRGL2')
sporo = dplyr::filter(notsparse, dominant1_sp=='SPORO')
dapu = dplyr::filter(notsparse, dominant1_sp=='DAPU7')
muar = dplyr::filter(notsparse, dominant1_sp=='MUAR')
muar2 = dplyr::filter(notsparse, dominant1_sp=='MUAR2')
plmu = dplyr::filter(notsparse, dominant1_sp=='PLMU3')
scbr = dplyr::filter(notsparse, dominant1_sp=='SCBR2')
flce = dplyr::filter(notsparse, dominant1_sp=='FLCE')
paob = dplyr::filter(notsparse, dominant1_sp=='PAOB')
latr = dplyr::filter(notsparse, dominant1_sp=='LATR2')

# add column for category
categories = alldominants
categories$dominant1_sp = as.character(categories$dominant1_sp)
categories$dominant2_sp = as.character(categories$dominant2_sp)
categories$community = rep(NA)

for (r in 1:nrow(categories)) {
  if (categories$totalcover[r]<.01) {
    categories$community[r] <- 'sparse'
  } else if (categories$dominant1_pct[r]>=.85) {
    categories$community[r] <- categories$dominant1_sp[r]
  } else {
    categories$community[r] <- paste(categories$dominant1_sp[r], categories$dominant2_sp[r], sep='_')
  }
}
