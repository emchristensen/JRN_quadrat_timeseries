## data folder
Data files and code to create them.

### Covariate data
* __get_covariates.R__ Pulls covariate data (soil, landform, topography, pastures) from the data repo and creates covariates.csv
* __covariates.csv__ Covariate values for all 122 quadrats. Includes soil texture, landform, topography, topography, average shrub (from remote sensed model), and pasture.

### Veg data
* __get_data.R__ File to create data files used for analysis. Depends on species_name_changes.csv (created manually). 
* __all_species_counts_cover.csv__ Total area and number of individuals by species, quadrat, and sampling date. Contains only quadrats and dates from quadrats_dates_for_analysis.csv. 
* __all_veg_count_crosstab.csv__ Wide-format table of number of individuals of species per quadrat and sampling time. 
* __grass_shrub_timeseries_imputed.csv__ yearly time series of grass, shrub, total cover, and bare ground with data gaps filled by linear interpolation. Quadrats that were missing more than 4 years in a row were not included (leaving 31 quadrats). Created by trends/overall_grass_shrub_trends_interpolate.R
* __quadrat_veg.csv__ total grass, total shrub, total veg cover, total bare ground, and total # of forb individuals per quadrat and times. Does not include Yucca or Cyperus species. Areas calculated using maptools::unionSpatialPolygons function on the shapefile polygons in order to remove effect of any overlap of plants. 
* __quadrats_dates_for_analysis.csv__ contains list of quadrats and dates used for the analyses. 92 quadrats are used, and for each quadrat I have selected one sampling per project_year (the sampling that falls latest in the year). 
* __quadyearcrosstab.csv__ Crosstab version of quadrat_dates_for_analysis. Useful as a quick reference to see what quadrats were sampled in what years. 
* __species_name_changes.csv__ contains list of species name changes and aggregations. Some species are grouped by genus where species ID could be mistaken, and species perform similar function to each other (e.g. ARPA9, ARPU9, ARPUL become ARIST). 
