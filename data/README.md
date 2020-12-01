## data folder
Data files and code to create them.

### Covariate data
* __get_covariates.R__ Pulls covariate data (soil, landform, topography, pastures) from the data repo and creates covariates.csv
* __covariates.csv__ Covariate values for all 122 quadrats. Includes soil texture, landform, topography, topography, average shrub (from remote sensed model), and pasture.

### Veg data
* __get_data.R__ File to create data files used for analysis. Depends on species_name_changes.csv (created manually). 
* __all_species_counts_cover.csv__ Total area and number of individuals by species, quadrat, and sampling date. Contains only quadrats and dates from quadrats_dates_for_analysis.csv. 
* __grass_species_totals.csv__ Total area of grass species by species, quadrat, and sampling date. Contains only quadrats and dates from quadrats_dates_for_analysis.csv. Cyperus sp. has been removed as it is not technically a grass. 
* __quadrat_veg.csv__ total grass, total shrub, and total # of forb individuals per quadrat and times. Does not include Yucca or Cyperus species. 
* __quadrats_dates_for_analysis.csv__ contains list of quadrats and dates used for the analyses. Only 91 quadrats are used, and for each quadrat I have selected one sampling per project_year (the latest sampling). 
* __shrub_species_totals.csv__ Total area of shrub species by species, quadrat, and sampling date. Contains only quadrats and dates from quadrats_dates_for_analysis.csv. Yucca species have been removed-- they are large and relatively short-lived, and therefore not relevant to general shrub-encroachment processes. 
* __species_name_changes.csv__ contains list of species name changes and aggregations. These follow name changes made by Adler's group in their data processing where applicable, and some species are grouped by genus where species ID could be suspect (e.g. ARPA9, ARPU9, ARPUL become ARIST). 
