building_dataset <- function(country){
  path_raw_data_iucn_countries <- here::here('data','raw-data','IUCN','countries.csv')
  iucn_raw_countries <- utils::read.csv(path_raw_data_iucn_countries)
  iucn_countries <- base::subset(iucn_raw_countries, select = c('scientificName','name','presence','origin'))
  iucn_countries$scientificName <- gsub(' ','_',iucn_countries$scientificName)
  iucn_countries <- iucn_countries[iucn_countries$name %in% country,]
  iucn_countries <- base::subset(iucn_countries,select=-name)
  iucn_countries <-  dplyr::distinct(iucn_countries,scientificName,.keep_all = T)
  ##selecting the mammals only
  path_raw_data_iucn_summaries <- here::here('data','raw-data','IUCN','simple_summary.csv')
  iucn_raw_summaries <- utils::read.csv(path_raw_data_iucn_summaries)
  iucn_summaries <- base::subset(iucn_raw_summaries, select = c('scientificName','className'))
  iucn_summaries$scientificName <- base::gsub(' ','_',iucn_summaries$scientificName)
  iucn_summaries <- iucn_summaries[iucn_summaries$className =='MAMMALIA',]
  iucn_summaries <-  dplyr::distinct(iucn_summaries,scientificName,.keep_all = T)
  
  iucn <- base::merge(iucn_summaries,iucn_countries,by='scientificName',all.x = T)
  iucn <- base::subset(iucn,select=-className)
  iucn <- stats::na.omit(iucn)
  
  #loading traits info for mammal species
  path_raw_data_trait <- here::here('data','raw-data','Traits','all_mammals')
  mammal_traits_all<- base::readRDS(path_raw_data_trait)
  mammal_traits_all$binomial <- base::gsub(' ','_',mammal_traits_all$binomial)
  mammal_traits_all <- mammal_traits_all |> dplyr::select(binomial,Habitat,Main.diet,insular_endemic)
  path_raw_data_trait_bis <- here::here('data','raw-data','Traits','trait_data_imputed.csv')
  mammal_traits_all_bis <- utils::read.csv(path_raw_data_trait_bis)
  mammal_traits_all_bis$iucn2020_binomial <- base::gsub(' ','_',mammal_traits_all_bis$iucn2020_binomial)
  mammal_traits_all_bis <- mammal_traits_all_bis |> dplyr::select(iucn2020_binomial,adult_mass_g,brain_mass_g,activity_cycle,foraging_stratum,trophic_level,litter_size_n)
  mammal_traits_all <- merge(mammal_traits_all,mammal_traits_all_bis,by.x="binomial",by.y = "iucn2020_binomial",all.x = T)
  
  #Merging IUCN info + traits of mammal species
  mammal_data <- merge(iucn,mammal_traits_all,by.y='binomial',by.x='scientificName',all.x=T) 

  #Removing species which do not match my criteria
  mammal_data <- mammal_data[mammal_data$presence =='Extant',]
  mammal_data <- mammal_data[mammal_data$origin =='Native',]
  mammal_data <- mammal_data[mammal_data$insular_endemic ==0,]
  mammal_data <- mammal_data |> dplyr::select(-origin,-presence,-insular_endemic)
  mammal_data <- janitor::clean_names(mammal_data)
  return(mammal_data)
}


