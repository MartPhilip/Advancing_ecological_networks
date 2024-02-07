require(caret)
require(xgboost)

###Step_0########Data Prep################

#I am focusing on European country only
countries <- c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", 
               "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", 
               "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", 
               "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", 
               "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", 
               "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", 
               "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", 
               "Ukraine", "United Kingdom", "Vatican City")


#Building one dataframe per predators focusing on Europe with prey at species level
path_diet_data <- here::here('data','raw-data','diet','diet.csv')
diet_data <- utils::read.csv(path_diet_data)
diet_data <-  diet_data |> 
  dplyr::filter(!is.na(scientificNamePrey)) |> 
  dplyr::filter(is.na(island))

diet_data <- diet_data |> dplyr::filter(country %in% countries)

# vector of predator species
predator_species_list <- unique(diet_data$scientificNameCarni)

for (species in predator_species_list) {
  # Create a dataframe for the current species
  assign(paste0(tolower(species), "_data"), diet_data[diet_data$scientificNameCarni == species, ],envir = parent.frame())
}

#Building database of all mammals in Europe with their traits
mammal_data <- building_dataset(countries)

#path_mammal_ranges <- here::here('data','raw-data','mammal_range','MAMMALS.shp')
#mammal_ranges <- sf::st_read(path_mammal_ranges)
#mammal_ranges$binomial <- base::gsub(' ','_',mammal_ranges$binomial)
#species_data <- mammal_data$scientific_name
#mammal_ranges <- mammal_ranges[mammal_ranges$binomial %in% species_data, ]
#mammal_ranges <- sf::st_make_valid(mammal_ranges)
#print(sf::st_is_valid(mammal_ranges))

#precision = 1e7

# if geometry still invalid, decrease precision
#while (sum(sf::st_is_valid(mammal_ranges))<nrow(mammal_ranges)){
  #precision = precision/2
  #mammal_ranges <- sf::st_make_valid(sf::st_set_precision(mammal_ranges, precision))
#}

##############################################################################

mammal_ranges <- readRDS(here::here('data','derived-data','mammal_ranges.RDS'))

###Optimization of the reg and class RF for 7 pred

path_eig <- here::here('data','raw-data','Phylo','mammal_eig_400.RDS')
eig <- readRDS(path_eig)
eig$scientific_name <- rownames(eig)

##########################################

###Step_1########Mapping records################


vulpes_vulpes_data_map <- vulpes_vulpes_data |> dplyr::select(scientificNameCarni,country)
lynx_lynx_data_map <- lynx_lynx_data |> dplyr::select(scientificNameCarni,country)
felis_silvestris_data_map <- felis_silvestris_data |> dplyr::select(scientificNameCarni,country)
martes_martes_data_map <- martes_martes_data |> dplyr::select(scientificNameCarni,country)
martes_foina_data_map <- martes_foina_data |> dplyr::select(scientificNameCarni,country)
canis_aureus_data_map <- canis_aureus_data |> dplyr::select(scientificNameCarni,country)
canis_lupus_data_map <- canis_lupus_data |> dplyr::select(scientificNameCarni,country)
genetta_genetta_data_map <- genetta_genetta_data |> dplyr::select(scientificNameCarni,country)
neovison_vison_data_map <- neovison_vison_data |> dplyr::select(scientificNameCarni,country)

df <- rbind(vulpes_vulpes_data_map,
            lynx_lynx_data_map,
            felis_silvestris_data_map,
            martes_martes_data_map,
            martes_foina_data_map,
            canis_aureus_data_map,
            canis_lupus_data_map,
            genetta_genetta_data_map,
            neovison_vison_data_map)




# Ensure that scientificNameCarni is a factor for proper color mapping
df$scientificNameCarni <- as.factor(df$scientificNameCarni)
df$scientificNameCarni <- gsub("_"," ",df$scientificNameCarni)
# Creating the plot
predator_names <- c("Canis aureus", "Canis lupus", "Felis silvestris",
                    "Genetta genetta", "Lynx lynx", "Martes foina",
                    "Martes martes", "Neovison vison", "Vulpes vulpes")
df$scientificNameCarni <- factor(df$scientificNameCarni, levels = predator_names)

species_count <- df |> 
  dplyr::group_by(country, scientificNameCarni) |> 
  dplyr::summarise(Records = dplyr::n(), .groups = 'drop')


library(ggplot2)
library(ggplotify)
library(rnaturalearth)
library(sf)
# Get European countries' geometries
europe_countries <- ne_countries(scale = "medium", returnclass = "sf") |>
  dplyr::filter(continent == "Europe") |>
  dplyr::mutate(country = name) # Ensure country names match your data for merging

# Merge your observations data with the map data
europe_map_data <- europe_countries|>
  dplyr::left_join(species_count, by = "country") |>
  dplyr::filter(scientificNameCarni %in% predator_names)

# Plot faceted maps, one for each species, with observations count
Map <- ggplot(data = europe_map_data) +
  geom_sf(aes(fill = Records), color = NA) + # Color countries based on observation count
  scale_fill_viridis_c(direction = -1 ,option="rocket", na.value = "white", guide = "colorbar", name = "Predation record number") +
  coord_sf(xlim = c(-25, 40), ylim = c(35, 75), expand = FALSE) + # Adjust the map view to focus on Europe
  facet_wrap(~scientificNameCarni) + # Create a map for each species
  labs(x = "", y = "") +
  theme_void() + # Clean theme without axes and background
  theme(
    strip.background = element_blank(), 
    strip.text.x = element_text(size = 15,face = "italic")) # Adjust facet labels and legend
Map
##########################################

###Step_2########Correlation analysis################

###Preparing all the dataset need for the analysis
XGBOOST_vulpes_vulpes <- preparing_dataset_xgboost(pred_data = vulpes_vulpes_data, prey_data = mammal_data, range = mammal_ranges,eig = eig)
XGBOOST_lynx_lynx <- preparing_dataset_xgboost(pred_data = lynx_lynx_data, prey_data = mammal_data, range = mammal_ranges,eig = eig)
XGBOOST_felis_silvestris <- preparing_dataset_xgboost(pred_data = felis_silvestris_data, prey_data = mammal_data, range = mammal_ranges,eig = eig)
XGBOOST_martes_martes <- preparing_dataset_xgboost(pred_data = martes_martes_data, prey_data = mammal_data, range = mammal_ranges,eig = eig)
XGBOOST_martes_foina <- preparing_dataset_xgboost(pred_data = martes_foina_data, prey_data = mammal_data, range = mammal_ranges,eig = eig)
XGBOOST_ursus_arctos <- preparing_dataset_xgboost(pred_data = ursus_arctos_data, prey_data = mammal_data, range = mammal_ranges,eig = eig)
XGBOOST_canis_aureus <- preparing_dataset_xgboost(pred_data = canis_aureus_data, prey_data = mammal_data, range = mammal_ranges,eig = eig)
XGBOOST_canis_lupus <- preparing_dataset_xgboost(pred_data = canis_lupus_data, prey_data = mammal_data, range = mammal_ranges,eig = eig)
XGBOOST_genetta_genetta <- preparing_dataset_xgboost(pred_data = genetta_genetta_data, prey_data = mammal_data, range = mammal_ranges,eig = eig)
XGBOOST_neovison_vison <- preparing_dataset_xgboost(pred_data = neovison_vison_data, prey_data = mammal_data, range = mammal_ranges,eig = eig)


###Computing framework
#XGBOOST_vulpes_vulpes_result <- full_framework_xgboost(bootstrap= 100, XGBOOST_data = XGBOOST_vulpes_vulpes, bootstrap_xg = 250, species_name = 'Vulpes_vulpes')
#XGBOOST_lynx_lynx_result <- full_framework_xgboost(bootstrap= 100, XGBOOST_data = XGBOOST_lynx_lynx, bootstrap_xg = 250, species_name = 'Lynx_lynx')
#XGBOOST_felis_silvestris_result <- full_framework_xgboost(bootstrap= 100, XGBOOST_data = XGBOOST_felis_silvestris, bootstrap_xg = 250, species_name = 'Felis_silvestris')
#XGBOOST_martes_martes_result <- full_framework_xgboost(bootstrap= 100, XGBOOST_data = XGBOOST_martes_martes, bootstrap_xg = 250, species_name = 'Martes_martes')
#XGBOOST_martes_foina_result <- full_framework_xgboost(bootstrap= 100, XGBOOST_data = XGBOOST_martes_foina, bootstrap_xg = 250, species_name = 'Martes_foina')
#XGBOOST_ursus_arctos_result <- full_framework_xgboost(bootstrap= 100, XGBOOST_data = XGBOOST_ursus_arctos, bootstrap_xg = 250, species_name = 'Ursus_arctos')
#XGBOOST_canis_aureus_result <- full_framework_xgboost(bootstrap= 100, XGBOOST_data = XGBOOST_canis_aureus, bootstrap_xg = 250, species_name = 'Canis_aureus')
#XGBOOST_canis_lupus_result <- full_framework_xgboost(bootstrap= 100, XGBOOST_data = XGBOOST_canis_lupus, bootstrap_xg = 250, species_name = 'Canis_lupus')
#XGBOOST_genetta_genetta_result <- full_framework_xgboost(bootstrap= 100, XGBOOST_data = XGBOOST_genetta_genetta, bootstrap_xg = 250, species_name = 'Genetta_genetta')
#XGBOOST_neovison_vison_result <- full_framework_xgboost(bootstrap= 100, XGBOOST_data = XGBOOST_neovison_vison, bootstrap_xg = 250, species_name = 'Neovison_vison')

###Saving objects (1 week of computation)
XGBOOST_vulpes_vulpes_result <- readRDS(here::here('data','derived-data',"XGBOOST_vulpes_vulpes_result.RDS"))
XGBOOST_lynx_lynx_result <- readRDS(here::here('data','derived-data',"XGBOOST_lynx_lynx_result.RDS"))
XGBOOST_felis_silvestris_result <- readRDS(here::here('data','derived-data',"XGBOOST_felis_silvestris_result.RDS"))
XGBOOST_martes_martes_result <- readRDS(here::here('data','derived-data',"XGBOOST_martes_martes_result.RDS"))
XGBOOST_martes_foina_result <- readRDS(here::here('data','derived-data',"XGBOOST_martes_foina_result.RDS"))
XGBOOST_ursus_arctos_result <- readRDS(here::here('data','derived-data',"XGBOOST_ursus_arctos_result.RDS"))
XGBOOST_canis_aureus_result <- readRDS(here::here('data','derived-data',"XGBOOST_canis_aureus_result.RDS"))
XGBOOST_canis_lupus_result <- readRDS(here::here('data','derived-data',"XGBOOST_canis_lupus_result.RDS"))
XGBOOST_genetta_genetta_result <- readRDS(here::here('data','derived-data',"XGBOOST_genetta_genetta_result.RDS"))
XGBOOST_neovison_vison_result <- readRDS(here::here('data','derived-data',"XGBOOST_neovison_vison_result.RDS"))


XGBOOST_vulpes_vulpes_result <- do.call(rbind,XGBOOST_vulpes_vulpes_result)
XGBOOST_lynx_lynx_result <- do.call(rbind,XGBOOST_lynx_lynx_result)
XGBOOST_felis_silvestris_result <- do.call(rbind,XGBOOST_felis_silvestris_result)
XGBOOST_martes_martes_result <- do.call(rbind,XGBOOST_martes_martes_result)
XGBOOST_martes_foina_result <- do.call(rbind,XGBOOST_martes_foina_result)
XGBOOST_ursus_arctos_result <- do.call(rbind,XGBOOST_ursus_arctos_result)
XGBOOST_canis_aureus_result <- do.call(rbind,XGBOOST_canis_aureus_result)
XGBOOST_canis_lupus_result <- do.call(rbind,XGBOOST_canis_lupus_result)
XGBOOST_genetta_genetta_result <- do.call(rbind,XGBOOST_genetta_genetta_result)
XGBOOST_neovison_vison_result <- do.call(rbind,XGBOOST_neovison_vison_result)



test_XGBOOST_results <- rbind(XGBOOST_vulpes_vulpes_result[1,],
                             XGBOOST_lynx_lynx_result[1,],
                             XGBOOST_felis_silvestris_result[1,],
                             XGBOOST_martes_martes_result[1,],
                             XGBOOST_martes_foina_result[1,],
                             XGBOOST_ursus_arctos_result[1,],
                             XGBOOST_canis_aureus_result[1,],
                             XGBOOST_canis_lupus_result[1,],
                             XGBOOST_genetta_genetta_result[1,],
                             XGBOOST_neovison_vison_result[1,])
null_XGBOOST_results <- rbind(XGBOOST_vulpes_vulpes_result[2:100,],
                             XGBOOST_lynx_lynx_result[2:100,],
                             XGBOOST_felis_silvestris_result[2:100,],
                             XGBOOST_martes_martes_result[2:100,],
                             XGBOOST_martes_foina_result[2:100,],
                             XGBOOST_ursus_arctos_result[2:100,],
                             XGBOOST_canis_aureus_result[2:100,],
                             XGBOOST_canis_lupus_result[2:100,],
                             XGBOOST_genetta_genetta_result[2:100,],
                             XGBOOST_neovison_vison_result[2:100,])



info_species <- data.frame(scientific_name=c('Vulpes_vulpes',
                                             'Lynx_lynx',
                                             'Felis_silvestris',
                                             'Martes_martes',
                                             'Martes_foina',
                                             'Ursus_arctos',
                                             'Canis_aureus',
                                             'Canis_lupus',
                                             'Genetta_genetta',
                                             'Neovison_vison'),
                           observation_number=c(800,
                                                315,
                                                172,
                                                207,
                                                271,
                                                193,
                                                168,
                                                806,
                                                120,
                                                103))

combine_traits <- utils::read.csv(here::here('data','raw-data','Traits','trait_data_imputed.csv'))
bd_combine_traits <- combine_traits |> dplyr::select(iucn2020_binomial,adult_mass_g)
bd_combine_traits$iucn2020_binomial <- gsub(" ","_",bd_combine_traits$iucn2020_binomial)
diet_div <- functional_div(XGBOOST_vulpes_vulpes,
                           XGBOOST_lynx_lynx,
                           XGBOOST_felis_silvestris,
                           XGBOOST_martes_martes,
                           XGBOOST_martes_foina,
                           XGBOOST_ursus_arctos,
                           XGBOOST_canis_aureus,
                           XGBOOST_canis_lupus,
                           XGBOOST_genetta_genetta,
                           XGBOOST_neovison_vison)
diet_div <- t(diet_div)
diet_div <- as.data.frame(diet_div[,'FD_q'])
colnames(diet_div) <- 'Functional_richness'
diet_div$scientific_name <- rownames(diet_div)

info_species_bis <- merge(diet_div,bd_combine_traits,by.x = 'scientific_name',by.y = 'iucn2020_binomial',all.x = T)
info_species <- merge(info_species,info_species_bis,by='scientific_name')


test_XGBOOST_results <- merge(test_XGBOOST_results,info_species,by='scientific_name')
test_XGBOOST_results <- test_XGBOOST_results |> dplyr::mutate(type='test') 
null_XGBOOST_results <- merge(null_XGBOOST_results,info_species,by='scientific_name')
null_XGBOOST_results <- null_XGBOOST_results |> dplyr::mutate(type='null') 
XGBOOST_results <- rbind(test_XGBOOST_results,null_XGBOOST_results)
XGBOOST_results <- XGBOOST_results |> dplyr::filter(scientific_name!='Ursus_arctos')

data <- XGBOOST_results

path_data_data <- here::here('data','derived-data','data_data.csv')
data_data <- read.csv(path_data_data)

data$scientific_name <- gsub("_"," ",data$scientific_name)
# Custom color palette for the two types
color_palette <- c("test" = "#1f77b4", "null" = "#ff7f0e")  # Colors for 'test' and 'null'

# Vector of predator names in alphabetical order
predator_names <- c("Canis aureus", "Canis lupus", "Felis silvestris",
                    "Genetta genetta", "Lynx lynx", "Martes foina",
                    "Martes martes", "Neovison vison", "Vulpes vulpes")
data$scientific_name <- factor(data$scientific_name, levels = predator_names)
# Create the plot with enhanced aesthetics
Corr_plot <- ggplot() +
  # Points and error bars for 'test' (non-transparent)
  geom_point(data = subset(data, type == "test"), aes(x = mean_mcc, y = Correlation_exact, color = type), size = 3) +
  geom_errorbarh(data = subset(data, type == "test"), aes(xmin = mean_mcc - sd_mcc, xmax = mean_mcc + sd_mcc, y = Correlation_exact, color = type), height = 0.1) +
  # Points and error bars for 'null' (transparent)
  geom_point(data = subset(data, type == "null"), aes(x = mean_mcc, y = Correlation_exact, color = type), size = 3, alpha = 0.4) +
  geom_errorbarh(data = subset(data, type == "null"), aes(xmin = mean_mcc - sd_mcc, xmax = mean_mcc + sd_mcc, y = Correlation_exact, color = type), height = 0.1, alpha = 0.4) +
  # Facet by species
  facet_wrap(~ scientific_name) +
  # Use custom color palette
  scale_color_manual(values = color_palette) +
  # Theme and labels
  theme_minimal(base_size = 14) +
  labs(x = "Mean value of Matthews correlation coefficient for the boostraps of binary classifiers", y = "Exact correlation between predation probablities and preference", color = "Type") +
  theme(
    legend.position = "bottom", 
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_line(color = "grey90", linetype = "dotted"),
    strip.text = element_text(face = "italic") # Set facet names to italic
  )

Corr_plot


data_result <- data |> dplyr::filter(type=='test')
mean(data_result$mean_mcc)
median(data_result$mean_mcc)
min(data_result$mean_mcc)
max(data_result$mean_mcc)
mean(data_result$sd_mcc)

null_mcc_vulpes_vulpes <- distrib_corr(XGBOOST_vulpes_vulpes_result,"mean_mcc")
null_mcc_lynx_lynx <- distrib_corr(XGBOOST_lynx_lynx_result,"mean_mcc")
null_mcc_felis_silvestris <- distrib_corr(XGBOOST_felis_silvestris_result,"mean_mcc")
null_mcc_martes_martes <- distrib_corr(XGBOOST_martes_martes_result,"mean_mcc")
null_mcc_martes_foina <- distrib_corr(XGBOOST_martes_foina_result,"mean_mcc")
null_mcc_canis_aureus <- distrib_corr(XGBOOST_canis_aureus_result,"mean_mcc")
null_mcc_canis_lupus <- distrib_corr(XGBOOST_canis_lupus_result,"mean_mcc")
null_mcc_genetta_genetta <- distrib_corr(XGBOOST_genetta_genetta_result,"mean_mcc")
null_mcc_neovison_vison <- distrib_corr(XGBOOST_neovison_vison_result,"mean_mcc")




mean(data_result$Correlation_exact)
median(data_result$Correlation_exact)
min(data_result$Correlation_exact)
max(data_result$Correlation_exact)
mean(data_result$Correlation_rank)
median(data_result$Correlation_rank)
min(data_result$Correlation_rank)
max(data_result$Correlation_rank)

null_correxact_vulpes_vulpes <- distrib_corr(XGBOOST_vulpes_vulpes_result,"Correlation_exact")
null_correxact_lynx_lynx <- distrib_corr(XGBOOST_lynx_lynx_result,"Correlation_exact")
null_correxact_felis_silvestris <- distrib_corr(XGBOOST_felis_silvestris_result,"Correlation_exact")
null_correxact_martes_martes <- distrib_corr(XGBOOST_martes_martes_result,"Correlation_exact")
null_correxact_martes_foina <- distrib_corr(XGBOOST_martes_foina_result,"Correlation_exact")
null_correxact_canis_aureus <- distrib_corr(XGBOOST_canis_aureus_result,"Correlation_exact")
null_correxact_canis_lupus <- distrib_corr(XGBOOST_canis_lupus_result,"Correlation_exact")
null_correxact_genetta_genetta <- distrib_corr(XGBOOST_genetta_genetta_result,"Correlation_exact")
null_correxact_neovison_vison <- distrib_corr(XGBOOST_neovison_vison_result,"Correlation_exact")

null_corrrank_vulpes_vulpes <- distrib_corr(XGBOOST_vulpes_vulpes_result,"Correlation_rank")
null_corrrank_lynx_lynx <- distrib_corr(XGBOOST_lynx_lynx_result,"Correlation_rank")
null_corrrank_felis_silvestris <- distrib_corr(XGBOOST_felis_silvestris_result,"Correlation_rank")
null_corrrank_martes_martes <- distrib_corr(XGBOOST_martes_martes_result,"Correlation_rank")
null_corrrank_martes_foina <- distrib_corr(XGBOOST_martes_foina_result,"Correlation_rank")
null_corrrank_canis_aureus <- distrib_corr(XGBOOST_canis_aureus_result,"Correlation_rank")
null_corrrank_canis_lupus <- distrib_corr(XGBOOST_canis_lupus_result,"Correlation_rank")
null_corrrank_genetta_genetta <- distrib_corr(XGBOOST_genetta_genetta_result,"Correlation_rank")
null_corrrank_neovison_vison <- distrib_corr(XGBOOST_neovison_vison_result,"Correlation_rank")

data_result <- data_result |>
  dplyr::select(-Correlation_rank,-type) |>
  dplyr::rename('Scientific name'=scientific_name) |>
  dplyr::rename('Exact correlation'= Correlation_exact ) |>
  dplyr::rename('Average value of MCC'=mean_mcc) |>
  dplyr::rename('Standard deviation of MCC'=sd_mcc) |>
  dplyr::rename('Number of predation record with preference'=observation_number) |>
  dplyr::rename('Functional richness of the diet'=Functional_richness) |>
  dplyr::rename('Body mass of adult (g)'=adult_mass_g) 
  
data_result$`Average value of MCC` <- round(data_result$`Average value of MCC`,2)
data_result$`Exact correlation` <- round(data_result$`Exact correlation`,2)
data_result$`Standard deviation of MCC` <- round(data_result$`Standard deviation of MCC`,2)
data_result$`Functional richness of the diet` <- round(data_result$`Functional richness of the diet` ,2)
data_result$`Body mass of adult (g)` <- round(data_result$`Body mass of adult (g)` ,2)

# Load necessary libraries
library(kableExtra)
library(knitr)
Table1 <- kable(data_result, "html", escape = FALSE) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, italic = TRUE)
Table1
##########################################

###Step_3########SHAP computation################

XGBOOST_vulpes_vulpes_shap <- XGBOOST_binary_shap(bootstrap= 250, data = XGBOOST_vulpes_vulpes, core_nbr = 3 )
XGBOOST_lynx_lynx_result_shap  <- XGBOOST_binary_shap(bootstrap= 250, data = XGBOOST_lynx_lynx,core_nbr = 3 )
XGBOOST_felis_silvestris_result_shap  <- XGBOOST_binary_shap(bootstrap= 250, data = XGBOOST_felis_silvestris,core_nbr = 3  )
XGBOOST_martes_martes_result_shap  <- XGBOOST_binary_shap(bootstrap= 250, data = XGBOOST_martes_martes, core_nbr = 3 )
XGBOOST_martes_foina_result_shap  <- XGBOOST_binary_shap(bootstrap= 250, data = XGBOOST_martes_foina, core_nbr = 3 )
XGBOOST_canis_aureus_result_shap  <- XGBOOST_binary_shap(bootstrap= 250, data = XGBOOST_canis_aureus, core_nbr = 3 )
XGBOOST_canis_lupus_result_shap  <- XGBOOST_binary_shap(bootstrap= 250, data = XGBOOST_canis_lupus,core_nbr = 3 )
XGBOOST_genetta_genetta_result_shap  <- XGBOOST_binary_shap(bootstrap= 250, data = XGBOOST_genetta_genetta, core_nbr = 3 )
XGBOOST_neovison_vison_result_shap  <- XGBOOST_binary_shap(bootstrap= 250, data = XGBOOST_neovison_vison,core_nbr = 3 )

XGBOOST_vulpes_vulpes_shap_sum <- XGBOOST_vulpes_vulpes_shap  |> dplyr::group_by(feature) |> dplyr::summarise('mean(|SHAP value|)'=mean(value),'mean(|SHAP value|) standard error'=sd(value)/sqrt(100)) |> dplyr::mutate(Predator = 'Vulpes vulpes')
XGBOOST_lynx_lynx_result_shap_sum  <- XGBOOST_lynx_lynx_result_shap |> dplyr::group_by(feature)|> dplyr::summarise('mean(|SHAP value|)'=mean(value),'mean(|SHAP value|) standard error'=sd(value)/sqrt(100)) |> dplyr::mutate(Predator = 'Lynx lynx') 
XGBOOST_felis_silvestris_result_shap_sum  <- XGBOOST_felis_silvestris_result_shap|> dplyr::group_by(feature)  |> dplyr::summarise('mean(|SHAP value|)'=mean(value),'mean(|SHAP value|) standard error'=sd(value)/sqrt(100)) |> dplyr::mutate(Predator = 'Felis silvestris')
XGBOOST_martes_martes_result_shap_sum  <- XGBOOST_martes_martes_result_shap|> dplyr::group_by(feature) |> dplyr::summarise('mean(|SHAP value|)'=mean(value),'mean(|SHAP value|) standard error'=sd(value)/sqrt(100)) |> dplyr::mutate(Predator = 'Martes martes') 
XGBOOST_martes_foina_result_shap_sum  <- XGBOOST_martes_foina_result_shap |> dplyr::group_by(feature) |> dplyr::summarise('mean(|SHAP value|)'=mean(value),'mean(|SHAP value|) standard error'=sd(value)/sqrt(100)) |> dplyr::mutate(Predator = 'Martes foina')
XGBOOST_canis_aureus_result_shap_sum  <- XGBOOST_canis_aureus_result_shap |> dplyr::group_by(feature) |> dplyr::summarise('mean(|SHAP value|)'=mean(value),'mean(|SHAP value|) standard error'=sd(value)/sqrt(100)) |> dplyr::mutate(Predator = 'Canis aureus')
XGBOOST_canis_lupus_result_shap_sum  <- XGBOOST_canis_lupus_result_shap|> dplyr::group_by(feature) |> dplyr::summarise('mean(|SHAP value|)'=mean(value),'mean(|SHAP value|) standard error'=sd(value)/sqrt(100)) |> dplyr::mutate(Predator = 'Canis lupus') 
XGBOOST_genetta_genetta_result_shap_sum  <- XGBOOST_genetta_genetta_result_shap|> dplyr::group_by(feature)  |> dplyr::summarise('mean(|SHAP value|)'=mean(value),'mean(|SHAP value|) standard error'=sd(value)/sqrt(100)) |> dplyr::mutate(Predator = 'Genetta genetta')
XGBOOST_neovison_vison_result_shap_sum  <- XGBOOST_neovison_vison_result_shap|> dplyr::group_by(feature) |> dplyr::summarise('mean(|SHAP value|)'=mean(value),'mean(|SHAP value|) standard error'=sd(value)/sqrt(100)) |> dplyr::mutate(Predator = 'Neovison vison') 


Shap_data <- rbind(XGBOOST_vulpes_vulpes_shap_sum,
                   XGBOOST_lynx_lynx_result_shap_sum,
                   XGBOOST_felis_silvestris_result_shap_sum,
                   XGBOOST_martes_martes_result_shap_sum,
                   XGBOOST_martes_foina_result_shap_sum,
                   XGBOOST_canis_aureus_result_shap_sum,
                   XGBOOST_canis_lupus_result_shap_sum,
                   XGBOOST_genetta_genetta_result_shap_sum,
                   XGBOOST_neovison_vison_result_shap_sum)






transform_feature_names <- function(feature_name) {
  # Replace spaces and periods with underscores
  feature_name <- gsub("_", " ", feature_name)
  feature_name <- gsub("\\.", " ", feature_name)
  
  # Capitalize the first letter
  feature_name <- stringr::str_to_sentence(feature_name)
  
  # Expand abbreviated terms
  feature_name <- gsub("Main diet omni", "Maint diet [omnivore]", feature_name)
  feature_name <- gsub("Main diet herbi", "Maint diet [herbivore]", feature_name)
  feature_name <- gsub("Main diet inv", "Maint diet [invertebrate]", feature_name)
  feature_name <- gsub("Main diet vert", "Maint diet [vertebrate]", feature_name)
  feature_name <- gsub("Main diet multi anim", "Maint diet [multiple animals]", feature_name)
  feature_name <- gsub("Activity cycle 1", "Activity cycle [nocturnal only]", feature_name)
  feature_name <- gsub("Activity cycle 2", "Activity cycle [other]", feature_name)
  feature_name <- gsub("Activity cycle 3", "Activity cycle [diurnal only]", feature_name)
  feature_name <- gsub("Foraging stratum g", "Foraging stratum [ground level]", feature_name)
  feature_name <- gsub("Foraging stratum s", "Foraging stratum [scansorial]", feature_name)
  feature_name <- gsub("Foraging stratum ar", "Foraging stratum [arboreal]", feature_name)
  feature_name <- gsub("Foraging stratum a", "Foraging stratum [aerial]", feature_name)
  feature_name <- gsub("Litter size n", "Litter size", feature_name)
  feature_name <- gsub("Litter size n", "Litter size", feature_name)
  feature_name <- gsub("Adult mass g", "Adult body mass (g)", feature_name)
  feature_name <- gsub("Brain mass g", "Brain mass (g)", feature_name)
  
  return(feature_name)
}

# Apply the transformation to the 'feature' column
Shap_data$feature <- transform_feature_names(Shap_data$feature)

Shap_data$feature <- factor(Shap_data$feature, levels = Shap_data |>
                              dplyr::group_by(feature) |>
                              dplyr::summarize(Avg_SHAP = mean(`mean(|SHAP value|)`))|>
                              dplyr::arrange(desc(Avg_SHAP)) |>
                              dplyr::pull(feature))







colors <- RColorBrewer::brewer.pal(9, "Set1")

# Reverse the order of Predator
Shap_data$Predator <- factor(Shap_data$Predator, levels = rev(unique(Shap_data$Predator)))

predator_names <- c("Canis aureus", "Canis lupus", "Felis silvestris",
                    "Genetta genetta", "Lynx lynx", "Martes foina",
                    "Martes martes", "Neovison vison", "Vulpes vulpes")
Shap_data$Predator <- factor(Shap_data$Predator, levels = predator_names)

path_Shap_data <- here::here('data','derived-data','Shap_data.csv')
Shap_data <- read.csv(path_Shap_data)
Shap_data <- Shap_data[,c(2:5)]
colnames(Shap_data) <- c('feature','mean(|SHAP value|)','mean(|SHAP value|) standard error','Predator')

Shap_data$feature <- factor(Shap_data$feature, levels = Shap_data |>
                              dplyr::group_by(feature) |>
                              dplyr::summarize(Avg_SHAP = mean(`mean(|SHAP value|)`))|>
                              dplyr::arrange(desc(Avg_SHAP)) |>
                              dplyr::pull(feature))

# Creating the plot
Shap_plot <- ggplot(Shap_data, aes(x = feature, y = `mean(|SHAP value|)`, color = Predator)) +
  geom_point(position = position_dodge(0.7)) +
  geom_line(aes(group = Predator), position = position_dodge(0.7)) +  # Add lines
  geom_errorbar(aes(ymin = `mean(|SHAP value|)` - `mean(|SHAP value|) standard error`, 
                    ymax = `mean(|SHAP value|)` + `mean(|SHAP value|) standard error`), 
                width = 0.2, 
                position = position_dodge(0.7)) +
  facet_wrap(~ Predator, ncol = 3) +  # Facet by Predator
  scale_color_manual(values = colors) +
  labs(x = "Feature", y = "Mean(|SHAP value|)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_line(color = "grey90", linetype = "dotted"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust=1),
    strip.text = element_text(face = "italic"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(color = "grey20")
  )

Shap_plot

Shap_matrix <- Shap_data |>
  dplyr::select(-`mean(|SHAP value|) standard error`)|>
  tidyr::pivot_wider(names_from = feature, values_from = `mean(|SHAP value|)`)
rownames(Shap_matrix) <- Shap_matrix$Predator
Shap_matrix <- as.matrix(Shap_matrix)
Shap_matrix <- Shap_matrix[,2:18]
Shap_matrix[is.na(Shap_matrix)] <- 0
class(Shap_matrix) <- "numeric"
Shap_matrix <- Shap_matrix[,-c(4,5,8)]
pca_result <- prcomp(Shap_matrix, scale. = TRUE)

# Create a dataframe for ggplot
pca_df <- as.data.frame(pca_result$x)
pca_df$Predator <- rownames(pca_df)
# Extracting variance explained by each principal component
pca_summary <- summary(pca_result)

# The standard deviation of each principal component
std_dev <- pca_summary$sdev
# The proportion of variance explained by each component
prop_variance <- pca_summary$importance["Proportion of Variance",]

# Plotting PCA results
# Plot of individual predators
require(ggrepel)
predator_names <- c("Canis aureus", "Canis lupus", "Felis silvestris",
                    "Genetta genetta", "Lynx lynx", "Martes foina",
                    "Martes martes", "Neovison vison", "Vulpes vulpes")
pca_df$Predator <- factor(pca_df$Predator, levels = predator_names)

p1 <- ggplot(pca_df, aes(x = PC1, y = PC2, label = Predator)) +
  geom_point(aes(color = Predator))+
  scale_color_manual(values = colors) +
  geom_text_repel(aes(color = Predator),fontface='italic') +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_line(color = "grey90", linetype = "dotted"),
    legend.position = 'none'
  )+ xlim(-3.5,3.5) + ylim(-3.5,3.5) +   labs(x = "Principal Component 1 (25%)", y = "Principal Component 2(23%)")

p1
# Plot of loadings (effect of features)
loadings <- as.data.frame(pca_result$rotation)
loadings$Feature <- rownames(loadings)

# Plot of feature effects
p2 <- ggplot(loadings, aes(x = PC1, y = PC2, label = Feature)) +
  geom_segment(aes(xend = PC1, yend = PC2), arrow = arrow(type = "closed", length = unit(0.2, "inches"))) +
  geom_label_repel(size = 4, box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_line(color = "grey90", linetype = "dotted"),
  ) + xlim(-1,1) + ylim(-1,1)  + labs(x = "Principal Component 1 (25%)", y = "")

p2

require(patchwork)
p1|p2

##########################################

###Step_4########Precise corr################

XGBOOST_vulpes_vulpes_test <- XGBOOST_binary(core_nbr = 3, data = XGBOOST_vulpes_vulpes, bootstrap = 250)|> 
  dplyr::select(scientific_name,y_pred)|> 
  dplyr::group_by(scientific_name) |>
  dplyr::summarise(y_pred=mean(y_pred)) |> 
  dplyr::mutate(Predator='Vulpes_vulpes') 
XGBOOST_lynx_lynx_test  <- XGBOOST_binary(core_nbr = 3, data = XGBOOST_lynx_lynx, bootstrap = 250)|> 
  dplyr::select(scientific_name,y_pred)|> 
  dplyr::group_by(scientific_name) |>
  dplyr::summarise(y_pred=mean(y_pred)) |> 
  dplyr::mutate(Predator='Lynx_lynx') 
XGBOOST_felis_silvestris_test  <- XGBOOST_binary(core_nbr = 3, data = XGBOOST_felis_silvestris, bootstrap = 250)|> 
  dplyr::select(scientific_name,y_pred)|> 
  dplyr::group_by(scientific_name) |>
  dplyr::summarise(y_pred=mean(y_pred)) |> 
  dplyr::mutate(Predator='Felis_silvestris') 
XGBOOST_martes_martes_test  <- XGBOOST_binary(core_nbr = 3, data = XGBOOST_martes_martes, bootstrap = 250)|> 
  dplyr::select(scientific_name,y_pred)|> 
  dplyr::group_by(scientific_name) |>
  dplyr::summarise(y_pred=mean(y_pred)) |> 
  dplyr::mutate(Predator='Martes_martes')
XGBOOST_martes_foina_test  <- XGBOOST_binary(core_nbr = 3, data = XGBOOST_martes_foina, bootstrap = 250)|> 
  dplyr::select(scientific_name,y_pred)|> 
  dplyr::group_by(scientific_name) |>
  dplyr::summarise(y_pred=mean(y_pred)) |> 
  dplyr::mutate(Predator='Martes_foina') 
XGBOOST_canis_aureus_test <- XGBOOST_binary(core_nbr = 3, data = XGBOOST_canis_aureus, bootstrap = 250)|> 
  dplyr::select(scientific_name,y_pred)|> 
  dplyr::group_by(scientific_name) |>
  dplyr::summarise(y_pred=mean(y_pred)) |> 
  dplyr::mutate(Predator= 'Canis_aureus') 
XGBOOST_canis_lupus_test  <- XGBOOST_binary(core_nbr = 3, data = XGBOOST_canis_lupus, bootstrap = 250)|> 
  dplyr::select(scientific_name,y_pred)|> 
  dplyr::group_by(scientific_name) |>
  dplyr::summarise(y_pred=mean(y_pred)) |> 
  dplyr::mutate(Predator= 'Canis_lupus') 
XGBOOST_genetta_genetta_test  <- XGBOOST_binary(core_nbr = 3, data = XGBOOST_genetta_genetta, bootstrap = 250)|> 
  dplyr::select(scientific_name,y_pred)|> 
  dplyr::group_by(scientific_name) |>
  dplyr::summarise(y_pred=mean(y_pred)) |> 
  dplyr::mutate(Predator='Genetta_genetta')
XGBOOST_neovison_vison_test  <- XGBOOST_binary(core_nbr = 3, data = XGBOOST_neovison_vison, bootstrap = 250)|> 
  dplyr::select(scientific_name,y_pred)|> 
  dplyr::group_by(scientific_name) |>
  dplyr::summarise(y_pred=mean(y_pred)) |> 
  dplyr::mutate(Predator='Neovison_vison') 


YOBS_vulpes_vulpes<- XGBOOST_vulpes_vulpes|> 
  dplyr::select(scientific_name,percentage)|> 
  dplyr::mutate(Predator='Vulpes_vulpes') 
YOBS_lynx_lynx <- XGBOOST_lynx_lynx|> 
  dplyr::select(scientific_name,percentage)|> 
  dplyr::mutate(Predator='Lynx_lynx') 
YOBS_felis_silvestris  <-  XGBOOST_felis_silvestris|> 
  dplyr::select(scientific_name,percentage)|> 
  dplyr::mutate(Predator='Felis_silvestris') 
YOBS_martes_martes <-  XGBOOST_martes_martes|> 
  dplyr::select(scientific_name,percentage)|> 
  dplyr::mutate(Predator='Martes_martes')
YOBS_martes_foina  <-  XGBOOST_martes_foina|> 
  dplyr::select(scientific_name,percentage)|> 
  dplyr::mutate(Predator='Martes_foina') 
YOBS_canis_aureus <- XGBOOST_canis_aureus|> 
  dplyr::select(scientific_name,percentage)|> 
  dplyr::mutate(Predator= 'Canis_aureus') 
YOBS_canis_lupus <- XGBOOST_canis_lupus|> 
  dplyr::select(scientific_name,percentage)|> 
  dplyr::mutate(Predator= 'Canis_lupus') 
YOBS_genetta_genetta  <- XGBOOST_genetta_genetta|> 
  dplyr::select(scientific_name,percentage)|> 
  dplyr::mutate(Predator='Genetta_genetta')
YOBS_neovison_vison  <- XGBOOST_neovison_vison|> 
  dplyr::select(scientific_name,percentage)|> 
  dplyr::mutate(Predator='Neovison_vison') 



Compa_vulpes_vulpes <- merge(XGBOOST_vulpes_vulpes_test,YOBS_vulpes_vulpes,by=c('Predator','scientific_name'))
Compa_lynx_lynx <- merge(XGBOOST_lynx_lynx_test,YOBS_lynx_lynx,by=c('Predator','scientific_name'))
Compa_felis_silvestris <- merge(XGBOOST_felis_silvestris_test,YOBS_felis_silvestris,by=c('Predator','scientific_name'))
Compa_martes_martes <- merge(XGBOOST_martes_martes_test,YOBS_martes_martes,by=c('Predator','scientific_name'))
Compa_martes_foina <- merge(XGBOOST_martes_foina_test,YOBS_martes_foina,by=c('Predator','scientific_name'))
Compa_canis_aureus <- merge(XGBOOST_canis_aureus_test,YOBS_canis_aureus,by=c('Predator','scientific_name'))
Compa_canis_lupus <- merge(XGBOOST_canis_lupus_test,YOBS_canis_lupus,by=c('Predator','scientific_name'))
Compa_genetta_genetta <- merge(XGBOOST_genetta_genetta_test,YOBS_genetta_genetta,by=c('Predator','scientific_name'))
Compa_neovison_vison <- merge(XGBOOST_neovison_vison_test,YOBS_neovison_vison,by=c('Predator','scientific_name'))


Compa <- rbind(Compa_vulpes_vulpes,
               Compa_lynx_lynx,
               Compa_felis_silvestris,
               Compa_martes_martes,
               Compa_martes_foina,
               Compa_canis_aureus,
               Compa_canis_lupus,
               Compa_genetta_genetta,
               Compa_neovison_vison)


path_Compa_data <- here::here('data','derived-data','Compa.csv')
Compa <- read.csv(path_Compa_data)

colors <- RColorBrewer::brewer.pal(9, "Set1")

Compa$Predator <- gsub("_"," ",Compa$Predator)
predator_names <- c("Canis aureus", "Canis lupus", "Felis silvestris",
                    "Genetta genetta", "Lynx lynx", "Martes foina",
                    "Martes martes", "Neovison vison", "Vulpes vulpes")
Compa$Predator <- factor(Compa$Predator, levels = predator_names)
PredvsObs <- ggplot(Compa, aes(x = y_pred, y = percentage)) +
  geom_point(aes(color=Predator),alpha=0.5,size=3.5) +  # Add points
  scale_x_sqrt() +  # Log scale for x-axis
  scale_y_sqrt() +  # Log scale for y-axis
  geom_smooth(aes(color=Predator),method = "lm", formula = y ~ x, se = F)+
  scale_color_manual(values = colors) +
  facet_wrap(~ Predator, scales = "free_y") +  # Create a facet for each predator
  theme_minimal() +  # Use a minimal theme
  labs(x = "Predicted probabilistic predation", y = "Predator preference") +
  theme(strip.text.x = element_text(face = "italic"),legend.position = 'none',
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey90", linetype = "dotted"),
  )  # Italicize facet labels

PredvsObs

