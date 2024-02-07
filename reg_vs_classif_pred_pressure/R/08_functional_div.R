functional_div <- function(XGBOOST_vulpes_vulpes,
                           XGBOOST_lynx_lynx,
                           XGBOOST_felis_silvestris,
                           XGBOOST_martes_martes,
                           XGBOOST_martes_foina,
                           XGBOOST_ursus_arctos,
                           XGBOOST_canis_aureus,
                           XGBOOST_canis_lupus,
                           XGBOOST_genetta_genetta,
                           XGBOOST_neovison_vison){
# Example data: list of species vectors (each vector represents a site)
  XGBOOST_vulpes_vulpes <- XGBOOST_vulpes_vulpes |> dplyr::filter(percentage>0)
  XGBOOST_lynx_lynx <- XGBOOST_lynx_lynx |> dplyr::filter(percentage>0)
  XGBOOST_felis_silvestris <- XGBOOST_felis_silvestris |> dplyr::filter(percentage>0)
  XGBOOST_martes_martes <- XGBOOST_martes_martes |> dplyr::filter(percentage>0)
  XGBOOST_martes_foina <- XGBOOST_martes_foina |> dplyr::filter(percentage>0)
  XGBOOST_ursus_arctos <- XGBOOST_ursus_arctos |> dplyr::filter(percentage>0)
  XGBOOST_canis_aureus <- XGBOOST_canis_aureus |> dplyr::filter(percentage>0)
  XGBOOST_canis_lupus <- XGBOOST_canis_lupus |> dplyr::filter(percentage>0)
  XGBOOST_genetta_genetta <- XGBOOST_genetta_genetta |> dplyr::filter(percentage>0)
  XGBOOST_neovison_vison <- XGBOOST_neovison_vison |> dplyr::filter(percentage>0)
  
  species_list <- list(
  Vulpes_vulpes = XGBOOST_vulpes_vulpes$scientific_name,
  Lynx_lynx = XGBOOST_lynx_lynx$scientific_name,
  Felis_silvestris = XGBOOST_felis_silvestris$scientific_name,
  Martes_martes = XGBOOST_martes_martes$scientific_name,
  Martes_foina = XGBOOST_martes_foina$scientific_name,
  Ursus_arctos = XGBOOST_ursus_arctos$scientific_name,
  Canis_aureus = XGBOOST_canis_aureus$scientific_name,
  Canis_lupus = XGBOOST_canis_lupus$scientific_name,
  Genetta_genetta = XGBOOST_genetta_genetta$scientific_name,
  Neovison_vison = XGBOOST_neovison_vison$scientific_name
)

# Step 1: Combine all species names and find unique ones
all_species <- unique(unlist(species_list))

# Step 2: Initialize matrix
species_matrix <- matrix(0, nrow = length(species_list), ncol = length(all_species))
rownames(species_matrix) <- names(species_list)
colnames(species_matrix) <- all_species

# Step 3: Fill the matrix
for (i in 1:length(species_list)) {
  species_matrix[i, species_list[[i]]] <- 1
}

# View the matrix
species_matrix

trait_data <- unique(rbind(XGBOOST_vulpes_vulpes[,1:9],
                    XGBOOST_lynx_lynx[,1:9],
                    XGBOOST_felis_silvestris[,1:9],
                    XGBOOST_martes_martes[,1:9],
                    XGBOOST_martes_foina[,1:9],
                    XGBOOST_ursus_arctos[,1:9],
                    XGBOOST_canis_aureus[,1:9],
                    XGBOOST_canis_lupus[,1:9],
                    XGBOOST_genetta_genetta[,1:9],
                    XGBOOST_neovison_vison[,1:9])) 
rownames(trait_data) <- trait_data$scientific_name
trait_data <- subset(trait_data,select = -scientific_name)
result <- hillR::hill_func(
  species_matrix,
  trait_data,q=0)
return(result)
}
