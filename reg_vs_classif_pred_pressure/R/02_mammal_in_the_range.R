mammal_in_the_range <- function(pred_diet_data,mammal_ranges_shp){
 occurrence_data <- pred_diet_data |> dplyr::select(decimalLatitude,decimalLongitude)
 # Create sf points from occurrence data
 occurrence_sf <- sf::st_as_sf(occurrence_data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
 # Perform spatial overlay
 overlay_result <- sf::st_intersection(occurrence_sf, mammal_ranges_shp)

 # Check if there are any intersections (i.e., if predator occurrences fall inside mammal ranges)
 overlay_result_sum <- unique(overlay_result$binomial)
return(overlay_result_sum)
}


