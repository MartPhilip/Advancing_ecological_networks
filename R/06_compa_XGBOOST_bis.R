compa_XGBOOST_bis <- function(raw_data,data_noeig_class,
                          species_name){
  result_data_0eig_class_sum <- data_noeig_class |> 
    dplyr::select(scientific_name,y_pred)|> 
    dplyr::rename(y_pred_0eig_class=y_pred) |>
    dplyr::group_by(scientific_name) |>
    dplyr::summarise(y_pred_0eig_class=mean(y_pred_0eig_class))
  
  Yobs_data <- raw_data |> dplyr::select(scientific_name,percentage)
  
  XGBOOST_compa <- merge(Yobs_data,result_data_0eig_class_sum,by='scientific_name')
  XGBOOST_compa$class <- ifelse(XGBOOST_compa$percentage>0,1,0)
  XGBOOST_compa <- XGBOOST_compa |>
    dplyr::arrange(-percentage) |> 
    dplyr::mutate(rank = 1:length(XGBOOST_compa$scientific_name)) |>
    dplyr::arrange(-y_pred_0eig_class) |> 
    dplyr::mutate(rank_0eig_class = 1:length(XGBOOST_compa$scientific_name))
  XGBOOST_compa$y_pred_0eig_class <- round(XGBOOST_compa$y_pred_0eig_class ,2)
  cor_exact_val <- cor(XGBOOST_compa$percentage, y = XGBOOST_compa$y_pred_0eig_class, method =  "spearman")
  cor_rank_val <- cor(XGBOOST_compa$percentage, y = XGBOOST_compa$rank_0eig_class, method =  "spearman")
  results <- data.frame("Correlation_exact"=cor_exact_val,
                        "Correlation_rank"=cor_rank_val,
                        "scientific_name" = species_name )
  return(results)
}
