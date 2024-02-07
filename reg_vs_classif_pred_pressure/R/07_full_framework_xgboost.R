full_framework_xgboost <- function(bootstrap,XGBOOST_data,bootstrap_xg,species_name){
  list_results <- list()
  for(i in 1:bootstrap){
  set.seed(42)
  #I shuffle the response variable to produce a null database
  #I create 3 database selecting only the features which must be include for the RF
  XGBOOST_sp_noeig <- XGBOOST_data[,c(1:9,410)]
  #Computing the bootstrap + XGBOOST
  result_XGBOOST_sp_noeig_class <- XGBOOST_binary(data = XGBOOST_sp_noeig , bootstrap = bootstrap_xg, core_nbr = 3)
  mean_mcc <- mean(result_XGBOOST_sp_noeig_class$mcc_value)
  sd_mcc <- sd(result_XGBOOST_sp_noeig_class$mcc_value)
  compa_XGBOOST_sp <- compa_XGBOOST_bis(raw_data = XGBOOST_data,
                                               data_noeig_class = result_XGBOOST_sp_noeig_class,
                                               species_name = species_name)
  
  compa_XGBOOST_sp$mean_mcc <- mean_mcc
  compa_XGBOOST_sp$sd_mcc <- sd_mcc
  list_results[[i]] <- compa_XGBOOST_sp
  XGBOOST_data$percentage <- sample(XGBOOST_data$percentage)
  
 }
 return(list_results)
}
