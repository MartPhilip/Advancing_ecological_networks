preparing_dataset_xgboost <- function(prey_data,pred_data,range,eig){
  prey_pred_co_occurence <-  mammal_in_the_range(pred_data,range)
  pred_data_mammal <- prey_data[prey_data$scientific_name %in% prey_pred_co_occurence,]
  pred_data_mammal <- merge(pred_data_mammal, eig, by='scientific_name',  all.x=TRUE)
  pred_data_mammal <- pred_data_mammal[complete.cases(pred_data_mammal),] #all complete
  pred_data_mammal_clean <- pred_data |> dplyr::select(scientificNamePrey,percentage) |> dplyr::group_by(scientificNamePrey) |> dplyr::summarize(percentage=mean(percentage))
  pred_data_all <- merge(pred_data_mammal,pred_data_mammal_clean,by.x='scientific_name',by.y = 'scientificNamePrey',all.x = T)
  pred_data_all$percentage[is.na(pred_data_all$percentage)] <- 0
  pred_data_all <- pred_data_all[complete.cases(pred_data_all),] 
  pred_data_all$habitat <- as.numeric(pred_data_all$habitat)
  pred_data_all$activity_cycle <- as.factor(pred_data_all$activity_cycle)
  pred_data_all$main_diet <- as.factor(pred_data_all$main_diet)
  pred_data_all$foraging_stratum <- as.factor(pred_data_all$foraging_stratum)
  return(pred_data_all)
}