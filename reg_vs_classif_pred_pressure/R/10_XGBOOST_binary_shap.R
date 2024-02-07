LogLoss <- function(actual, predicted)
{
  result=-1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
  return(result)
}

XGBOOST_binary_shap <- function(data,bootstrap,core_nbr){
  set.seed(42)
  data <- data[,c(1:9,410)]
  cores <- core_nbr
  data$percentage <- ifelse(data$percentage>0,1,0)
  new_predictor_main_diet <- predict(dummyVars(~ main_diet, data),data)
  new_predictor_activity_cycle <- predict(dummyVars(~ activity_cycle, data),data)
  new_predictor_foraging_stratum <- predict(dummyVars(~ foraging_stratum, data),data)
  data <- cbind(data,new_predictor_activity_cycle,new_predictor_main_diet,new_predictor_foraging_stratum)
  data <- data |> dplyr::select(-main_diet,-activity_cycle,-foraging_stratum)
  data_xgboost <- data |> dplyr::select(-scientific_name)
  Y <-  data_xgboost |> dplyr::select(percentage)
  X <- data_xgboost |> dplyr::select(-percentage)
  bootstrap_data <- pbmcapply::pbmclapply(1:bootstrap, function(j) {
    train_indices <- sample(1:nrow(X), 0.6* nrow(X), replace = FALSE)
    X_train <- X[train_indices, ]
    Y_train <- Y[train_indices,]
    X_test <- X[-train_indices, ]
    Y_test <- Y[-train_indices,]
    train_matrix <- xgb.DMatrix(data = as.matrix(X_train), label = Y_train)
    test_matrix <- xgb.DMatrix(data = as.matrix(X_test), label = Y_test)
    # Define the parameter grid for grid search10
    param_grid <- expand.grid(max_depth = c(5,6,7,8),
                              eta = c(0.1, 0.01),
                              nrounds = c(10,25,50,100,200))
    
    
    best_params <- NULL
    best_score <- Inf
    
    for(i in 1:nrow(param_grid)) {
      params <- param_grid[i, ]
      
      # Train the model with current parameters
      model <- xgboost(train_matrix,
                       objective = "binary:logistic",
                       eval_metric = "logloss",
                       max_depth = params$max_depth,
                       eta = params$eta,
                       nrounds = params$nrounds,
                       verbose = 0)
      
      # Make predictions on the test set
      y_pred <- predict(model, newdata = as.matrix(X_test))
      
      # Calculate RMSE
      logloss <- LogLoss(Y_test,y_pred)
      # Update best parameters and score if current model performs better
      if (logloss < best_score) {
        best_params <- params
        best_score <- logloss
      }
    }
    # Train the model
    best_model <- xgboost(train_matrix,
                          max_depth = best_params$max_depth,
                          eta = best_params$eta,
                          nrounds = best_params$nrounds,
                          verbose = 0,
                          objective = "binary:logistic",
                          eval_metric = "logloss")
    # Make predictions on the test set
    data_shap <- X 
    x <- colnames(data_shap)
    size <- length(data_shap$habitat)
    shap_data <- data_shap[sample(nrow(data_shap), size), x]
    shp <- shapviz::shapviz(best_model, X_pred = data.matrix(shap_data), X = shap_data)
    shp_import <- shapviz::sv_importance(shp, show_numbers = TRUE)
    shp_import <- shp_import$data
    return(shp_import)
  }, mc.cores = cores)
  results <- do.call(rbind, bootstrap_data)
  return(results)
}

