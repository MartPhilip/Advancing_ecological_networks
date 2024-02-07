RMSE <- function(actual, predicted) {
  n <- length(actual)
  rmse <- sqrt(sum((actual - predicted)^2) / n)
  return(rmse)
}

XGBOOST_reg <- function(data,bootstrap,core_nbr){
set.seed(42)
cores <- core_nbr
new_predictor_main_diet <- predict(dummyVars(~ main_diet, data),data)
new_predictor_activity_cycle <- predict(dummyVars(~ activity_cycle, data),data)
new_predictor_foraging_stratum <- predict(dummyVars(~ foraging_stratum, data),data)
data <- cbind(data,new_predictor_activity_cycle,new_predictor_main_diet,new_predictor_foraging_stratum)
data <- data |> dplyr::select(-main_diet,-activity_cycle,-foraging_stratum)
data_xgboost <- data |> dplyr::select(-scientific_name)
Y <-  data_xgboost |> dplyr::select(percentage)
X <- data_xgboost |> dplyr::select(-percentage)
bootstrap_data <- data.frame()
importance_data <- data.frame()
bootstrap <- bootstrap
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
                     max_depth = params$max_depth,
                     eta = params$eta,
                     nrounds = params$nrounds,
                     verbose = 0)
    
    # Make predictions on the test set
    y_pred <- predict(model, newdata = as.matrix(X_test))
    
    # Calculate RMSE
    RMSE <- RMSE(Y_test,y_pred)
    # Update best parameters and score if current model performs better
    if (RMSE < best_score) {
      best_params <- params
      best_score <- RMSE
    }
  }
  
  
  
  # Train the model
  best_model <- xgboost(train_matrix,
                        max_depth = best_params$max_depth,
                        eta = best_params$eta,
                        nrounds = best_params$nrounds,
                        verbose = 0)
  # Make predictions on the test set

  y_pred <- predict(best_model, newdata = as.matrix(X_test))
  data_model <- data.frame('Y_test'=Y_test,
                           'y_pred'=y_pred)
  model <- glm(Y_test~y_pred,data=data_model,family= poisson(link = "log"))
  # Calculate the log likelihood of your model
  ll_model <- logLik(model)
  # Fit the null model (with only an intercept)
  null_model <- glm(Y_test ~ 1, data = data_model, family = poisson(link = "log"))
  # Calculate the log likelihood of the null model
  ll_null <- logLik(null_model)
  # Calculate McFadden's R-squared
  mcfadden_r2 <- 1 - (ll_model/ll_null)
  data_new <- data[-train_indices,]
  species <- data_new |> dplyr::select(scientific_name)
  species <- cbind(species,y_pred) 
  species$boostrap <- j
  species$mcfadden_r2 <- mcfadden_r2
  species <- cbind(species,best_params)
  return(species)
}, mc.cores = cores)
results <- do.call(rbind, bootstrap_data)
return(results)
}
