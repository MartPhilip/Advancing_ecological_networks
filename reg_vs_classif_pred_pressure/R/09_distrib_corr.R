distrib_corr <- function(dataset, column_name) {
 dataset$type <- c("test", rep("null", 99))
 
   # Extract the value from the 'test' row in the specified column
  test_value <- dataset[[column_name]][dataset$type == "test"]
  
  # Ensure there is exactly one 'test' value
  if (length(test_value) != 1) {
    stop("There must be exactly one 'test' value in the specified column.")
  }
  
  # Count the number of 'null' rows above the 'test' value
  null_count_above_test <- sum(dataset[[column_name]][dataset$type == "null"] > test_value, na.rm = TRUE)
  
  # Calculate the percentage of 'null' rows above the 'test' value
  total_null_count <- sum(dataset$type == "null", na.rm = TRUE)
  
  # Handle case where there are no 'null' values
  if (total_null_count == 0) {
    return(0)
  }
  
  percentage_null_above_test <- (null_count_above_test / total_null_count) * 100
  
  # Return the result
  return(percentage_null_above_test)
}
