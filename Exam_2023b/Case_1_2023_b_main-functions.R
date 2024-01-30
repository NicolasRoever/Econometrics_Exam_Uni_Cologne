#Packages
library(roxygen2)



run_standardized_regression<- function(data, regression_formula){
  #' This function takes a dataframe and the regression formula and returns
  #' a standardized regression model
  #' 
  #' @param data this is the data on which the regression is performed
  #' @param regression_formula this is the regression formula formulated as a string, 
  #' i.e. y ~ x1 + x2
  #' 
  #' @return a regression model object
  
  regression_formula <- as.formula(regression_formula)
  
  standardized_data <- as.data.frame(scale(data))
  
  regression_model <- lm(regression_formula, data = standardized_data)
  
  return(regression_model)

  
}


create_lag_of_variable <- function(column, lag){
  #' This function takes a column and creates a lag of that column
  #' 
  #' @param column this is the column for which we want to create a lag
  #' @param lag this is the number of lags we want to create -1 is creating one 
  #' lag back, -2 is creating 2 lags back etc.)
  #' 
  #' @return a vector with the lagged values
  
  if (lag > -1) {
    stop("Invalid value for lag. Lag should be -1 or less.")
  }
  
  lagged_column <- c(rep(NA, abs(lag)), column[1:(length(column) - abs(lag))])
  
  return(lagged_column)
  
}


check_if_columns_are_numeric_or_integer <- function(my_data) {
  # Check if all columns are numeric or integer
  if (!all(sapply(my_data, function(x) is.numeric(x) || is.integer(x)))) {
    stop("All columns must be numeric or integer.")
  }
}

check_for_nas_in_df <- function(data) {
  #Check if there are NA values in the data
  if (any(is.na(data))) {
    message("Careful: The data frame contains NA values.")
  } else {
    message("No NA values found in the data frame.")
  }
}