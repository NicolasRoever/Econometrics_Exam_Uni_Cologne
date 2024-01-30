#Packages
library(roxygen2)



plot_error_distribution_for_variable <- function(original_data, regression_model, 
                                                 heteroskedastic_variable){
  #'This function generates a graph with Boxplots for the empirical error distribution
  #'given a value of a variable where it is supposed that the error is heteroskedastic
  #'with respect to that variable, i.e. clustering could be a good idea.
  #'
  #'@args original_data: A dataframe with the original data, all columns are numeric/int
  #'@args regression_model: A regression model object.
  #'@args heteroskedastic_variable: A string with the name of the variable where 
  #'                                heteroskedasticity is supposed to be present.
  #'                                
  #'@return A graph with Boxplots for the empirical error distribution 
  #'
  
  check_if_columns_are_numeric_or_integer(original_data)
  check_for_nas_in_df(original_data)
  
  
  original_data$heteroskedastic_variable <- factor(original_data[,heteroskedastic_variable])
  
  original_data$residuals <- regression_model$residuals
  
  plot <- ggplot(original_data, aes(x = heteroskedastic_variable, y = residuals)) +
    geom_boxplot() +
    labs(x = heteroskedastic_variable, y = "Residuals") +
    theme_bw()
  
  return(plot)
  
}


check_if_columns_are_numeric_or_integer <- function(my_data) {
  # Check if all columns are numeric or integer
  if (!all(sapply(my_data, function(x) is.numeric(x) || is.integer(x)))) {
    stop("All columns must be numeric or integer.")
  }
}

check_for_nas_in_df <- function(data) {
  if (any(is.na(data))) {
    message("Careful: The data frame contains NA values.")
  } else {
    message("No NA values found in the data frame.")
  }
}


library(roxygen2)


compute_probit_confusion_matrix <- function(model, data, outcome_variable) {
  #' This function takes in a probit model and corresponding data and returns a confusion matrix
  #' @param model A probit model estimated with glm
  #' @param data A data frame containing the data used to estimate the model
  #' @param outcome_variable str The name of the outcome variable as a string
  #' 
  #' @return A confusion matrix for the model and the data
  
  check_for_nas_in_df(data)
  
  # Make Predictions
  predicted_probs <- predict(model, type = "response")
  
  # Convert Probabilities to Binary Predictions
  predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)
  
  # Create Confusion Matrix
  confusion_matrix <- table(observed = data[, outcome_variable], predicted = predicted_classes)
  
  return(confusion_matrix)
}



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