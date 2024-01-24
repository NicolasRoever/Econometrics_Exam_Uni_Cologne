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