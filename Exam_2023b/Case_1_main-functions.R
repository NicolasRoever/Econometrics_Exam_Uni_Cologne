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