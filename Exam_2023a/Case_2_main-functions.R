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



