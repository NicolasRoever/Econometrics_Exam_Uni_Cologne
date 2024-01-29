################################################################################
##Relevant Tests################################################################
################################################################################

################################################################################
##Set Root Directory and Load functions

library(here)
library(fs)

MAIN_DIRECTORY_PATH <- here()

source(path(MAIN_DIRECTORY_PATH, "Exam_2023b", "Case_1_2023_b_main-functions.R"))
library(testthat)



################################################################################
##Tests


# Test 1: Check if all columns are numeric
test_that("check_if_columns_are_numeric_or_integer stays_silent", {
  numeric_data <- data.frame(
    A = c(1, 2, 3),
    B = c(4, 5, 6),
    C = c(7, 8, 9)
  )
  
  expect_silent(check_if_columns_are_numeric_or_integer(numeric_data))
})

# Test 2: Check for an error when non-numeric column is present
test_that("check_if_columns_are_numeric_or_integer throws an error", {
  non_numeric_data <- data.frame(
    A = c(1, 2, 3),
    B = c(4, 5, 6),
    C = c("seven", "eight", "nine")
  )
  
  expect_error(check_if_columns_are_numeric_or_integer(non_numeric_data), 
               "All columns must be numeric or integer.")
})



# Write tests for the create_lag_of_variable function
test_that("create_lag_of_variable creates lagged values correctly", {
  
  # Example data
  column <- c(1, 2, 3, 4, 5)
  
  # Test lag -1 (one lag back)
  lagged_result <- create_lag_of_variable(column, lag = -1)
  expected_result <- c(NA, 1, 2, 3, 4)
  expect_identical(lagged_result, expected_result)

  # Test lag -2 (two lags back)
  lagged_result <- create_lag_of_variable(column, lag = -2)
  expected_result <- c(NA, NA, 1, 2, 3)
  expect_identical(lagged_result, expected_result)


  # Test with a different set of data
  column <- c(10, 20, 30, 40, 50)
  lagged_result <- create_lag_of_variable(column, lag = -1)
  expected_result <- c(NA, 10, 20, 30, 40)
  expect_identical(lagged_result, expected_result)
  
})
