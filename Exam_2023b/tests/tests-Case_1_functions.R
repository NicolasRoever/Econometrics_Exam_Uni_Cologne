################################################################################
##Relevant Tests################################################################
################################################################################

################################################################################
##Set Root Directory and Load functions

library(here)
library(fs)

MAIN_DIRECTORY_PATH <- here()

source(path(MAIN_DIRECTORY_PATH, "Exam_2023", "Case_1_main-functions.R"))
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