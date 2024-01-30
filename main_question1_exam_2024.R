################################################################################
##Code for Question 1 in the Exam on January 5th, 2024 in Advanced Econometrics###
################################################################################

################################################################################
##Set Root Directory

library(here)
library(fs)

MAIN_DIRECTORY_PATH <- here()

################################################################################
##Install Packages and Source Code

library(rio)
library(tidyverse)
library(lmtest)
library(car)
library(tseries)
library(dynlm)
source(path(MAIN_DIRECTORY_PATH, "functions_exam_2024.R"))

################################################################################
##Load and Clean Data

Data_Path = list.files(path = path(MAIN_DIRECTORY_PATH, "Data", "Question_1"),
                       pattern = ".csv", full.names = TRUE)

df <- import(Data_Path) 

check_if_columns_are_numeric_or_integer(df)

check_for_nas_in_df(df)

################################################################################
##Task a)
