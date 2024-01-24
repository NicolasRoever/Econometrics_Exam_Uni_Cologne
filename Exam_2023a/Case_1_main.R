################################################################################
##Code for Case 1 in the Exam on March 21st, 2023 in Advanced Econometrics###
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
source(path(MAIN_DIRECTORY_PATH, "Exam_2023b", "Case_1_main-functions.R"))


################################################################################
##Load and Clean Data

Data_Path = list.files(path = path(MAIN_DIRECTORY_PATH, "Exam_2023b", "Data", "Case_1"),
                                 pattern = ".csv", full.names = TRUE)

milk_df <- import(Data_Path) 

check_if_columns_are_numeric_or_integer(milk_df)

check_for_nas_in_df(milk_df)

milk_df["log_Sales"] <- log(milk_df["SALE"]) * (7354299/7123456)

################################################################################
##Task a)

demand_model <- lm(log_Sales ~ log(PRICE) + log(PRICE_FM) + log(PROMOTION), 
                   data = milk_df)

summary(demand_model)

demand_model$coefficients["log(PRICE)"] * 25 #This is because we increase our own price by 25%

#The demand for our own product decreases by 84% if we increase the price by 25%



