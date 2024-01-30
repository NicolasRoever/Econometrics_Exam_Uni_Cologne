################################################################################
##Code for Case 1 in the Exam on February 23th, 2023 in Advanced Econometrics###
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


################################################################################
##Task b)

standardized_regression <- run_standardized_regression(milk_df, "log_Sales ~ PRICE + PRICE_FM + PROMOTION")
#I assume he does not want us to use logs with the independent variables here 

r_squared <- summary(standardized_regression)$r.squared
residual_variance <- var(standardized_regression$residuals)

#This does not work :/ r_squared and residual variance are different :/

################################################################################
##Task c)

bgtest(demand_model, order = 3)

#I clearly reject the null-hypothesis of zero autocorrelation using the ARCH-LM test.

jarque.bera.test(demand_model$residuals)

#When testing for normality, I get a p-value of 6%, i.e. I reject normality
#at the 10% level.

residual_variance = var(demand_model$residuals)
sum(ifelse((demand_model$residuals)^2 > residual_variance *10, 1, 0)) 

#There is one very large outlier in the residuals. The test for normality is 
#very sensitive towards such an outlier, i.e. without this datapoint, the 
#p-value for the jarque.bera.test would be much higher. 

################################################################################
##Task d)

#Chow test was not covered this semester

################################################################################
##Task e)

milk_df["log_Sales_lag_1"] <- create_lag_of_variable(milk_df$log_Sales, -1)
milk_df["log_Sales_lag_2"] <- create_lag_of_variable(milk_df$log_Sales, -2)

adl_2_0_model <- dynlm(log_Sales ~ log_Sales_lag_1 + log_Sales_lag_2 + log(PRICE) + log(PRICE_FM) + log(PROMOTION), 
                       data = milk_df)

summary(adl_2_0_model)

sum_y_coefficients = summary(adl_2_0_model)$coefficients["log_Sales_lag_1", "Estimate"] +
                      summary(adl_2_0_model)$coefficients["log_Sales_lag_2", "Estimate"]

long_run_price_coefficient = summary(adl_2_0_model)$coefficients["log(PRICE)", "Estimate"] /
                              (1-sum_y_coefficients) 

long_run_price_coefficient*10

#I am not sure if this is correct! But, I obtain that increasing the own price
#by 10% decreases the long-run demand by 38.37%.
#The p-value of the coefficient for log(PRICE) is < 2e-16 (see the summary 
#of the output). Thus, the effect of log(PRICE) is significantly different
#from 0 at the 5% level. 




