################################################################################
##Code for Question "Milk" in the Exam on January 5th, 2024 in Advanced Econometrics###
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

df <- df %>%
  mutate(log_Sales = log(SALE * (7354299/7123456)))


################################################################################
##Task a)

demand_model <- lm(log_Sales ~ log(PRICE) + log(PRICE_FM) + log(PROMOTION), data = df)

car::durbinWatsonTest(demand_model, max.lag= 4) 

#The errors are highly autocorrelated! Every lag up until the fourth has a p-value
#very close to 0. 

coeftest(demand_model)

################################################################################
##Task b)

approximate_effect <- demand_model$coefficients["log(PRICE)"]

approximate_effect

#Approximately, sales decrease by -3.281473% 

exact_effect = exp(demand_model$coefficients["log(PRICE)"]*log(1.01))

1- exact_effect

#Exact effect, is given by -0.03212443 

################################################################################
##Task c)

linearHypothesis(demand_model, c("log(PRICE) + log(PRICE_FM) = 0"))

#The p-value is 0.3369, so I cannot reject the null-hypothesis on a reasonable 
#significance level. 

#I would reformulate the following way: Only include the variable log(PRICE) - log(PRICE_FM);
#so I would not include log(PRICE) and log(PRICE_FM) separately. This way, the model 
#represents our hypothesis that the coeffecients sum up to zero (this can be derived
#using simple algebra, just plugging in -beta_{log_Price} for beta_{log_Price_FM} in 
#the model specifcitation.

################################################################################
##Task d)

df <- df %>%
  mutate(log_price_squared = log(PRICE)^2, 
         log_price = log(PRICE))

quadratic_model <-lm(log_Sales ~ log_price + log_price_squared, data = df)

summary(quadratic_model)

minimum <- quadratic_model$coefficients["log_price"]/(2*quadratic_model$coefficients["log_price_squared"])

minimum_real_number <- exp(minimum)

minimum_real_number

#Note that this is a minimum, because the coefficient of the squared term is positive

linearHypothesis(quadratic_model , c("log_price - log_price_squared = 0"))

#I can reject that the minimum sales are at 1 euro at any reasonable significance level.
