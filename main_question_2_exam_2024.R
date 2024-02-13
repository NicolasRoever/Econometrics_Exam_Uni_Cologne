################################################################################
##Code for Question "Unemployment" in the Exam on January 5th, 2024 in Advanced Econometrics###
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
library(pscl)
source(path(MAIN_DIRECTORY_PATH, "functions_exam_2024.R"))

################################################################################
##Load and Clean Data

Data_Path = list.files(path = path(MAIN_DIRECTORY_PATH, "Data", "Question_2"),
                       pattern = ".csv", full.names = TRUE)

df <- import(Data_Path) 

check_if_columns_are_numeric_or_integer(df)

check_for_nas_in_df(df)

constant <- 5/(1+5)

df = df %>%
  mutate(age = age + constant) %>%
  na.omit()

################################################################################
##Task a)

lpm <- lm(unemployed ~ age + age^2+ educ + health +
            female + migback, data = df)


#I assume heteroskedasticity for the error dependent on the value of unemployed. 
#The reason is that unemployed follows a bernoulli distribution, which means the 
#variance of the error depends on the values of the dependent variables.

coeftest(lpm)

################################################################################
##Task b)

estimations <- lpm$fitted.values

hist(estimations, breaks = 50, main = "Histogram of Estimations", xlab = "Estimations")

#In the plot, we observe negativ values for the probability to be unemployed. The reason
#is that the linear probability model does not restrict the values of the dependent variable
#to be between 0 and 1. This is a problem, because the dependent variable is a probability
#which can only be between 0 and 1 (see kolmogorovs definition of probabilities.)
#A solution is to use a probit or logit model, which restricts the range of values
#through the choice of the link function.

################################################################################
##Task c)

mean(df$unemployed)

#Unconditionally, the probability is 5%. 

coefficient_age = lpm$coefficients['age']

coefficient_age * 5

# the probability decreases by -0.008623624. 


################################################################################
##Task d)

probit_model <- glm(unemployed ~ age + age^2+ educ + health +
                      female + migback, data = df, family = binomial(link = "probit"))

summary(probit_model)

fit_measures <- pR2(probit_model, type = "McFadden")

fit_measures["McFadden"]

df["probit_estimations"] <- predict(probit_model, type = "response")

extra_regression <- lm(unemployed ~ probit_estimations, data = df)

summary(extra_regression)

#The pseudo R^2 is higher for the probit model than when using the approach of 
#first estimating the predicted values and then using a normal R^2 from a linear 
#regression. This highlights that pseudo-R^2 and R^2 are different measures to 
#assess model fit. Both measures focus on different aspects of the model fit and
#thus do not give equal results. However, results for both measures should 
#always be in the same ballpark, as these measures try to assess the same thing 
#and are not that different. 
