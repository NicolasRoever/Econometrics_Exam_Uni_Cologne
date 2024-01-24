################################################################################
##Code for Case 2 in the Exam on February 23th, 2023 in Advanced Econometrics###
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
library(glm2)
library(pscl)
library(AER)
library(margins) #Computing effect at the average for probit/logit
source(path(MAIN_DIRECTORY_PATH, "Exam_2023a", "Case_2_main-functions.R"))
source(path(MAIN_DIRECTORY_PATH, "Exam_2023a", "Case_1_main-functions.R"))


################################################################################
##Load and Clean Data

Data_Path = list.files(path = path(MAIN_DIRECTORY_PATH, "Exam_2023", "Data", "Case_2"),
                                 pattern = ".csv", full.names = TRUE)

house_df <- import(Data_Path) 

check_if_columns_are_numeric_or_integer(house_df)

check_for_nas_in_df(house_df)


################################################################################
##Task a)

linear_probability_model <- lm(own_house ~ income + age + educ + hh_size + ethnic+ x,
                   data = house_df)

#Note that by CLT, the OLS estimator converges to a normal distribution no matter
#what the specific distribution of the error term is. Asymptotically, the OLS estimator
#is consistent no matter the distribution of the error term. However, as the dependent
#variable follows a bernoulli distribution, the the error is conditional heteroskedastic,
#i.e. the variance is different depending on the values of the independent variables.
#Thus, we should use a variance estimator that is robust to heteroskedasticity 
#(e.g. Newey-West, 1987)

################################################################################
##Task b)

probit_model <- glm(own_house ~ income + age + educ + hh_size + ethnic+ x,
                   data = house_df, family = binomial(link = "probit"))

pseudo_r_2_lpm <- pR2(linear_probability_model, type = "McFadden")
pseudo_r_2_probit <- pR2(probit_model, type = "McFadden")

#The probit model provides the better fit as measured by the McFadden R^2. 
#The estimated coefficients are vastly different because they represent something 
#different: For the linear probability model, the coefficients represent the marginal
#effect of raising a dependent variable by one unit on the probability of owning a house.
#For the probit model, the coefficients represent the marginal effect of raising a
#dependent variable on the logarithmic odds of owning a house!
#Hoewever, the t-statistics measure the significance of the estimated coefficients -
#this remains unchanged no matter which model we use.


################################################################################
##Task c)

linear_probability_model$coefficients["hh_size"]

#In the linear probability model, increasing the hh_size by one unit increases the
#probability of owning a house by about 11% c.p.

margins(probit_model)

#In the probit model, increasing the hh_size OF AN AVERAGE HOUSEHOLD IN THE DATASET
#increases the probability of owning a house by 12%. The key thing to note here is
#that this marginal effect changes dependent on what the values of the independent
#The reported number is thus only valid for an average household. 
#And yes, the marginal probability effect is scale-invariant. 


################################################################################
##Task d)


#SKIPPED