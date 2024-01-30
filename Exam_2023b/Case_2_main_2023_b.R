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
library(broom)
library(margins) #Computing effect at the average for probit/logit
source(path(MAIN_DIRECTORY_PATH, "Exam_2023b", "Case_2_main-functions.R"))
source(path(MAIN_DIRECTORY_PATH, "Exam_2023b", "Case_1_2023_b_main-functions.R"))


################################################################################
##Load and Clean Data

Data_Path = list.files(path = path(MAIN_DIRECTORY_PATH, "Exam_2023b", "Data", "Case_2"),
                                 pattern = ".csv", full.names = TRUE)

wages_df <- import(Data_Path) 

check_if_columns_are_numeric_or_integer(wages_df)

check_for_nas_in_df(wages_df)

wages_df["lwage_new"] <- wages_df["lwage"] * (7354299/7123456)


################################################################################
##Task a)

pooled_OLS_1 <- lm(lwage_new ~ educ + exper, data = wages_df)

summary(pooled_OLS_1)

glance(pooled_OLS_1)

pooled_OLS_2 <- lm(lwage_new ~ educ + log(exper), data = wages_df)

summary(pooled_OLS_2)

glance(pooled_OLS_2)

#The glance function gives the absolute value for AIC and BIC. Specification 2
#with the logarithm of exper has a larger AIC and BIC and is thus preferred. 
#This also makes intuitive sense, as diminishing returns of experience seem
#likely. 
#In model 1, increasing experience from 10 to 11 years leads to a wage increase
#of 13%. I cannot say how much the increase is in Dollars as this depends on the
#years of education of the individual. 
#In model 2, increasing experience from 10 to 11 years (which is 10%) leads to a wage increase
#of 10*0.35 % = 3.5%

################################################################################
##Task b)

#Original Breusch-Pagan Test
bptest(pooled_OLS_2, studentize = FALSE)

#Koenker Test
bptest(pooled_OLS_2, studentize = TRUE)

#Plot Residuals

hist(pooled_OLS_2$residuals)

#The original breusch pagan test assumes a normal ditribution for the errors.
#From the histogram however, we see that the estimated errors have a very fat left
#tail. Thus, the assumption of normality seems unreasonable and the results os the 
#original breusch pagan test are not reliable. The studentized version of the test
#(koenker test) does not assume normality and is thus more reliable. This 
#explains the vastly different p-values obtained. 

################################################################################
##Task c)

#I plug in the formula for \beta_{1,i} into the original regression equation and 
#simplify. Using the notation from the exam sheet, I get:
#wage_{i} = \beta_0 + \gamma_0 exper_i + \gamma_1 educ_i exper_i + \beta_2 educ_i +
#v_i exper_i +\epsilon_i. 
#Note that the last 2 terms constitute the error. Thus, this model is heteroskedastic
#by assumption: For larger values of exper_i, the variance of the error term increases.

interaction_model <- lm(lwage_new ~ educ + exper + educ:exper, data = wages_df)

#The estimate for \gamma_1 is 0.001762. 

plot_data = wages_df %>% 
  mutate(squared_residuals = interaction_model$residuals^2) 

ggplot(plot_data, aes(x=exper, y=squared_residuals)) + geom_point()

#In the plot, heteroscedasticity is honestly not clearly visible though. 

################################################################################
##Task d)

full_model <- lm(lwage_new ~ educ + exper + inst1 + inst2, data = wages_df)

summary(full_model)

#The variable inst1 is not significant at the 5% level. And the variable inst2
#is insignificant at any reasonable level. However, this does not mean that educ
#is not endogenous! It could be that the IQ affects education and education affects
#the wage. In such a case, including educ would be called a "bad control" as
#it also captures some of the effect of the IQ. 
#The coefficient of educ is estimated consistently if Cov(e_i, educ_i) = 0. 
#Including the two extra variables sovles helps to estimate the coefficient 
#consistently if the variables are correlated with education and the outcome, i.e.
#Cov(educ_i, inst1_i) unequal to 0. Then, a specification without inst1 would definitely
#lead to an inconsistent estimate of the educ coefficient. 

################################################################################
##Task e)

first_stage <- lm(educ ~ log(exper) + inst1 + inst2, data = wages_df)

summary(first_stage)

#The R^2 is 0.07588. Note that it is best practice to include all controls in 
#the first stage as well to avoid misspecification. 

second_stage <- lm(lwage_new ~ log(exper) + first_stage$fitted.values, data = wages_df)

summary(second_stage)

#One can show that the IV coefficients can be obtained by regressing the variable
#in question on ghe fitted values of the first stage. This is just a mathematical 
#fact that can be shown by manipulating the equations. 
#However, ther reported standard errors are wrong. The reason is that R ignores the 
#fact that the fitted values were estimated with errors. Thus, the standard
#formula for standard errors cannot be applied within the IV setting! The correctly
#estimated standard errors will be larger. 



