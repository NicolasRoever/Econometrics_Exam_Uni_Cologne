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

Happiness_Data_Path = list.files(path = path(MAIN_DIRECTORY_PATH, "Exam_2023", "Data", "Case_1"),
                                 pattern = ".csv", full.names = TRUE)

happiness_df <- import(Happiness_Data_Path) 

happiness_df["happiness_new"] <- happiness_df["happiness"] * (7354299/7123456)

happiness_df <- na.omit(happiness_df)


################################################################################
##Task a)

regression_a <- lm(happiness_new ~ hinc + unemployed + age + married +
                     migback + health, data = happiness_df)

hist(regression_a$residuals, breaks= 100)

bptest(regression_a)

plot_error_distribution_for_variable(happiness_df, regression_a, "migback")


#Distribution of errors.

#The histogram shows that empirical distribution of the residuals is not normal.
#We have a very fat left tail. This hints at the fact that the true error distribution
#is not normal as well. If the true errors are not normal, we do not have a distribution
#for the OLS estimator in finite sample. However, we can still use the CLT to argue
#that the OLS estimator is asymptotically normal. In our case, we have 3400 datapoints, 
#which is a plausible sample size for such an asymptotic argument. 

#Heteroskedasticity

#The breusch-pagan test is rejected at any reasonable significance level. Also, the
#boxplots drawn indicate that there is error heteroskedasticity with respect to the
#migration background. People with a migration background seem to have a higher error 
#variance.
#We can solve this to get a more efficient estimator by using the GLS approach: We
#estimate the variance of the error terms e.g. with the Newey-West approach
#from our data and then estimate with the coefficients with the feasable GLS estimator. 


################################################################################
##Task b)

coefficient <- regression_a$coefficients["hinc"]

1/coefficient 

summary(regression_a)

#We need to increase household income by 30661.49 c.p. to raise happiness_new by
#one unit.
#The standard error is 1.740e-05.


################################################################################
#Task c)

regression_c <-  lm(happiness_new ~ hinc + unemployed + age + I(age^2) +  married +
                      migback + health, data = happiness_df)

regression_c$coefficients["age"]
regression_c$coefficients["I(age^2)"]

- regression_c$coefficients["age"]/ (2* regression_c$coefficients["I(age^2)"])

summary(regression_c)

age_no_effect <- linearHypothesis(regression_c, c("age = 0", "I(age^2) = 0"))

#Age is affecting happiness with the equation -0.01923326 * age + 0.0002296368  * age^2.
#This has a minimum at age 41.87755. 
#We reject the null-hypothesis that age has no effect on happiness at a siginificance level
#smaller or equal to 1.2%

age_minimum_40 <- linearHypothesis(regression_c, c("age + 80 * I(age^2) = 0"))

#For the hypothesis that the minimum age is 40, we get a p-value of 76.59%

################################################################################
#Task d)

order(regression_c$coefficients)

order(summary(regression_c)$coefficients[,"t value"])

#The order of the coefficients is very different depending on whether we use the 
#t-value or the absolute value. The reason is that the absolute value of the coefficient
#is not invariant to the scale (the coefficient changes if e.g. income is 
#measured in dollars or in cents). The t-value is invariant to the scale.
#Thus, ordering based on the t-statistics is preferable. 

################################################################################
#Task e)

happiness_df_no_zeros <- happiness_df[happiness_df$happiness_new != 0,]

regression_e <- lm(log(happiness_new) ~ hinc + unemployed + age + married +
                     migback + health, data = happiness_df_no_zeros)

summary(regression_a)
summary(regression_e)

#I observe that the R^2 is higher for the regression without the log of happiness_new.
#(regression_a). This is an argument that the linear specification is closer to the
#real data generating process.

