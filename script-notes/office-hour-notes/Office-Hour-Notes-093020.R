# ----- ----- ----- -----
#
# Title: APSTA-GE 2003: Intermediate Quantitative Methods
# Subtitle: Office Hour
# 
# Data Created: 9/30/2020
# Data Modified: 10/07/2020
#
# ----- ----- ----- -----
#
# Author: Tong Jin
# Email: tj1061@nyu.edu
# Affiliation: New York University
# Copyright (c) Tong Jin, 2020
#
# ----- ----- ----- -----
# 
# Topics: 
# 
# Review assignment 2
# Code explanation
# ----- ----- ----- -----

### Instructions
# In this script, we will examine the relationship between age and runner's 
# finish time in the run17.csv dataset. 

# Step 1 - Set up the working directory
# Set R working directory to the folder where your data set is stored.

# Step 2 - Import the dataset to R
dat <- read.csv("data/run17.csv")

# Step 3 - Take a look at the data
# Examine the first 6 rows
head(dat)
# Examine the last 6 rows
tail(dat)
# Examine the structure
str(dat)
# Examine if there is any missing value
table(is.na(dat))
# 

# Step 4 - Interpret the research question
# Dependent variable: net_sec
# Independent variable: age
# Data set naming: dat
# Linear regression equation: net_sec = intercept + slope * age

# Step 5 - Fit a regression model based on the above equation
lm(formula = net_sec ~ age, data = dat)
# Record the estimated intercept and the slope
# Intercept: 5074.041
# Slope: 9.535

# Step 6 - Examine the confidence interval (CI) of model estimation
# Copy and paste the linear model code line into the confint( ) function
confint(lm(formula = net_sec ~ age, data = dat))
# The (Intercept) line is the CI of estimated intercept
# The age line is the CI of estimated slope
# 2.5% is the lower boundary
# 97.5% is the higher boundary
# Why not 5% and 95%? Because p-value is two-tailed.

# Step 7 - Predict avarage finish time
# If we want to predict the average finish time of runners who are 40 years old,
# we can use the predict function with confidence interval
# The predict function mainly takes three arguments:
# object: the linear model we just created
# newdata: a new data frame containing the new values of independent variable 
#          that need to be put in the model
# interval: type of interval, can be "none", "confidence", or "prediction"
predict(object = lm(formula = net_sec ~ age, data = dat), newdata = data.frame(age = 40), interval = "confidence")
# Then, we get three results:
# fit: model prediction, in this case, fit is the average finish time of runners 
#      who are 40 years old.
# lwr: lower bound
# upr: upper bound

# Step 8 - Predict individual finish time
# My uncle likes to run. He is 45 years old. Can we predict his finish time if 
# he participates a 10 Mile race?
# Now we need to use prediction interval instead of confidence interval. Not 
# because that we are not confident about my uncle's performance, the reason is 
# that prediction based on individual value is highly biased due to individual 
# variability. Taking averaga can reduce individual variability so that model 
# prediction can be more accurate.
predict(object = lm(formula = net_sec ~ age, data = dat), newdata = data.frame(age = 45), interval = "prediction")
# Then, we get three results:
# fit: model prediction, in this case, fit is my uncle's finish time given that 
# my uncle is 45 years old.
# lwr: lower bound
# upr: upper bound

# Step 9 - Examine the model performance
summary(lm(formula = net_sec ~ age, data = dat))
# Here is the result:

### Model formula
# Call:
#   lm(formula = net_sec ~ age, data = dat)

### Summary statistics of residuals
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4280.6  -657.3   234.1  1000.2  3070.6 
#
# Note: the mean of residuals is equal to zero.

### Estimated intercept and slope, and their performance
# Coefficients:
#                Estimate  Std. Error  t value   Pr(>|t|)    
# (Intercept)   5074.0407    37.5874   134.993   <2e-16 ***
#   age            9.5348     0.9707     9.823   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# In this case, p-value is zero (< 2^-16 = 0), which is significant
# Standard error = standard deviation divided by the square root of sample size
# t value measures the standardized distance between sample mean and population 
# mean. The larger the t score, the larger the difference is between sample mean 
# and population mean.

### Model performance
# Residual standard error: 1507 on 19955 degrees of freedom
# (4 observations deleted due to missingness)
# Multiple R-squared:  0.004812,	Adjusted R-squared:  0.004762 
# F-statistic: 96.49 on 1 and 19955 DF,  p-value: < 2.2e-16

# Residual standard error measures how disperse is the residuals
# degrees of freedom = sample size - 2 (here we removed 4 rows that contain
# missing values.)
# Multiple R-squared (R^2) measures how well can model explain the variability 
# of the data. It is calculated as: MSS/TSS. The higher the R^2, the better the 
# model can explain the variability of data.
# Adjusted R-squared included a penalty feature to R-squared. The higher the
# better. A lower adjusted R^2 indicates poor fitting.
# F-statistic measures how many data variance can be explained by the model.

# Step 10 - Analysis of Variance
anova(lm(formula = net_sec ~ age, data = dat))
# Here is the result:
# Analysis of Variance Table
# 
# Response: net_sec
#              Df     Sum Sq   Mean Sq F value    Pr(>F)    
# age           1 2.1909e+08 219086315   96.49 < 2.2e-16 ***
# Residuals 19955 4.5309e+10   2270549                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# In our case: 
# the Sum Sq of age is Model Sum of Squares (MSS)
# the Sum Sq of Residuals is Residual Sum of Squares (RSS)

# ------------------------------------------------------------------------------