# ----- ----- ----- -----
#
# Title: APSTA-GE 2003: Intermediate Quantitative Methods
# Subtitle: Lab 4 Notes
# 
# Data Created: 09/29/2020
# Data Modified: 10/08/2020
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
# Linear Regression Model 
# TSS, MSS, RSS
# R-squared
# ANOVA
# 
# ----- ----- ----- -----

### Instructions: 
# In this script, we are going to examine how age affects runner's performance. 
# The research question is: is age associated with runner's finish time in 10 
# Mile race?

# Data Source: openintro
# Data Set: run17.csv

# DV: net_sec: net finish time in seconds
# IV: age
# Null hypothesis: not significant association
# Alternative hypothesis: significant association

### Data processing
# Import the data set
dat <- read.csv("data/run17.csv")

# Examine the data set
str(dat)

# Subset the data set to include the "10 Mile" event only
# Using brackets for slicing
dat_10m <- dat[dat$event == "10 Mile",  ]

# Fit a linear regression model
lm1 <- lm(formula = net_sec ~ age, data = dat_10m  )
summary(lm1)

# Confidence interval
confint(lm1)

# Model fit: net_sec_hat = 5388.0324 + 12.9782 * age + (error)
dat_new <- data.frame(
  age = c(30, 40, 50)
)

# Predict the finish time of new data set based on our linear model
predict(lm1 , newdata = dat_new, interval = "confidence")
predict(lm1 , newdata = dat_new, interval = "prediction")

# Analyze the variance of our data set using ANOVA
anova(lm1)

# END OF THIS SCRIPT -----------------------------------------------------------