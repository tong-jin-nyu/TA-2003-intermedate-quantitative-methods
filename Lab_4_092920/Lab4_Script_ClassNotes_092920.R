# APSTA-GE 2003: Intermediate Quantitative Methods
# Fall 2020
# Lab Section 003
# Week 4

# Linear Regression Model 
# TSS, MSS, RSS
# R-squared
# ANOVA

# Created on: 092920
# Modified on: 092920

# Data Source: openintro
# Data Set: run17.csv

# Import the data set
dat <- read.csv("data/run17.csv")

# Examine the data set
str(dat)

# Subset the data set to include the "10 Mile" event only
# Using brackets for slicing
dat_10m <- dat[dat$event == "10 Mile",  ]

# Research question:
# DV: net_sec: net finish time in seconds
# IV: age

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

8388.5 = 8388.50