# ----- ----- ----- -----
#
# Title: APSTA-GE 2003: Intermediate Quantitative Methods
# Subtitle: Office Hour
# 
# Data Created: 09/23/2020
# Data Modified: 10/06/2020
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
# Review assignment 1
#
#
# ----- ----- ----- -----

# Import the data set
# Make sure that your data set is in the same directory as your script file.
dat <- read.csv("lung_capacity0.csv")
# Overview
str(dat)

# Subset the data set based on smoking status
# dat_smoker:    smokers
# dat_nonsmoker: non-smokers
dat_smoker <- dat[dat$Smoker == 1, ]
dat_nonsmoker <- dat[dat$Smoker == 0, ]

# Calculate the mean of lung capacity of each group
mean(dat_smoker$LungCapacitycc)
mean(dat_nonsmoker$LungCapacitycc)

# Calculate the variance of lung capacity of each group
var(dat_smoker$LungCapacitycc)
var(dat_nonsmoker$LungCapacitycc)

# Calculate the standard error of lung capacity of each group
sqrt(var(dat_smoker$LungCapacitycc) / nrow(dat_smoker))
sqrt(var(dat_nonsmoker$LungCapacitycc) / nrow(dat_nonsmoker))

# Generate a linear regression
# Question 9
lm_Q9 <- lm(
  LungCapacitycc ~ Height,
  data = dat
)
summary(lm_Q9)

# How to interpret?
# On average, as height increases by 1 centimeter, we expect an increase of lung capacity by 88.798 cc.