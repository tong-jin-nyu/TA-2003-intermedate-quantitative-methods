# ----- ----- ----- ----- *
#
# Title: APSTA-GE 2003: Intermediate Quantitative Methods
# Subtitle: Class Notes
# 
# Data Created: 10/27/2020
# Data Modified: 10/28/2020
#
# ----- ----- ----- ----- *
#
# Author: Tong Jin
# Email: tj1061@nyu.edu
# Affiliation: New York University
# Copyright (c) Tong Jin, 2020
#
# ----- ----- ----- ----- *
# 
# Topics: ----
# 
# Review Assignment 5
# 
# ----- ----- ----- ----- *

# In this notes, we will conduct a multiple regression analysis using the 
# marketing dataset from the "datarium" package. 

# Load data ----
library(datarium)
dat <- datarium::marketing

# Examine data structure ----
# DV: sales
# IV1: youtube
# IV2: facebook (larger than 25 or not)
# IV3: newspaper

# Create a binary variable: facebook0
# If: marketing budget on facebook is greater than 25, mark 1
# If: marketing budget on facebook is smaller or equal than 25, mark 0
dat$facebook0 <- ifelse(dat$facebook > 25, yes = 1, no = 0)

# Center the youtube variable ----
dat$youtube0 <- dat$youtube - mean(dat$youtube)

# Fit a multiple regression model using centered youtube ----
# Include interactive effects
mod1 <- lm(sales ~ youtube0 + facebook + youtube0*facebook, data = dat)

# Fit two models using subsetted datasets ----
# DV: sales
# IV1: youtube0
# IV2: facebook (1, 0)

# Subset the data ----
dat1 <- dat[dat$facebook == 1, ]
dat0 <- dat[dat$facebook == 0, ]
# Fit models
mod2 <- lm(sales ~ youtube0, data = dat1)
mod3 <- lm(sales ~ youtube0, data = dat0)

# Examine model results ----
summary(mod1)
summary(mod2)
summary(mod3)

# Get sum of squares ----
# TSS = MSS + RSS
anova(mod1)
anova(mod2)
anova(mod3)

# END of lab 8 class notes -----------------------------------------------------