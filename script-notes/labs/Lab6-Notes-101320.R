# ----- ----- ----- -----
#
# Title: APSTA-GE 2003: Intermediate Quantitative Methods
# Subtitle: Lab 6 Notes
# 
# Data Created: 10/13/2020
# Data Modified: 10/14/2020
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
# Regression Diagnostics
# 
# ----- ----- ----- -----

# This script summarizes some useful functions you can use for regression 
# diagnostics.

# Dataset: `marketing`
# Source: the "datarium" package

# First, let's load the dataset.
install.packages("datarium")
library(datarium)
dat <- datarium::marketing
# Alternatively:
data("marketing", package = "datarium")

# Check dimensions and contents
dim(dat)
head(dat)
tail(dat)
str(dat)

# Param: youtube: youtube performance index (online video media)
# Param: facebook: facebook performance index (social media)
# Param: newspaper: newspaper performance index (traditional media)
# Param: sales: sales index

# Research Question:
#   How does youtube marketing affects sales?
#   Are their any outliers?
#   Is the assumption of linearity hold here?

# Then, we fit a simple linear regression model:
#    D.V.: sales
#    I.V.: youtube
mod_lin <- lm(sales ~ youtube, data = dat)
summary(mod_lin)
# Regerssion diagnostics:
# Model equation: sales  = 8.439 + 0.048 * youtube
# Significant relationship between youtube and sales
# Standard error: very low
# R-squared: 0.6119 (pretty good)
# Adjusted R-squared: 0.6099

# Seems like linear model is a good fit. Let's check data distribution.
# Visualize data distribution
plot(sales ~ youtube, data = dat)
# Check linearity
plot(mod_lin, which = 1)
# The linearity does not hold here because the variances across fitted values
# are not similar.

# Check fit for other types of models:
# Log
mod_log <- lm(sales ~ log(youtube), data = dat)
summary(mod_log)
# R-squared: 0.565 (decreased, indicating poorer model fit)
# Check residuals vs. fitted values
plot(mod_log, which = 1)

# Quadratic
mod_quad <- lm(sales ~ youtube^2, data = dat)
summary(mod_quad)
# R-squared: 0.6119 (does not change)
# Check residuals vs. fitted values
plot(mod_quad, which = 1)

# Polynomial
mod_poly <- lm(sales ~ poly(youtube, 3), data = dat)
summary(mod_poly)
# R-squared: 0.622 (increased!)
# Adjusted R-squared: 0.6162
# Check residuals vs. fitted values
plot(mod_poly, which = 1)
# However, the second and third power terms in our poly model do not have 
# significant relationship with the D.V.
# Increasing power does not benefit model fit much.

# Check if there are any outliers:
# 1. Calculate standardized residuals
library(MASS)
dat$std_res <- stdres(mod_lin)

# Manually select outliers
plot(std_res ~ predict(mod_lin), data = dat)
text(std_res ~ predict(mod_lin), data = dat, labels = rownames(dat), cex = 0.5, pos = 3)
abline(h =  2, col = "red", lty = "dotted")
abline(h = -2, col = "red", lty = "dotted")

# 2. Check influential plot
library(car)
influencePlot(mod_lin)

# 3. Calculate studentized residuals
dat$student_res <- studres(mod_lin)
plot(student_res ~ predict(mod_lin), data = dat)

# Difference between studentized and standardized residuals vs. fitted values
par(mfrow = c(1, 2))
plot(std_res ~ predict(mod_lin), data = dat)
plot(student_res ~ predict(mod_lin), data = dat)

# END of Lab 6 Notes -----------------------------------------------------------