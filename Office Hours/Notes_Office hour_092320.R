# APSTA-GE 2003: Intermediate Quantitative Methods
# Office Hour
# 092320
# Fall 2020

# Topics:
# 1. Review assignment 1

# Import the data set
dat <- read.csv("data/lung_capacity0 (2).csv")
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