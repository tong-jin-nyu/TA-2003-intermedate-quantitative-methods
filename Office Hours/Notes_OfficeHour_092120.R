# title: "Office Hour_IQM_Week 3"
# author: "Tong Jin"
# date: "9/21/2020"

## Setup 
# Import the data set
dat <- read.csv("lung_capacity0.csv")

# To get an overview of the data set
# "str" means structure
str(dat)

# To subset the data set by smoking status
# Create a variable to store rows of smokers
dat_smoker <- dat[dat$Smoker == 1, ]
# Create a variable to store rows of nonsmokers
dat_nonsmoker <- dat[dat$Smoker == 0, ]

# Get the standard deviation of nonsmokers' height
sd(dat_nonsmoker$Height)
# or
sd(dat$Height[dat$Smoker == 0])

# Count the number of smokers
nrow(dat_smoker)
# or
nrow(dat[dat$Smoker == 1, ])

## Linear regression modeling
# Create a linear regression: 
# Depedent: lung capacity
# Independent: Height
# Store the result in a new variable called lin_mod1
lin_mod1 <- lm(LungCapacitycc ~ Height, data = dat)
# Check the result by calling the summary function
summary(lin_mod1)
# Refer to the Estimate column for coefficients
# or using the following code
lin_mod1$coefficients
# Refer to the Pr(>|t|) column for p-value(s)

## Add a new column 
# De-mean the Height column
# Assign the result to a new column called Height1
dat$Height1 <- dat$Height - mean(dat$Height)
