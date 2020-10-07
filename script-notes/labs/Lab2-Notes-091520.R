# ----- ----- ----- -----
#
# Title: APSTA-GE 2003: Intermediate Quantitative Methods
# Subtitle: Lab 2 Notes
# 
# Data Created: 09/14/2020
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
# After conducting the descriptive analysis, let's explore the 
# association between marathon finish time and biological sex.
# 
# ----- ----- ----- -----

# Recall -----------------------------------------------------------------------
# Load
marathon <- read.csv("marathon.csv")

# Inspect
str(marathon)
head(marathon)         # by default, shows first 6 rows
tail(marathon)         # by default, shows last 6 rows

# Summary
nrow(marathon)
mean(marathon$Time)
var(marathon$Time)
sd(marathon$Time)
sqrt(var(marathon$Time) / nrow(marathon)) # SE
median(marathon$Time)
summary(marathon)

# Setting up for a T-test ------------------------------------------------------
marathon$Sex <- ifelse(
  marathon$Sex == "f", 
  yes = 0, 
  no = 1
)

# Subset
female_times <- marathon$Time[marathon$Sex == 0]
male_times <- marathon$Time[marathon$Sex == 1]

# Summary
#    N
female_N <- length(female_times) 
male_N <- length(male_times)
#    Mean
female_mean <- mean(female_times) 
male_mean <- mean(male_times)
#    Variance
female_variance <- var(female_times) 
male_variance <- var(male_times)
#    Standard Error
female_SE <- sqrt(female_variance/female_N)
male_SE <- sqrt(male_variance/male_N)

# Levene's test ----------------------------------------------------------------
# Now, we run a Levene's test to examine if female and male groups have same 
# variance.

# 1. Load the "car" package

install.packages("car")
library(car)

# 2. Levene's test hypotheses
#    Null hypothesis: H0: equal variance 
#    Alternative hypothesis: HA: non-equal variance

leveneTest(Time ~ factor(Sex), data = marathon)

#    > p = 0.1589 
#    At a .05 significance level, we cannot reject the null that the variances 
#    are equal. So we can use equal variances for T test.

# 3. T test assuming equal variances
#    This tests: 
#    H0: mean female time = mean male time 
#    vs. 
#    HA: mean female time not equal to mean male time

t.test(Time ~ Sex, data = marathon, var.equal = TRUE)

# Running a simple linear regression -------------------------------------------

# There are a lot of arguments you can specify, but we just need the basics, so most of the default setting should be fine.

# First we specify the model. "Outcome/response" variable goes on the left,
# "explanatory" variables go on the right.
# We don't need to reference the data set and use $ because we've specified the
# data set with the argument "data = marathon".

lm1 <- lm(formula = Time ~ Sex, data = marathon)
summary(lm1)

# We can verify that this object is a list using is.list().
is.list(lm1)

# We can see all the first level items in the list using ls()

ls(lm1)

# Check coefficients

lm1$coefficients

# Using just "coef" as done in the lecture notes is simply a shortcut for typing
# out the whole word.

# You'll be using summary() on models more as you progress and need to evaluate
# how well your model fits the data or when you might decide to go with one model
# over another.

# Visualize the model


# You'll be able to use the "plot()" function on your model to check assumptions
# about that model, such as whether it meets assumptions of normality or
# homoscedasticity. Again, more on this later!

# Plotting your data and your model --------------------------------------------
# Usually we want to plot our data early on when doing descriptive analysis so 
# we will have a visual of what's going on in the data. 
# We'll do a quick scatter plot here though.
plot(marathon$Sex, marathon$Time)

# We can add better labels to the plot and a title.

plot(marathon$Sex, 
     marathon$Time, 
     xlab = "Sex", 
     ylab = "Time",
     main = "Marathon Finish Times between Male and Female Runners"
)
# We can also add our newly found regression line (in red).

abline(lm1, col = "blue", lwd = 1)

# We can also add in the points (0, mean female time) and (1, mean male time).
# The regression line will run through these points.

points(0, female_mean, col = "blue", pch = 19, )
points(1, male_mean, col = "blue", pch = 19)

# END of Lab 2 -----------------------------------------------------------------

