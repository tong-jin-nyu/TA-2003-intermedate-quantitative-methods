### Created on: 09/08/2020
### Modified on: 09/08/2020

# APSTA-GE 2003: Intermediate Quantitative Methods
# Fall 2020

# Lab 1 ------------------------------------------------------------------------
# Class Notes ------------------------------------------------------------------

# Introduction to R ------------------------------------------------------------

## Before coding in RStudio ----------------------------------------------------

# 1. To "comment out" a line, add a # (number sign) at the beginning of the line 
#    or select the line and use "Ctrl/Command + Shift + C".

# 2. To run a line of code, hit the "Run" button or use "Ctrl/Command + Enter".

# 3. Confused? To get help, type "?" in the console followed by the command you 
#    would like to look up and then run it
#    e.g. ?setwd
#    or go to the "Help" tab in the lower right window.

# 4. Text wrapping: Tools > Global Options > Code (Editing tab) > Soft-wrap R
#    source files
#    **Best Practice**: 
#      Keep line length within 80 characters.
#      Show Margin Line: Tools > Global Options > Code > Display > Show margin

# 5. Personalize: Tools > Global Options > Appearance

## Getting started -------------------------------------------------------------

# 1. Get and set working directory ---------------------------------------------
#    Your working directory is the folder you are working in and where new files 
#    will be created and saved.
#    a. Use getwd() function to print the current working directory
# Code Here
getwd()

#    Use the "Files" tab to set the working directory with graphic UI. 
#    There is a small button with three dots (...) at the upper right corner of 
#    the "Files" tab. It's called "Go to directory". After clicking, a file 
#    selection window will pop out, allowing you to change directory using graphic 
#    user interface. After locating the new directory, click "More" in the "Files" 
#    tab, and then select "Set As Working Directory".

# 2. Load a data set and inspect -----------------------------------------------
#    a. Loading data using the "read.csv()" function
#    "read.csv()" imports csv (Comma Separated Values) files to R.
#    Here, I import the "marathon.csv" file to R and name it "marathon".
# Code Here

marathon <- read.csv(file = "marathon.csv")

#    b. Inspect data
#    For small data sets, you can use the "View()" function to pull the entire 
#    data table. This command is the same as clicking on the data name in the 
#    "Environment" tab.


#    For large data sets, use the "str()" function to get an overview.
# Code Here
# Structure
str(dat)

#    Inspect the first and last rows of the data set.
# Code Here
head(dat, n = 10)
tail(dat, n = 5)

# 3. Summary Statistics --------------------------------------------------------
#    Generate descriptive summary on the "Time" column.
#    a. Number of runners
# Code Here
N <- nrow(dat)

#    b. Mean (Average)
# Code Here
mean(dat$Time)

#    c. Variance
# Code Here
var(dat$Time)

#    d. Standard Deviation
# Code Here
sd(dat$Time)

#    e. Standard Error (SE)
# Code Here
sqrt( var(dat$Time) / N )

#    f. Median
# Code Here
median(dat$Time)

# Summary table
summary(dat)

# 4. A trick to avoid referencing "marathon$" every time: ----------------------
#    *Define variables*
time <- marathon$Time                     # define time as the "Time column
male <- marathon[marathon$Gender == "m", ]  # define male as male records
marathon$Time[marathon$Gender == "m"]

# Setting up for a T-test (or Simple Regression) -------------------------------

# For this part, we will run a simple linear regression of "Time" as a function 
# of "Gender". In other words, we want to know whether marathon times differ by 
# gender.

# 1. First, let's check the class of the variables that we will use.
class(marathon$Gender)
class(marathon$Time)

#    Note that gender is a factor variable with two levels, "f" and "m". 
#    You can also use levels to see the order:
levels(marathon$Gender)

#    "f" comes first, so essentially, f = 0 and m = 1 when we run the linear
#    regression.
#    Note: If you want to make this more explicit, we will change all of the 
#    f's to 0 and m's to 1. These are also called dummy variables.
#    (this is not necessary in order to run the model, but will be helpful for 
#    plotting):
marathon$Gender <- ifelse(marathon$Gender == "f", yes = 0, no = 1)
# ifelse(
#   test = marathon$Gender == "f",
#   yes = 0, # female
#   no = 1   # male
# )

# 2. Re-inspect the gender variable to confirm it is now numeric with 0s and 1s.
table(marathon$Gender)

# 3. Subset the data into two vectors: marathon times for females and marathon 
#    times for males.
female_times <- marathon$Time[marathon$Gender == 0]
male_times <- marathon$Time[marathon$Gender == 1]

# Levene's test ------------------------------------------------------------------
# Next, we run a Levene's test to examine if female and male groups have the same 
# variance.

# 1. Load the "car" package
install.packages("car")
library(car)

# 2. Levene's test hypotheses
#    Null hypothesis: H0: equal variance 
#    Alternative hypothesis: HA: non-equal variance
leveneTest(Time ~ factor(Gender), data = marathon)

#    Note: need to use "factor()" to coerce Gender back to a factor variable for
#    purposes of this test.

#    > p = 0.1589 
#    At a .05 significance level, we cannot reject the null that the variances 
#    are equal. So we can use equal variances for T test.

# 3. T test assuming equal variances
#    This tests: 
#    H0: mean female time = mean male time 
#    vs. 
#    HA: mean female time not equal to mean male time
t.test(Time ~ Gender, data = marathon, var.equal = TRUE)

#    Alternatively (does the same thing):
t.test(female_times, male_times, var.equal = TRUE)

#    If you wanted to run the T test not assuming equal variances:
t.test(Time ~ Gender, data = marathon, var.equal = FALSE)

# Running a simple linear regression -------------------------------------------

# There are a lot of arguments you can specify, but we just need the basics, so most of the default setting should be fine.

# First we specify the model. "Outcome/response" variable goes on the left,
# "explanatory" variables go on the right.
# We don't need to reference the data set and use $ because we've specified the
# data set with the argument "data = marathon".
lin_model <- lm(Time ~ Gender, data = marathon) # Assign the model to lin_model

# Check coefficients
lin_model$coefficients

# Using just "coef" as done in the lecture notes is simply a shortcut for typing
# out the whole word.

# Get an overview of the model
summary(lin_model)

# You'll be using summary() on models more as you progress and need to evaluate
# how well your model fits the data or when you might decide to go with one model
# over another.

# Visualize the model
plot(lin_model)

# You'll be able to use the "plot()" function on your model to check assumptions
# about that model, such as whether it meets assumptions of normality or
# homoscedasticity. Again, more on this later!

# Plotting your data and your model --------------------------------------------
# Usually we want to plot our data early on when doing descriptive analysis so 
# we will have a visual of what's going on in the data. 
# We'll do a quick scatter plot here though.
plot(marathon$Gender, marathon$Time)

# We can add better labels to the plot and a title.
plot(marathon$Gender, 
     marathon$Time, 
     xlab = "Gender", 
     ylab = "Time",
     main = "Marathon Finish Times between Male and Female Runners"
)
# We can also add our newly found regression line (in red).
abline(lin_model, col = "red")

# We can also add in the points (0, mean female time) and (1, mean male time)
# The regression line will run through these points
points(0, female_mean, col = "blue", pch = 19)
points(1, male_mean, col = "blue", pch = 19)

# END of Lab 1 -----------------------------------------------------------------