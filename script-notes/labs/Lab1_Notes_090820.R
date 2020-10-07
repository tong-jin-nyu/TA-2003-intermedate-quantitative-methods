# ----- ----- ----- -----
#
# Title: APSTA-GE 2003: Intermediate Quantitative Methods, Fall 2020
# Subtitle: Lab 1 Notes
# 
# Data Created: 09/08/20
# Data Modified: 10/06/20
#
# ----- ----- ----- -----
#
# Author: Tong Jin
# Email: tj1061@nyu.edu
# Affiliation: New York University
# Copyright (c) Tong Jin, 2020
#
# ----- ----- ----- -----

# Introduction to R ------------------------------------------------------------

## Coding in RStudio -----------------------------------------------------------

### Using "console" pane -------------------------------------------------------

# To code in console, just type in the code you want R to evaluate and then hit 
# the "Enter/Return" key.

### Using "source" pane --------------------------------------------------------

# The source pane allows you to write a script. To start, create a script file by 
# selecting "File - New File - R Script". 

# After creating a blank script file, we can start typing in our codes. 
# There are two types of line in a script:
# 1. Code
# 2. Comment
# A code line is an executable sentence that can be evaluated by R.
# A comment begins with a number sign (#). Till now, all lines in this script are 
# comments.

# Here is an example of code line: 
print("hello, world")
# Here is an example of comment:
# Print "hello, world"

# Other things to keep in mide:
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

# 5. Personalization: Tools > Global Options > Appearance

## Getting started -------------------------------------------------------------

# 1. Get and set working directory ---------------------------------------------
#    Your working directory is the folder you are working in and where new files 
#    will be created and saved.
#    a. Use getwd() function to print the current working directory
# Set working directory to the root file
getwd()
setwd()

#    Use the "Files" tab to set the working directory with graphic UI. 
#    There is a small button with three dots (...) at the upper right corner of 
#    the "Files" tab. It's called "Go to directory". After clicking, a file 
#    selection window will pop out, allowing you to change directory using graphic 
#    user interface. After locating the new directory, click "More" in the "Files" 
#    tab, and then select "Set As Working Directory".

# 2. Load a data set and inspect -----------------------------------------------
#    a. Loading data using the "read.csv()" function ---------------------------
#    "read.csv()" imports csv (Comma Separated Values) files to R.
#    Here, we import the "marathon.csv" file to R.
# Import the data set
read.csv("marathon.csv")

#    We can also assign a name to the above command so that we can refer the data
#    set later by using the name instead of the whole sentence.
# Import the data set and define it as "dat"
dat <- read.csv("data/marathon.csv")

#    b. Inspect data -----------------------------------------------------------
#    For small data sets, you can use the "View()" function to pull the entire 
#    data table. This command is the same as clicking on the data name in the 
#    "Environment" tab.
View(dat)

#    For large data sets, use the "str()" function to get an overview.
str(dat)

#    Inspect the first and last rows of the data set.
head(dat)
tail(dat)

# 3. Summary Statistics --------------------------------------------------------
#    Generate descriptive summary on the "Time" column.
#    a. Number of runners
nrow(dat)

#    b. Mean (Average)
mean(dat$Time)

#    c. Variance
var(dat$Time)

#    d. Standard Deviation
sd(dat$Time)

#    e. Standard Error (SE)
sqrt( var(dat$Time) / N )

#    f. Median
median(dat$Time)

#    Summary table
summary(dat)

# 4. A trick to avoid referencing "dat$" every time: ----------------------
#    *Define variables*
time <- dat$Time                     # define time as the "Time column
male <- dat[dat$Gender == "m", ]  # define male as male records
dat$Time[dat$Gender == "m"]

# Setting up for a T-test (or Simple Regression) -------------------------------

# For this part, we will run a simple linear regression of "Time" as a function 
# of "Gender". In other words, we want to know whether marathon times differ by 
# gender.

# 1. First, let's check the class of the variables that we will use.
class(dat$Gender)
class(dat$Time)

#    Note that gender is a factor variable with two levels, "f" and "m". 
#    You can also use levels to see the order:
levels(dat$Gender)

#    "f" comes first, so essentially, f = 0 and m = 1 when we run the linear
#    regression.
#    Note: If you want to make this more explicit, we will change all of the 
#    f's to 0 and m's to 1. These are also called dummy variables.
#    (this is not necessary in order to run the model, but will be helpful for 
#    plotting):
dat$Gender <- ifelse(dat$Gender == "f", yes = 0, no = 1)
# ifelse(
#   test = marathon$Gender == "f",
#   yes = 0, # female
#   no = 1   # male
# )

# 2. Re-inspect the gender variable to confirm it is now numeric with 0s and 1s.
table(dat$Gender)

# 3. Subset the data into two vectors: marathon times for females and marathon 
#    times for males.
female_times <- dat$Time[dat$Gender == 0]
male_times <- dat$Time[dat$Gender == 1]

# Levene's test ------------------------------------------------------------------
# Next, we run a Levene's test to examine if female and male groups have the same 
# variance.

# 1. Load the "car" package
install.packages("car")
library(car)

# 2. Levene's test hypotheses
#    Null hypothesis: H0: equal variance 
#    Alternative hypothesis: HA: non-equal variance
leveneTest(Time ~ factor(Gender), data = dat)

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
t.test(Time ~ Gender, data = dat, var.equal = TRUE)

#    Alternatively (does the same thing):
t.test(female_times, male_times, var.equal = TRUE)

#    If you wanted to run the T test not assuming equal variances:
t.test(Time ~ Gender, data = dat, var.equal = FALSE)

# Running a simple linear regression -------------------------------------------

# There are a lot of arguments you can specify, but we just need the basics, so most of the default setting should be fine.

# First we specify the model. "Outcome/response" variable goes on the left,
# "explanatory" variables go on the right.
# We don't need to reference the data set and use $ because we've specified the
# data set with the argument "data = marathon".
lin_model <- lm(Time ~ Gender, data = dat) # Assign the model to lin_model

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
plot(dat$Gender, dat$Time)

# We can add better labels to the plot and a title.
plot(dat$Gender, 
     dat$Time, 
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