# ----- ----- ----- -----
#
# Title: APSTA-GE 2003: Intermediate Quantitative Methods, Fall 2020
# Subtitle: Lab 1 Extras
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
# 
# Notes:
# This is an additional version of Lab 1 Notes. It contains extra code lines and 
# functions.
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

# 2. Create, open, or save an R script -----------------------------------------
#    a. Create
#    - File > New File > R Script
#    - New file icon at top left
#    - Ctrl/Command + Shift + N

#    b. Open
#    - File > Open File...
#    - Ctrl/Command + O

#    c. Save
#    - File > Save
#    - Ctrl/Command + S

# 3. Package Management --------------------------------------------------------
#    (About R packages: https://rstudio.com/products/rpackages/)

#    a. Install a package using the "install.packages()" function
#    Example: the "openintro" package
#    You will need internet access to download packages.
install.packages("openintro") # Make sure the package name is in quotes.

#    b. Load a package using the "library()" function
library(openintro)

# 4. Load a data set and inspect -----------------------------------------------
#    a. Loading data using the "read.csv()" function
#    "read.csv()" imports csv (Comma Separated Values) files to R.
#    Here, I import the "marathon.csv" file to R and name it "marathon".
marathon <- read.csv("data/marathon.csv")

#    Use "file.choose()" function to manually select the data set you want to 
#    import. This option is great if you're feeling lazy or desperate, 
#    but not so great for running R markdown files or anything you want someone
#    else to be able to run...
marathon <- read.csv(file.choose())

#    There are also some built in data sets in R
#    (some are also available via packages).

#    The marathon dataset above can also be accessed via the "openintro" package.
#    So, once you've run the code above to load the library, you can load the
#    dataset simply by using the "data()" function.
data(marathon)

#    To delete an object, use "rm()". 
#    *BE VERY CAREFUL WITH DELETING THINGS!*
rm(marathon)

#    b. Inspect data
#    For small data sets, you can use the "View()" function to pull the entire 
#    data table. This command is the same as clicking on the data name in the 
#    "Environment" tab.
View(marathon)

#    For large data sets, use the "str()" function to get an overview.
str(marathon)

#    Check the data type using the "class()" function. This is generally not
#    necessary to check every time you import a data set. 
#    It's just for future reference
class(marathon)

#    Inspect the first and last rows of the data set.
head(marathon)         # by default, shows first 6 rows
tail(marathon)         # by default, shows last 6 rows
head(marathon, n = 10) # change how many rows to show
tail(marathon, n = 2)  # shows last 2 rows


# 5. Inspect variables ---------------------------------------------------------
#    a. Referencing a column
#    Use $ (dollar sign) to reference a specific column.
marathon$Year # prints all values in the "Year" column

#    b. Use [] (square brackets) to slice an object (data frame, vector, ...)
marathon[, 2]   # prints all values in the second column ("Year")
marathon[1:6, ] # prints the first 6 rows
marathon[10, 3] # prints the gender of the 10th player

#    c. Using the "table()" function to count unique values
table(marathon$Gender) # counts the gender of players

#    d. Using the "range()" function to get the lowest and the highest values
range(marathon$Time)   # prints the shortest and longest finish times 

#    e. Quick summary stats of variables
summary(marathon$Time)
summary(marathon$Time, digits = 2) # round to 2 digits

# 6. Calculate individual stats ------------------------------------------------
#    a. Mean (Average)
mean(marathon$Time)

#    b. Variance
var(marathon$Time)

#    c. Standard Deviation
sd(marathon$Time)

#    d. Square Root
sqrt(var(marathon$Time)) # Note: sd = sqrt(var)

#    e. Median
median(marathon$Time)

#    f. Correlation and Co-variance
cor(marathon$Year, marathon$Time) 
cov(marathon$Year, marathon$Time)

# Question: How could you interpret the negative correlation/co-variance between year and marathon time?

# 7. A trick to avoid referencing "marathon$" every time: ----------------------
#    *Define variables*
time <- marathon$Time                     # define time as the "Time column
male <- marathon[marathon$Gender == "m", ]  # define male as male records

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

# 2. Re-inspect the gender variable to confirm it is now numeric with 0s and 1s.
class(marathon$Gender)
table(marathon$Gender)

# 3. Subset the data into two vectors: marathon times for females and marathon 
#    times for males.
female_times <- marathon$Time[marathon$Gender == 0]
male_times <- marathon$Time[marathon$Gender == 1]

# 4. Calculate N (count), mean, variance, and standard error (SE) for each group.

#    N
female_N <- length(female_times) 
male_N <- length(male_times)

female_N
male_N

#    Mean
female_mean <- mean(female_times) 
male_mean <- mean(male_times)

female_mean
male_mean

#    Variance
female_variance <- var(female_times) 
male_variance <- var(male_times)

female_variance
male_variance

#    Standard Error
female_SE <- sqrt(female_variance/female_N)
male_SE <- sqrt(male_variance/male_N)

female_SE
male_SE

# Levene's test ------------------------------------------------------------------
# Next, we run a Levene's test to examine if female and male groups have same 
# variance

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

# We can verify that this object is a list using is.list().
is.list(lin_model)

# We can see all the first level items in the list using ls()
ls(lin_model)

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

# END of Lab 1 Extra -----------------------------------------------------------