# ----- ----- ----- -----
#
# Title: APSTA-GE 2003: Intermediate Quantitative Methods
# Subtitle: Lab Notes - Week 11
# 
# Data Created: 
# Data Modified: 11/17/2020
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
# Model Selection using the NYC Property Sale Data
# 
# ----- ----- ----- -----

# Descriptions: In this lab, we discussed several model selection models, including:
#
# - R-squared
# - Adjusted R-squared
# - Likelihood Ratio Test
# - Log Likelihood
# - AIC
# - BIC
# 
# In this notes, we are going to select the model that has the best simulation 
# performance. The method we will use is AIC. We will use the "stepAIC" function 
# in the `MASS` package to do this. 

# Dependencies ----
library(corrplot)
library(stringr)
library(MASS)

# Load the dataset ----
na_names <- c(" -  ", NA)
dat <- read.csv("nyc-rolling-sales.csv", na.strings = na_names)

# Data Processing ----
n_col <- ncol(dat)
# Fix column names
colnames(dat) <- str_to_lower(colnames(dat))
colnames(dat) <- str_replace_all(colnames(dat), pattern = "[.]", replacement = "_")
# Drop unrelated columns
col_name_to_drop <- c("x", "ease_ment", "neighborhood", 
                      "building_class_at_present", "tax_class_at_present", 
                      "address", "apartment_number", "tax_class_at_time_of_sale")
col_index_to_drop <- which(colnames(dat) %in% col_name_to_drop)
dat <- dat[, -col_index_to_drop]
# Drop rows with missing values
col_name_w_NA <- c("sale_price", "land_square_feet", "gross_square_feet")
col_index_w_NA <- which(colnames(dat) %in% col_name_w_NA)
row_w_NA <- which(is.na(dat[, col_index_w_NA[1]]))
dat <- dat[-row_w_NA, ]
row_w_NA <- which(is.na(dat[, col_index_w_NA[2]]))
dat <- dat[-row_w_NA, ]
row_w_NA <- which(is.na(dat[, col_index_w_NA[3]]))
dat <- dat[-row_w_NA, ]
dat$sale_date <- as.Date(dat$sale_date)

# Remove extreme values ----
summary(dat$sale_price)
row_index_to_drop <- which(dat$sale_price > 3000000)
dat <- dat[-row_index_to_drop, ]

# Store a copy of the original dataset by duplicating
dat1 <- dat

# Check correlation ----
index_categorical_columns <- c(2, 12, 14)
cor_dat <- cor(dat[, -index_categorical_columns])
corrplot::corrplot(cor_dat, type = "lower") 

# Remove highly-correlated IVs and building classes ----
col_name_to_drop <- c("residential_units", "commercial_units", "gross_square_feet", 
                      "building_class_category", "building_class_at_time_of_sale")
col_index_to_drop <- which(colnames(dat) %in% col_name_to_drop)
dat <- dat[, -col_index_to_drop]

# Start by fiting a baseline model ----
# DV: sale_price
mod_BL <- lm(sale_price ~ 1, data = dat)
summary(mod_BL)

# Then, fit a full model (assume fully interactive) ----
mod_full <- lm(sale_price ~ borough * block * lot * zip_code * total_units * 
                 land_square_feet * year_built * sale_date, 
               data = dat
)

# Perform a step-wise AIC going forward ----
# First, check the help doc for "stepAIC"
?stepAIC
# There are three major arguments in this function:
# 1. object: a model
# 2. scope: the range of step-wise comparison
# 3. direction: the direction of comparison
# Basically, this function starts by calculating the AIC of the object (the model 
# you select). Then, for each predictor (independent variable) in the model, the
# function calculates a model AIC without that predictor. The function then gets 
# the AIC of that specific predictor by subtracting the new model AIC from the 
# original model AIC. After that, the function removes the predictor that has the 
# lowest AIC value. This means that the predictor contributes the least to model 
# performance and, therefore, removing it will not lose much. 

# The function then repeats the above process by updating the object to the new 
# model. 
stepAIC(mod_BL, score = list(lower = mod_BL, upper = mod_full), direction = "forward")