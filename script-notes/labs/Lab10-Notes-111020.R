# ----- ----- ----- -----
#
# Title: APSTA-GE 2003: Intermediate Quantitative Methods
# Subtitle: Lab Notes - Week 10
# 
# Data Created: 
# Data Modified: 11/10/2020
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
# Predict NYC Property Sale Price using Multiple Linear Regression Modeling
# 
# ----- ----- ----- -----

# Dependencies ----
library(corrplot)
library(stringr)
library(corrplot)

# Load the dataset ----
na_names <- c(" -  ", NA)
dat <- read.csv("nyc-rolling-sales.csv", na.strings = na_names)

# Data Processing
colnames(dat) <- str_to_lower(colnames(dat))
colnames(dat) <- str_replace_all(colnames(dat), 
                                 pattern = "[.]", replacement = "_")

# Data Cleaning

col_name_to_drop <- c("x", "ease_ment", "neighborhood", 
                      "building_class_at_present", "tax_class_at_present", 
                      "address", "apartment_number", "tax_class_at_time_of_sale")
col_index_to_drop <- which(colnames(dat) %in% col_name_to_drop)
dat <- dat[, -col_index_to_drop]
dim(dat)

col_name_w_NA <- c("sale_price", "land_square_feet", "gross_square_feet")
col_index_w_NA <- which(colnames(dat) %in% col_name_w_NA)
row_w_NA <- which(is.na(dat[, col_index_w_NA[1]]))
dat <- dat[-row_w_NA, ]
row_w_NA <- which(is.na(dat[, col_index_w_NA[2]]))
dat <- dat[-row_w_NA, ]
row_w_NA <- which(is.na(dat[, col_index_w_NA[3]]))
dat <- dat[-row_w_NA, ]
dim(dat)

# Visualize variable distributions ----


# Store a copy of the original dataset by duplicating
dat1 <- dat

# Correlation matrix ----
dat_cor <- dat[, -c(2, 12, 14)]
corrplot(cor(dat_cor), type = "lower")

# Start by fiting a simple linear regression model
# DV: sale_price
mod_simple <- lm(sale_price ~ gross_square_feet, data = dat)
summary(mod_simple)

# Increase model performance by adding more predictors 

mod_add <- lm(sale_price ~ borough + block + land_square_feet + gross_square_feet, data = dat_cor)
summary(mod_add)
# sale_price = 3.03e6 - 2.27e5 * borough - 71.9 * block - 142.8 * land_square_feet + 362.8 * gross_square_feet + error

new_dat <- data.frame(
  borough = 1,
  block = 300,
  land_square_feet = 2000,
  gross_square_feet = 2200
)
predict(mod_add, newdata = new_dat, interval = "confidence")

mod_inter <- lm(sale_price ~ borough + block * land_square_feet * gross_square_feet, data = dat_cor)
summary(mod_inter)

predict(mod_inter, newdata = new_dat, interval = "confidence")

# Increase model performance by adding interaction relationship



