# plot_heart_attack_mortality.R
# ------------------------------------------------------------
# Programming Assignment 3 - Part 1
# Plotting 30-day Mortality Rates for Heart Attack
# Coursera "R Programming" (Hospital Quality)
# ------------------------------------------------------------

# Step 0: Set working directory to the folder containing the CSV files
# Example (for my machine):
# setwd("E:/coursera_r/assignment3/rprog_data_ProgAssignment3-data")
# Uncomment and change the path if needed

# Step 1: Read the outcome data
# We read all columns as character to prevent R from guessing column types
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# Step 2: Inspect the first few rows and dataset structure
head(outcome)          # View first 6 rows
nrow(outcome)          # Number of rows (hospitals)
ncol(outcome)          # Number of columns
names(outcome)         # Column names

# Step 3: Coerce the 30-day mortality rates for heart attack to numeric
# Column 11 corresponds to "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
# Warnings about NAs are expected because some hospitals have missing data
outcome[, 11] <- as.numeric(outcome[, 11])

# Step 4: Plot a histogram of the 30-day mortality rates for heart attack
# This gives a visual distribution of the 30-day death rates across hospitals
hist(outcome[, 11],
     main = "30-Day Death Rates for Heart Attack",
     xlab = "Death Rate",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")
