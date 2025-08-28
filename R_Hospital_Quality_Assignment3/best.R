# best.R
# ------------------------------------------------------------------
# Programming Assignment 3 - Part 1
# Coursera "R Programming" (Hospital Quality)
#
# Function: best()
# Purpose:
#   Given a U.S. state abbreviation and a health outcome
#   ("heart attack", "heart failure", "pneumonia"),
#   this function returns the name of the hospital in that
#   state with the lowest 30-day mortality rate for that outcome.
#
# Setup:
#   1. Download "ProgAssignment3-data.zip" from the assignment page.
#   2. Unzip it. Inside you will find "outcome-of-care-measures.csv".
#   3. Set your R working directory to the folder containing that file:
#        setwd("path/to/unzipped/data")
#
# Data source:
#   outcome-of-care-measures.csv contains hospital-level
#   30-day mortality rates for 3 conditions:
#     - Heart Attack
#     - Heart Failure
#     - Pneumonia
#
# Notes:
#   - Hospitals with missing data for the chosen outcome
#     are excluded from ranking.
#   - In case of ties, the hospital names are sorted alphabetically,
#     and the first one is returned.
#
# Errors:
#   - If the state argument is invalid: stop("invalid state")
#   - If the outcome argument is invalid: stop("invalid outcome")
#
# Example usage (uncomment to run):
# source("best.R")
# best("TX", "heart attack")  # Returns: "CYPRESS FAIRBANKS MEDICAL CENTER"
# best("MD", "pneumonia")     # Returns: "GREATER BALTIMORE MEDICAL CENTER"
# best("BB", "heart attack")  # Returns error: invalid state
# best("NY", "hert attack")   # Returns error: invalid outcome
# ------------------------------------------------------------------

best <- function(state, outcome) {
  
  ## Step 1: Read outcome data
  # Read CSV as character to avoid automatic type conversion issues
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Step 2: Map valid outcomes to their dataset columns
  # This allows us to reference the correct column based on the outcome string
  outcome_columns <- list(
    "heart attack"  = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia"     = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  
  ## Step 3: Validate inputs
  # Check if state exists in the dataset
  if (!state %in% data$State) stop("invalid state")
  # Check if outcome is valid
  if (!outcome %in% names(outcome_columns)) stop("invalid outcome")
  
  ## Step 4: Select correct column for chosen outcome
  outcome_col <- outcome_columns[[outcome]]
  
  ## Step 5: Filter data for hospitals in this state
  # Keep only Hospital.Name and the selected outcome column
  state_data <- data[data$State == state, c("Hospital.Name", outcome_col)]
  
  ## Step 6: Convert outcome values to numeric
  # suppressWarnings prevents warnings about NAs introduced by coercion
  state_data[[outcome_col]] <- suppressWarnings(as.numeric(state_data[[outcome_col]]))
  
  ## Step 7: Remove hospitals with missing outcome data
  state_data <- na.omit(state_data)
  
  ## Step 8: Find the minimum (best) mortality rate in this state
  # This is the key step for "best" hospital determination
  min_rate <- min(state_data[[outcome_col]])
  
  ## Step 9: Get all hospitals with this minimum rate
  # There may be ties, i.e., multiple hospitals with the same lowest rate
  best_hospitals <- state_data[state_data[[outcome_col]] == min_rate, ]
  
  ## Step 10: If there are ties, order alphabetically and pick first
  best_hospitals <- best_hospitals[order(best_hospitals$Hospital.Name), ]
  
  ## Step 11: Return hospital name
  return(best_hospitals$Hospital.Name[1])
}
