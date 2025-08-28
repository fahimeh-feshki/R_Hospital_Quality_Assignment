# rankhospital.R
# ------------------------------------------------------------------
# Programming Assignment 3 - Part 3
# Coursera "R Programming" (Hospital Quality)
#
# Function: rankhospital()
# Purpose:
#   Given a U.S. state abbreviation, a health outcome
#   ("heart attack", "heart failure", "pneumonia"), and a rank (num),
#   this function returns the name of the hospital in that
#   state with the specified ranking for 30-day mortality rate.
#
#   num can be:
#     - "best"   -> hospital with lowest mortality rate
#     - "worst"  -> hospital with highest mortality rate
#     - integer  -> hospital with that rank (smaller numbers are better)
#
# Notes:
#   - Hospitals with missing data for the chosen outcome are excluded.
#   - Ties in mortality rates are broken alphabetically by hospital name.
#
# Errors:
#   - If the state argument is invalid: stop("invalid state")
#   - If the outcome argument is invalid: stop("invalid outcome")
#   - If the rank argument is invalid: stop("invalid rank")
#
# Example usage (uncomment to run):
# source("rankhospital.R")
# rankhospital("TX", "heart failure", 4)   # Returns: "DETAR HOSPITAL NAVARRO"
# rankhospital("MD", "heart attack", "worst")  # Returns: "HARFORD MEMORIAL HOSPITAL"
# rankhospital("MN", "heart attack", 5000)    # Returns: NA
# ------------------------------------------------------------------

rankhospital <- function(state, outcome, num = "best") {
  
  ## Step 1: Read outcome data
  # Read CSV as character to avoid automatic type conversion issues
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Step 2: Map valid outcomes to their dataset columns
  outcome_columns <- list(
    "heart attack"  = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia"     = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  
  ## Step 3: Validate inputs
  if (!state %in% data$State) stop("invalid state")
  if (!outcome %in% names(outcome_columns)) stop("invalid outcome")
  
  ## Step 4: Select correct column for chosen outcome
  outcome_col <- outcome_columns[[outcome]]
  
  ## Step 5: Filter data for hospitals in this state
  # Keep only Hospital.Name and the outcome column
  state_data <- data[data$State == state, c("Hospital.Name", outcome_col)]
  
  ## Step 6: Convert outcome values to numeric
  # suppressWarnings prevents warnings about NAs introduced by coercion
  state_data[[outcome_col]] <- suppressWarnings(as.numeric(state_data[[outcome_col]]))
  
  ## Step 7: Remove hospitals with missing outcome data
  state_data <- na.omit(state_data)
  
  ## Step 8: Determine the rank to use
  # - If num is "best", pick the hospital with lowest mortality (rank_index = 1)
  # - If num is "worst", pick the hospital with highest mortality (rank_index = total hospitals)
  # - If num is numeric:
  #     * it indicates the desired rank (1 = best, 2 = second best, etc.)
  #     * if num > number of hospitals in state, return NA
  # - Any other value of num is invalid and will throw an error
  if (num == "best") {
    rank_index <- 1
  } else if (num == "worst") {
    rank_index <- nrow(state_data)
  } else if (is.numeric(num)) {
    if (num > nrow(state_data)) return(NA)
    rank_index <- num
  } else {
    stop("invalid rank")
  }
  
  ## Step 9: Order by outcome rate, then by hospital name to break ties
  # This ensures that hospitals with the same mortality rate are ranked alphabetically
  state_data <- state_data[order(state_data[[outcome_col]], state_data$Hospital.Name), ]
  
  ## Step 10: Return hospital name at the desired rank
  return(state_data$Hospital.Name[rank_index])
}
