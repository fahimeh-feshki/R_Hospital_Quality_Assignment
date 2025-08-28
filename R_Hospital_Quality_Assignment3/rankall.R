# rankall.R
# ------------------------------------------------------------------
# Programming Assignment 3 - Part 4
# Coursera "R Programming" (Hospital Quality)
#
# Function: rankall()
# Purpose:
#   Given a health outcome ("heart attack", "heart failure", "pneumonia")
#   and a rank (num), this function returns a data frame with
#   the hospital of that rank in **every state**.
#
#   num can be:
#     - "best"   -> hospital with lowest mortality rate in each state
#     - "worst"  -> hospital with highest mortality rate in each state
#     - integer  -> hospital with that rank in each state
#
# Notes:
#   - Hospitals with missing data for the chosen outcome are excluded.
#   - Ties in mortality rates are broken alphabetically by hospital name.
#
# Errors:
#   - If the outcome argument is invalid: stop("invalid outcome")
#
# Example usage (uncomment to run):
# source("rankall.R")
# head(rankall("heart attack", 20), 10)
# tail(rankall("pneumonia", "worst"), 3)
# ------------------------------------------------------------------

rankall <- function(outcome, num = "best") {
  
  ## Step 1: Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Step 2: Map valid outcomes to their dataset columns
  outcome_columns <- list(
    "heart attack"  = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia"     = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  
  ## Step 3: Validate outcome input
  if (!outcome %in% names(outcome_columns)) stop("invalid outcome")
  outcome_col <- outcome_columns[[outcome]]
  
  ## Step 4: Split data by state
  states <- sort(unique(data$State))
  result <- data.frame(hospital = character(), state = character(), stringsAsFactors = FALSE)
  
  ## Step 5: Loop through each state to determine the hospital at the given rank
  for (st in states) {
    
    # Filter hospitals for this state and select required columns
    state_data <- data[data$State == st, c("Hospital.Name", outcome_col)]
    
    # Convert outcome values to numeric
    state_data[[outcome_col]] <- suppressWarnings(as.numeric(state_data[[outcome_col]]))
    
    # Remove hospitals with missing outcome data
    state_data <- na.omit(state_data)
    
    # Skip if no hospital data is available
    if (nrow(state_data) == 0) {
      result <- rbind(result, data.frame(hospital = NA, state = st))
      next
    }
    
    # Order by outcome rate, then hospital name to break ties
    state_data <- state_data[order(state_data[[outcome_col]], state_data$Hospital.Name), ]
    
    # Determine rank index
    rank_index <- if (num == "best") {
      1
    } else if (num == "worst") {
      nrow(state_data)
    } else if (is.numeric(num)) {
      if (num > nrow(state_data)) NA else num
    } else {
      stop("invalid rank")
    }
    
    # Get hospital name at rank_index
    hospital_name <- if (is.na(rank_index)) NA else state_data$Hospital.Name[rank_index]
    
    # Append to result data frame
    result <- rbind(result, data.frame(hospital = hospital_name, state = st, stringsAsFactors = FALSE))
  }
  
  ## Step 6: Return the final data frame
  return(result)
}
