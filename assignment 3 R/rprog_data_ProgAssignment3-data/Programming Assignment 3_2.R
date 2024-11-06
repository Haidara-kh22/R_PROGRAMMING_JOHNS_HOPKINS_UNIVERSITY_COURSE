rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data

  outcome_data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  
  ## Check that state is valid
  if (!state %in% unique(outcome_data$State)) {
    stop("invalid input")
  }
  
  ## Check that outcome is valid
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  }
  
  ### Determine the column index for the specified outcome
  if (outcome == "heart attack") {
    outcome_col <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    outcome_col <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else { # pneumonia
    outcome_col <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  
  ## Filter data for the specified state
  state_data <- subset(outcome_data, State == state)
  
  ## Convert the relevant column to numeric and remove NAs
  state_data[, outcome_col] <- as.numeric(state_data[, outcome_col])
  
  ## Remove rows with NA values in the outcome column
  state_data <- state_data[!is.na(state_data[, outcome_col]), ]
  
  ## Check if there are any hospitals left after filtering
  if (nrow(state_data) == 0) {
    return(NA)
  }
  ## Order hospitals by mortality rate and name to handle ties
  ordered_hospitals <- state_data[order(state_data[, outcome_col], state_data$Hospital.Name), ]
  
  ## Determine the ranking based on num argument
  if (num == "best") {
    return(ordered_hospitals[1, "Hospital.Name"])
  } else if (num == "worst") {
    return(ordered_hospitals[nrow(ordered_hospitals), "Hospital.Name"])
  } else if (is.numeric(num) && num > 0 && num <= nrow(ordered_hospitals)) {
    return(ordered_hospitals[num, "Hospital.Name"])
  } else {
    return(NA) # If num is out of bounds
  }
}
rankhospital("MD", "heart attack", "worst")
