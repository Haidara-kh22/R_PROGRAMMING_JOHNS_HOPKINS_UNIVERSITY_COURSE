outcome<- read.csv("outcome-of-care-measures.csv", colClasses = "character"
head(outcome)
outcome[,11]<-as.numeric(outcome[,11])
hist(outcome[,11])
processed<-list()

# PART_2
best <- function(state, outcome) {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  
  ## Check that state is valid
  if (!state %in% unique(outcome_data$State)) {
    stop("invalid state")
  }
  
  ## Check that outcome is valid
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  }
  
  ## Determine the column index for the specified outcome
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
  
  ## Find the minimum mortality rate and corresponding hospitals
  min_rate <- min(state_data[, outcome_col])
  
  ## Get hospitals with the minimum rate
  best_hospitals <- state_data[state_data[, outcome_col] == min_rate, "Hospital.Name"]
  
  ## Sort hospital names alphabetically and return the first one
  return(sort(best_hospitals)[1])
}
#test
best("MD", "pneumonia")
