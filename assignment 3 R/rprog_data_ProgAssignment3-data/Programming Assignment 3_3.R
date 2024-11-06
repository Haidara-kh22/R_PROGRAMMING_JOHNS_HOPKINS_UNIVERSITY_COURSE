rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = 'character')
  
  ## Check that outcome is valid
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  
  ## Determine the column index based on the outcome
  if (outcome == "heart attack") {
    column_index <- 11
  } else if (outcome == "heart failure") {
    column_index <- 17
  } else {
    column_index <- 23
  }
  
  ## Unique states
  unique_states <- unique(outcome_data$State)
  
  ## Initialize result list
  results <- list()
  
  for (state in unique_states) {
    ## Filter data for the current state
    state_data <- subset(outcome_data, State == state)
    
    ## Convert the relevant column to numeric and remove NAs
    state_data[, column_index] <- as.numeric(state_data[, column_index])
    state_data <- state_data[!is.na(state_data[, column_index]), ]
    
    ## Handle ranking based on num input
    if (num == "best") {
      rank_position <- 1
    } else if (num == "worst") {
      rank_position <- nrow(state_data)
    } else if (is.numeric(as.numeric(num))) {
      rank_position <- as.numeric(num)
      if (rank_position < 1 || rank_position > nrow(state_data)) {
        results[[state]] <- NA
        next
      }
    } else {
      stop("invalid num")
    }
    
    ## Order hospitals by the specified outcome and hospital name
    ordered_hospitals <- state_data[order(state_data[, column_index], state_data$Hospital.Name), ]
    
    ## Get the hospital name at the specified rank position
    hospital_name <- ordered_hospitals[rank_position, "Hospital.Name"]
    
    results[[state]] <- hospital_name
  }
  
  ## Create a data frame from the results list
  final_results <- data.frame(hospital = unlist(results), state = names(results), stringsAsFactors = FALSE)
  
  return(final_results)
}
head(rankall("heart attack", 20), 10)

