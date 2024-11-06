#### PART_1
pollutantmean<-function(directory, pollutant, id=1:332){
  values<- c()
  for (monitor in id){
    #location of the file
    file_loc<-paste0(directory, '/', sprintf("%03d", monitor), ".csv")
    
    #read the lines
    data_of_file<- read.csv(file_loc)
    
    #extract the data
    pollutant_data<-data_of_file[[pollutant]]
    
    #removing NAs
    values<- c(values, pollutant_data[!is.na(pollutant_data) ])
  
  }
  #calculating means
  mean(values)  
}

x<-pollutantmean("D:/specdata", 1:10)

##### PART_2


complete <- function(directory, id = 1:332) {
  # Initialize a data frame to store results
  results <- data.frame(file = character(), nobs = integer(), stringsAsFactors = FALSE)
  
  for (monitor in id) {
    # Construct the file path
    file_loc <- paste0(directory, '/', sprintf("%03d", monitor), ".csv")
    
    # Read the CSV file
    if (file.exists(file_loc)) {  # Check if the file exists
      data_of_file <- read.csv(file_loc)
      
      # Count complete cases
      nobse <- sum(complete.cases(data_of_file))
      
      # Append results to the data frame
      results <- rbind(results, data.frame(file = basename(file_loc), nobs = nobse))
    } else {
      warning(paste("File does not exist:", file_loc))
    }
  }
  
# Return the results data frame
  return(results)  
}

y<-complete("D:/specdata", 10:12)


#### PART_3
corr<-function (directory, threshold=0){
  values<- c()
  #location of the file
  files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  
  for (file in files){ 
    #read the lines
    data_of_file<- read.csv(file)
    
    nobse <- sum(complete.cases(data_of_file))
    #checking if the it is NA or not and then checking of the correlation is above the threshold
    
    if ( nobse>threshold ){
      corelation<-cor(data_of_file$sulfate[complete.cases(data_of_file)],data_of_file$nitrate[complete.cases(data_of_file)])
      
      values<- c(values, corelation)
    }
  }
  return(values)
  
}
results<- corr("D:/specdata", 100)
