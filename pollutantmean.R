pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  
  files_full<-list.files(directory, full.names = TRUE)
  data <- data.frame()
  for (i in 1:length(files_full)) {
    data <- rbind(data, read.csv(files_full[i]))
  }

  mean(data[which(data$ID %in% id),][[pollutant]], na.rm = TRUE)
}
