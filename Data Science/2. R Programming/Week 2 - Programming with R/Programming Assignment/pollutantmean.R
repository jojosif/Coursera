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
    ## NOTE: Do not round the results!
    
    accum <- c() # Create an empty vector
        
    for(i in id) {
        file_name <- sprintf("%03d.csv", i)
        file_name <- paste(directory, "/", file_name, sep = "")
                                
        dataset <- read.csv(file = file_name, head = TRUE, sep = ",")
        vec <- dataset[, pollutant] # Create a vector with the specified values
        val <- vec[!is.na(vec)] # Extract the values of the vector and filter missing values      
        accum <- c(accum, val) # Accumulate non-missing values
    }

    mean(accum)
}

## Testing code
# pollutantmean("~/Desktop/specdata", "sulfate", 1:10)
# pollutantmean("~/Desktop/specdata", "nitrate", 70:72)
# pollutantmean("~/Desktop/specdata", "nitrate", 23)
