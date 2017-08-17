complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
        
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
        
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    output <- data.frame()
    
    for(i in id) {
        file_name <- sprintf("%03d.csv", i)
        file_name <- paste0(directory, "/", file_name)
                
        dataset <- read.csv(file = file_name, header = TRUE, sep = ",")
        valSul <- !is.na(dataset[, "sulfate"])  # Extract the values in the sulfate column
        valNut <- !is.na(dataset[, "nitrate"])  # Extract the values in the nitrade column
        nobs <- sum(valSul & valNut)
        newRow <- data.frame(i, nobs)
        output <- rbind(output, newRow)
    }

    names(output) <- c("id", "nobs")
}

## Test
complete("~/Desktop/specdata", 1)
complete("~/Desktop/specdata", c(2, 4, 8, 10, 12))
complete("~/Desktop/specdata", 30:25)
complete("~/Desktop/specdata", 3)
