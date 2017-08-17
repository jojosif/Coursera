corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations

    corRel <- c() # Create an empty vector

    id <- 1:332
    for(i in id)
    {
        file_name <- sprintf("%03d.csv", i)
        file_name <- paste(directory, "/", file_name, sep = "")
        
        dataset <- read.csv(file = file_name, header = TRUE, sep = ",")
        valSul <- !is.na(dataset[, "sulfate"])  # Extract the values in the sulfate column
        valNut <- !is.na(dataset[, "nitrate"])  # Extract the values in the nitrade column
        completeVal <- valSul & valNut # Get non-missing values satisfying both sulfate & nitrate
            
        if(sum(completeVal) >= threshold)
        {
            sulfate <- dataset[, "sulfate"][completeVal]
            nitrade <- dataset[, "nitrate"][completeVal]
            corRel <- c(corRel, cor(sulfate, nitrade))
        }
    }
    
    corRel
}

## Test
# cr <- corr("~/Desktop/specdata", 150)
# head(cr)
# summary(cr)
# 
# cr <- corr("~/Desktop/specdata", 400)
# head(cr)
# summary(cr)
# 
# cr <- corr("~/Desktop/specdata", 5000)
# summary(cr)
# length(cr)
# 
# cr <- corr("~/Desktop/specdata")
# summary(cr)
# length(cr)
