rankall <- function(outcome, num = "best") {       
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    # Check the validity of the outcome
    if(outcome %in% valid_outcomes) {
        file <- read.csv(file = "outcome-of-care-measures.csv", colClasses = "character")

        # 2  -> Hospital Name
        # 7  -> State Abbreviation
        # 11 -> outcome = Heart Attack
        # 17 -> outcome = Heart Failure
        # 23 -> outcome = Pneumonia
        filterData <- file[c(2, 7, 11, 17, 23)]

        # Set readable column names
        names(filterData) <- c("name", "state", "heart attack", "heart failure", "pneumonia")

        # Get the position of the valid row with the info about the given state 
        dFrameFilter <- filterData[filterData[outcome] != "Not Available", ]
        
        # Get correct order first by rate and ties should be 
        # broken by using the hospital name if appliable
        index <- order(dFrameFilter$name) # Order by name
        dFrameFilter <- dFrameFilter[index, ]
        index <- order(as.numeric(dFrameFilter[, outcome])) # Order by outcome
        orderData <- dFrameFilter[index, ]                  # The dataFrame ordered by outcome and name
        allStates <- unique(dFrameFilter[, "state"])        # The vector with all unique states
        allStates <- allStates[order(allStates)]            # Order the vector of unique states
        
        output <- data.frame()
        for(state in allStates) { # For all state in the unique list of states
            getStateData <- orderData[orderData$state == state, c(1, 2)] # Get state data (name, state)
            if(is.numeric(num) & nrow(getStateData) <= num) {
                name <- NA
            }
            else {       
                    if (is.numeric(num))
                        name <- getStateData[, "name"][num]
                    else if (num == "best")   
                        name <- getStateData[, "name"][1]
                    else                     
                        name <- tail(getStateData, n = 1)[,"name"] # Worst
            }
            newRow <- data.frame(name, state) # Create a new row
            names(newRow) <- names(output)
            output <- rbind(output, newRow)
        }
                
        names(output) <- c("hospital", "state")
        output
    }
    else 
        stop("invalid outcome")   
}
