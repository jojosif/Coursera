best <- function(state, outcome) {
    ## Read outcome data
    ## Check if state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death rate
    
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
        
        # Check the validity of the state
        if(state %in% filterData[, "state"]) {       
            # Get the position of the valid row with the info about the given state 

            # Get valid data      
            validData <- filterData[, "state"] == state & filterData[, outcome] != "Not Available"              
            # Get the position of the lowest outcome rate
            # which.min returns the index of the first layer that has the min value for a cell
            minPos <- which.min(filterData[, outcome][validData])
            # Get hospital name
            name <- filterData[, "name"][validData][minPos]
        } else 
            stop("invalid state")       
    }
    else 
        stop("invalid outcome")
}
