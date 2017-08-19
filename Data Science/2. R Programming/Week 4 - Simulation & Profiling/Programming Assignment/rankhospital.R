rankhospital <- function(state, outcome, num = "best"){        
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank 30-day death rate
    
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
            dFrameFilter <- filterData[filterData$state == state & filterData[outcome] != "Not Available", ]
            
            if (is.numeric(num) & nrow(dFrameFilter) <= num) {
                NA
            }
            else {      
                # Get correct order first by rate and ties should be 
                # broken by using the hospital name if appliable
                index <- order(dFrameFilter$name) # Order by name
                dFrameFilter <- dFrameFilter[index, ]
                index <- order(as.numeric(dFrameFilter[, outcome])) # Order by outcome
                
                if (is.numeric(num)) 
                    dFrameFilter[index, ][, "name"][num]
                else if (num == "best") 
                    dFrameFilter[index, ][, "name"][1]
                else 
                    tail(dFrameFilter[index, ], n = 1)[, "name"] # Worst
            }
        }
        else 
            stop("invalid state")   
    }
    else 
        stop("invalid outcome")   
}
