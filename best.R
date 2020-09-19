best <- function(state, outcome) {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## List of state abbrs and illness outcomes
    states <- unique(df[, 7])
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if (!state %in% states) {
        stop("invalid state")
    }
    if (!outcome %in% outcomes) {
        stop("invalid outcome")
    }
    
    ## Reassign outcome to column number
    column <- numeric()
    if (outcome == "heart attack") {
        column <- 11
    }
    if (outcome == "heart failure") {
        column <- 17
    }
    if (outcome == "pneumonia") {
        column <- 23
    }
    
    ## Extract dataframe w. corresponding state and only w. 3 outcomes
    subdf <- df[df$State == state, c(2,7,column)]
    subdf[, 3] <- sapply(subdf[, 3], as.numeric)
    
    ## Return hospital in that state w. lowest 30-day death rate
    bestrate <- min(subdf[, 3], na.rm = TRUE)
    besthospitals <- subdf[subdf[, 3] == bestrate & !is.na(subdf[, 3]),]
    besthospitals <- besthospitals[order(besthospitals$Hospital.Name, na.last = NA), ]
    besthospitals [1,1]
}