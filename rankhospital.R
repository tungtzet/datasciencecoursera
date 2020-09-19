rankhospital <- function(state, outcome, num = "best") {
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
    
    ## Extract dataframe w. corresponding state and outcome
    ## then convert the outcome colunm to numeric and remove NA obs
    subdf <- df[df$State == state, c(2,7,column)]
    subdf[, 3] <- sapply(subdf[, 3], as.numeric)
    subdf <- subdf[complete.cases(subdf), ]
    
    ## Sort this extract df by rate and name
    subdf <- subdf[order(subdf[, 3], subdf[, 1]), ]
    
    ## Return hospital name by specified ranking
    if (num == "best") {
        return(subdf[1,1])
    } else if (num == "worst") {
        return(tail(subdf, 1)[1, 1])
    } else if (num > nrow(subdf)) {
        return(NA)
    } else {
        return(subdf[num,1])
    }
}