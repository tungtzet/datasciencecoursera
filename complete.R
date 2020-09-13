complete <- function(directory, id = 1:322) {
    ## create an empty data frame called df to store results
    df <- data.frame(id=integer(0), nobs=integer(0))
    for(i in id) {
        file_dir <- ""
        ## determine the directory name of the csv file
        if(i < 10) {
            file_dir <- paste(directory, '/00', i, '.csv', sep = '')
        } else if(i < 100) {
            file_dir <- paste(directory, '/0', i, '.csv', sep = '')
        } else {
            file_dir <- paste(directory, '/', i, '.csv', sep = '')
        }
        ## read the csv, calculate complete cases, create a row added to df
        file_data <- read.csv(file_dir)
        file_nobs <- sum(complete.cases(file_data))
        new_row <- data.frame(id=i, nobs=file_nobs)
        df <- rbind(df, new_row)
    }
    ## return df with result rows
    df
}