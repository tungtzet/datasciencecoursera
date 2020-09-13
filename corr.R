corr <- function(directory, threshold = 0) {
    corr_vect <- numeric()
    
    for(i in 1:322) {
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
        
        if(file_nobs > threshold) {
            sulfate <- file_data$sulfate
            nitrate <- file_data$nitrate
            sn_cor <- cor(sulfate, nitrate, use = "complete.obs")
            corr_vect <- c(corr_vect, sn_cor)
        }
    }
    
    corr_vect
}