select_data <- function(directory, id) {
    join_data <- data.frame()
    
    for(i in id) {
        if(i < 10) {
            file_dir <- paste(directory, '/00', i, '.csv', sep = '')
            join_data <- rbind(join_data, read.csv(file_dir))
        } else if(i < 100) {
            file_dir <- paste(directory, '/0', i, '.csv', sep = '')
            join_data <- rbind(join_data, read.csv(file_dir))
        } else {
            file_dir <- paste(directory, '/', i, '.csv', sep = '')
            join_data <- rbind(join_data, read.csv(file_dir))
        }
    }
    
    join_data
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
    join_data <- select_data(directory, id)
    mean(join_data[[pollutant]], na.rm = TRUE)
}

