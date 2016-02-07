
#working directory
wd <- "~/R/Hopkins_Data_Science_Specialization/Programming_with_R/Air_Pollution/"


pollutantmean <- function(directory, pollutant, id = 1:332) {
    # directory is a string with the csv files location
    # pollutant is the string; "sulfate" or "nitrate"
    # id is the monitor id.
    # Return the mean of the pollutant across all monitors indicated by id.
    
    
    directory <- paste(directory,"/",sep = "")
    fileNames <- paste(wd, directory, list.files(paste(wd,directory,sep = "")), sep = "")
    
    
    monitorReadings <- c()
    for(i in id){
        x <- read.csv(fileNames[i])
        x <- x[,pollutant]
        monitorReadings <- c(monitorReadings, x[!is.na(x)])
    }
    
    #mean across all monitors given by id
    print(mean(monitorReadings))
    
}

