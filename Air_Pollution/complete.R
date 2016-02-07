
#working directory
wd <- "~/R/Hopkins_Data_Science_Specialization/Programming_with_R/Air_Pollution/"

complete <- function(directory, id = 1:332){
    # directory is a string with the csv files location
    # id is the monitor id.
    # Return the id and number of complete cases
    
    
    directory <- paste(directory,"/",sep = "")
    fileNames <- paste(wd, directory, list.files(paste(wd,directory,sep = "")), sep = "")
    
    myDF <- data.frame()
    for(i in id){
        x <- read.csv((fileNames[i]))
        x <- x[complete.cases(x),]
        myDF <- rbind(myDF, c(x[1,"ID"], dim(x)[1])) 
    }
    `names<-`(myDF, c("id","nobs"))
}
