
#working directory
wd <- "~/R/Hopkins_Data_Science_Specialization/Programming_with_R/Air_Pollution/"


corr <- function(directory, threshold = 0){
    # directory is a string, location of files
    # threshold is the number of completely observed observations
    # in a file required for us to compute the correlation
    
    myDF <- complete(directory)
    myDF <- myDF[myDF$nobs>threshold,]
    id <- myDF$id
    
    directory <- paste(directory,"/",sep = "")
    fileNames <- paste(wd, directory, list.files(paste(wd,directory,sep = "")), sep = "")
    
    correlations <- vector(mode = "numeric")
    for(i in id){
        x <- read.csv((fileNames[i]))
        x <- x[complete.cases(x),]
        correlations <- c(correlations, cor(x$sulfate, x$nitrate))
    }
    return(correlations)
}

source("complete.R")
cr <- corr("specdata", 150)
print(head(cr))
print(summary(cr))

cr <- corr("specdata", 400)
print(head(cr))
print(summary(cr))

cr <- corr("specdata", 5000)
print(summary(cr))
print(length(cr))

cr <- corr("specdata")
print(summary(cr))
print(length(cr))