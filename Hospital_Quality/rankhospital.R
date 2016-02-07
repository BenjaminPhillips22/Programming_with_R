

rankhospital <- function(state, outcome, num = "best") {
    
    #set working directory
    setwd("~/R/Hopkins_Data_Science_Specialization/Programming_with_R/Hospital_Quality")
    ## Read outcome data
    df <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if(!state %in% df$State){
        stop("invalid state")
    }
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop("invalid outcome")
    }
    # only look at data for 'outcome'
    if(outcome=="heart attack"){
        vars <- df[,c(2,7,11)] 
    }else if(outcome=="heart failure"){
        vars <- df[,c(2,7,17)] 
    }else if(outcome=="pneumonia"){
        vars <- df[,c(2,7,23)]
    }
    
    # vars[,1] hospital names
    # vars[.2] state
    # vars[,3] outcome data
    
    # make data numeric, some data becomes NA by coercion. 
    vars[,3] <- as.numeric(vars[,3])
    #remove NAs
    vars <- vars[!is.na(vars[,3]),]
    #only look at data for 'state'
    vars <- vars[vars[,2]==state,]
    #order by outcome data and then hospital name
    vars <- vars[order(vars[,3],vars[,1]), c(1,3)]
    
    # make num an numeric if it's not one already
    if(num=="best"){
        num <- 1
    }else if(num=="worst"){
        num <- nrow(vars)
    }else if(!is.numeric(num)){
        stop("invalid rank") 
    }
    
    ## Return hospital name in that state with the given rank 30-day death rate
    print(vars[num,1])
}

# tests
rankhospital("TX", "heart failure", 4)
#[1] "DETAR HOSPITAL NAVARRO"
rankhospital("MD", "heart attack", "worst")
#3[1] "HARFORD MEMORIAL HOSPITAL"
rankhospital("MN", "heart attack", 5000)
#[1] NA