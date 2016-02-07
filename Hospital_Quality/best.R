

best <- function(state, outcome) {
    # state is a two character string
    #outcome is one of c("heart attack", "heart failure", "pneumonia")
    
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
    #print(head(vars))
    ## Return hospital name in that state with lowest 30-day death rate
    a <- which(vars[,3]==min(vars[,3]))
    print(sort(vars[a,1])[1])
}


# tests
best("TX", "heart attack")
#[1] "CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart failure")
#[1] "FORT DUNCAN MEDICAL CENTER"
best("MD", "heart attack")
#[1] "JOHNS HOPKINS HOSPITAL, THE"
best("MD", "pneumonia")
#[1] "GREATER BALTIMORE MEDICAL CENTER"
best("BB", "heart attack")
#Error in best("BB", "heart attack") : invalid state
best("NY", "hert attack")
#Error in best("NY", "hert attack") : invalid outcome