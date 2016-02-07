

rankall <- function(outcome, num = "best") {
    # set working directory
    setwd("~/R/Hopkins_Data_Science_Specialization/Programming_with_R/Hospital_Quality")
    ## Read outcome data
    df <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
    
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
    # vars[,2] State
    # vars[,3] outcome data (rate)
    names(vars) <- c("hospital", "State", "rate")
    
    # make data numeric, some data becomes NA by coercion. 
    vars[,3] <- as.numeric(vars[,3])
    #remove NAs
    vars <- vars[!is.na(vars[,3]),]
    
    library(dplyr)
    if(num=="worst"){
        myTable <- vars %>% group_by(State) %>% arrange(State,rate,hospital) %>% filter(rate==max(rate)) %>% filter(hospital==hospital[1]) %>% select(hospital, State) 
    }else if(num=="best"){
        myTable <- vars %>% group_by(State) %>% arrange(State,rate,hospital) %>% filter(rate==min(rate)) %>% filter(hospital==hospital[1]) %>% select(hospital, State)
    }else if(!is.numeric(num)){
        stop("invalid num")
    }else{
        myTable <- vars %>% group_by(State) %>% arrange(State,rate,hospital) %>% filter(rate==rate[num]) %>% filter(hospital==hospital[1]) %>% select(hospital, State)
    }
    
    #print(myTable)
    
    
    #### Below is the code without using dplyr
    
    #     # make num an numeric if it's not one already
    #     if(num=="worst"){
    #         #nothing  
    #     }else if(num=="best"){
    #         numT <- 1
    #     }else if(!is.numeric(num)){
    #         stop("invalid rank")
    #     }else{
    #         numT <- num
    #     }
    #       
    #     hospital <- vector(mode = "character")
    #     states <- sort(unique(vars[,2]))
    #     #print((states))
    #     for(state in states){
    #         #select by state
    #         temp <- vars[vars[,2]==state, ]
    #         #order by outcome data and then hospital name
    #         temp <- temp[order(temp[,3],temp[,1]), ]
    #         if(num=="worst"){
    #             numT <- nrow(temp)
    #         }
    #         #print(temp[num,])
    #         hospital <- c(hospital, temp[numT,1])
    #     }
    #     
    #     myDataFrame <- as.data.frame(cbind(hospital, states))
    #     names(myDataFrame) <- c("hospital", "state")
    #     (myDataFrame)
}

# tests
print(head(rankall("heart attack", 20), 10))
# hospital state
# 1                                 <NA>    AK
# 2       D W MCMILLAN MEMORIAL HOSPITAL    AL
# 3    ARKANSAS METHODIST MEDICAL CENTER    AR
# 4  JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
# 5                SHERMAN OAKS HOSPITAL    CA
# 6             SKY RIDGE MEDICAL CENTER    CO
# 7              MIDSTATE MEDICAL CENTER    CT
# 8                                 <NA>    DC
# 9                                 <NA>    DE
# 10      SOUTH FLORIDA BAPTIST HOSPITAL    FL

print(tail(rankall("pneumonia", "worst"), 3))
# hospital state
# 52 MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
# 53                     PLATEAU MEDICAL CENTER    WV
# 54           NORTH BIG HORN HOSPITAL DISTRICT    WY

print(tail(rankall("heart failure"), 10))
# hospital state
# 45                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
# 46                                        FORT DUNCAN MEDICAL CENTER    TX
# 47 VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
# 48                                          SENTARA POTOMAC HOSPITAL    VA
# 49                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
# 50                                              SPRINGFIELD HOSPITAL    VT
# 51                                         HARBORVIEW MEDICAL CENTER    WA
# 52                                    AURORA ST LUKES MEDICAL CENTER    WI
# 53                                         FAIRMONT GENERAL HOSPITAL    WV
# 54                                        CHEYENNE VA MEDICAL CENTER    WY

rankall("heart failure")
# hospital state
# 1                                           SOUTH PENINSULA HOSPITAL    AK
# 2                                 GEORGE H. LANIER MEMORIAL HOSPITAL    AL
# 3                       VA CENTRAL AR. VETERANS HEALTHCARE SYSTEM LR    AR
# 4                               BANNER GOOD SAMARITAN MEDICAL CENTER    AZ
# 5                                  CENTINELA HOSPITAL MEDICAL CENTER    CA
# 6                                          PARKER ADVENTIST HOSPITAL    CO