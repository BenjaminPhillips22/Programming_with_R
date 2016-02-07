

setwd("~/R/Hopkins_Data_Science_Specialization/Programming_with_R/Hospital_Quality")
print(list.files())

outcome <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")

print(names(outcome))
print(head(outcome[,19]))
print(head(outcome$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))

outcome[, 11] <- as.numeric(outcome[, 11])
# may get a warning about NAs being introduced; that is okay
str(outcome[,11])

#plot the 30-day mortality rates for heart attack
hist(outcome[, 11], xlab = names(outcome)[11])