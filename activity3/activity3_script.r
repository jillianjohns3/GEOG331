#install.packages(c("lubridate"))
#lubridate has helpful functions for working with dates and times in R
library(lubridate)
#can use require instead of library
#warning messages: date function from base r functions is now going to be masked by lubridate date function
#also lubridate will be used now

#create a function. The names of the arguements for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "no error: equal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")


#You are the project data scientist to manage their data. Your first task is do the data QA/QC and make a plot of the data
