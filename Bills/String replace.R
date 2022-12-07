##################################
#   Replace value String in R 
###################################

suppressMessages(if(!require(readxl)){ install.packages("stringr")}) 

suppressMessages(library(stringr))


fruits <- c("2312321NA", "56354234234NA", "NA3423434234")
str_replace(fruits, "NA", "")
