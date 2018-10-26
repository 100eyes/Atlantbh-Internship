library(jsonlite)

# Setting working directory
setwd("/home/besim/Documents/Atlantbh-Internship/First month")

# Transfering .json format into .Rdata format
file_name = "/home/besim/Documents/Praksa/yelp_dataset/yelp_academic_dataset_business.json"
business<-stream_in(textConnection(readLines(file_name, n=188593)),verbose=T)

# Saving database as .csv file
write.table(business, file = "yelp_business.csv", sep = "#", row.names = FALSE, na = "")

# Loading necessary databases
library(readr)
yelp_business <- read_delim("yelp_business.csv", "#", escape_double = FALSE, trim_ws = TRUE)
us_states <- read_csv("states.csv")
canada_states <- read_csv("canada_states.csv")

