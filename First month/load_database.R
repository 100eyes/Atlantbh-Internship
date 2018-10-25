library(jsonlite)
setwd("/home/besim/Documents/Praksa")
file_name = "/home/besim/Documents/Praksa/yelp_dataset/yelp_academic_dataset_business.json"
business<-stream_in(textConnection(readLines(file_name, n=188593)),verbose=T)

write.table(business, file = "yelp_business.csv", sep = "#", row.names = FALSE, na = "")
