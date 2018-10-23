# checking dummy values
sapply(business, function(x) sum(is.na(x)))
sapply(business, function(x) sum(as.integer(x == "")))
