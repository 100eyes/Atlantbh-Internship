# checking dummy values
na_num = sapply(yelp_business, function(x) sum(is.na(x)))
total_nas = sum(na_num)
dummy_percent = total_nas/(nrow(yelp_business)*ncol(yelp_business))*100
which.max(na_num)
which.min(na_num)
