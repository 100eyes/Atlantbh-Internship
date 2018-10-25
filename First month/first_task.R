# Setting working directory
setwd("/home/besim/Documents/Atlantbh-Internship/First month")

# checking num of empty values
na_num = sapply(yelp_business, function(x) sum(is.na(x)))
total_nas = sum(na_num)

# Total completeness of database in percents
dummy_percent = total_nas/(nrow(yelp_business)*ncol(yelp_business))*100
which.max(na_num)
which.min(na_num)

# Dummy values
states <- table(yelp_business$state)
sum(states[c("01", "10", "11", "4", "45", "6")])

stars_content <- table(yelp_business$stars)

names_missing_geocode <- yelp_business$name[which(is.na(yelp_business$latitude) | (is.na(yelp_business$longitude)))]
address_missing_geocode <- yelp_business$address[which(is.na(yelp_business$latitude) | (is.na(yelp_business$longitude)))]
missing_geocode <- rbind(names_missing_geocode, address_missing_geocode)
rm(names_missing_geocode, address_missing_geocode)
places_missing_address <- yelp_business$name[which(is.na(yelp_business$address))]

# Data frequency
hist(yelp_business$stars, xlab = "Stars", main = "Stars frequency")
hist(yelp_business$review_count, breaks = 1000, xlim = 1000, xlab = "Review count", main = "Review count frequency")
