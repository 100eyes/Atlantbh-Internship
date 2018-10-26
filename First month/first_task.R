# Setting working directory
setwd("/home/besim/Documents/Atlantbh-Internship/First month")

# checking num of empty values
na_num = sapply(yelp_business, function(x) sum(is.na(x)))
total_nas = sum(na_num)

# Total completeness of database in percents
dummy_percent = total_nas/(nrow(yelp_business)*ncol(yelp_business))*100
which.max(na_num)
which.min(na_num)

# Percentage of completeness by attributes
percent_by_column <- na_num[1:length(na_num)]/nrow(yelp_business)*100
which.max(percent_by_column)
which.min(percent_by_column)
important_attributes = percent_by_column[c("name", "address", "city", "state", "longitude", "latitude")]

# Dummy values
states <- table(yelp_business$state)
sum(states[c("01", "10", "11", "4", "45", "6")])

stars_content <- table(yelp_business$stars)

names_missing_geocode <- yelp_business$name[which(is.na(yelp_business$latitude) | (is.na(yelp_business$longitude)))]
address_missing_geocode <- yelp_business$address[which(is.na(yelp_business$latitude) | (is.na(yelp_business$longitude)))]
missing_geocode <- rbind(names_missing_geocode, address_missing_geocode)
rm(names_missing_geocode, address_missing_geocode)
places_missing_address <- yelp_business$name[which(is.na(yelp_business$address))]

yelp_business$longitude[which.max(yelp_business$longitude)]
yelp_business$longitude[which.min(yelp_business$longitude)]

yelp_business$latitude[which.max(yelp_business$latitude)]
yelp_business$latitude[which.min(yelp_business$latitude)]

# Data frequency
hist(yelp_business$stars, xlab = "Stars", main = "Stars frequency")
hist(yelp_business$review_count, breaks = 1000, xlim = c(0, 150), ylim = c(0, 105000), xlab = "Review count", main = "Review count frequency")

tail(sort(table(yelp_business$city)), n=8)
tail(sort(table(yelp_business$state)), n=8)
tail(sort(table(yelp_business$name)), n=8)


# Sorting businesses by countries in North America
us_business <- yelp_business[which(yelp_business$state %in% us_states$Abbreviation),]
canada_business <- yelp_business[which(yelp_business$state %in% canada_states$Code),] 

dummy_states <- yelp_business[which(!((yelp_business$state %in% us_states$Abbreviation) | (yelp_business$state %in% canada_states$Code))),]

table(dummy_states$state)

# Checking for duplicates in businessID and names
businessID_occur <- data.frame(table(yelp_business$business_id))
nrow(yelp_business[yelp_business$business_id %in% businessID_occur$Var1[businessID_occur$Freq > 1],])

names_occur <- data.frame(table(yelp_business$name))
same_names <- yelp_business[yelp_business$name %in% names_occur$Var1[names_occur$Freq >1],]
nrow(same_names)
