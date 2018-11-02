# Combining longitude and latitude into new column called geo coordinates
yelp_business$geo_coordinates <- paste(yelp_business$latitude, yelp_business$longitude, sep = ", ")

# Looking for same addresses and testing if they have same geo coordinates
num_same_address <- length(unique(yelp_business$address))
same_address <- data.frame(table(yelp_business$address))
business_same_address <- yelp_business[yelp_business$address %in% same_address$Var1[same_address$Freq > 1],]

agg <- aggregate(geo_coordinates~address, business_same_address, length)
agg1 <- aggregate(address~geo_coordinates, business_same_address, length)
agg2 <- aggregate(cbind(latitude, longitude)~address, business_same_address, mean)
agg2 <- agg2[order(agg2$latitude),]
new_df <- data.frame(addres = agg$address, Consistent = agg$geo_coordinates == 1)
agg <- agg[order(agg$latitude), ]
agg1 <- agg1[order(agg)]

agg3 <- aggregate(cbind(latitude, longitude)~address, business_same_address, max)
agg4 <- aggregate(cbind(latitude, longitude)~address, business_same_address, min)
agg3 <- agg3[order(agg3$latitude), ]

# K-means clustering of data based on longitude and latitude
states_clusters <- kmeans(na.omit(yelp_business[c("latitude", "longitude")]), centers = 64, iter.max = 20)
str(states_clusters)