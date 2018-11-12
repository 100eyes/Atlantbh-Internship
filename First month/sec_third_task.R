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

# Editing states

rows_with_las <- grep("Las", yelp_business$city)
lasvegas_businesses <- yelp_business[c(rows_with_las),]
new_business <- yelp_business
new_business$city[which(new_business$city %in% lasvegas_businesses$city)] <- "Las Vegas"

# K-means clustering of data based on longitude and latitude
lon_lat_na_rows <- which(is.na(yelp_business$latitude) | is.na(yelp_business$longitude))
business <- yelp_business[-c(lon_lat_na_rows[]),]

states_clusters <- kmeans(business[c("latitude", "longitude")], centers = 64, iter.max = 30)
str(states_clusters)
business$clusters <- as.factor(states_clusters$cluster)
library("ggmap")
north_america <- get_map(location = c(lon = -105.255119, lat = 54.525961), zoom = 3)
ggmap(north_america) + geom_point(aes(x = longitude[], y = latitude[], colour = as.factor(clusters)), data = business) +
  ggtitle("States in North America using KMean")

k_clusters <- kmeans(business[c("latitude", "longitude")], centers = 20000, iter.max = 100)
str(k_clusters)
business$k_clusters <- as.factor(k_clusters$cluster)
write.table(business[c("name", "address", "city", "postal_code", "latitude", "longitude", "k_clusters")], file = "business.csv", sep = "\t", row.names = FALSE)
View(business[which(business$k_clusters == "9378"),])

# Generating random clusters
random_clusters <- sample(1:20000, 20, replace = FALSE)
random_businesses <- business[which(business$k_clusters %in% random_clusters),]
write.table(random_businesses[c("name", "address", "city", "postal_code", "latitude", "longitude", "k_clusters")], file = "random_clusters.csv", sep = "\t", row.names = FALSE)
