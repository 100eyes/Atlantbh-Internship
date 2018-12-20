library(ggmap)
library(googleway)
geocodeQueryCheck()

set_of_rows <- which(enriched_poi@data$is_open == 1)
random_rows <- sample(set_of_rows, 20, replace = FALSE)
random_coordinates <- data.frame(lat = c(1:20), lng = c(1:20))
random_coordinates$lat <- enriched_poi@coords[random_rows,2]
random_coordinates$lng <- enriched_poi@coords[random_rows,1]
addresses <- data.frame(query_address = c(1:20), yelp_address = c(1:20))
addresses$query_address <- as.character(NA)
addresses$yelp_address <- as.character(NA)
rm(set_of_rows)
rm(random_rows)

api_key <- "AIzaSyAmVEAZ0bj89pwZKIaWpqEor-xq4d8FEMc"

for(x in 1:20){
  temp <- fromJSON(paste0("https://maps.googleapis.com/maps/api/geocode/json?&latlng=", random_coordinates$lat[x], ",", random_coordinates$lng[x], "&result_type=street_address&key=AIzaSyAmVEAZ0bj89pwZKIaWpqEor-xq4d8FEMc"))
  if(temp$status == "ZERO_RESULTS"){
    next
  }
  addresses$query_address[x] <- temp$results$formatted_address[1]
}

first_part <- mapply(FUN = function(index){
  paste(enriched_poi@data$address[index], enriched_poi@data$city[index], sep = ", ")
}, random_rows)

second_part <- mapply(FUN = function(index){
  paste(enriched_poi@data$state[index], enriched_poi@data$postal_code[index], sep = " ")
}, random_rows)

addresses$yelp_address <- mapply(FUN = function(index){
  paste(first_part[index], second_part[index], "USA", sep = ", ")
}, c(1:20))