#######################################
# Scoring algorithm for Facebook data #
#######################################

# Scoring is based on attributes: is_permanently_closed, 
# categories, name, distance between geo locations
library(fuzzywuzzyR)
library(geosphere)
init <- FuzzMatcher$new()

# Calculating distance and name score for Facebook
for(i in seq(1, 500)){
  if(length(fb_places[[i]]$data) == 0){
    next
  }
  fb_places[[i]]$data$name_score <- vector("integer", length(fb_places[[i]]$data$name))
  fb_places[[i]]$data$distance <- vector("integer", length = nrow(fb_places[[i]]$data))
  s1 <- sample_data$name[i]
  yelp_coord <- c(sample_data$longitude[i], sample_data$latitude[i])
  for(j in seq(1, length(fb_places[[i]]$data$name))){
    s2 <- fb_places[[i]]$data$name[j]
    fb_coord <- c(fb_places[[i]]$data$location$longitude[j], fb_places[[i]]$data$location$latitude[j])
    if(is.na(s2)){
      fb_places[[i]]$data$name_score[j] <- 0
      next
    }
    fb_places[[i]]$data$name_score[j] <- init$Token_sort_ratio(string1 = s1, string2 = s2, force_ascii = TRUE, full_process = TRUE)
    fb_places[[i]]$data$distance[j] <- distHaversine(yelp_coord, fb_coord)
  }
}

# Calculating distance and name score for google
for(i in seq(1, 500)){
  if(google_data[[i]]$status == "ZERO_RESULTS"){
    next
  }
  google_data[[i]]$results$name_score <- vector("integer", length = length(google_data[[i]]$results$name))
  google_data[[i]]$results$distance <- vector("integer", length = length(google_data[[i]]$results$name))
  s1 <- sample_data$name[i]
  yelp_coord <- c(sample_data$longitude[i], sample_data$latitude[i])
  if(length(google_data[[i]]$results$name) == 1){
    s2 <- google_data[[i]]$results$name
    google_cords <- c(google_data[[i]]$results$geometry$location$lng, google_data[[i]]$results$geometry$location$lat)
    google_data[[i]]$results$name_score <- init$Token_sort_ratio(string1 = s1, string2 = s2, force_ascii = TRUE, full_process = TRUE)
    google_data[[i]]$results$distance <- distHaversine(yelp_coord, google_cords)
  } else {
    for(j in seq(1, length(google_data[[i]]$results$name))){
      s2 <- google_data[[i]]$results$name[j]
      google_cords <- c(google_data[[i]]$results$geometry$location$lng[j], google_data[[i]]$results$geometry$location$lat[j])
      google_data[[i]]$results$name_score[j] <- init$Token_sort_ratio(string1 = s1, string2 = s2, force_ascii = TRUE, full_process = TRUE)
      google_data[[i]]$results$distance[j] <- distHaversine(yelp_coord, google_cords)
    }
  }
}


