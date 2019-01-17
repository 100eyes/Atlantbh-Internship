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

# Adding yelp category mapping to facebook data
for(i in seq(1, 500)){
  # Loop through all facebook data
  for(j in seq(1, length(fb_places[[i]]$data$category_list))){
    # Loop through all categories in one record
    # Create array for yelp categories
    fb_places[[i]]$data$category_list[[j]] <- cbind(fb_places[[i]]$data$category_list[[j]], 
                                                    yelp_layer=character(nrow(fb_places[[i]]$data$category_list[[j]])))
    fb_places[[i]]$data$category_list[[j]] <- transform(fb_places[[i]]$data$category_list[[j]], 
                                                        yelp_layer=as.character(yelp_layer))
    for(k in seq(1, length(fb_places[[i]]$data$category_list[[j]]$name))){
      # Loop through categories of one facebook place in record
      for(l in seq(1, nrow(fb_categories))){
        # Loop through mapped facebook categories an yelp categories
        if(fb_places[[i]]$data$category_list[[j]]$name[k] == fb_categories$Category[l]){
          # If matched, add yelp layer
          if(fb_places[[i]]$data$category_list[[j]]$yelp_layer[k] == ""){
            # First yelp layer
            fb_places[[i]]$data$category_list[[j]]$yelp_layer[k] <- fb_categories$Yelp_category[l]
            break
          }
          if(fb_places[[i]]$data$category_list[[j]]$yelp_layer[k] == ""){
            # Second yelp layer
            fb_places[[i]]$data$category_list[[j]]$yelp_layer[k] <- fb_categories$Yelp_category[l]
            break
          }
          if(fb_places[[i]]$data$category_list[[j]]$yelp_layer[k] == ""){
            # Third yelp layer
            fb_places[[i]]$data$category_list[[j]]$yelp_layer[3] <- fb_categories$Yelp_category[l]
            break
          }
        }
      }
    }
  }
}

#####################################
# Scoring algorithm for Google data #
#####################################

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

for(i in seq(1, 500)){
  if(google_data[[i]]$status == "ZERO_RESULTS"){
    next
  }
  yelp_layer <- data.frame(name=character(length(google_data[[i]]$results$name)), 
                           first_layer=character(length(google_data[[i]]$results$name)),
                           second_layer=character(length(google_data[[i]]$results$name)),
                           third_layer=character(length(google_data[[i]]$results$name)))
  yelp_layer <- transform(yelp_layer, name=as.character(name), first_layer=as.character(first_layer),
                          second_layer=as.character(second_layer), third_layer=as.character(third_layer))
  google_data[[i]]$results$yelp_layer <- yelp_layer
  for(j in seq(1, length(google_data[[i]]$results$types))){
    google_data[[i]]$results$yelp_layer$name[j] <- google_data[[i]]$results$name[j]
    for(k in seq(1, length(google_data[[i]]$results$types[[j]]))){
      for(l in seq(1, nrow(google_categories))){
        if(is.na(google_data[[i]]$results$yelp_layer$first_layer[j])){
          break
        }
        if(is.na(google_data[[i]]$results$yelp_layer$second_layer[j])){
          break
        }
        if(is.na(google_data[[i]]$results$yelp_layer$third_layer[j])){
          break
        }
        if(google_data[[i]]$results$types[[j]][k] == google_categories$Google_category[l]){
          if(google_data[[i]]$results$yelp_layer$first_layer[j] == ""){
            google_data[[i]]$results$yelp_layer$first_layer[j] <- google_categories$Yelp_layer[l]
            break
          }
          if(google_data[[i]]$results$yelp_layer$second_layer[j] == ""){
            google_data[[i]]$results$yelp_layer$second_layer[j] <- google_categories$Yelp_layer[l]
            break
          }
          if(google_data[[i]]$results$yelp_layer$third_layer[j] == ""){
            google_data[[i]]$results$yelp_layer$third_layer[j] <- google_categories$Yelp_layer[l]
            break
          }
        }
      }
    }
  }
}




