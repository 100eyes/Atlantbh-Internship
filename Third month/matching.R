# Adding layers to sample Yelp data
for(i in 1:nrow(sample_data)){
  for(j in 1:10){
    # Getting the one category from categories string
    category <- word(sample_data$categories[i], j, sep = fixed(", "))
    # Break the loop if we reached the end of categories string in sample_data
    if(is.na(category)){
      break
    }
    
    for(k in 1:nrow(yelp_categories)){
      
      if(category == yelp_categories$title[k]){
        
        # First layer vaule
        # if it doesn't exist yet, copy categories$layer in it and break 
        # categories for loop (third one)
        if(is.na(sample_data$layer1[i])){
          if(is.na(yelp_categories$`parents/0`[k])){
            sample_data$layer1[i] <- yelp_categories$alias[k]
            break
          }
          sample_data$layer1[i] <- yelp_categories$`parents/0`[k]
          break
        }
        
        # Second layer value
        # if it is NA and it's not included in previus layer
        # copy categories$layer in it and break
        if((is.na(sample_data$layer2[i])) & (sample_data$layer1[i] != yelp_categories$alias[k])){
          if(is.na(yelp_categories$`parents/0`[k])){
            sample_data$layer2[i] <- yelp_categories$alias[k]
            break
          }
          sample_data$layer2[i] <- yelp_categories$`parents/0`[k]
          break
        }
        
        # Third layer value
        # if it isn't NA and not equal to two previus layers
        # copy value of categories$layer in it and break
        if((is.na(sample_data$layer3[i])) & (sample_data$layer1[i] != yelp_categories$alias[k]) & (sample_data$layer2[i] != yelp_categories$alias[k])){
          if(is.na(yelp_categories$`parents/0`[k])){
            sample_data$layer3[i] <- yelp_categories$alias[k]
            break
          }
          sample_data$layer3[i] <- yelp_categories$alias[k]
          break
        }
      }
    }
    
    # If all three layers have value
    # go to the next row
    if(!(is.na(sample_data$layer3[i]))){
      break
    }
  }
}




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

# Calculating matching score for Facbook and Yelp data
# Score is calculated by adding name score, scaled distance, phone number and category matching
# score = name_score + scaled_distance + phone_score + category_score
# name_score = token sort ratio, 
# scaled_distance = 20, if distance <= 10, 15, if 10 < distance <= 50, 10, if 50 < distance <= 100, 
#                   5, if 100 <= distance < 250, 0 othervise
# phone_score = 10, if matched, 0 if not
# category_score = if one of layers match score is 20, if not score is 0
for(i in seq(1, 500)){
  if(is.null(fb_places[[i]]$data$name_score)){
    next
  }
  score <- data.frame(name_score=integer(nrow(fb_places[[i]]$data)),
                      distance_score=integer(nrow(fb_places[[i]]$data)),
                      phone_score=integer(nrow(fb_places[[i]]$data)),
                      category_score=integer(nrow(fb_places[[i]]$data)),
                      sum_score = integer(nrow(fb_places[[i]]$data)),
                      matching = character(nrow(fb_places[[i]]$data)))
  score <- transform(score, name_score=as.integer(name_score),
                     distance_score=as.integer(distance_score),
                     phone_score=as.integer(phone_score),
                     category_score=as.integer(category_score),
                     sum_score=as.integer(nrow(sum_score)),
                     matching=as.character(matching))
  for(j in seq(1, nrow(fb_places[[i]]$data))){
    score$name_score[j] <- fb_places[[i]]$data$name_score[j]
    if(fb_places[[i]]$data$distance[j] <= 10){
      score$distance_score[j] <- 20
    }
    if((fb_places[[i]]$data$distance[j] > 10) & (fb_places[[i]]$data$distance[j] <= 20)){
      score$distance_score[j] <- 15
    }
    if((fb_places[[i]]$data$distance[j] > 20) & (fb_places[[i]]$data$distance[j] <= 30)){
      score$distance_score[j] <- 10
    }
    if((fb_places[[i]]$data$distance[j] > 30) & (fb_places[[i]]$data$distance[j] <= 40)){
      score$distance_score[j] <- 5
    }
    if((fb_places[[i]]$data$distance[j] > 40) & (fb_places[[i]]$data$distance[j] <= 50)){
      score$distance_score[j] <- 2
    }
    if((fb_places[[i]]$data$distance[j] > 50) & (fb_places[[i]]$data$distance[j] <= 65)){
      score$distance_score[j] <- 0
    }
    if((fb_places[[i]]$data$distance[j] > 65) & (fb_places[[i]]$data$distance[j] <= 100)){
      score$distance_score[j] <- -5
    }
    if(fb_places[[i]]$data$distance[j] > 100){
      score$distance_score[j] <- -8
    }
    if(!is.null(fb_places[[i]]$data$phone[j])){
      if((sample_data$display_phone[i] == fb_places[[i]]$data$phone[j]) &
        !is.na(sample_data$display_phone[i]) & !is.na(fb_places[[i]]$data$phone[j])){
        score$phone_score[j] <- 10
      } else {
        score$phone_score[j] <- 0
      }
    } else {
      score$phone_score[j] <- 0
    }
    for(k in seq(1, length(fb_places[[i]]$data$category_list[[j]]$yelp_layer))){
      if(sample_data$layer1[i] == fb_places[[i]]$data$category_list[[j]]$yelp_layer[k] & !is.na(sample_data$layer1[i])
         & !is.na(fb_places[[i]]$data$category_list[[j]]$yelp_layer[k])){
        score$category_score[j] <- 20
        break
      }
      if(sample_data$layer2[i] == fb_places[[i]]$data$category_list[[j]]$yelp_layer[k] & !is.na(sample_data$layer2[i])
         & !is.na(fb_places[[i]]$data$category_list[[j]]$yelp_layer[k])){
        score$category_score[j] <- 20
        break
      }
      if(sample_data$layer3[i] == fb_places[[i]]$data$category_list[[j]]$yelp_layer[k] & !is.na(sample_data$layer3[i])
         & !is.na(fb_places[[i]]$data$category_list[[j]]$yelp_layer[k])){
        score$category_score[j] <- 20
        break
      }
    }
    score$sum_score[j] <- sum(score$name_score[j], score$distance_score[j], score$phone_score[j], score$category_score[j], na.rm = TRUE)
  }
  fb_places[[i]]$data$score <- score
}

# Editing phones in Yelp and Facebook data for better matching
# Adjusting name score
for(i in seq(1, 500)){
  if(is.na(sample_data$display_phone[i])){
    fb_places[[i]]$data$score$phone_score <- 0
    next
  }
  if(is.null(fb_places[[i]]$data$name_score)){
    next
  }
  if(substring(sample_data$display_phone[i], 1, 1) == "+"){
    sample_data$display_phone[i] <- gsub("-", "", substring(sample_data$display_phone[i], 4))
  }
  if(substring(sample_data$display_phone[i], 1, 1) == "("){
    sample_data$display_phone[i] <- gsub(")", "", substring(sample_data$display_phone[i], 2))
    sample_data$display_phone[i] <- gsub(" ", "", sample_data$display_phone[i])
    sample_data$display_phone[i] <- gsub("-", "", sample_data$display_phone[i])
  }
  for(j in seq(1, length(fb_places[[i]]$data$phone))){
    if(is.null(fb_places[[i]]$data$phone[j])){
      fb_places[[i]]$data$score$phone_score[j] <- 0
      next
    }
    if(is.na(fb_places[[i]]$data$phone[j])){
      fb_places[[i]]$data$score$phone_score[j] <- 0
      next
    }
    fb_places[[i]]$data$phone[j] <- gsub(")", "", substring(fb_places[[i]]$data$phone[j], 2))
    fb_places[[i]]$data$phone[j] <- gsub(" ", "", fb_places[[i]]$data$phone[j])
    fb_places[[i]]$data$phone[j] <- gsub("-", "", fb_places[[i]]$data$phone[j])
    
    if(sample_data$display_phone[i] == fb_places[[i]]$data$phone[j]){
      fb_places[[i]]$data$score$phone_score[j] <- 10
    } else {
      fb_places[[i]]$data$score$phone_score <- 0
    }
    
    fb_places[[i]]$data$score$name_score[j] <- fb_places[[i]]$data$score$name_score[j]/2
    fb_places[[i]]$data$score$sum_score[j] <- sum(fb_places[[i]]$data$score$name_score[j], fb_places[[i]]$data$score$distance_score[j],
                                                  fb_places[[i]]$data$score$phone_score[j], fb_places[[i]]$data$score$category_score[j],
                                                  na.rm = TRUE)
  }
}

for(i in seq(1, 500)){
  if(is.null(fb_places[[i]]$data$name_score)){
    next
  }
  for(j in seq(1, nrow(fb_places[[i]]$data$score))){
    if(fb_places[[i]]$data$score$sum_score[j] >= 78){
      fb_places[[i]]$data$score$matching[j] <- "MATCHED"
    } else {
      fb_places[[i]]$data$score$matching[j] <- "NOT MATCHED"
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




