# orginazing facebook data
score <- data.frame(name_score=integer(700),
                    distance_score=integer(700),
                    phone_score=integer(700),
                    category_score=integer(700),
                    sum_score = integer(700),
                    matching = character(700))
score <- transform(score, name_score=as.integer(name_score),
                   distance_score=as.integer(distance_score),
                   phone_score=as.integer(phone_score),
                   category_score=as.integer(category_score),
                   sum_score=as.integer(sum_score),
                   matching=as.character(matching))

facebook_data <- data.frame(name=character(700),
                            street=character(700),
                            city=character(700),
                            state=character(700),
                            zip=character(700),
                            country=character(700),
                            latitude=integer(700),
                            longitude=integer(700),
                            categories=character(700),
                            yelp_layer=character(700),
                            phone=character(700),
                            rating_count=integer(700),
                            hours_monday=character(700),
                            hours_tuesday=character(700),
                            hours_wednesday=character(700),
                            hours_thursday=character(700),
                            hours_friday=character(700),
                            hours_saturday=character(700),
                            hours_sunday=character(700),
                            name_score=integer(700),
                            distance=integer(700),
                            index=integer(700))
facebook_data <- transform(facebook_data, name=as.character(name),
                           street=as.character(street),
                           city=as.character(city),
                           state=as.character(state),
                           zip=as.character(zip),
                           country=as.character(country),
                           latitude=as.integer(latitude),
                           longitude=as.integer(longitude),
                           categories=as.character(categories),
                           yelp_layer=as.character(yelp_layer),
                           phone=as.character(phone),
                           rating_count=as.integer(rating_count),
                           hours_monday=as.character(hours_monday),
                           hours_tuesday=as.character(hours_tuesday),
                           hours_wednesday=as.character(hours_wednesday),
                           hours_thursday=as.character(hours_thursday),
                           hours_friday=as.character(hours_friday),
                           hours_saturday=as.character(hours_saturday),
                           hours_sunday=as.character(hours_sunday),
                           name_score=as.integer(name_score),
                           distance=as.integer(distance),
                           index=as.integer(index))
facebook_data$score <- score

iterator <- 0
for(i in seq(1, 500)){
  if(is.null(fb_places[[i]]$data$name_score)){
    next
  }
  for(j in seq(1, nrow(fb_places[[i]]$data))){
    if(fb_places[[i]]$data$score$matching[j] == "MATCHED"){
      iterator <- iterator + 1
      
      facebook_data$name[iterator] <- fb_places[[i]]$data$name[j]
      if(!is.null(fb_places[[i]]$data$location$street[j])){
        facebook_data$street[iterator] <- fb_places[[i]]$data$location$street[j]
      }
      facebook_data$city[iterator] <- fb_places[[i]]$data$location$city[j]
      facebook_data$state[iterator] <- fb_places[[i]]$data$location$state[j]
      facebook_data$zip[iterator] <- fb_places[[i]]$data$location$zip[j]
      facebook_data$country[iterator] <- fb_places[[i]]$data$location$country[j]
      facebook_data$latitude[iterator] <- fb_places[[i]]$data$location$latitude[j]
      facebook_data$longitude[iterator] <- fb_places[[i]]$data$location$longitude[j]
      for(k in seq(1, length(fb_places[[i]]$data$category_list[[j]]$name))){
        if(length(fb_places[[i]]$data$category_list[[j]]$name) == 1){
          facebook_data$categories[iterator] <- fb_places[[i]]$data$category_list[[j]]$name[k]
          break
        }
        if(k == 1){
          facebook_data$categories[iterator] <- fb_places[[i]]$data$category_list[[j]]$name[k]
          next
        }
        facebook_data$categories[iterator] <- paste0(facebook_data$categories[iterator], ", ", fb_places[[i]]$data$category_list[[j]]$name[k])
      }
      temp_layers <- fb_places[[i]]$data$category_list[[j]]$yelp_layer[!duplicated(fb_places[[i]]$data$category_list[[j]]$yelp_layer)]
      for(l in seq(1, length(temp_layers))){
        if(length(temp_layers) == 1){
          facebook_data$yelp_layer[iterator] <- temp_layers[l]
          break
        }
        if(l == 1){
          facebook_data$yelp_layer[iterator] <- temp_layers[l]
          next
        }
        facebook_data$yelp_layer[iterator] <- paste0(facebook_data$yelp_layer[iterator], ", ", temp_layers[l])
      }
      facebook_data$phone[iterator] <- fb_places[[i]]$data$phone[j]
      facebook_data$rating_count[iterator] <- fb_places[[i]]$data$rating_count[j]
      if(!is.null(fb_places[[i]]$data$hours[[j]])){
        facebook_data$hours_monday[iterator] <- paste0(fb_places[[i]]$data$hours[[j]]$value[1], "-", fb_places[[i]]$data$hours[[j]]$value[2])
        facebook_data$hours_tuesday[iterator] <- paste0(fb_places[[i]]$data$hours[[j]]$value[3], "-", fb_places[[i]]$data$hours[[j]]$value[4])
        facebook_data$hours_wednesday[iterator] <- paste0(fb_places[[i]]$data$hours[[j]]$value[5], "-", fb_places[[i]]$data$hours[[j]]$value[6])
        facebook_data$hours_thursday[iterator] <- paste0(fb_places[[i]]$data$hours[[j]]$value[7], "-", fb_places[[i]]$data$hours[[j]]$value[8])
        facebook_data$hours_friday[iterator] <- paste0(fb_places[[i]]$data$hours[[j]]$value[9], "-", fb_places[[i]]$data$hours[[j]]$value[10])
        facebook_data$hours_saturday[iterator] <- paste0(fb_places[[i]]$data$hours[[j]]$value[11], "-", fb_places[[i]]$data$hours[[j]]$value[12])
        facebook_data$hours_sunday[iterator] <- paste0(fb_places[[i]]$data$hours[[j]]$value[13], "-", fb_places[[i]]$data$hours[[j]]$value[14])
      }
      facebook_data$name_score[iterator] <- fb_places[[i]]$data$name_score[j]
      facebook_data$distance[iterator] <- fb_places[[i]]$data$distance[j]
      facebook_data$index[iterator] <- i
      facebook_data$score[iterator,] <- fb_places[[i]]$data$score[j,]
    }
  }
}

# Organizing google data
score <- data.frame(name_score=integer(500),
                    distance_score=integer(500),
                    phone_score=integer(500),
                    category_score=integer(500),
                    sum_score = integer(500),
                    matching = character(500))
score <- transform(score, name_score=as.integer(name_score),
                   distance_score=as.integer(distance_score),
                   phone_score=as.integer(phone_score),
                   category_score=as.integer(category_score),
                   sum_score=as.integer(sum_score),
                   matching=as.character(matching))
matched_google_data <- data.frame(name = character(500),
                                  street = character(500),
                                  city = character(500),
                                  state = character(500),
                                  zip = character(500),
                                  country = character(500),
                                  latitude = integer(500),
                                  longitude = integer(500),
                                  categories = character(500),
                                  yelp_layer = character(500),
                                  phone = character(500),
                                  rating= integer(500),
                                  hours_monday=character(500),
                                  hours_tuesday=character(500),
                                  hours_wednesday=character(500),
                                  hours_thursday=character(500),
                                  hours_friday=character(500),
                                  hours_saturday=character(500),
                                  hours_sunday=character(500),
                                  name_score = integer(500),
                                  distance = integer(500),
                                  index = integer(500))
matched_google_data <- transform(matched_google_data, name=as.character(name),
                                 street=as.character(street),
                                 city=as.character(city),
                                 state=as.character(state),
                                 zip=as.character(zip),
                                 country=as.character(country),
                                 latitude=as.character(latitude),
                                 longitude=as.character(longitude),
                                 categories=as.character(categories),
                                 yelp_layer=as.character(yelp_layer),
                                 phone=as.character(phone),
                                 rating=as.integer(rating),
                                 hours_monday=as.character(hours_monday),
                                 hours_tuesday=as.character(hours_tuesday),
                                 hours_wednesday=as.character(hours_wednesday),
                                 hours_thursday=as.character(hours_thursday),
                                 hours_friday=as.character(hours_friday),
                                 hours_saturday=as.character(hours_saturday),
                                 hours_sunday=as.character(hours_sunday),
                                 name_score=as.integer(name_score),
                                 distance=as.integer(distance),
                                 index=as.integer(index))
matched_google_data$score <- score

iterator <- 0
for(i in seq(1, 500)){
  if(google_data[[i]]$status == "ZERO_RESULTS"){
    next
  }
  for(j in seq(1, nrow(google_data[[i]]$results))){
    if(google_data[[i]]$results$score$matching[j] == "MATCHED"){
      iterator <- iterator + 1
      matched_google_data$name[iterator] <- google_data[[i]]$results$name[j]
      rijeci <- ""
      nabrajalica <- 0
      for(z in seq(1, 10)){
        rijeci[z] <- word(google_place_details[[i]][[j]]$result$formatted_address, z, sep = fixed(", "))
        print(rijeci[z])
        if(z == 1){
          nabrajalica <- nabrajalica + nchar(rijeci[z])
        } 
        if(z != 1){
          nabrajalica <- nabrajalica + nchar(rijeci[z]) + 2
        }
        print(nabrajalica)
        if(nabrajalica == nchar(google_place_details[[i]][[j]]$result$formatted_address)){
          break
        }
      }
      matched_google_data$street[iterator] <- rijeci[1]
      if(length(rijeci) > 4){
        for(x in seq(2, (length(rijeci)-3))){
          matched_google_data$street[iterator] <- paste0(matched_google_data$street[iterator], rijeci[x])
        }
      }
      matched_google_data$city[iterator] <- rijeci[length(rijeci)-2]
      matched_google_data$state[iterator] <- word(rijeci[length(rijeci)-1], 1, sep = fixed(" "))
      matched_google_data$zip[iterator] <- word(rijeci[length(rijeci)-1], 2, sep = fixed(" "))
      matched_google_data$country[iterator] <- rijeci[length(rijeci)]
      matched_google_data$latitude[iterator] <- google_data[[i]]$results$geometry$location$lat[j]
      matched_google_data$longitude[iterator] <- google_data[[i]]$results$geometry$location$lng[j]
      for(k in seq(1, length(google_data[[i]]$results$types[[j]]))){
        if(k == 1){
          matched_google_data$categories[iterator] <- google_data[[i]]$results$types[[j]][k]
          next
        }
        matched_google_data$categories[iterator] <- paste0(matched_google_data$categories[iterator], ", ", google_data[[i]]$results$types[[j]][k])
      }
      if(!(google_data[[i]]$results$yelp_layer$first_layer[j] == "") & !(is.na(google_data[[i]]$results$yelp_layer$first_layer[j]))){
        matched_google_data$categories[iterator] <- google_data[[i]]$results$yelp_layer$first_layer[j]
      }
      if(!(google_data[[i]]$results$yelp_layer$second_layer[j] == "") & !(is.na(google_data[[i]]$results$yelp_layer$second_layer[j]))){
        matched_google_data$categories[iterator] <- paste0(matched_google_data$categories[iterator], ", ", google_data[[i]]$results$yelp_layer$second_layer[j])
      }
      if(!(google_data[[i]]$results$yelp_layer$third_layer[j] == "") & !(is.na(google_data[[i]]$results$yelp_layer$third_layer[j]))){
        matched_google_data$categories[iterator] <- paste0(matched_google_data$categories[iterator], ", ", google_data[[i]]$results$yelp_layer$third_layer[j])
      }
      if(!(is.null(google_place_details[[i]][[j]]$result$international_phone_number))){
        matched_google_data$phone[iterator] <- google_place_details[[i]][[j]]$result$international_phone_number
      }
      matched_google_data$rating[iterator] <- google_data[[i]]$results$rating[j]
      if(!(is.null(google_place_details[[i]][[j]]$result$opening_hours))){
        matched_google_data$hours_monday[iterator] <- word(google_place_details[[i]][[j]]$result$opening_hours$weekday_text[1], 2, sep = fixed(": "))
        matched_google_data$hours_tuesday[iterator] <- word(google_place_details[[i]][[j]]$result$opening_hours$weekday_text[2], 2, sep = fixed(": "))
        matched_google_data$hours_wednesday[iterator] <- word(google_place_details[[i]][[j]]$result$opening_hours$weekday_text[3], 2, sep = fixed(": "))
        matched_google_data$hours_thursday[iterator] <- word(google_place_details[[i]][[j]]$result$opening_hours$weekday_text[4], 2, sep = fixed(": "))
        matched_google_data$hours_friday[iterator] <- word(google_place_details[[i]][[j]]$result$opening_hours$weekday_text[5], 2, sep = fixed(": "))
        matched_google_data$hours_saturday[iterator] <- word(google_place_details[[i]][[j]]$result$opening_hours$weekday_text[6], 2, sep = fixed(": "))
        matched_google_data$hours_sunday[iterator] <- word(google_place_details[[i]][[j]]$result$opening_hours$weekday_text[7], 2, sep = fixed(": "))
      }
      matched_google_data$name_score[iterator] <- google_data[[i]]$results$name_score[j]
      matched_google_data$distance[iterator] <- google_data[[i]]$results$distance[j]
      matched_google_data$index[iterator] <- i
      matched_google_data$score[iterator,] <- google_data[[i]]$results$score[j,]
    }
  }
}

matched_google_data <- matched_google_data[1:317,]

# Separating fully matched data
score <- data.frame(name_score=integer(700),
                    distance_score=integer(700),
                    phone_score=integer(700),
                    category_score=integer(700),
                    sum_score = integer(700),
                    matching = character(700))
score <- transform(score, name_score=as.integer(name_score),
                   distance_score=as.integer(distance_score),
                   phone_score=as.integer(phone_score),
                   category_score=as.integer(category_score),
                   sum_score=as.integer(sum_score),
                   matching=as.character(matching))
fully_matched <- data.frame(provider = character(700),
                            name = character(700),
                            street = character(700),
                            city = character(700),
                            state = character(700),
                            zip = character(700),
                            country = character(700),
                            latitude = integer(700),
                            longitude = integer(700),
                            categories = character(700),
                            phone = character(700),
                            hours_monday=character(700),
                            hours_tuesday=character(700),
                            hours_wednesday=character(700),
                            hours_thursday=character(700),
                            hours_friday=character(700),
                            hours_saturday=character(700),
                            hours_sunday=character(700),
                            name_score = integer(700),
                            distance = integer(700),
                            index = integer(700))
fully_matched <- transform(fully_matched, provider=as.character(provider), 
                           name=as.character(name),
                           street=as.character(street),
                           city=as.character(city),
                           state=as.character(state),
                           zip=as.character(zip),
                           country=as.character(country),
                           latitude=as.character(latitude),
                           longitude=as.character(longitude),
                           categories=as.character(categories),
                           phone=as.character(phone),
                           hours_monday=as.character(hours_monday),
                           hours_tuesday=as.character(hours_tuesday),
                           hours_wednesday=as.character(hours_wednesday),
                           hours_thursday=as.character(hours_thursday),
                           hours_friday=as.character(hours_friday),
                           hours_saturday=as.character(hours_saturday),
                           hours_sunday=as.character(hours_sunday),
                           name_score=as.integer(name_score),
                           distance=as.integer(distance),
                           index=as.integer(index))
fully_matched$score <- score
# facebook_data$yelp_layer <- NULL
# matched_google_data$yelp_layer <- NULL
iterator <- 0
for(i in seq(1, nrow(matched_google_data))){
  for(j in seq(1, nrow(facebook_data))){
    if(matched_google_data$index[i] == facebook_data$index[j]){
      # Google data
      iterator <- iterator + 1
      fully_matched$provider[iterator] <- "Google"
      fully_matched$name[iterator] <- matched_google_data$name[i]
      fully_matched$street[iterator] <- matched_google_data$street[i]
      fully_matched$city[iterator] <- matched_google_data$city[i]
      fully_matched$state[iterator] <- matched_google_data$state[i]
      fully_matched$zip[iterator] <- matched_google_data$zip[i]
      fully_matched$country[iterator] <- matched_google_data$country[i]
      fully_matched$latitude[iterator] <- matched_google_data$latitude[i]
      fully_matched$longitude[iterator] <- matched_google_data$longitude[i]
      fully_matched$categories[iterator] <- matched_google_data$categories[i]
      fully_matched$phone[iterator] <- matched_google_data$phone[i]
      fully_matched$hours_monday[iterator] <- matched_google_data$hours_monday[i]
      fully_matched$hours_tuesday[iterator] <- matched_google_data$hours_tuesday[i]
      fully_matched$hours_wednesday[iterator] <- matched_google_data$hours_wednesday[i]
      fully_matched$hours_thursday[iterator] <- matched_google_data$hours_thursday[i]
      fully_matched$hours_friday[iterator] <- matched_google_data$hours_friday[i]
      fully_matched$hours_saturday[iterator] <- matched_google_data$hours_saturday[i]
      fully_matched$hours_sunday[iterator] <- matched_google_data$hours_sunday[i]
      fully_matched$name_score[iterator] <- matched_google_data$name_score[i]
      fully_matched$distance[iterator] <- matched_google_data$distance[i]
      fully_matched$index[iterator] <- matched_google_data$index[i]
      fully_matched$score[iterator,] <- matched_google_data$score[i,]
      # Facebook data
      iterator <- iterator + 1
      fully_matched$provider[iterator] <- "Facebook"
      fully_matched$name[iterator] <- facebook_data$name[j]
      fully_matched$street[iterator] <- facebook_data$street[j]
      fully_matched$city[iterator] <- facebook_data$city[j]
      fully_matched$state[iterator] <- facebook_data$state[j]
      fully_matched$zip[iterator] <- facebook_data$zip[j]
      fully_matched$country[iterator] <- facebook_data$country[j]
      fully_matched$latitude[iterator] <- facebook_data$latitude[j]
      fully_matched$longitude[iterator] <- facebook_data$longitude[j]
      fully_matched$categories[iterator] <- facebook_data$categories[j]
      fully_matched$phone[iterator] <- facebook_data$phone[j]
      fully_matched$hours_monday[iterator] <- facebook_data$hours_monday[j]
      fully_matched$hours_tuesday[iterator] <- facebook_data$hours_tuesday[j]
      fully_matched$hours_wednesday[iterator] <- facebook_data$hours_wednesday[j]
      fully_matched$hours_thursday[iterator] <- facebook_data$hours_thursday[j]
      fully_matched$hours_friday[iterator] <- facebook_data$hours_friday[j]
      fully_matched$hours_saturday[iterator] <- facebook_data$hours_saturday[j]
      fully_matched$hours_sunday[iterator] <- facebook_data$hours_sunday[j]
      fully_matched$name_score[iterator] <- facebook_data$name_score[j]
      fully_matched$distance[iterator] <- facebook_data$distance[j]
      fully_matched$index[iterator] <- facebook_data$index[j]
      fully_matched$score[iterator,] <- facebook_data$score[j,]
      # Yelp data
      iterator <- iterator + 1
      fully_matched$provider[iterator] <- "Yelp"
      fully_matched$name[iterator] <- sample_data$name[facebook_data$index[j]]
      fully_matched$street[iterator] <- sample_data$address[facebook_data$index[j]]
      fully_matched$city[iterator] <- sample_data$city[facebook_data$index[j]]
      fully_matched$state[iterator] <- sample_data$state[facebook_data$index[j]]
      fully_matched$zip[iterator] <- sample_data$postal_code[facebook_data$index[j]]
      fully_matched$latitude[iterator] <- sample_data$latitude[facebook_data$index[j]]
      fully_matched$longitude[iterator] <- sample_data$longitude[facebook_data$index[j]]
      fully_matched$categories[iterator] <- sample_data$categories[facebook_data$index[j]]
      fully_matched$phone[iterator] <- paste0(sample_data$display_phone[facebook_data$index[j]], ", ", sample_data$international_phone[facebook_data$index[j]])
      fully_matched$hours_monday[iterator] <- sample_data$hours_monday[facebook_data$index[j]]
      fully_matched$hours_tuesday[iterator] <- sample_data$hours_tuesday[facebook_data$index[j]]
      fully_matched$hours_wednesday[iterator] <- sample_data$hours_wendsday[facebook_data$index[j]]
      fully_matched$hours_thursday[iterator] <- sample_data$hours_thursday[facebook_data$index[j]]
      fully_matched$hours_friday[iterator] <- sample_data$hours_friday[facebook_data$index[j]]
      fully_matched$hours_saturday[iterator] <- sample_data$hours_saturday[facebook_data$index[j]]
      fully_matched$hours_sunday[iterator] <- sample_data$hours_monday[facebook_data$index[j]]
      fully_matched$index[iterator] <- facebook_data$index[j]
      break
    }
  }
}
fully_matched <- fully_matched[1:615,]

i <- 1
index_name_bad_match <- as.integer(NA)
index_distance_bad <- as.integer(NA)
index_category_bad <- as.integer(NA)
index_phone_bad <- as.integer(NA)
while (i<=615) {
  if((fully_matched$name_score[i] < 85) | (fully_matched$name_score[i+1] < 85)){
    index_name_bad_match <- c(index_name_bad_match, i, i+1, i+2)
  }
  if((fully_matched$score$distance_score[i] < 10) | (fully_matched$score$distance_score[i+1] < 10)){
    index_distance_bad <- c(index_distance_bad, i, i+1, i+2)
  }
  if((fully_matched$score$phone_score[i] == 0) | (fully_matched$score$phone_score[i+1] == 0)){
    index_phone_bad <- c(index_phone_bad, i, i+1, i+2)
  }
  if((fully_matched$score$category_score[i] == 0) | (fully_matched$score$category_score[i+1] == 0)){
    index_category_bad <- c(index_category_bad, i, i+1, i+2)
  }
  i <- i + 3
}
name_missmatch <- fully_matched[index_name_bad_match, ]
distance_missmatch <- fully_matched[index_distance_bad, ]
phone_missmatch <- fully_matched[index_phone_bad, ]
category_missmatch <- fully_matched[index_category_bad, ]

write.table(name_missmatch, file = "name_missmatch.csv", sep = "\t", row.names = FALSE, na = "")
write.table(distance_missmatch, file = "distance_missmatch.csv", sep = "\t", row.names = FALSE, na = "")
write.table(phone_missmatch, file = "phone_missmatch.csv", sep = "\t", row.names = FALSE, na = "")
write.table(category_missmatch, file = "category_missmatch.csv", sep = "\t", row.names = FALSE, na = "")

####################################### 
# Checking for potentially matched data

# Google bucket
indices_both_matched <- integer(205)
iterator <- 0
for(i in seq(1, nrow(matched_google_data))){
  for(j in seq(1, nrow(facebook_data))){
    if(matched_google_data$index[i] == facebook_data$index[j]){
      iterator <- iterator + 1
      indices_both_matched[iterator] <- facebook_data$index[j]
    }
  }
}

indices_potentially_google <- which(!(matched_google_data$index[] %in% indices_both_matched))
score <- data.frame(name_score=integer(2500),
                    distance_score=integer(2500),
                    phone_score=integer(2500),
                    category_score=integer(2500),
                    sum_score = integer(2500),
                    matching = character(2500))
score <- transform(score, name_score=as.integer(name_score),
                   distance_score=as.integer(distance_score),
                   phone_score=as.integer(phone_score),
                   category_score=as.integer(category_score),
                   sum_score=as.integer(sum_score),
                   matching=as.character(matching))
potentially_google_match <- data.frame(provider = character(2500),
                            name = character(2500),
                            street = character(2500),
                            city = character(2500),
                            state = character(2500),
                            zip = character(2500),
                            country = character(2500),
                            latitude = integer(2500),
                            longitude = integer(2500),
                            categories = character(2500),
                            phone = character(2500),
                            hours_monday=character(2500),
                            hours_tuesday=character(2500),
                            hours_wednesday=character(2500),
                            hours_thursday=character(2500),
                            hours_friday=character(2500),
                            hours_saturday=character(2500),
                            hours_sunday=character(2500),
                            name_score = integer(2500),
                            distance = integer(2500),
                            index = integer(2500))
potentially_google_match <- transform(potentially_google_match, provider=as.character(provider), 
                           name=as.character(name),
                           street=as.character(street),
                           city=as.character(city),
                           state=as.character(state),
                           zip=as.character(zip),
                           country=as.character(country),
                           latitude=as.character(latitude),
                           longitude=as.character(longitude),
                           categories=as.character(categories),
                           phone=as.character(phone),
                           hours_monday=as.character(hours_monday),
                           hours_tuesday=as.character(hours_tuesday),
                           hours_wednesday=as.character(hours_wednesday),
                           hours_thursday=as.character(hours_thursday),
                           hours_friday=as.character(hours_friday),
                           hours_saturday=as.character(hours_saturday),
                           hours_sunday=as.character(hours_sunday),
                           name_score=as.integer(name_score),
                           distance=as.integer(distance),
                           index=as.integer(index))
potentially_google_match$score <- score

iterator <- 0
for(i in indices_potentially_google){
  # Google data
  iterator <- iterator + 1
  potentially_google_match$provider[iterator] <- "Google"
  potentially_google_match$name[iterator] <- matched_google_data$name[i]
  potentially_google_match$street[iterator] <- matched_google_data$street[i]
  potentially_google_match$city[iterator] <- matched_google_data$city[i]
  potentially_google_match$state[iterator] <- matched_google_data$state[i]
  potentially_google_match$zip[iterator] <- matched_google_data$zip[i]
  potentially_google_match$country[iterator] <- matched_google_data$country[i]
  potentially_google_match$latitude[iterator] <- matched_google_data$latitude[i]
  potentially_google_match$longitude[iterator] <- matched_google_data$longitude[i]
  potentially_google_match$categories[iterator] <- matched_google_data$categories[i]
  potentially_google_match$phone[iterator] <- matched_google_data$phone[i]
  potentially_google_match$hours_monday[iterator] <- matched_google_data$hours_monday[i]
  potentially_google_match$hours_tuesday[iterator] <- matched_google_data$hours_tuesday[i]
  potentially_google_match$hours_wednesday[iterator] <- matched_google_data$hours_wednesday[i]
  potentially_google_match$hours_thursday[iterator] <- matched_google_data$hours_thursday[i]
  potentially_google_match$hours_friday[iterator] <- matched_google_data$hours_friday[i]
  potentially_google_match$hours_saturday[iterator] <- matched_google_data$hours_saturday[i]
  potentially_google_match$hours_sunday[iterator] <- matched_google_data$hours_sunday[i]
  potentially_google_match$name_score[iterator] <- matched_google_data$name_score[i]
  potentially_google_match$distance[iterator] <- matched_google_data$distance[i]
  potentially_google_match$index[iterator] <- matched_google_data$index[i]
  potentially_google_match$score[iterator,] <- matched_google_data$score[i,]
  
  # Facebook data
  sample_index <- matched_google_data$index[i]
  for(fb_index in seq(1, nrow(fb_places[[sample_index]]$data))){
    iterator <- iterator + 1
    potentially_google_match$provider[iterator] <- "Facebook"
    potentially_google_match$name[iterator] <- fb_places[[sample_index]]$data$name[fb_index]
    potentially_google_match$street[iterator] <- fb_places[[sample_index]]$data$location$street[fb_index]
    potentially_google_match$city[iterator] <- fb_places[[sample_index]]$data$location$city[fb_index]
    potentially_google_match$state[iterator] <- fb_places[[sample_index]]$data$location$state[fb_index]
    potentially_google_match$zip[iterator] <- fb_places[[sample_index]]$data$location$zip[fb_index]
    potentially_google_match$country[iterator] <- fb_places[[sample_index]]$data$location$country[fb_index]
    potentially_google_match$latitude[iterator] <- fb_places[[sample_index]]$data$location$latitude[fb_index]
    potentially_google_match$longitude[iterator] <- fb_places[[sample_index]]$data$location$longitude[fb_index]
    for(k in seq(1, length(fb_places[[sample_index]]$data$category_list[[fb_index]]$name))){
      if(length(fb_places[[sample_index]]$data$category_list[[fb_index]]$name) == 1){
        potentially_google_match$categories[iterator] <- fb_places[[sample_index]]$data$category_list[[fb_index]]$name[k]
        break
      }
      if(k == 1){
        potentially_google_match$categories[iterator] <- fb_places[[sample_index]]$data$category_list[[fb_index]]$name[k]
        next
      }
      potentially_google_match$categories[iterator] <- paste0(potentially_google_match$categories[iterator], ", ", fb_places[[sample_index]]$data$category_list[[fb_index]]$name[k])
    }
    potentially_google_match$phone[iterator] <- fb_places[[sample_index]]$data$phone[fb_index]
    if(!is.null(fb_places[[sample_index]]$data$hours[[fb_index]])){
      potentially_google_match$hours_monday[iterator] <- paste0(fb_places[[sample_index]]$data$hours[[fb_index]]$value[1], "-", fb_places[[sample_index]]$data$hours[[fb_index]]$value[2])
      potentially_google_match$hours_tuesday[iterator] <- paste0(fb_places[[sample_index]]$data$hours[[fb_index]]$value[3], "-", fb_places[[sample_index]]$data$hours[[fb_index]]$value[4])
      potentially_google_match$hours_wednesday[iterator] <- paste0(fb_places[[sample_index]]$data$hours[[fb_index]]$value[5], "-", fb_places[[sample_index]]$data$hours[[fb_index]]$value[6])
      potentially_google_match$hours_thursday[iterator] <- paste0(fb_places[[sample_index]]$data$hours[[fb_index]]$value[7], "-", fb_places[[sample_index]]$data$hours[[fb_index]]$value[8])
      potentially_google_match$hours_friday[iterator] <- paste0(fb_places[[sample_index]]$data$hours[[fb_index]]$value[9], "-", fb_places[[sample_index]]$data$hours[[fb_index]]$value[10])
      potentially_google_match$hours_saturday[iterator] <- paste0(fb_places[[sample_index]]$data$hours[[fb_index]]$value[11], "-", fb_places[[sample_index]]$data$hours[[fb_index]]$value[12])
      potentially_google_match$hours_sunday[iterator] <- paste0(fb_places[[sample_index]]$data$hours[[fb_index]]$value[13], "-", fb_places[[sample_index]]$data$hours[[fb_index]]$value[14])
    }
    potentially_google_match$name_score[iterator] <- fb_places[[sample_index]]$data$name_score[fb_index]
    potentially_google_match$distance[iterator] <- fb_places[[sample_index]]$data$distance[fb_index]
    potentially_google_match$index[iterator] <- sample_index
    potentially_google_match$score[iterator,] <- fb_places[[sample_index]]$data$score[fb_index,]
  }
}
potentially_google_match <- potentially_google_match[1:2200,]
write.table(potentially_google_match, file = "potentially_google_match.csv", sep = "\t", row.names = FALSE, na = "")


distance_fb_not_matched <- integer(1000)
iterator <- 0
for(i in seq(1, 500)){
  for(j in seq(1, nrow(fb_places[[i]]$data))){
    if(is.null(fb_places[[i]]$data$distance)){
      iterator <- iterator + 1
      distance_fb_not_matched[iterator] <- as.integer(NA)
      break
    }
    if(fb_places[[i]]$data$score$matching[j] == "NOT MATCHED"){
      iterator <- iterator + 1
      distance_fb_not_matched[iterator] <- fb_places[[i]]$data$distance[j]
    }
  }
}

distance_google_not_matched <- integer(1000)
iterator <- 0
for(i in seq(1, 500)){
  if(google_data[[i]]$status == "ZERO_RESULTS"){
    iterator <- iterator + 1
    distance_google_not_matched[iterator] <- as.integer(NA)
    next
  }
  for(j in seq(1, nrow(google_data[[i]]$results))){
    if(is.null(google_data[[i]]$results$distance)){
      iterator <- iterator + 1
      distance_google_not_matched[iterator] <- as.integer(NA)
      break
    }
    if(google_data[[i]]$results$score$matching[j] == "NOT MATCHED"){
      iterator <- iterator + 1
      distance_google_not_matched[iterator] <- google_data[[i]]$results$distance[j]
    }
  }
}
