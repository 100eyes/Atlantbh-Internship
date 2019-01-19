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


