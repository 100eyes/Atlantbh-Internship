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
                            hours_key=character(700),
                            hours_value=character(700),
                            name_score=integer(700),
                            distance=integer(700))
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
                           hours_key=as.character(hours_key),
                           hours_value=as.character(hours_value),
                           name_score=as.integer(name_score),
                           distance=as.integer(distance))
facebook_data$score <- score

iterator <- 1
for(i in seq(1, 500)){
  if(is.null(fb_places[[i]]$data$name_score)){
    next
  }
  for(j in seq(1, nrow(fb_places[[i]]$data))){
    if(fb_places[[i]]$data$score$matching == "MATCHED"){
      facebook_data$name[iterator] <- fb_places[[i]]$data$name[j]
      facebook_data$
    }
  }
}


