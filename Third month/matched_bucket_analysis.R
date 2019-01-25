index_name_better_google <- integer(15)
name_better_google <- 0
index_distance_better_google <- integer(23)
distance_better_google <-  0
index_category_better_google <- integer(17)
category_better_google <- 0
index_phone_better_google <- integer(62)
phone_better_google <- 0


index_name_better_facebook <- integer(15)
name_better_facebook <- 0
index_distance_better_facebook <- integer(12)
distance_better_facebook <- 0
index_category_better_facebook <- integer(18)
category_better_facebook <- 0
index_phone_better_facebook <- integer(4)
phone_better_facebook <- 0

name_bad_yelp <- 0
index_name_bad_yelp <- integer(15)
index_location_bad_yelp <- integer(11)
location_bad_yelp <- 0
index_category_bad_yelp <- integer(3)
category_bad_yelp <- 0
index_phone_bad_yelp <- integer(8)
phone_bad_yelp <- 0
for(i in seq(1, nrow(fully_matched), 3)){
  if((fully_matched$name_score[i] >= 88) & (fully_matched$name_score[i+1] < 78)){
    name_better_google <- name_better_google + 1
    index_name_better_google[name_better_google] <- i
  }
  if((fully_matched$name_score[i] < 78) & (fully_matched$name_score[i+1] >= 88)){
    name_better_facebook <- name_better_facebook + 1
    index_name_better_facebook[name_better_facebook] <- i
  }
  if((fully_matched$name_score[i] < 78) & (fully_matched$name_score[i+1] < 78)){
    name_bad_yelp <- name_bad_yelp + 1
    index_name_bad_yelp[name_bad_yelp] <- i
  }
  if((fully_matched$score$distance_score[i] > 10) & (fully_matched$score$distance_score[i+1] <= 10)){
    distance_better_google <- distance_better_google + 1
    index_distance_better_google[distance_better_google] <- i
  }
  if((fully_matched$score$distance_score[i] <= 10) & (fully_matched$score$distance_score[i+1] > 10)){
    distance_better_facebook <- distance_better_facebook + 1
    index_distance_better_facebook[distance_better_facebook] <- i
  }
  if((fully_matched$score$distance_score[i] < 10) & (fully_matched$score$distance_score[i+1] < 10)){
    location_bad_yelp <- location_bad_yelp + 1
    index_location_bad_yelp[location_bad_yelp] <- i
  }
  if((fully_matched$score$category_score[i] == 20) & (fully_matched$score$category_score[i+1] == 0)){
    category_better_google <- category_better_google + 1
    index_category_better_google[category_better_google] <- i
  }
  if((fully_matched$score$category_score[i] == 0) & (fully_matched$score$category_score[i+1] == 20)){
    category_better_facebook <- category_better_facebook + 1
    index_category_better_facebook[category_better_facebook] <- i
  }
  if((fully_matched$score$sum_score[i] == 80) & (fully_matched$score$sum_score[i+1] == 80)
     & (fully_matched$score$category_score[i] == 0) & (fully_matched$score$category_score[i+1] == 0)){
    category_bad_yelp <- category_bad_yelp + 1
    index_category_bad_yelp[category_bad_yelp] <- i
  }
  if((fully_matched$score$phone_score[i] == 10) & (fully_matched$score$phone_score[i+1] == 0)){
    phone_better_google <- phone_better_google + 1
    index_phone_better_google[phone_better_google] <- i
  }
  if((fully_matched$score$phone_score[i] == 0) & (fully_matched$score$phone_score[i+1] == 10)){
    phone_better_facebook <- phone_better_facebook + 1
    index_phone_better_facebook[phone_better_facebook] <- i
  }
  if((fully_matched$score$phone_score[i] == 0) & (fully_matched$score$phone_score[i+1] == 0)){
    phone_bad_yelp <- phone_bad_yelp + 1
    index_phone_bad_yelp[phone_bad_yelp] <- i
  }
}