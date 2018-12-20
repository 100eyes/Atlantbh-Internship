#################################################
# Matching algorithm for osm pois and yelp data #
#################################################

# Names scoring algorithm using Token sort ratio
if(check_availability()){
  
  distance_matrix@data$name_matching_score <- as.integer(NA)
  
  library(fuzzywuzzyR)
  
  init <- FuzzMatcher$new()
  
  for(index in 1:nrow(distance_matrix@data)){
    s1 <- distance_matrix@data$business_name[index]
    s2 <- distance_matrix@data$name[index]
    
    
    distance_matrix@data$name_matching_score[index] <- init$Token_sort_ratio(string1 = s1, string2 = s2, force_ascii = TRUE, full_process = TRUE)
  }
  
}

# Three possible options: matched, partially matched and not matched
# Partially matched has additional params: by_name or by_layer

distance_matrix@data$yelp_layer <- as.character(NA)
distance_matrix@data$osm_layer <- as.character(NA)
distance_matrix@data$matching <- as.character(NA)
distance_matrix@data$additional_matching_params <- as.character(NA)

for(index in 1:nrow(distance_matrix@data)){
  
  yelp_layer1 <- arizona_business$layer1[which(arizona_business$business_id == distance_matrix@data$business_i[index])]
  yelp_layer2 <- arizona_business$layer2[which(arizona_business$business_id == distance_matrix@data$business_i[index])]
  yelp_layer3 <- arizona_business$layer3[which(arizona_business$business_id == distance_matrix@data$business_i[index])]
  
  # making sure that no NA values end up in if statements
  if(is.na(yelp_layer2)){yelp_layer2 <- yelp_layer1}
  if(is.na(yelp_layer3)){yelp_layer3 <- yelp_layer1}
  
  if(is.na(yelp_layer1)){
    # if yelp point has no layer perform matching based only on name matching score
    if(distance_matrix@data$name_matching_score[index] < 50){
      distance_matrix@data$yelp_layer[index] <- yelp_layer1
      distance_matrix@data$osm_layer[index] <- osm_layer
      distance_matrix@data$matching[index] <- "not matched"
    }
    if((distance_matrix@data$name_matching_score[index] > 50) & (distance_matrix@data$name_matching_score[index] < 95)){
      # if there is no yelp layer, points cannot be matched, only partially matched
      distance_matrix@data$yelp_layer[index] <- yelp_layer1
      distance_matrix@data$osm_layer[index] <- osm_layer
      distance_matrix@data$matching[index] <- "partially matched"
      distance_matrix@data$additional_matching_params[index] <- "by name"
    }
    if(distance_matrix@data$name_matching_score[index] >= 95){
      distance_matrix@data$yelp_layer[index] <- yelp_layer1
      distance_matrix@data$osm_layer[index] <- osm_layer
      distance_matrix@data$matching[index] <- "matched"
      distance_matrix@data$additional_matching_params[index] <- "by name"
    }
    next
  }
  
  osm_layer <- pois_data$layer[which((pois_data$osm_id == distance_matrix@data$osm_id[index]) & (pois_data$fclass == distance_matrix@data$fclass[index]))]
  
  # conditions for not matched points
  if((distance_matrix@data$name_matching_score[index] < 50) & !(is.na(distance_matrix@data$name[index]))){
    distance_matrix@data$yelp_layer[index] <- yelp_layer1
    distance_matrix@data$osm_layer[index] <- osm_layer
    distance_matrix@data$matching[index] <- "not matched"
  }
  
  # conditions for partially matching by name and second case of not matched points
  if((distance_matrix@data$name_matching_score[index] > 50) & (distance_matrix@data$name_matching_score[index] < 90)){
    
    if((osm_layer == yelp_layer1) | (osm_layer == yelp_layer2) | (osm_layer == yelp_layer3)){
      # partially matched by name
      if(osm_layer == yelp_layer1){distance_matrix@data$yelp_layer[index] <- yelp_layer1}
      if(osm_layer == yelp_layer2){distance_matrix@data$yelp_layer[index] <- yelp_layer2}
      if(osm_layer == yelp_layer3){distance_matrix@data$yelp_layer[index] <- yelp_layer3}
      distance_matrix@data$osm_layer[index] <- osm_layer
      distance_matrix@data$matching[index] <- "partially matched"
    } else {
      # partially matched name but layers don't match
      # implies that points are not matched
      distance_matrix@data$yelp_layer[index] <- yelp_layer1
      distance_matrix@data$osm_layer[index] <- osm_layer
      distance_matrix@data$matching[index] <- "partially matched"
      distance_matrix@data$additional_matching_params[index] <- "by name"
    }
  }
  
  # Partially matched by layer
  # OSM point has no name but layers in yelp and OSM match
  if(is.na(distance_matrix@data$name[index])){
    if((osm_layer == yelp_layer1) | (osm_layer == yelp_layer2) | (osm_layer == yelp_layer3)){
      if(osm_layer == yelp_layer1){distance_matrix@data$yelp_layer[index] <- yelp_layer1}
      if(osm_layer == yelp_layer2){distance_matrix@data$yelp_layer[index] <- yelp_layer2}
      if(osm_layer == yelp_layer3){distance_matrix@data$yelp_layer[index] <- yelp_layer3}
      distance_matrix@data$osm_layer[index] <- osm_layer
      distance_matrix@data$matching[index] <- "partially matched"
      distance_matrix@data$additional_matching_params[index] <- "by layer"
    } else {
      # if layers are not same and there is no osm point name
      # points cannot be matched
      distance_matrix@data$yelp_layer[index] <- yelp_layer1
      distance_matrix@data$osm_layer[index] <- osm_layer
      distance_matrix@data$matching[index] <- "not matched"
    }
  }
  
  # condition for matched points and second condition for partially matched points by name
  if(distance_matrix@data$name_matching_score[index] > 90){
    if((osm_layer == yelp_layer1) | (osm_layer == yelp_layer2) | (osm_layer == yelp_layer3)){
      # if name matching score is above 90 and layers match
      # points from yelp and osm are matched
      if(osm_layer == yelp_layer1){distance_matrix@data$yelp_layer[index] <- yelp_layer1}
      if(osm_layer == yelp_layer2){distance_matrix@data$yelp_layer[index] <- yelp_layer2}
      if(osm_layer == yelp_layer3){distance_matrix@data$yelp_layer[index] <- yelp_layer3}
      distance_matrix@data$osm_layer[index] <- osm_layer
      distance_matrix@data$matching[index] <- "matched"
      next
    }
    if(distance_matrix@data$name_matching_score[index] >= 95){
      distance_matrix@data$yelp_layer[index] <- yelp_layer1
      distance_matrix@data$osm_layer[index] <- osm_layer
      distance_matrix@data$matching[index] <- "matched"
      distance_matrix@data$additional_matching_params[index] <- "by name"
      next
    } else {
      distance_matrix@data$yelp_layer[index] <- yelp_layer1
      distance_matrix@data$osm_layer[index] <- osm_layer
      distance_matrix@data$matching[index] <- "partially matched"
      distance_matrix@data$additional_matching_params[index] <- "by name"
    }
  }
}









