# Popularization of layers in arizona_business table
for(i in 1:nrow(arizona_categories)){
  for(j in 3:ncol(arizona_categories)){
    
    # Skip iteration if there is no category in arizona_categories
    if(is.na(arizona_categories[i,j])){
      next
    }
    
    for(k in 1:nrow(categories)){
      
      # If there is no layer in current category skip this iteration
      if(is.na(categories$layer[k])){
        next
      }

      if(arizona_categories[i,j] == categories$title[k]){
        
        # First layer vaule
        # if it doesn't exist yet, copy categories$layer in it and break 
        # categories for loop (third one)
        if(is.na(arizona_business$layer1[i])){
          arizona_business$layer1[i] <- categories$layer[k]
          break
        }
        
        # Second layer value
        # if it is NA and it's not included in previus layer
        # copy categories$layer in it and break
        if((is.na(arizona_business$layer2[i])) & (arizona_business$layer1[i] != categories$layer[k])){
          arizona_business$layer2[i] <- categories$layer[k]
          break
        }
        
        # Third layer value
        # if it isn't NA and not equal to two previus layers
        # copy value of categories$layer in it and break
        if((is.na(arizona_business$layer3[i])) & (arizona_business$layer1[i] != categories$layer[k]) & (arizona_business$layer2[i] != categories$layer[k])){
          arizona_business$layer3[i] <- categories$layer[k]
          break
        }
      }
    }
    
    # If all three layers have value
    # go to the next row
    if(!(is.na(arizona_business$layer3[i]))){
      break
    }
  }
}

# Popularization of layers in pois_data table
library(stringr)
pois_data$str_code <- str_extract(pois_data$str_code[], "^.{2}")
for(index in 1:nrow(pois_data)){
  if(pois_data$str_code[index] == "20"){
    pois_data$layer[index] <- "public"
    next
  }
  if(pois_data$str_code[index] == "21"){
    pois_data$layer[index] <- "health"
    next
  }
  if(pois_data$str_code[index] == "22"){
    pois_data$layer[index] <- "leisure"
    next
  }
  if(pois_data$str_code[index] == "23"){
    pois_data$layer[index] <- "catering"
    next
  }
  if(pois_data$str_code[index] == "24"){
    pois_data$layer[index] <- "accommodation"
    next
  }
  if(pois_data$str_code[index] == "25"){
    pois_data$layer[index] <- "shopping"
    next
  }
  if(pois_data$str_code[index] == "26"){
    pois_data$layer[index] <- "money"
    next
  }
  if(pois_data$str_code[index] == "27"){
    pois_data$layer[index] <- "tourism"
    next
  }
  if(pois_data$str_code[index] == "29"){
    pois_data$layer[index] <- "miscpoi"
    next
  }
}