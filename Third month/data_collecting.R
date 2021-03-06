library(jsonlite)

## Collecting facebook places data
list_of_fb_places <- vector("list", 135)
list_of_fb_places1 <- vector("list", 200)
list_of_fb_places2 <- vector("list", 136)
acces_token = "EAARFvAknMHkBAODkqgpu3pac33HsZAjS6aETGP84wZAvMWOLgDp49oyToeTj4WNpDpXDfHLpNXWZCPmkJrZBzuOWbZCTMMqePaVO9NsHpZC7B424h8BD48xvZCZCrFfA0oOfHjOuZB1D3sif2wt71YUS3G1WqzLZB06XAZD"
for(i in seq(1, 136)){
  list_of_fb_places2[[i]] <- fromJSON(paste0("https://graph.facebook.com/v3.2/search?access_token=", acces_token, "&type=place&fields=category_list,hours,is_always_open,is_permanently_closed,location,name,overall_star_rating,parking,payment_options,phone,price_range,rating_count,restaurant_services&center=", sample_data$latitude[i+364], ",", sample_data$longitude[i+364], "&distance=100"))  
}
fb_places <- vector("list", 0)
# Increasing distance for zero results queries
query_count <- 0 # app can't make more than 200 requests
iter_stop <- 0 # if app makes 200 requests, iter_stop remembers what was loop iteration
for(i in seq(1, 500)){
  if(length(fb_places[[i]]$data) == 0){
    fb_places[[i]] <- fromJSON(paste0("https://graph.facebook.com/v3.2/search?access_token=", acces_token, "&type=place&fields=category_list,hours,is_always_open,is_permanently_closed,location,name,overall_star_rating,parking,payment_options,phone,price_range,rating_count,restaurant_services&center=", sample_data$latitude[i], ",", sample_data$longitude[i], "&distance=500"))
    query_count <- query_count + 1
  }
  if(query_count == 200){
    iter_stop <- i
    break
  }
}

## Collecting Google places data
google_data <- vector("list", 500)
google_key <- "AIzaSyDgkKU2ko8K_13xyhU7BTDaOTYTPANWG3o"

for(i in seq(1, nrow(sample_data))){
  keyword <- word(sample_data$name[i], 1, sep = fixed(" "))
  gsub("'", "%27", keyword)
  google_data[[i]] <- fromJSON(paste0("https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=", sample_data$latitude[i], ",", sample_data$longitude[i], "&radius=150&keyword=", keyword, "&key=", google_key))
}

# Place details from Google API
google_place_details <- vector("list", 500)
fields <- paste0("formatted_address,", "geometry,", "place_id,", "permanently_closed,", "type,", "vicinity,", "international_phone_number,", "opening_hours/weekday_text,", "price_level,", "rating")

for(i in seq(1, length(google_data))){
  if(length(google_data[[i]]$results$place_id > 1)){
    google_place_details[[i]] <- vector("list", length(google_data[[i]]$results$place_id))
    for(j in seq(1, length(google_data[[i]]$results$place_id))){
      google_place_details[[i]][[j]] <- fromJSON(paste0("https://maps.googleapis.com/maps/api/place/details/json?placeid=", 
                                                        google_data[[i]]$results$place_id[j],
                                                        "&fields=", fields, "&key=", google_key))
    }
    next
  }
  google_place_details[[i]] <- fromJSON(paste0("https://maps.googleapis.com/maps/api/place/details/json?placeid=", 
                                               google_data[[i]]$results$place_id,
                                               "&fields=", fields, "&key=", google_key))
}

# Getting phone numbers from Yelp
sample_data$international_phone <- NA
sample_data$display_phone <- NA
for(i in seq(407, nrow(sample_data))){
  url <- paste0("https://api.yelp.com/v3/businesses/", sample_data$business_id[i])
  temp_response <- GET(url, add_headers(Authorization = "Bearer MKkF5RHHSNaZP0zQwjw0jHluUER8ySewOTsiyi9cgAutb9bgqauz3T3mbEW4xP7NF8ppmTapUo6uqMzQgBOM_beSD5YhDeaxu6BlA_EPr2zk4dvNen3tfJUErqs4XHYx"))
  if(http_error(temp_response)){
    next
  }
  temp_var <- content(temp_response)
  sample_data$international_phone[i] <- temp_var$phone
  sample_data$display_phone[i] <- temp_var$display_phone
}


