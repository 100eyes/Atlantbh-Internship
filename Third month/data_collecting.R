# Collecting facebook places data
list_of_fb_places <- vector("list", 135)
list_of_fb_places1 <- vector("list", 200)
list_of_fb_places2 <- vector("list", 136)
acces_token = "EAARFvAknMHkBAODkqgpu3pac33HsZAjS6aETGP84wZAvMWOLgDp49oyToeTj4WNpDpXDfHLpNXWZCPmkJrZBzuOWbZCTMMqePaVO9NsHpZC7B424h8BD48xvZCZCrFfA0oOfHjOuZB1D3sif2wt71YUS3G1WqzLZB06XAZD"
for(i in seq(1, 136)){
  list_of_fb_places2[[i]] <- fromJSON(paste0("https://graph.facebook.com/v3.2/search?access_token=", acces_token, "&type=place&fields=category_list,hours,is_always_open,is_permanently_closed,location,name,overall_star_rating,parking,payment_options,phone,price_range,rating_count,restaurant_services&center=", sample_data$latitude[i+364], ",", sample_data$longitude[i+364], "&distance=100"))  
}
fb_places <- vector("list", 0)

# Collecting Google places data
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