# Library for converting binary numbers to decimal numbers
library(compositions)

# Creating two new columns for scoring and quality
yelp_business$score <- NA
yelp_business$quality <- NA

binary_score <- ""

# Highest score is 255 (or 11111111 in binary) and lowest is 0 (00000000 in binary)
for (x in 1:nrow(yelp_business)) 
{
  # If restaurant is closed, it loses timeliness quality and it is irrelevant to us
  # so if restaurant is closed (is.open == 0) restaurant has LOW quality
  if(yelp_business$is_open[x] == 0)
  {
    yelp_business$quality[x] <- "LOW"
    yelp_business$score[x] <- 0
  } else
  {
    # Testing for longitude and latitude
    # Those two attributes have highest value, so they are most signicant beats (MSB)
    if((is.na(yelp_business$latitude[x])) | (is.na(yelp_business$longitude[x])))
    {
      binary_score <- paste(binary_score, "0", sep = "")
    } else
    {
      binary_score <- paste(binary_score, "1", sep = "")
    }
    
    # Testing for addresses
    if(is.na(yelp_business$address[x]))
    {
      binary_score <- paste(binary_score, "0", sep = "")
    } else
    {
      binary_score <- paste(binary_score, "1", sep = "")
    }
    
    # Testing for city
    if(is.na(yelp_business$city[x]))
    {
      binary_score <- paste(binary_score, "0", sep = "")
    } else
    {
      binary_score <- paste(binary_score, "1", sep = "")
    }
    
    # Testing for postal code
    if(is.na(yelp_business$postal_code[x]))
    {
      binary_score <- paste(binary_score, "0", sep = "")
    } else
    {
      binary_score <- paste(binary_score, "1", sep = "")
    }
    
    # Testing for state
    if(is.na(yelp_business$state[x]))
    {
      binary_score <- paste(binary_score, "0", sep = "")
    } else
    {
      binary_score <- paste(binary_score, "1", sep = "")
    }
    
    # Testing for category
    if(is.na(yelp_business$categories[x]))
    {
      binary_score <- paste(binary_score, "0", sep = "")
    } else
    {
      binary_score <- paste(binary_score, "1", sep = "")
    }
    
    # Testing for neighborhood
    if(is.na(yelp_business$neighborhood[x]))
    {
      binary_score <- paste(binary_score, "0", sep = "")
    } else
    {
      binary_score <- paste(binary_score, "1", sep = "")
    }
    
    # Testing for hours
    if((is.na(yelp_business$hours_monday[x])) | (is.na(yelp_business$hours_tuesday[x])) | (is.na(yelp_business$hours_wendsday[x]))
       | (is.na(yelp_business$hours_thursday[x])) | is.na(yelp_business$hours_friday[x])
       | (is.na(yelp_business$hours_saturday[x])) | (is.na(yelp_business$hours_sunday[x])))
    {
      binary_score <- paste(binary_score, "0", sep = "")
    } else
    {
      binary_score <- paste(binary_score, "1", sep = "")
    }
    
    # Converting score to decimal value
    yelp_business$score[x] <- unbinary(binary_score)
    binary_score <- ""
  }
  
  # Records with score less then first quantile 
  if(yelp_business$score[x] <= 252)
  {
    yelp_business$quality[x] <- "LOW"
  }
  
  # Records with score between first and third quantile
  if((yelp_business$score[x] > 252) & (yelp_business$score[x] < 254))
  {
    yelp_business$quality[x] <- "MEDIUM"
  }
  
  # Records with score larger then third quantile
  if(yelp_business$score[x] >= 254)
  {
    yelp_business$quality[x] <- "HIGH"
  }
  
  print(x) # to show current iteration 
}

num_high_qua <- nrow(yelp_business[which(yelp_business$quality == "HIGH"),]) 
num_medium_qua <- nrow(yelp_business[which(yelp_business$quality == "MEDIUM"),])
num_low_qua <- nrow(yelp_business[which(yelp_business$quality == "LOW"),])

write.table(yelp_business, file = "yelp_business.csv", sep = "#", row.names = FALSE)

