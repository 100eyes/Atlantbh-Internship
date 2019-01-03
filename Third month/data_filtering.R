# 14 - 61 columns poor completeness
imp_col <- seq(1, 21)
business <- yelp_business[which(!is.na(yelp_business$neighborhood)),]
business <- business[,imp_col]

for (i in seq(14,20)) {
  business <- business[which(!is.na(business[,i])),]
}

business <- business[which(business$is_open == 1),]

library(dplyr)
sample_data <- sample_n(business, 500)
