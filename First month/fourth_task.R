# Training and test sets for linear regression
yelp_business$scaled_review_count <- scale(yelp_business$review_count, center = TRUE, scale = max(yelp_business$review_count))
end_of_training <- nrow(yelp_business) -  floor(nrow(yelp_business)/4)
training_set <- yelp_business[1:end_of_training,]
test_set <- yelp_business[end_of_training:nrow(yelp_business),]

# Building linear model and checking correlation
cor_factor <- cor(yelp_business$stars, yelp_business$review_count)
model = lm(stars ~ review_count, data = training_set)
model2 <- lm(stars ~ scaled_review_count + restaurants_price_range_2, data = training_set, na.action = na.omit)
model3 <- lm(stars ~ scaled_review_count + is_open, data = training_set, na.action = na.omit)
summary(model)
plot(cooks.distance(model))
summary(model2)
summary(model3)
predict_test <- predict(model, newdata = test_set)
predict_test3 <- predict(model3, newdata = test_set)
summary(predict_test)
summary(test_set$stars)

# Calculating error of prediction for first model
sse <- sum((test_set$stars - predict_test)^2)
sst <- sum((test_set$stars - mean(training_set$stars))^2)
r_squared <- 1 - sse/sst

# Calculating error of prediction for third model
sse3 <- sum((test_set$stars - predict_test3)^2)
sst3 <- sum((test_set$stars - mean(training_set$stars))^2)
r_squared3 <- 1 - sse/sst

cor(yelp_business$stars, yelp_business$restaurants_price_range_2, use = "na.or.complete")
boxplot(yelp_business$restaurants_price_range_2 ~ yelp_business$stars)

# Plotting density of stars and review counts
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(yelp_business$stars), main="Density Plot: Stars", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(yelp_business$stars), 2)))  # density plot for 'stars'
polygon(density(yelp_business$stars), col="red")
plot(density(yelp_business$review_count), main="Density Plot: Review count", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(yelp_business$review_count), 2)))  # density plot for 'review count'
polygon(density(yelp_business$review_count), col="red")

