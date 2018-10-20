# Set working directory

setwd("/home/besim/Documents/Praksa")

# Safe copy of database

copy_of_business <- business

# Simplifying nested attribute "hours"

business$hours_tuesday <- business$hours$Tuesday
business$hours_wendsday <- business$hours$Wednesday
business$hours_thursday <- business$hours$Thursday
business$hours_friday <- business$hours$Friday
business$hours_saturday <- business$hours$Saturday
business$hours_sunday <- business$hours$Sunday

# Simplifying nested attribute "attributes"

business$bike_parking <-business$attributes$BikeParking
business$business_accepts_credit_cards <- business$attributes$BusinessAcceptsCreditCards
business$business_parking <- business$attributes$BusinessParking
business$good_for_kids <- business$attributes$GoodForKids
business$has_TV <- business$attributes$HasTV
business$noise_level <- business$attributes$NoiseLevel
business$outdoor_seating <- business$attributes$OutdoorSeating
business$restaurants_attire <- business$attributes$RestaurantsAttire
business$restaurants_delivery <- business$attributes$RestaurantsDelivery
business$restaurants_good_for_groups <- business$attributes$RestaurantsGoodForGroups
business$restaurants_price_range_2 <- business$attributes$RestaurantsPriceRange2
business$restaurants_reservations <- business$attributes$RestaurantsReservations
business$restaurants_takeout <- business$attributes$RestaurantsTakeOut
business$alcohol <- business$attributes$Alcohol
business$caters <- business$attributes$Caters
business$dogs_allowed <- business$attributes$DogsAllowed
business$drive_thru <- business$attributes$DriveThru
business$good_for_meal <- business$attributes$GoodForMeal
business$restaurants_table_service <- business$attributes$RestaurantsTableService
business$wheelchair_accesible <- business$attributes$WheelchairAccessible
business$wifi <- business$attributes$WiFi
business$ambience <- business$attributes$Ambience
business$BYOB <- business$attributes$BYOB
business$BYOB_corkage <- business$attributes$BYOBCorkage
business$best_nights <- business$attributes$BestNights
business$coat_check <- business$attributes$CoatCheck
business$corkage <- business$attributes$Corkage
business$good_for_dancing <- business$attributes$GoodForDancing
business$happy_hour <- business$attributes$HappyHour
business$music <- business$attributes$Music
business$smoking <- business$attributes$Smoking
business$by_appointment_only <- business$attributes$ByAppointmentOnly
business$accepts_insurance <- business$attributes$AcceptsInsurance
business$business_accepts_bitcoin <- business$attributes$BusinessAcceptsBitcoin
business$hair_specializes_in <- business$attributes$HairSpecializesIn
business$ages_allowed <- business$attributes$AgesAllowed
business$restaurants_counter_service <- business$attributes$RestaurantsCounterService
business$open_24_hours <- business$attributes$Open24Hours
business$dietary_restrictions <- business$attributes$DietaryRestrictions

# Deleting nested attributes
business$attributes <- NULL
business$hours <- NULL
