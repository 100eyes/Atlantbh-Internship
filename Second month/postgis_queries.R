library(RPostgres)
library(DBI)
library(RPostgreSQL)
library(postGIStools)

pw<- {
  "postgres"
}

drv <- dbDriver("PostgreSQL")

con <- RPostgreSQL::dbConnect(drv, host = 'localhost', dbname = 'postgres', user = 'postgres', password = pw, port = 5432)


rm(pw) # removes the password

write.table(business[,1:20], file = "imp_business.csv", sep = "#", col.names = FALSE, row.names = FALSE)
write.table(arizona_business[,1:20], file = "arizona_business.csv", sep = "#", row.names = FALSE)

dbTableInfo(con, 'az_business', allinfo = TRUE)

dbVacuum(conn = con, 'az_business', full = FALSE, verbose = TRUE)
dbVacuum(conn = con, 'pois', full = FALSE, verbose = TRUE)

checking <- dbReadDataFrame(conn = con, name = "az_business")
all.equal(arizona_business[,1:20], checking[2:21])
pgListGeom(conn = con)
pgPostGIS(con)

result <- get_postgis_query(con, "SELECT * FROM pois LIMIT 100", geom_name = "geom")
result_az <- get_postgis_query(con, "SELECT * FROM az_business LIMIT 100", geom_name = "geom")

pois_data <- osm_pois@data

selected_yelp_business <- get_postgis_query(con, "SELECT * FROM near_yelp_business", geom_name = "geom")
selected_osm_pois <- get_postgis_query(con, "SELECT * FROM near_pois", geom_name = "geom")

arizona_categories <- data.frame("business_id" = arizona_business$business_id, "business_name" = arizona_business$name, "cat1" = NA,  "cat2" = NA, "cat3" = NA, "cat4" = NA, "cat5" = NA, "cat6" = NA, "cat7" = NA, "cat8" = NA, "cat9" = NA, "cat10" = NA)

for (i in 1:nrow(arizona_business)) {
  arizona_categories[i, 3:12] <- word(arizona_business$categories[i], 1:10, sep = fixed(", "))
}

unique_categories <- unique(data.frame(all_cat = union(categories$cat1, union(categories$cat2, union(categories$cat3, union(categories$cat4, union(categories$cat5, union(categories$cat6, union(categories$cat7, union(categories$cat8, union(categories$cat9, categories$cat10)))))))))))
osm_pois <- get_postgis_query(con, "SELECT * FROM pois", geom_name = "geom")
unique_osm_categories <- unique(osm_pois@data$fclass)

library(jsonlite)
# Transfering .json format into .Rdata format
setwd("~/Documents")
file_name = "categories.json"
all_yelp_categories<-stream_in(textConnection(readLines(file_name, n=1197)),verbose=T)

write.table(categories, file = "unique_yelp_categories.csv", sep = "\t", row.names = FALSE)
write.table(arizona_business, file = "arizona_business_with_layers.csv", sep = "\t", row.names = FALSE)

distance_matrix <- get_postgis_query(con, "SELECT * FROM distance_matrix", geom_name = "geom")

library(rpostgis)
pgInsert(con, name = c("public", "distance_matrix"), data.obj = distance_matrix, overwrite = TRUE)

grouped_data <- get_postgis_query(con, "SELECT * FROM grouped_poi_yelp", geom_name = "geom")
