# Enrichment algorithm for OSM poi data with Yelp data

enriched_poi <- get_postgis_query(con, "SELECT * FROM enriched_pois", geom_name = "geom")
pois_before_enrichment <- get_postgis_query(con, "SELECT * FROM pois_before_enrichment", geom_name = "geom")

num_of_businesses <- nrow(enriched_poi@data)
num_of_names_before <- nrow(pois_before_enrichment@data[!(is.na(pois_before_enrichment@data$name)),])
percent_of_new_names <- without_names/num_of_businesses * 100

num_of_improved_names <- nrow(enriched_poi@data[which((enriched_poi$matching == "partially matched") & (enriched_poi@data$additional_matching_params == "by layer")),])
nrow(distance_matrix@data[which(is.na(distance_matrix@data$name) & distance_matrix@data$matching == "partially matched"),])

na_names <- nrow(distance_matrix@data[which(is.na(distance_matrix@data$name)),]) - nrow(distance_matrix@data[which(distance_matrix@data$osm_layer == "miscpoi"),])

all_unique_yelp_businesses <- length(unique(distance_matrix@data$business_i[]))
all_unique_osm_businesses <- length(unique(distance_matrix@data$osm_id[]))

num_of_enriched_pois <- nrow(enriched_poi@data)
