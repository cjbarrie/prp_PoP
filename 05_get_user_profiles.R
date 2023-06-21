library(dplyr)
library(academictwitteR)
library(tidygeocoder)
library(tidylog)
set.seed(123L)

# tweets available from: https://www.dropbox.com/s/edo3ih1rykbrmjk/twts_all.csv?dl=0. Contact author for password.
twts <- read_csv("data/raw/twts_all.csv")
## Generate new dataset with just tweet, tweeter, and date
shtwts <- subset(twts, select = c(V3,V5, V8))
## Rename columns
names(shtwts) <- c("tweet", "username", "date")

usernames <- unique(shtwts$username)
# remove faulty ingest rows
usernames <- usernames[-c(1207, 1710, 1711, 1712,4864, 5807, 5811, 6710, 6711, 6712, 6715, 6716, 6717, 8878)]
# usernames <- sample(usernames, 1000)

userids <- c()
# usernames <- usernames[3086:length(usernames)]
for (i in seq_along(usernames)) {
  
  cat("Getting user ID for username ", usernames[[i]], " number ", i, "of ", length(usernames), "\n" )
  
  userid <- get_user_id(usernames[[i]])
  userids <- cbind(userid, userids)
}

saveRDS(userids, "data/output/userids.rds")

userids <- userids[!is.na(userids)]
userids <- userids[2995:length(userids)]
# profiles <- data.frame()
for (i in seq_along(userids)) {
  
  cat("Getting user proofile for user ID ", userids[[i]], " number ", i, "of ", length(userids), "\n" )
  uprof <- get_user_profile(userids[[i]])
  profiles <- bind_rows(profiles, uprof)
}

saveRDS(profiles, "data/output/userprofs.rds")

# geocode the addresses
lat_longs <- profiles %>%
  geocode(location, method = 'osm', 
          lat = latitude , long = longitude, full_results = T)

saveRDS(lat_longs, "data/output/latlongs.rds")

reverse <- lat_longs %>%
  reverse_geocode(lat = latitude, long = longitude, method = 'osm',
                  address = address_found, full_results = TRUE)

saveRDS(reverse, "data/output/reversegeo.rds")