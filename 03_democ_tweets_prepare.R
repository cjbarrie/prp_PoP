library(tidyverse)
library(tidytext)
library(stringr)
library(scales)
library(reshape2)
library(readr)
library(lubridate)
library(arabicStemR)
library(ggthemes)

# tweets available from: https://www.dropbox.com/s/edo3ih1rykbrmjk/twts_all.csv?dl=0. Contact author for password.
twts <- read_csv("data/raw/twts_all.csv")
## Generate new dataset with just tweet, tweeter, and date
shtwts <- subset(twts, select = c(V3,V5, V7, V8))
## Rename columns
names(shtwts) <- c("tweet", "username", "tweet_id", "date")

## Cut date string to just date (not time)
shtwts$date <- substr(shtwts$date, 5, 16)
## Reformat to R readable date
shtwts$date <- as.Date(shtwts$date, format = "%d %b %Y")

# Transliterating Arabic tweets for use
shtwts$tweet_trans <- transliterate(shtwts$tweet)

saveRDS(shtwts, "data/output/twts_transliterated.rds")