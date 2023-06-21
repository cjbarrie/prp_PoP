library(dplyr)
library(tidylog)
library(ggplot2)
library(readr)
library(RColorBrewer)
library(kableExtra)

# tweets available from: https://www.dropbox.com/s/edo3ih1rykbrmjk/twts_all.csv?dl=0. Contact author for password.
twts <- read_csv("data/raw/twts_all.csv")
## Generate new dataset with just tweet, tweeter, and date
shtwts <- subset(twts, select = c(V3,V5, V8))
## Rename columns
names(shtwts) <- c("tweet", "username", "date")

lat_longs <- readRDS("data/output/latlongs.rds")

world <- map_data("world")
# plot tweets that can be located
shtwtsgeo <- shtwts %>%
  left_join(lat_longs, by = "username") %>%
  filter(!is.na(longitude) & !is.na(latitude))

## Cut date string to just date (not time)
shtwtsgeo$date <- substr(shtwtsgeo$date, 5, 16)
## Reformat to R readable date
shtwtsgeo$date <- as.Date(shtwtsgeo$date, format = "%d %b %Y")

shtwtsgeo <- shtwtsgeo %>%
  drop_na(date)

p <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  )

p + stat_density2d(
  aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
  size = 0.5, bins = 1000, data = shtwtsgeo,
  geom = "polygon"
) +
  scale_fill_gradientn(colours=rev(brewer.pal(7,"Spectral"))) +
  scale_alpha(range = c(.4, .75), guide = FALSE) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.1,.4),
        legend.key.size = unit(2, 'cm'), 
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'), 
        legend.title = element_text(size=15), 
        legend.text = element_text(size=10),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank())

ggsave("data/plots/twtmapstat.png", width=600, height = 400,
       dpi=300, units="mm", bg = "white")

reversegeo <- readRDS("data/output/reversegeo.rds")

#get Tunisia users for robustness
tunusers <- reversegeo %>%
  filter(country == "تونس") %>%
  pull(username)

saveRDS(tunusers, "data/output/tunusers.rds")

# output table
reversegeo$country <- ifelse(reversegeo$country == "تونس", "Tunisia", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "مصر", "Egypt", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "السعودية", "Saudi Arabia", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "الإمارات العربية المتحدة", "United Arab Emirates", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "Deutschland", "Germany", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "الكويت", "Kuwait", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "Netherlands", "Nederland", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "الأردن", "Jordan", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "België / Belgique / Belgien", "Belgium", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "España", "Spain", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "Sverige", "Sweden", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "Italia", "Italy", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "日本", "Japan", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "Schweiz/Suisse/Svizzera/Svizra", "Switzerland", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "Italia", "Italia", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "Maroc / ⵍⵎⵖⵔⵉⴱ / المغرب", "Morocco",reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "لبنان", "Lebanon", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "Algérie / ⵍⵣⵣⴰⵢⴻⵔ / الجزائر", "Algeria", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "Brasil", "Brazil", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "Éire / Ireland", "Ireland", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "البحرين", "Bahrain", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "قطر", "Qatar", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "México", "Mexico", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "Россия", "Russia", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "Türkiye", "Turkey", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "Ελλάς", "Greece", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "中国", "China", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "Österreich", "Austria", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "Danmark", "Denmark", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "Norge", "Norway", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "ประเทศไทย", "Thailand", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "ישראל", "Israel", reversegeo$country)
reversegeo$country <- ifelse(reversegeo$country == "العراق", "Iraq", reversegeo$country)

check <- reversegeo %>%
  group_by(country) %>%
  summarise(counts = n()) %>%
  arrange(-counts) %>%
  filter(!is.na(country)) %>%
  select(country, counts) %>%
  top_n(20)

checkdf <- as.data.frame(check)
checkdftex <- kable(checkdf, "latex", booktabs=T)
writeLines(checkdftex, "data/output/countrycountsdf.tex")