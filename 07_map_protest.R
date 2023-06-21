library(tidyverse)
library(cartogram)
library(geogrid)
library(gridExtra)
library(grid)
library(magick)
library(maptools)
library(sf)
library(ggthemes)
library(cowplot)

# Get Hex shapefile
resulthex <- st_read("data/raw/shapefiles/tunhex1.shp")
resulthex <- resulthex %>%
  select(dlg_n_1)

## Read in data
tunfd <- read_csv("data/analysis/tunfd.csv")

## Pass data to object, specifying variables to keep
shdf <- tunfd %>%
  select(lon, lat, date, deleg_na_1, priorprot, governorate) 
head(shdf)

## Reformat date to r readable and numeric
shdf$date1 <- as.Date(shdf$date,format='%m/%d/%Y')
head(shdf)
shdf$date2 <- as.numeric(shdf$date1)
head(shdf)

## Merge protest data with geometric data
names(shdf)[names(shdf) == "deleg_na_1"] <- "dlg_n_1"
shdf = left_join(resulthex, shdf, by = "dlg_n_1")
head(shdf)

shdf <- subset(shdf, !is.na(priorprot))

## Merge protest data with geometric data from non-hex shapefile
## Merge protest data with geometric data from non-hex shapefile
shp <- st_read("data/raw/shapefiles/delegations.shp")
shp <- shp %>%
  select(deleg_na_1)

shdf1 <- tunfd %>%
  select(lon, lat, date, deleg_na_1, priorprot)

#Reformat date to R readable and numeric
shdf1$date1 <- as.Date(shdf1$date,format='%m/%d/%Y')
shdf1$date2 <- as.numeric(shdf1$date1)
shdf1 <- left_join(shp, shdf1, by = "deleg_na_1")


#strip out polygon borders and just use outer edge
shapes <- st_read("data/raw/shapefiles/tunhex1.shp")
resulthex <- as_Spatial(shapes)

lps <- coordinates(resulthex)
ids <- factor(rep(c("1"), each=272))
tunbord <- unionSpatialPolygons(resulthex, ids)

fortbord <- fortify(tunbord)
border.layer <- geom_polygon(aes(x = long, y = lat, group = group),
                             data = fortbord, fill = NA, color = "black", lwd=.1)

#add date and legend
shdf1 <- shdf %>%
  filter(date1 <="2011-01-14")

## add capital overlay
tun_bord <- shdf %>% 
  subset(governorate == "Tunis")

tun_bord <- shapes %>% 
  subset(gv_nm_f_y == "Tunis")

resulthex <- as_Spatial(tun_bord)
lps <- coordinates(resulthex)
ids <- factor(rep(c("1"), each=21))
tunbord <- unionSpatialPolygons(resulthex, ids)
fortbord <- fortify(tunbord)
cap.layer <- geom_polygon(aes(x = long, y = lat, group = group),
                          data = fortbord, fill = NA, color = "black", lwd=.5)

#plot
diffmap <- ggplot(shdf1) +
  geom_sf(aes(fill=factor(priorprot)), lwd=0) + 
  scale_fill_manual(values = c("white", "red"),
                    labels = c("No protest", "Protest"),
                    name = "") +
  labs(title = "",
       y = "", x = "") +
  facet_wrap(~ date1, ncol=10) +
  theme_classic() + coord_sf(datum=NA) +
  theme(legend.position = "bottom",
        legend.key = element_rect(colour = 'black', linewidth = 0.6),
        strip.background = element_blank()) + border.layer + cap.layer

png("data/plots/diffmap.png", 
    width = 465, height = 225, units='mm', res = 300)
diffmap
dev.off()

## get IDR and weekly maps 
# Week 1
shdf1 <- shdf %>% 
  filter(date2 == 14966)
w1 <- ggplot(shdf1) +
  geom_sf(aes(fill=factor(priorprot)), lwd=.1) +
  theme_classic() + coord_sf(datum=NA) +
  labs(title = "Week 1",
       y = "", x = "", fill = "Protest") +
  scale_fill_manual(values = c("white", "red")) +
  theme(legend.position = "none",
        plot.title = element_text(size=15)) +
  border.layer + cap.layer

## Week 2
shdf2 <- shdf %>% 
  filter(date2 == 14973)
w2 <- ggplot(shdf2) +
  geom_sf(aes(fill=factor(priorprot)), lwd=.1) +
  theme_classic() + coord_sf(datum=NA) +
  labs(title = "Week 2",
       y = "", x = "", fill = "Protest") +
  scale_fill_manual(values = c("white", "red")) +
  theme(legend.position = "none",
        plot.title = element_text(size=15)) +
  border.layer + cap.layer


## Week 3
shdf3 <- shdf %>% 
  filter(date2 == 14980)
w3 <- ggplot(shdf3) +
  geom_sf(aes(fill=factor(priorprot)), lwd=.1) +
  theme_classic() + coord_sf(datum=NA) +
  labs(title = "Week 3",
       y = "", x = "", fill = "Protest") +
  scale_fill_manual(values = c("white", "red")) +
  theme(legend.position = "none",
        plot.title = element_text(size=15)) +
  border.layer + cap.layer

## Week 4
shdf4 <- shdf %>% 
  filter(date2 == 14988)
w4 <- ggplot(shdf4) +
  geom_sf(aes(fill=factor(priorprot)), lwd=.1) +
  theme_classic() + coord_sf(datum=NA) +
  labs(title = "Week 4",
       y = "", x = "", fill = "Protest") +
  scale_fill_manual(values = c("white", "red")) +
  theme(legend.position = "none",
        plot.title = element_text(size=15)) +
  border.layer + cap.layer

## get IDR map
# Get Hex shapefile
resulthex <- st_read("data/raw/shapefiles/tunhex1.shp")
resulthex <- resulthex %>%
  select(dlg_n_1)

## Read in data
dat <- read_csv("data/output/tundatnls.csv")
## Pass data to object, specifying variables to keep
shdf = dat %>%
  select(lon, lat, date, deleg_na_1, priorprot, idr, ln_nlights) 
head(shdf)
## Reformat date to r readable and numeric
shdf$date1 <- as.Date(shdf$date,format='%m/%d/%Y')
head(shdf)
shdf$date2 <- as.numeric(shdf$date1)
head(shdf)
## Merge protest data with geometric data
names(shdf)[names(shdf) == "deleg_na_1"] <- "dlg_n_1"
shdf <- left_join(resulthex, shdf, by = "dlg_n_1")
head(shdf)
shdf5 <- shdf %>% 
  filter(date2 == 14960)
shdf5$idr <- cut(shdf5$idr, 
                 breaks = c(-Inf, .19, .32, .42, 1),
                 right = FALSE)

shdf5 <- subset(shdf5, !is.na(idr))
w5 <- ggplot(shdf5) +
  geom_sf(aes(fill=idr), lwd=.1) +
  theme_classic() + coord_sf(datum=NA) +
  labs(title = "IDR",
       y = "", x = "", fill = "IDR") +
  scale_fill_brewer(palette="YlOrRd",
                    labels = c("0,0.19", "0.19,0.32", "0.32,0.42", "0.42,1")) +
  theme(legend.key = element_rect("black"),
        plot.title = element_text(size=15),
        legend.title = element_text(size = 0),
        legend.text = element_text(size = 10)) +
  border.layer

grid.arrange(w1, w2, w3, w4, w5, layout_matrix = rbind(c(5,5,5,1,2), c(5,5,5,3,4)))

## Add nightlights hexmap
shdf6 <- shdf %>% 
  filter(date2 == 14960)
shdf6$ln_nlights <- cut(shdf6$ln_nlights, 
                        breaks = c(-2.94, -1.47, -0.57, 1.03, Inf),
                        right = FALSE)

shdf6 <- subset(shdf6, !is.na(ln_nlights))
w7 <- ggplot(shdf6) +
  geom_sf(aes(fill=ln_nlights), lwd=.1) +
  theme_classic() + coord_sf(datum=NA) +
  labs(title = "Nightlights (logged)",
       y = "", x = "", fill = "IDR") +
  scale_fill_brewer(palette="YlOrRd",
                    labels = c("-2.94,-1.47", "-1.47,-0.57", "-0.57,1.03", "1.03,4.31")) +
  theme(legend.key = element_rect("black"),
        plot.title = element_text(size=15),
        legend.title = element_text(size = 0),
        legend.text = element_text(size = 10)) +
  border.layer

#add nighlights raster map inset
w6 <- image_read("data/raw/nlrast.png",
                 density = 1000)
w6 <- w6 %>%
  image_quantize(colorspace = 'gray')
w6 <- rasterGrob(w6)

g1 <- grid.arrange(w1, w2, w3, w4, layout_matrix = rbind(c(1,2,3,4), c(1,2,3,4)))
g <- grid.arrange(w5, layout_matrix = rbind(c(1,1), c(1,1)))
g2 <- grid.arrange(w6, layout_matrix = rbind(c(1,1)))
g3 <- grid.arrange(w7,w6, layout_matrix = rbind(c(1,1), 
                                                c(1,1,2)))
cg1 <- plot_grid(g1, labels = "AUTO", nrow = 1)
cg2 <- plot_grid(g,g3, labels = c("B", "C"), nrow = 1)
grid.arrange(cg1,cg2, layout_matrix = rbind(c(1,1,1,1),
                                            c(2,2,2,2)))

png("data/plots/diffmapidr.png", 
    width = 400, height = 300, units='mm', res = 300)
grid.arrange(cg1,cg2, layout_matrix = rbind(c(1,1,1,1),
                                            c(2,2,2,2)))
dev.off()
