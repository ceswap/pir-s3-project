library(tidyverse)
library(leaflet)
library(maps)
library(sf)
library(stringr)



# download.file(
#   "https://gisco-services.ec.europa.eu/distribution/v2/nuts/shp/NUTS_RG_03M_2021_4326_LEVL_0.shp.zip",
#   "data/geo_europe_2020.shp.zip")

# 
# unzip("data/geo_europe_2020.shp.zip", exdir="data/")
# geo <- read_sf("data/NUTS_RG_03M_2021_4326_LEVL_0.shp")


download.file(
  "https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/download/?format=shp&timezone=Europe/Paris&lang=en",
  "data/geo_world.shp.zip"
)
unzip("data/geo_world.shp.zip", exdir="data")
geo_world <- read_sf("data/world-administrative-boundaries.shp")

geo_europe <- geo_world %>% 
  filter(grepl(".*europe.*", continent, ignore.case=T))




 

leaflet(geo_europe) %>% 
  # addProviderTiles(providers$Stamen.Toner) %>% 
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
  # addTopoJSON(geo, weight = 1, color = "#444444", fill = TRUE)
  # setView(lng = -98.583, lat = 39.833, zoom = 3) %>% 















# POUBELLE -----

# install.packages("rnaturalearth", "rnaturalearthdata")
# library(rnaturalearth)
# library(rnaturalearthdata)
# geo_world <- ne_countries(scale = 50, returnclass = 'sf')
# 
# eu_vect <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
#              "Czech Rep.","Denmark","Estonia","Finland","France",
#              "Germany","Greece","Hungary","Ireland","Italy","Latvia",
#              "Lithuania","Luxembourg","Malta","Netherlands","Poland",
#              "Portugal","Romania","Slovakia","Slovenia","Spain",
#              "Sweden","United Kingdom")

# eu_map <- geo_world %>% 
#   filter()

# europe <- as_Spatial(geo)
# europe <- geo$geometry #map("france", fill=TRUE, plot = FALSE)