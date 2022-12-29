# line 104 ----
# Downloading, unzipping and importing map of Europe as geojson DELETE THIS
if (FALSE) {
  europe_geo_json <- download.file(
    "https://gisco-services.ec.europa.eu/distribution/v2/countries/download/ref-countries-2020-10m.geojson.zip",
    "data/geo_europe_2020.geojson.zip")
  
  unzip("data/geo_europe_2020.geojson.zip", exdir="data/geo_europe")
  
  geo <- read_sf("data/geo_europe/CNTR_BN_10M_2020_3035.geojson")
}

# line 365 ----
# DELETE
#  FROM https://rstudio.github.io/leaflet/json.html
# leaflet(nycounties) %>%
#   addTiles() %>%
#   addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
#               fillColor = ~pal(log10(pop)),
#               label = ~paste0(county, ": ", formatC(pop, big.mark = ","))) %>%
#   addLegend(pal = pal, values = ~log10(pop), opacity = 1.0,
#             labFormat = labelFormat(transform = function(x) round(10^x)))

# first lesson ----
{
  # alc_deaths <- read.csv("data/HFAMDB_87_EN.csv", skip=28)
  # alc_conso <- read.csv("data/API_SH.ALC.PCAP.LI_DS2_en_csv_v2_4499335.csv", skip=4)
  # 
  # alc_deaths_ui <- alc_deaths %>% 
  #   select(!COUNTRY_GRP) %>% 
  #   drop_na()
  # 
  # countries <- read_csv("data/countries.csv") %>% 
  #   select(name, code = `alpha-3`, region)
  # 
  # europe <- countries %>% 
  #   filter(region == "Europe")
  # 
  # alc_deaths_ui_eu <- alc_deaths_ui %>% 
  #   filter(COUNTRY %in% europe$code)
  #   
  # # moyenne par sexe, par année, par pays
  # 
  # # moyenne par sexe tout confondu
  # alc_deaths %>% 
  #   select(COUNTRY, SEX, YEAR, VALUE) %>% 
  #   group_by(SEX) %>% 
  #   summarise(avg=mean(VALUE))
  # 
  # alc_deaths_2 <- alc_deaths %>% 
  #   select(COUNTRY, SEX, YEAR, VALUE) %>% 
  #   group_by(SEX, YEAR) %>% 
  #   filter(SEX %in% c("MALE", "FEMALE")) %>%
  #   summarise(mean=mean(VALUE)) %>%
  #   mutate(SEX=as.factor(SEX))
  # 
  # alc_deaths_2 %>%
  #   ggplot(aes(x=YEAR, y=mean, color=SEX)) +
  #   geom_line()
  # 
  # # Max par pays
  # alc_deaths %>% 
  #   select(COUNTRY, SEX, YEAR, VALUE) %>% 
  #   group_by(COUNTRY) %>% 
  #   summarise(avg=max(VALUE))
  # 
}