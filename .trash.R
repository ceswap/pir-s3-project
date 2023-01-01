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

# Lots of stuff (refactoring) ----

# alcohol_geo <- alcohol %>%
# group_by(country_code, sex) %>%
# mutate(total_avg = mean(deaths)) %>%
# ungroup() %>%
# pivot_longer(c(total_avg, year), names_to = "type", values_to = "year") %>%
# mutate(
#   deaths = if_else(type == "total_avg", year, round(deaths)),
#   year = ifelse(type == "total_avg", NA, round(year)),
# ) %>%
# unique() %>%
# mutate(deaths = round(deaths)) %>%
# pivot_wider(names_from = sex, names_prefix = "deaths_", values_from = deaths) %>% 
# right_join(y = geo_europe, by = c("country_code" = "color_code")) %>% 
# select(starts_with("country_"), type, year, starts_with("deaths_"), geometry) %>% 
# st_as_sf() %>% # <<<<<<<<-------- THIS
# mutate(geometry_center = st_centroid(geometry))

# if (!any(grepl("geo_world_.*\\.shp\\.zip", list.files(path="data")))) {
#   filepath <- file.path( # Chemin du fichier avec date dans le nom (eg. `geo_world_2022-12-25_00-00.shp.zip`)
#     "data", paste0("geo_world", "_", Sys.Date(), "_", format(Sys.time(), "%H-%M"), ".shp.zip")
#   )
#   
#   cat("Downloading zip file to ", filepath, "...\n")
#   
#   download.file(
#     "https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/download/?format=shp&timezone=Europe/Paris&lang=en",
#     filepath
#   )
#   unzip(filepath, exdir="data")
# }




# alcohol <- download_rename_csv(
#   url  = "https://dw.euro.who.int/api/v3/export/download/03e871525401419398cc389280fc6654",
#   name = "alcohol_deaths", # Le nom du fichier qui sera enregistré dans `/data`
#   dir  = "data",           # Le nom de notre dossier de données
#   skip = 28                # Sauter les 28 lignes d'information au début du csv
# ) %>% 
#   head(-7) %>% # `head()` pour retirer les lignes de copyright à la fin
#   select(country_code = COUNTRY, # On select et renomme les colonnes en même temps
#          sex          = SEX,
#          year         = YEAR,
#          deaths       = VALUE) %>% 
#   mutate(sex = tolower(sex))

# CSV avec les noms de pays + codes
# countries <- download_rename_csv(
#   url  = "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv",
#   name = "countries",
#   dir  = "data"
# ) %>% 
#   select(
#     country_name   = name,
#     country_code   = `alpha-3`,
#     country_region = region) %>%
#   mutate( # `mutate()` pour renommer le Royaume-Uni avec un nom plus court
#     country_name = ifelse(
#       country_name == "United Kingdom of Great Britain and Northern Ireland", 
#       "United Kingdom", 
#       country_name)
#   ) 

# CSV avec les noms de pays en français
# countries_fr <- download_rename_csv(
#   url  = "https://raw.githubusercontent.com/stefangabos/world_countries/master/data/countries/fr/countries.csv",
#   name = "fr_countries",
#   dir  = "data"
# ) %>% 
#   select(
#     country_name_fr = name, 
#     country_code    = alpha3) %>% 
#   mutate(country_code = toupper(country_code))

# On join pour avoir un df avec les codes + les noms FR
# countries <- right_join(countries, countries_fr)

# On filtre pour ne garder que les pays d'Europe
# countries_europe <- countries %>% 
#   filter(country_region == "Europe")

# Ajout des noms et codes de pays au df principal
# alcohol <- right_join(countries, alcohol)

# # `filter()` pour ne garder que les payrs européens
# geo_europe <- geo_world %>% 
#   filter(grepl(".*europe.*", continent, ignore.case = TRUE))

# Assemblage du df pour leaflet ---- 

# # Il y a sans doute un moyen plus concis de faire ces transformations...
# alcohol_geo <- alcohol %>%
#   group_by(country_code, sex) %>%
#   mutate(total_avg = mean(deaths)) %>%
#   ungroup() %>%
#   pivot_longer(c(total_avg, year), names_to = "type", values_to = "year") %>%
#   mutate(
#     deaths = if_else(type == "total_avg", year, round(deaths)),
#     year = ifelse(type == "total_avg", NA, round(year)),
#   ) %>%
#   unique() %>%
#   mutate(deaths = round(deaths)) %>% 
#   pivot_wider(names_from = sex, names_prefix = "deaths_", values_from = deaths) %>% 
#   right_join(y = geo_europe, by = c("country_code" = "color_code")) %>% 
#   select(starts_with("country_"), type, year, starts_with("deaths_"), geometry) %>% 
#   st_as_sf() %>% 
#   mutate(geometry_center = st_centroid(geometry))

# ----
# tags$li(a("L. Duncalfe", href="https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes")),
# tags$li(a("Wikipedia", href="https://en.wikipedia.org/wiki/ISO_3166-1#Officially_assigned_code_elements")),
# tags$li(a("UN Statistics", href="https://unstats.un.org/unsd/methodology/m49/overview"))