# PIR Langage de Traitement des Données 2022-2023
# Projet de fin de semestre
# Robinson Maury et César Wapler

packages <- c("shiny", 
              "tidyverse", 
              "plotly", 
              "leaflet", 
              "maps",
              "sf",
              "stringr")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))
remove(packages, installed_packages)

# library(sf) # cartographie
# library(DT)
# library(shinydashboard)

download_rename_csv <- function(url, name = "data", dir = ".", sep = ",", skip = 0) {
  # Fonction qui télécharge un CSV depuis l'url en nommant le fichier avec la
  # date, et ouvre le fichier en le passant en valeur de retour. 
  
  search <- grep(paste0(name, ".*"), list.files(path=dir))
  
  if (length(search) < 1) {
    cat("Creating directory ", dir, "/ if it does not exist...\n", sep = "")
    dir.create(dir, showWarnings = FALSE)
    
    filename <- paste0(name, "_", Sys.Date(), "_", format(Sys.time(), "%H-%M"),".csv")
    filepath <- file.path(dir, filename)
    
    cat("Downloading file from URL to", file.path(dir, filename), "...\n")
    download.file(url, filepath)
    
    cat("Reading file...\n")
    
    return(read_delim(filepath, delim = sep, skip = skip))
    
  } else {
    existing_file <- list.files(path = dir, full.names = TRUE)[search[1]]
    cat("File exists:", existing_file, "\nReading file...\n")
    
    return(read_delim(existing_file, delim = sep, skip = skip))
  }
}

# Download and import main data csv ----
alcohol <- download_rename_csv(
  url  = "https://dw.euro.who.int/api/v3/export/download/03e871525401419398cc389280fc6654",
  name = "alcohol_deaths",
  dir  = "data",
  skip = 28
) %>% 
  head(-7) %>% # head() pour retirer les lignes de copyright à la fin
  select(country_code = COUNTRY, 
         sex          = SEX, 
         year         = YEAR, 
         deaths       = VALUE)

# Download df of country codes and names ----
countries <- download_rename_csv(
  url  = "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv",
  name = "countries",
  dir  = "data"
) %>% 
  select(
    country_name   = name, 
    country_code   = `alpha-3`, 
    country_region = region) %>% 
  mutate(
    country_name = ifelse(
      country_name == "United Kingdom of Great Britain and Northern Ireland", 
      "United Kingdom", 
      country_name)
  ) # Overwriting very long name for UK

# country names in french...
countries_fr <- download_rename_csv(
  url  = "https://raw.githubusercontent.com/stefangabos/world_countries/master/data/countries/fr/countries.csv",
  name = "fr_countries",
  dir  = "data"
) %>% 
  select(
    country_name_fr = name, 
    country_code    = alpha3) %>% 
  mutate(country_code = toupper(country_code))

countries <- right_join(countries, countries_fr)

countries_europe <- countries %>% 
  filter(country_region == "Europe")

# Join `alcohol` AND `countries` ------
# ... This way we have the countries' full names in the main df

alcohol <- right_join(countries, alcohol) # Overwriting main df (or should it be a new one?)

# -------------------------------------
# Downloading, unzipping and importing map of Europe as geojson
if (FALSE) {
  europe_geo_json <- download.file(
    "https://gisco-services.ec.europa.eu/distribution/v2/countries/download/ref-countries-2020-10m.geojson.zip",
    "data/geo_europe_2020.geojson.zip")
  
  unzip("data/geo_europe_2020.geojson.zip", exdir="data/geo_europe")
  
  geo <- read_sf("data/geo_europe/CNTR_BN_10M_2020_3035.geojson")
}
# ggplot() +
#   geom_sf(data=geo)

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

# Server functions for shiny ----------

draw_table <- function(alcohol) {
  alcohol <- alcohol %>%
    select(
      Pays = country_name_fr,
      Année = year, 
      Sexe = sex,
      `Morts par intoxication alcoolique` = deaths)
  
  return(alcohol)
}

draw_plotly <- function(alcohol, selected_country_code = "EUROPE") {
  if (selected_country_code == "EUROPE" | !(selected_country_code %in% alcohol$country_code)) {
    alcohol_for_plot <- alcohol %>% 
      group_by(year, sex) %>% 
      summarise(deaths = sum(deaths))
  } else {
    alcohol_for_plot <- alcohol %>% 
      filter(country_code == selected_country_code) %>% 
      group_by(year, sex)
  }
  
  plot <- alcohol_for_plot %>%
  ggplot(aes(x = year, y = deaths, color = sex)) +
    geom_line() +
    theme_minimal()
  
  return(ggplotly(plot))
}

get_statistics <- function(alcohol, selected_country_code = "EUROPE") {
  if (!selected_country_code %in% alcohol$country_code) {
    selected_country_code = "EUROPE"
  }
  
  alcohol_filtered <- alcohol %>% 
    filter(country_code == selected_country_code | selected_country_code == "EUROPE")
    
  sex_summarise <- alcohol_filtered %>% 
    group_by(sex) %>% 
    summarise(mean = mean(deaths))
    
  max_year <- alcohol_filtered$year[which(alcohol_filtered$deaths == max(alcohol_filtered$deaths))]
  
  return(
    list(
      country_name = ifelse(selected_country_code == "EUROPE", "Toute l'Europe", filter(alcohol, country_code == selected_country_code)$country_name_fr[1]),
      mean_all     = sex_summarise$mean[1],
      mean_female  = sex_summarise$mean[2],
      mean_male    = sex_summarise$mean[3],
      max_year     = max_year[1]
      # TODO Add start and end year with data
    )
  )
}

# UI for application ------------------
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),
    
    tags$head(
      tags$style(HTML("
        ul {
          list-style: none;
          margin-top: 20px;
        }
        footer {
          font-size: 0.7rem;
        }
        footer ul {
          padding-left: 10px;
        }
      "))
    ),
    
    # Application title
    titlePanel("Alcool"),
    
    # Layout with a Sidebar
    sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "country_eu",
            label   = "Pays",
            choices = setNames(
              c("EUROPE", unique(alcohol$country_code)), 
              c("Europe", unique(alcohol$country_name_fr))
          )),
          textOutput("selected_code"),
          hr(),
          tags$footer(
            tags$h6("Sources"),
            tags$ul(
              tags$li(
                tags$a("WHO - European Health Information Gateway", 
                       href="https://gateway.euro.who.int/en/indicators/hfamdb_87-deaths-alcohol-abuse-including-alcoholic-psychosis/")
              ),
              tags$li(tags$a("L. Duncalfe", href="https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes")),
              tags$li(tags$a("Wikipedia", href="https://en.wikipedia.org/wiki/ISO_3166-1#Officially_assigned_code_elements")),
              tags$li(tags$a("UN Statistics", href="https://unstats.un.org/unsd/methodology/m49/overview"))
            ))
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Résumé", htmlOutput("stats_ui")),
            tabPanel("Données brutes", dataTableOutput("table")),
            tabPanel("Graphique", plotlyOutput("plot")),
            tabPanel("Carte", "TODO")
          ),

        )
    )
)

# Server logic ------------------------
server <- function(input, output) {

  output$table <- renderDataTable(draw_table(alcohol))
  output$plot <- renderPlotly(draw_plotly(alcohol, selected_country_code = input$country_eu))
  output$selected_code <- renderText(paste0("(debug) country_code: ", input$country_eu))

  output$stats_ui <- renderUI({
    stats <- get_statistics(alcohol, input$country_eu)
    tags$ul(
      tags$li("Pays : ", stats$country_name),
      tags$li("Moyenne morts/an :        ", round(stats$mean_all, 2)),
      tags$li("Moyenne morts/an hommes : ", round(stats$mean_male)),
      tags$li("Moyenne morts/an femmes : ", round(stats$mean_female)),
      tags$li("Année max morts :         ", stats$max_year)
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


# LEAFLETTTTT

download.file(
  "https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/download/?format=shp&timezone=Europe/Paris&lang=en",
  "data/geo_world.shp.zip"
)
unzip("data/geo_world.shp.zip", exdir="data")
geo_world <- read_sf("data/world-administrative-boundaries.shp")

geo_europe <- geo_world %>% 
  filter(grepl(".*europe.*", continent, ignore.case=T))




palette <- colorNumeric(palette = "Reds", domain = alcohol_mean_geo$mean_deaths, reverse = FALSE)
# palette <- colorNumeric(c("red", "green", "blue"), 0:100)

leaflet(alcohol_mean_geo) %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  # addTiles() %>%
  addPolygons(fillColor = ~palette(mean_deaths),
              label = paste0(alcohol_mean_geo$french_shor, " : ", alcohol_mean_geo$mean_deaths, " morts/an"),
              stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1) %>%
  # addTopoJSON(geo_europe, weight = 1, color = "#444444", fill = TRUE) %>% 
  setView(lng = 16, lat = 53, zoom = 3) %>% 
  addLabelOnlyMarkers(label = if_else(grepl("Royaume-Uni", alcohol_mean_geo$french_shor), "Royaume-Uni", alcohol_mean_geo$french_shor), 
                      data = alcohol_mean_geo$centroid,
                      labelOptions = labelOptions(noHide = T, direction = "center", textOnly = TRUE,
                                                  style = list(
                                                    "color" = "white",
                                                    "font-family" = "sans-serif",
                                                    "font-weight" = "normal",
                                                    "font-size" = "0.5rem",
                                                    "text-shadow" = "1px  0px 2px black,
                                                                    -1px  0px 2px black,
                                                                     0px  1px 2px black,
                                                                     0px -1px 2px black")))


#  FROM https://rstudio.github.io/leaflet/json.html
# leaflet(nycounties) %>%
#   addTiles() %>%
#   addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
#               fillColor = ~pal(log10(pop)),
#               label = ~paste0(county, ": ", formatC(pop, big.mark = ","))) %>%
#   addLegend(pal = pal, values = ~log10(pop), opacity = 1.0,
#             labFormat = labelFormat(transform = function(x) round(10^x)))