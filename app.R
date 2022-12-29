# PIR Langage de Traitement des Données 2022-2023
# Projet de fin de semestre
# Robinson Maury et César Wapler

# Packages : Installation (si besoin) et chargement ----
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

# Téléchargement des données non présentes dans `data/` ----
download_rename_csv <- function(url, name = "data", dir = ".", sep = ",", skip = 0) {
  # Fonction qui télécharge un CSV depuis l'url en nommant le fichier avec la
  # date, et ouvre le fichier en le passant en valeur de retour.
  

  search <- grepl(pattern = paste0(name, ".*"),
                  x = list.files(path=dir))
  
  if (!any(search)) {
    cat("Creating directory ", file.path(dir, ""), " if it does not exist...\n", sep = "")
    dir.create(dir, showWarnings = FALSE)
    
    filename <- paste0(name, "_", Sys.Date(), "_", format(Sys.time(), "%H-%M"),".csv")
    filepath <- file.path(dir, filename)
    
    cat("Downloading file from URL to ", filepath, "...\n")
    download.file(url, filepath)
    
    cat("Reading file...\n")
    
    return(read_delim(filepath, delim = sep, skip = skip))
    
  } else {
    existing_file <- list.files(path = dir, full.names = TRUE)[which(search == TRUE)][1]
    cat("File exists:", existing_file, "\nReading file...\n")
    
    return(read_delim(existing_file, delim = sep, skip = skip))
  }
}

# CSV avec les données principales sur la mortalité par intoxication alcoolique
alcohol <- download_rename_csv(
  url  = "https://dw.euro.who.int/api/v3/export/download/03e871525401419398cc389280fc6654",
  name = "alcohol_deaths", # Le nom du fichier qui sera enregistré dans `/data`
  dir  = "data",           # Le nom de notre dossier de données
  skip = 28                # Sauter les 28 lignes d'information au début du csv
) %>% 
  head(-7) %>% # `head()` pour retirer les lignes de copyright à la fin
  select(country_code = COUNTRY, # On select et renomme les colonnes en même temps
         sex          = SEX,
         year         = YEAR,
         deaths       = VALUE) %>% 
  mutate(sex = tolower(sex))

# CSV avec les noms de pays + codes
countries <- download_rename_csv(
  url  = "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv",
  name = "countries",
  dir  = "data"
) %>% 
  select(
    country_name   = name,
    country_code   = `alpha-3`,
    country_region = region) %>%
  mutate( # `mutate()` pour renommer le Royaume-Uni avec un nom plus court
    country_name = ifelse(
      country_name == "United Kingdom of Great Britain and Northern Ireland", 
      "United Kingdom", 
      country_name)
  ) 

# CSV avec les noms de pays en français
countries_fr <- download_rename_csv(
  url  = "https://raw.githubusercontent.com/stefangabos/world_countries/master/data/countries/fr/countries.csv",
  name = "fr_countries",
  dir  = "data"
) %>% 
  select(
    country_name_fr = name, 
    country_code    = alpha3) %>% 
  mutate(country_code = toupper(country_code))

# On join pour avoir un df avec les codes + les noms FR
countries <- right_join(countries, countries_fr)

# On filtre pour ne garder que les pays d'Europe
countries_europe <- countries %>% 
  filter(country_region == "Europe")

# Ajout des noms et codes de pays au df principal
alcohol <- right_join(countries, alcohol)

# Données pour leaflet ----
# Téléchargement et dézippage du Shapefile (si un zipfile n'existe pas déjà)
if (!any(grepl("geo_world_.*\\.shp\\.zip", list.files(path="data")))) {
  filepath <- file.path( # Chemin du fichier avec date dans le nom (eg. `geo_world_2022-12-25_00-00.shp.zip`)
    "data", paste0("geo_world", "_", Sys.Date(), "_", format(Sys.time(), "%H-%M"), ".shp.zip")
    )
  
  cat("Downloading zip file to ", filepath, "...\n")
  
  download.file(
    "https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/download/?format=shp&timezone=Europe/Paris&lang=en",
    filepath
  )
  unzip(filepath, exdir="data")
}

# Chargement du Shapefile
geo_world <- read_sf(file.path("data", "world-administrative-boundaries.shp"))

# `filter()` pour ne garder que les payrs européens
geo_europe <- geo_world %>% 
  filter(grepl(".*europe.*", continent, ignore.case = TRUE))

# Certaines informations telles que le nom des pays, codes, et noms français sont
# présents dans ce df, rendant l'import fait plus tôt (ligne ~70...) redondant.
# À re-factoriser...

# Assemblage du df pour leaflet ---- 

# Il y a sans doute un moyen plus concis de faire ces transformations...
alcohol_geo <- alcohol %>%
  group_by(country_code, sex) %>%
  mutate(total_avg = mean(deaths)) %>%
  ungroup() %>%
  pivot_longer(c(total_avg, year), names_to = "type", values_to = "year") %>%
  mutate(
    deaths = if_else(type == "total_avg", year, round(deaths)),
    year = ifelse(type == "total_avg", NA, round(year)),
  ) %>%
  unique() %>%
  mutate(deaths = round(deaths)) %>% 
  pivot_wider(names_from = sex, names_prefix = "deaths_", values_from = deaths) %>% 
  right_join(y = geo_europe, by = c("country_code" = "color_code")) %>% 
  select(starts_with("country_"), type, year, starts_with("deaths_"), geometry) %>% 
  st_as_sf() %>% 
  mutate(geometry_center = st_centroid(geometry))




# Functions to be called by shiny's server function ----

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
  # if (!(selected_country_code %in% alcohol$country_code)) {
  #   selected_country_code = "EUROPE"
  # }
  
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
      max_year     = max_year[1],
      start_year   = min(alcohol_filtered$year),
      end_year     = max(alcohol_filtered$year)
    )
  )
}

stats_HTML <- function(alcohol, country_eu) {
  stats <- get_statistics(alcohol, country_eu)
  return({
    tags$ul(
      tags$li("Pays : ", stats$country_name),
      tags$li("Moyenne morts/an :        ", round(stats$mean_all, 2)),
      tags$li("Moyenne morts/an hommes : ", round(stats$mean_male)),
      tags$li("Moyenne morts/an femmes : ", round(stats$mean_female)),
      tags$li("Année max morts :         ", stats$max_year),
      tags$li(`style` = "font-size: 0.6rem; font-style: italics;", 
              "Données disponibles : ", stats$start_year, " à ", stats$end_year, ".")
    )
  })
}


draw_leaflet <- function(alcohol_geo, year, sex = "all") {
  alcohol_geo_local <- alcohol_geo
  if (is.na(year))
    alcohol_geo_local <- filter(alcohol_geo, type != "year")
  else
    alcohol_geo_local <-  filter(alcohol_geo, type == "year")
  
  deaths_sex <- paste0("deaths_", sex)

  palette <- colorNumeric(palette = "Reds", domain = alcohol_geo_local[[deaths_sex]], reverse = FALSE)
  
  country_labels_options <- labelOptions(noHide = T, direction = "center", textOnly = TRUE,
                              style = list(
                                "color" = "white", 
                                "font-family" = "sans-serif", 
                                "font-weight" = "normal", 
                                "font-size"   = "0.5rem", 
                                "text-shadow" = "1px 0 2px black, -1px 0 2px black, 0 1px 2px black, 0 -1px 2px black"))
  
  leaflet_map <- leaflet(alcohol_geo_local) %>%
    addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
    addPolygons(fillColor = ~palette(alcohol_geo_local[[deaths_sex]]),
                label = paste0(alcohol_geo$country_name_fr, " : ", alcohol_geo[[deaths_sex]], " morts/an ??"),
                stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1) %>%
    setView(lng = 16, lat = 53, zoom = 3) %>%
    addLabelOnlyMarkers(label = alcohol_geo_local$country_name_fr,
                        data = alcohol_geo_local$geometry_center,
                        labelOptions = country_labels_options)
  
  return(leaflet_map)
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
            tabPanel("Carte", 
                     leafletOutput("leaflet"), 
                     sliderInput(inputId = "selected_year", label = "Année", min = 1976, max = 2020, value = 2000)) # TODODODOO
          ),

        )
    )
)

# Server function ------------------------
server <- function(input, output) {

  output$table <- renderDataTable(draw_table(alcohol))
  output$plot <- renderPlotly(draw_plotly(alcohol, selected_country_code = input$country_eu))
  output$leaflet <- renderLeaflet(draw_leaflet(alcohol_geo, NA, "male"))
  output$selected_code <- renderText(paste0("(debug) country_code: ", input$country_eu))

  output$stats_ui <- renderUI(stats_HTML(alcohol, input$country_eu))
}

# Run the application 
shinyApp(ui, server)
# runGadget(ui, server, viewer = dialogViewer(dialogName = "Projet PIR", width = 1280, height = 800))
# runGadget(ui, server, viewer = browserViewer(browser = getOption("browser")))
