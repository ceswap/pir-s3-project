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
download_rename_file <- function(url, filename, dir = ".") {
  # Fonction qui télécharge un CSV depuis l'url en nommant le fichier avec la
  # date, et renvoie le nom du fichier à ouvrir.
  
  name <- sub("\\..*", "", filename)
  ext  <- sub(".*?\\.", ".", filename)
  
  search <- grepl(pattern = paste0(name, ".*"),
                  x = list.files(path=dir))
  
  if (!any(search)) {
    cat("Creating directory", file.path(dir, ""), "if it does not exist...\n")
    dir.create(dir, showWarnings = FALSE)
    
    filename <- paste0(name, "_", Sys.Date(), "_", format(Sys.time(), "%H-%M"), ext)
    filepath <- file.path(dir, filename)
    
    cat("Downloading file from URL to", filepath, "...\n")
    download.file(url, filepath)
    
    if (any(grepl("\\.zip", ext))) {
      cat("Unzipped contents of", filename, "to", file.path(dir, ""))
      unzip(filepath, exdir="data")
    }
    
    return(filepath)
  } else {
    existing_filepath <- list.files(path = dir, full.names = TRUE)[which(search == TRUE)][1]
    cat("File exists:", existing_filepath)

    return(existing_filepath)
  }
}

# CSV avec les données principales sur la mortalité par intoxication alcoolique
alcohol_csv_path <- download_rename_file(
  url = "https://dw.euro.who.int/api/v3/export/download/03e871525401419398cc389280fc6654",
  filename = "alcohol.csv", 
  dir = "data")

alcohol <- read_delim(alcohol_csv_path, delim = ",", skip = 28) %>% 
  head(-7) %>% # `head()` pour retirer les lignes de copyright à la fin
  select(country_code = COUNTRY, # On select et renomme les colonnes en même temps
         sex          = SEX,
         year         = YEAR,
         deaths       = VALUE) %>% 
  mutate(sex = tolower(sex)) %>% 
  pivot_wider(names_from = sex, names_prefix = "deaths_", values_from = deaths)

alcohol_avg <- alcohol %>% 
  group_by(country_code) %>%
  mutate(deaths_female = round(mean(deaths_female, na.rm = T)),
         deaths_male   = round(mean(deaths_male, na.rm = T)),
         deaths_all    = round(mean(deaths_all, na.rm = T))) %>%
  mutate(data_range = paste0(min(year), "-", max(year))) %>% 
  ungroup() %>%
  select(-year) %>% 
  unique()


# Données pour leaflet ---- ET DONNEES PAYS + NOMS, NOMS EN FR ...
# Téléchargement et dézippage du Shapefile (si un zipfile n'existe pas déjà)
download_rename_file(
  url = "https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/download/?format=shp&timezone=Europe/Paris&lang=en", 
  filename = "geo_world.shp.zip",  dir = "data")

# Chargement du Shapefile
geo_world <- read_sf(file.path("data", "world-administrative-boundaries.shp")) %>% 
  select(country_code = iso3,
         continent,
         country_name_fr = french_shor,
         geometry) %>% 
  mutate(geometry_centroid = st_centroid(geometry),
         country_name_fr = if_else(grepl("Royaume-Uni", country_name_fr), "Royaume-Uni", country_name_fr))



alcohol_geo <- right_join(geo_world, alcohol) %>% 
  st_as_sf() %>% 
  relocate(contains("geometry"), .after = last_col())

alcohol_avg_geo <- right_join(geo_world, alcohol_avg) %>%  
  st_as_sf() %>% 
  relocate(contains("geometry"), .after = last_col())



alcohol <- alcohol_geo %>% 
  st_drop_geometry() %>% 
  select(-contains("geometry"))
  
alcohol_avg <- alcohol_avg_geo %>% 
  st_drop_geometry() %>% 
  select(-contains("geometry"))


# Cleanup
remove(alcohol_csv_path, geo_world)

# --------- STOPPED REFACTORING HERE -----------
# --------- STOPPED REFACTORING HERE -----------
# --------- STOPPED REFACTORING HERE -----------
# --------- STOPPED REFACTORING HERE -----------

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
