# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                     #
#   PIR Langage de Traitement des Données 2022-2023   #
#             Projet de fin de semestre               #
#           Robinson Maury et César Wapler            #
#                                                     #
#                                      voir README.md #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# TODO setwd() !!!!

# TODO Ajouter /million dans graph
# TODO Ajouter /million et toggle stats dans datatable
# TODO Quantile ? explain ?
# TODO README quelle version R ??? etc...
# TODO Désactiver slider annes si case cochée
# TODO Légende carte ajouter par million d'habitants quand relevant
# TODO UNZIP READ DELETE UNZIPPED everytime!

if (Sys.getenv("RSTUDIO") == "1") {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  setwd(getSrcDirectory()[1])
}

# PACKAGES ----
# Installation (si besoin) et chargement
packages <- c("tidyverse", "shiny", "plotly", "leaflet", "sf")

installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))
remove(packages, installed_packages)

# SETUP ----

# Fonction pour gérer le téléchargement des fichiers si ils ne sont pas déjà présents
download_rename_file <- function(url, filename, dir = ".") {
  name <- sub("\\..*", "", filename)
  ext  <- sub(".*?\\.", ".", filename)
  search <- grepl(pattern = paste0(name, ".*"), x = list.files(path=dir))
  
  if (!any(search)) {
    cat("Creating directory", file.path(dir, ""), "if it does not exist...\n")
    dir.create(dir, showWarnings = FALSE)
    
    filename <- paste0(name, "_", Sys.Date(), "_", format(Sys.time(), "%H-%M"), ext)
    filepath <- file.path(dir, filename)
    
    cat("Downloading file from URL to", filepath, "...\n")
    download.file(url, filepath)
    
    if (any(grepl("\\.zip", ext))) {
      cat("Unzipped contents of", filename, "to", file.path(dir, ""), "\n")
      unzip(filepath, exdir="data")
    }
    
    return(filepath)

  } else {
    existing_filepath <- list.files(path = dir, full.names = TRUE)[which(search == TRUE)][1]
    cat("File exists:", existing_filepath, "\n")
    
    return(existing_filepath)
  }
}

  # SETUP ALCOHOL ----
# Téléchargement CSV avec les données principales sur la mortalité par intoxication alcoolique
alcohol_csv_path <- download_rename_file(
  url = "https://dw.euro.who.int/api/v3/export/download/03e871525401419398cc389280fc6654",
  filename = "alcohol.csv", 
  dir = "data")

# Chargement et transformations (avec un pivot_wider() notamment)
alcohol <- read_delim(alcohol_csv_path, delim = ",", skip = 28) %>% 
  head(-7) %>% # head() pour retirer les lignes de copyright à la fin
  select(country_code = COUNTRY, # On select et renomme les colonnes en même temps
         sex          = SEX,
         year         = YEAR,
         deaths       = VALUE) %>% 
  mutate(sex = tolower(sex)) %>% 
  pivot_wider(names_from = sex, names_prefix = "deaths_", values_from = deaths, names_sort = TRUE)

  # SETUP POPULATION ----
# Très long CSV (ZIP) avec données de population par sexe, pays et année afin de
# calculer des chiffres par million d'habitants plus tard
# /!\ ERREUR SSL QUAND ON TENTE DE LE TÉLÉCHARGER AVEC download.file() /!\
download_rename_file(
  url = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_TotalPopulationBySex.zip",
  filename = "population",
  dir = "data") 

# Chargement et transformations
# population <- read_delim(file.path("data", "WPP2022_TotalPopulationBySex.csv"), delim = ",") %>% # ORIGINAL LINE, crashed RStudio on the server
population <- read.csv(file.path("data", "WPP2022_TotalPopulationBySex.csv")) %>% as.tibble() %>%cd
  filter(LocTypeName == "Country/Area") %>% # exclusion des données agrégées
  select(country_code = ISO3_code,
         year         = Time,
         pop_all      = PopTotal, 
         pop_female   = PopFemale,
         pop_male     = PopMale) %>% 
  mutate(across(starts_with("pop_"), ~ .x / 1000)) # changement d'unité de millier à million d'habitants

# left_join() : Ajout des données de population aux 
# lignes du df principal (alcohol) et calcul des données par million avec mutate()
alcohol <- alcohol %>% 
  left_join(population) %>% 
  mutate(deaths_all_million    = round(deaths_all / pop_all, 2),
         deaths_female_million = round(deaths_female / pop_female, 2),
         deaths_male_million   = round(deaths_male / pop_male, 2)) %>% 
  select(-starts_with("pop_")) # Pas besoin de garder les colonnes de population
  
  # SETUP ALCOHOL_STATS ----
# df qui précalcule des infos statistiques par pays et sexe 
alcohol_stats <- alcohol %>% 
  group_by(country_code) %>% # Tout est calculé ci-dessous PAR PAYS
  mutate(highest_year = year[which.max(deaths_all)], # L'année avec le plus de morts
         max          = max(deaths_all), # Le maximum de morts/an
         start_year   = min(year),       # La première année avec des données disponibles
         end_year     = max(year),       # La dernière année avec des données disponibles
         deaths_all    = round(mean(deaths_all, na.rm = T)),    # Moyenne morts/an sur toutes les années
         deaths_female = round(mean(deaths_female, na.rm = T)), # ... femmes uniquement
         deaths_male   = round(mean(deaths_male, na.rm = T)),   # ... hommes uniquement
         max_million            = max(deaths_all_million), # Le maximum de morts/an rapporté àla population
         deaths_all_million     = mean(deaths_all_million, na.rm = T), # Moyenne de morts/an rapporté à la population
         deaths_female_million  = mean(deaths_female_million, na.rm = T),
         deaths_male_million    = mean(deaths_male_million, na.rm = T),
         across(ends_with("_million"), ~ round(.x, 2) )) %>%
  select(-starts_with("pop_"), -year) %>% 
  ungroup() %>%
  unique() # Retirer les lignes en double puisque la colonne année (year) ne change rien

  # SETUP GEO ----
# Données pour leaflet notamment

# Téléchargement et dézippage du Shapefile (si un zipfile n'existe pas déjà)
download_rename_file(
  url = "https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/download/?format=shp&timezone=Europe/Paris&lang=en", 
  filename = "geo_world.zip",  dir = "data")

# Chargement du Shapefile et cuisine des colonnes, 
geo_world <- read_sf(file.path("data", "world-administrative-boundaries.shp")) %>% 
  select(country_code = iso3,
         continent,
         country_name_fr = french_shor,
         geometry) %>% 
  mutate(geometry_centroid = st_centroid(geometry), # Génération d'un POINT centroid pour chaque POLYGON (mais peu utilisé)
         country_name_fr = if_else(grepl("Royaume-Uni", country_name_fr), "Royaume-Uni", country_name_fr)) # Le nom du R-U était trop long

# Construction des df avec données principale + données géographiques
alcohol_geo <- right_join(geo_world, alcohol) %>% 
  st_as_sf() %>% # Besoin de convertir en objet "Simple Features" pour la carte
  relocate(contains("geometry"), .after = last_col()) # Colonne geometry à la fin

# Idem pour alcohol_stats
alcohol_stats_geo <- right_join(geo_world, alcohol_stats) %>% 
  st_as_sf() %>% 
  relocate(contains("geometry"), .after = last_col())


# Ajout des données non géographiques de alcohol_geo (noms des pays, noms français des pays)
# à alcohol (ça semble très redondant, certainement pas le meilleur moyen de faire ça)
alcohol <- alcohol_geo %>% 
  st_drop_geometry() %>% 
  select(-contains("geometry"))
  
alcohol_stats <- alcohol_stats_geo %>% 
  st_drop_geometry() %>% 
  select(-contains("geometry"))

# Noms de pays et leurs code iso3 (eg. FRA) pour le selectInput() dans l'interface
country_list <- alcohol %>%
  select(contains("country_")) %>%
  unique() %>%
  arrange(country_name_fr) %>%
  add_row(country_code = "ALL",
          country_name_fr = "Total",
          .before = 1)

remove(alcohol_csv_path, geo_world) # Plus besoin de ça

# FIN DE SETUP

# FONCTIONS POUR SHINY SERVER ----

  # FONCTION A PROPOS ---- 
draw_info_HTML <- function() {
  return(
    tags$div(
      tags$h3("À propos"),
      tags$p("Données provenant du data set",
             tags$em("Alcohol abuse (including alcoholic psychosis), number of deaths, by sex"),
             "disponible sur le European Health Information Gateway de l'OMS."))
    )
}

  # FONCTION DATATABLE ---- 
draw_table <- function(average = FALSE) {
  if (average) {
    alcohol_for_table <- alcohol_stats %>% 
      select(Pays = country_name_fr,
             `Intervalle données disponibles` = paste0(year_start, "-", year_end),
             `Pic de mortalité` = highest_year,
             `Moyenne morts/an (F)`   = deaths_female,
             `Moyenne morts/an (H)`   = deaths_male,
             `Moyenne morts/an (H+F)` = deaths_all)
  } else {
    alcohol_for_table <- alcohol %>%
      select(Pays = country_name_fr,
             Année = year,
             `Morts (F)`   = deaths_female,
             `Morts (H)`   = deaths_male,
             `Morts (H+F)` = deaths_all)
  }
  
  return(alcohol_for_table)
}

  # FONCTION PLOT ----
draw_plotly <- function(selected_country_code = "ALL", selected_country_code_2 = "NONE") {
  if (selected_country_code == "ALL" | !(selected_country_code %in% alcohol$country_code)) {
    alcohol_for_plot <- alcohol %>% 
      group_by(year) %>% 
      summarise(across(starts_with("deaths_"), .fns =  sum, na.rm=T)) %>% 
      mutate(country_code = "ALL",
             country_name_fr = "Total somme") %>% 
      bind_rows(filter(alcohol, country_code == selected_country_code_2))
  } else {
    alcohol_for_plot <- alcohol %>%
      filter(country_code %in% c(selected_country_code, selected_country_code_2)) %>%
      group_by(year)
  }
  
  custom_tooltip <- function(series, year, deaths) {
    return(paste0(series,
                  "\n  Année : ", year,
                  "\n  Morts : ", format(deaths, big.mark=" ")))
  }

  plotly_title <- paste0(
    "Morts par intoxication alcoolique <br>par année (",
       if_else(selected_country_code == "ALL", 
               "somme totale", 
               country_list$country_name_fr[which(country_list$country_code == selected_country_code)]),
       if_else(selected_country_code_2 == "NONE", "",
               paste0(" et ", country_list$country_name_fr[which(country_list$country_code == selected_country_code_2)])),
       ")"
    )
  
  if (selected_country_code_2 != "NONE") {
    plot <- alcohol_for_plot %>%
      ggplot(aes(x = year, y = deaths_all, colour = country_name_fr, group = 1)) +
      geom_line(aes(text = custom_tooltip(country_name_fr, year, deaths_all)), size = 1.2) +
      labs(title = "Morts par intoxications \nalcoolique par année",
           x = "Année", y = "Morts par intoxications alcoolique par année") +
      scale_colour_discrete(name = "") +
      theme_minimal()
    
  } else {
    plot <- alcohol_for_plot %>%
      ggplot(aes(x = year, group = 1)) +
        geom_line(aes(y = deaths_all,    colour = "Total",  text = custom_tooltip("Total",  year, deaths_all)),    size = 1.2) +
        geom_line(aes(y = deaths_female, colour = "Femmes", text = custom_tooltip("Femmes", year, deaths_female)), size = 1.2) +
        geom_line(aes(y = deaths_male,   colour = "Hommes", text = custom_tooltip("Hommes", year, deaths_male)),   size = 1.2) +
        labs(title = "Morts par intoxications \nalcoolique par année",
             x = "Année", y = "Morts par intoxications alcoolique par année") +
        scale_colour_discrete(name = "") +
        theme_minimal()
  }
  
  return(ggplotly(plot, tooltip = c("text")) %>% layout(title = list(text = plotly_title)))
}

  # FONCTION STATS TOOLTIP ----
stats_output <- function(selected_country_code, per_million = TRUE) {
  stats <- alcohol_stats %>% filter(country_code == selected_country_code)
  
  col_suffix <- if_else(per_million, "_million", "")
  value_name    <- if_else(per_million, "Moyenne morts/million par an", "Moyenne morts par an")
  
  return(paste0(
    "<table class='leaflet-label'>",
      "<tr><th colspan='2'>", stats$country_name_fr, "</th></tr>",
      "<tr><td>", value_name, " H+F",     "</td><td>", stats[[paste0("deaths_all", col_suffix)]],    "</td></tr>",
      "<tr><td>", value_name, " F",       "</td><td>", stats[[paste0("deaths_female", col_suffix)]], "</td></tr>",
      "<tr><td>", value_name, " H",       "</td><td>", stats[[paste0("deaths_male", col_suffix)]],   "</td></tr>",
      "<tr><td>", "Année pic",            "</td><td>", stats$highest_year,  "</td></tr>",
      "<tr><td>", "Données disponibles",  "</td><td>", stats$start_year, " à ", stats$end_year,      "</td></tr>",
    "</table>"
    ))
}

  # FONCTION CARTE LEAFLET ----
draw_leaflet <- function(avg = TRUE, sex_column = "all", selected_year = NULL, 
                         selected_palette = "quantile",  country_labels = FALSE,
                         legend = FALSE, per_million = TRUE) {
  if (avg) {
    alcohol_for_leaflet <- alcohol_stats_geo
    domain <- alcohol_for_leaflet[[sex_column]]
    labels <- stats_output(alcohol_for_leaflet$country_code, per_million) %>% lapply(HTML)
  } else {
    domain <- alcohol_geo[[sex_column]]
    alcohol_for_leaflet <- alcohol_geo %>%
      filter(year == selected_year)
    
    if (per_million) {
      labels <- paste0("<strong>", alcohol_for_leaflet$country_name_fr, "</strong><br/>",
                       alcohol_for_leaflet$deaths_all_million, " morts/million en ", selected_year) %>%  lapply(HTML)
    } else {
      labels <- paste0("<strong>", alcohol_for_leaflet$country_name_fr, "</strong><br/>", 
                       alcohol_for_leaflet$deaths_all, " morts en ", selected_year) %>%  lapply(HTML)    
    }
  }

  if (selected_palette == "quantile") {
    palette <- colorQuantile("Blues", alcohol_for_leaflet[[sex_column]], n = 8)
  } else {
    palette <- colorNumeric(palette = "Blues", domain = domain, reverse = FALSE)
  }
  
  country_labels_options <- labelOptions(noHide = T, direction = "center", textOnly = TRUE,
                                         style = list("color" = "white",
                                                "font-family" = "sans-serif",
                                                "font-weight" = "normal",
                                                "font-size"   = "0.5rem",
                                                "text-shadow" = "1px 0 2px black, -1px 0 2px black, 0 1px 2px black, 0 -1px 2px black"))
  
  sex_column <- paste0("deaths_", sex_column, if_else(per_million, "_million", ""))
  
  leaflet_map <- leaflet(alcohol_for_leaflet) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    addPolygons(fillColor = ~palette(alcohol_for_leaflet[[sex_column]]),
                label = ~labels,
                stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1) %>%
    {if (legend) addLegend(., "topright", pal = palette, values = ~alcohol_for_leaflet[[sex_column]],
              title = if_else(selected_palette == "quantile", "Percentiles", "Echelle linéaire"),
              labFormat = labelFormat(suffix = if_else(selected_palette == "quantile", "", " morts/an")),
              opacity = 0.75) else .} %>%
    setView(lng = 18, lat = 55, zoom = 3) %>%
    {if(country_labels) addLabelOnlyMarkers(., label = alcohol_for_leaflet$country_name_fr,
                      data = alcohol_for_leaflet$geometry_centroid,
                      labelOptions = country_labels_options
                      ) else .}

  return(leaflet_map)
}


# SHINY UI ----
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  # CSS ----
  tags$head(
    tags$style(HTML("
        .tab-pane.active { margin-top: 15px; }
        footer { font-size: 0.8rem; }
        .sources-list {
          list-style: none;
          margin-top: 10px;
          padding-left: 10px;
        }
        .sources-list > li { margin-bottom: 10px; }
        .table { --bs-table-striped-bg: #282828; }
        #DataTables_Table_0 > tfoot { display: none; }
        .paginate_button { margin: 3px; }
        .leaflet-label {
          white-space: pre;
          margin: 0;
        }
        .leaflet-label tr td { padding: 0 6px; }
        .leaflet-label th {
          font-weight: bold;
          font-size: 0.8rem;
          text-align: center;
        }
        .year-select-text {
          font-size: 0.8rem; 
          padding: 0; 
          margin: 0 0 15px 0; 
          font-style: italic;
        }
        .display-options::before {
          content: 'Affichage';
        }
        .display-options {
          font-size: 0.8rem;
          border-radius: 5px;
          background: #555;
          padding: 10px;
        }
        .display-options .shiny-input-container,
        .year-select .shiny-input-container{ margin-bottom: 0; }
        "))
  ),

  
  titlePanel("Alcool"), # Titre de l'application TODO
  
  # Sous-titre de l'application TODO
  fluidRow(tags$p(paste("Morts par intoxication à l'alcool par année en Europe de", min(alcohol$year), "à", max(alcohol$year)))),
  
  # LAYOUT PRINCIPAL avec sidebar ----
  sidebarLayout(
    
    # SIDEBAR ---- 
    sidebarPanel(width = 4,
      
      # Plusieurs conditionalPanel() pour modifier le contenu de la sidebar en fonction du tabset sélectionné
      # SIDEBAR POUR A PROPOS
      conditionalPanel(condition = "input.selected_tab == 'info_tab'", NULL), 
      
      # SIDEBAR POUR DATATABLE
      conditionalPanel(condition = "input.selected_tab == 'table'", NULL),    
      
      # SIDEBAR POUR PLOT
      conditionalPanel(condition = "input.selected_tab == 'plot'",            
                       selectInput(inputId = "country_select", # Choix pays 1
                                   label   = "Pays",
                                   choices = setNames(country_list$country_code, nm = country_list$country_name_fr)),
                       selectInput(inputId = "country_select_2", # Choix pays 2
                                   label   = "Comparer",
                                   choices = setNames(c("NONE", country_list$country_code[2:length(country_list$country_code)]), 
                                                      nm = c("-", country_list$country_name_fr[2:length(country_list$country_code)])))),
      # SIDEBAR POUR CARTE
      conditionalPanel(condition = "input.selected_tab == 'map'",             
                       checkboxInput(inputId = "avg_select",
                                     label = "Moyenne sur toutes les années disponibles",
                                     value = TRUE),
                       tags$p("Sélectionner une année ne fonctionne que si la case 'Moyenne sur toutes les années' n'est pas cochée.",
                              class = "year-select-text"),
                       sliderInput(inputId = "year_select",
                                   label   = "Année",
                                   min     = min(alcohol$year), # TODODODOO
                                   max     = max(alcohol$year),
                                   value   = max(alcohol$year),
                                   animate = animationOptions(interval = 500), # Pour le bouton 'play'
                                   sep     = " "),
                       selectInput(inputId = "palette_select", label = "Type de légende", 
                                   choices = c("Quantiles" = "quantile", "Numérique linéaire" = "numeric")),
                       radioButtons(inputId = "sex_select",
                                    label   = "Sexe", 
                                    choices = c("Total" = "all", "Femmes" = "female", "Hommes" = "male")),
                       checkboxInput(inputId = "per_million",
                                     label   = "Données en nombre de morts par million de personnes",
                                     value   = TRUE),
                       
                       tags$div(class = "display-options",
                         checkboxInput(inputId = "legend_select",
                                       label = "Afficher la légende",
                                       value = FALSE),
                         checkboxInput(inputId = "country_labels_select",
                                       label = "Afficher noms des pays",
                                       value = FALSE))
                       ),
      
    tags$hr(), # Ligne horizontale
    tags$footer( # Elément footer avec les sources et liens
      tags$a(tags$div(tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/9/91/Octicons-mark-github.svg", 
                               style = "max-width: 15px; margin-right: 10px;"), 
            tags$span("GitHub")), href="https://github.com/ceswap/pir-s3-project"),
      tags$h6("Sources"),
      tags$ul(class = "sources-list",
        tags$li(tags$a("WHO - European Health Information Gateway",
                       href="https://gateway.euro.who.int/en/indicators/hfamdb_87-deaths-alcohol-abuse-including-alcoholic-psychosis/")),
        tags$li(tags$a("World Administrative Boundaries, World Food Programme (UN agency)", 
                       href = "https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/information/")),
        tags$li(tags$a("UN, Department of Economic and Social Affairs, Population Division. World Population Prospects 2022", 
                       href = "https://population.un.org/wpp/Download/Standard/CSV/"))
        )
      )
    ),

    # PANNEAU PRINCIPAL ----
    mainPanel(
      tabsetPanel( # Onglets
        tabPanel("À propos",       htmlOutput("info_tab"),   value="info_tab"), # APROPOS
        tabPanel("Données brutes", dataTableOutput("table"), value="table"),    # DATATABLE
        tabPanel("Graphique",      plotlyOutput("plot"),     value="plot"),     # PLOT
        tabPanel("Carte",          htmlOutput("leaflet_title"), leafletOutput("leaflet"), value="map"), # MAP
        id = "selected_tab" # `id` et les `value` des tabPanel() sont nécessaires aux conditionalPanel() de la sidebar
        )
      )
  )
)

# SHINY SERVER ----
server <- function(input, output) {

  # OUTPUT A PROPOS
  output$info_tab <- renderUI(draw_info_HTML())
  
  # OUTPUT DATATABLE
  output$table <- renderDataTable(draw_table(FALSE),
                                  options = list("pageLength" = 10, 
                                                 "lengthMenu" = list(c(10, 25, 50, -1), c('10', '25', '50', 'Tout'))))
  
  # OUTPUT PLOT
  output$plot <- renderPlotly(draw_plotly(selected_country_code   = input$country_select, 
                                          selected_country_code_2 = input$country_select_2))
  # OUTPUT MAP
  output$leaflet <- renderLeaflet(draw_leaflet(avg = input$avg_select, 
                                               sex_column = input$sex_select, 
                                               selected_year = input$year_select,
                                               selected_palette = input$palette_select,
                                               country_labels = input$country_labels_select,
                                               legend = input$legend_select,
                                               per_million = input$per_million))
  
  # Output pour le titre en plus dans onglet Carte
  output$leaflet_title <- renderUI(tags$h4(if_else(input$avg_select, 
                                                   "Moyenne par pays", 
                                                   paste0("Données pour l'année ", input$year_select))))
}

# RUN IT
if (Sys.getenv("RSTUDIO") == "1") {
  shinyApp(ui, server) 
} else {
  runGadget(ui, server, viewer = browserViewer(browser = getOption("browser")))
}

# runGadget(ui, server, viewer = dialogViewer(dialogName = "Projet PIR", width = 1280, height = 800))
