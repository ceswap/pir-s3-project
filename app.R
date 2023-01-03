# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                     #
#   PIR Langage de Traitement des Données 2022-2023   #
#             Projet de fin de semestre               #
#           Robinson Maury et César Wapler            #
#                                                     #
#                                      voir README.md #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# PACKAGES & WORKING DIR ----

# Pour se placer dans le dossier de ce fichier
if (Sys.getenv("RSTUDIO") == "1") {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  setwd(getSrcDirectory()[1])
}

# Installation (si besoin) et chargement
packages <- c("tidyverse", "shiny", "shinyjs",  "plotly", "leaflet", "sf")

installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))
remove(packages, installed_packages)

# DATA SETUP ====

# Fonction pour gérer le téléchargement des fichiers si ils ne sont pas déjà présents
download_rename_file <- function(url, filename, dir = ".") {
  name   <- sub("\\..*", "", filename)
  ext    <- sub(".*?\\.", ".", filename)
  search <- grepl(pattern = paste0(name, ".*"), x = list.files(path=dir))
  zip    <- any(grepl("\\.zip", ext))
  
  if (!any(search)) {
    cat("Creating directory", file.path(dir, ""), "if it does not exist...\n")
    dir.create(dir, showWarnings = FALSE) # Pretty sure this is not necessary
    filename <- paste0(name, "_", Sys.Date(), "_", format(Sys.time(), "%H-%M"), ext)
    filepath <- file.path(dir, filename)
    cat("Downloading file from URL to", filepath, "...\n")
    download.file(url, filepath)
    
    if (zip) {
      unzipped <- unzip(filepath, exdir = dir)
      cat("Unzipped contents of", filename, "to", file.path(dir, ""), "\n")
      return(unzipped)
    }
    return(filepath)
    
  } else {
    existing_filepath <- list.files(path = dir, full.names = TRUE)[which(search == TRUE)][1]
    cat("File exists:", existing_filepath, "\n")
    
    if (zip) {
      unzipped <- unzip(existing_filepath, exdir = dir)
      cat("Unzipped contents of", filename, "to", file.path(dir, ""), "\n")
      return(unzipped)
    }
    return(existing_filepath)
  }
}

delete_file <- function(filepath_s) {
  if (all(file.exists(filepath_s))) {
    unlink(filepath_s) # on supprime le(s) fichier(s)
    cat("Deleted file(s)", filepath_s, "\n")
  } else {
    cat("Coundn't find and delete file(s):", filepath_s, "\n")
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
pop_unzipped_filepath <- download_rename_file(
  url = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_TotalPopulationBySex.zip",
  filename = "population.zip",
  dir = "data") 

# Chargement et transformations
# population <- read_delim(file.path("data", population_unzipped_filename), delim = ",") %>% # ORIGINAL LINE, crashed RStudio on the server
population <- read.csv(pop_unzipped_filepath) %>% as.tibble() %>%
  filter(LocTypeName == "Country/Area") %>% # exclusion des données agrégées
  select(country_code = ISO3_code,
         year         = Time,
         pop_all      = PopTotal, 
         pop_female   = PopFemale,
         pop_male     = PopMale) %>% 
  mutate(across(starts_with("pop_"), ~ .x / 1000)) # changement d'unité de millier à million d'habitants

delete_file(pop_unzipped_filepath) # Supprimer le fichier décompressé lourd (60.3 MB pour celui ci)

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
  mutate(highest_year         = year[which.max(deaths_all)], # L'année avec le plus de morts
         highest_year_million = year[which.max(deaths_all_million)],
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
geo_unzipped_filepaths <- download_rename_file(
  url = "https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/download/?format=shp&timezone=Europe/Paris&lang=en", 
  filename = "geo_world.zip",  dir = "data")

# Chargement du Shapefile et cuisine des colonnes, 
geo_world <- read_sf(geo_unzipped_filepaths[[grep(".*\\.shp$", geo_unzipped_filepaths)]]) %>% 
  select(country_code = iso3,
         continent,
         country_name_fr = french_shor,
         geometry) %>% 
  mutate(geometry_centroid = st_centroid(geometry), # Génération d'un POINT centroid pour chaque POLYGON (mais peu utilisé)
         country_name_fr = if_else(grepl("Royaume-Uni", country_name_fr), "Royaume-Uni", country_name_fr)) # Le nom du R-U était trop long

delete_file(geo_unzipped_filepaths)

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

remove(population, alcohol_csv_path, geo_world, pop_unzipped_filepath, 
       geo_unzipped_filepaths, download_rename_file, delete_file) # Plus besoin de tout ça

# FIN DE SETUP

# FONCTIONS DRAW POUR SHINY SERVER ====

  # FONCTION POUR 'A PROPOS' ---- 
draw_info_HTML <- function() {
  about_html <- tags$div(
    tags$div(
      tags$h3("À propos"),
        tags$p("Données provenant du data set",
               tags$em("Alcohol abuse (including alcoholic psychosis), number of deaths, by sex"),
               "disponible sur le European Health Information Gateway de l'OMS.", tags$br(),
               "Inclus le nombre de morts par \"abus d'alcool\" par pays d'Europe et par année de 1979 à 2020. Pas de données pour la Russie et Andorre.")
      ),
      tags$div(
        tags$hr(), # Ligne horizontale
        tags$footer( # Elément footer avec les sources et liens
          tags$a(tags$div(tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/9/91/Octicons-mark-github.svg", 
                                   class = "github-img"), 
                          tags$span("Dépôt Github")), href="https://github.com/ceswap/pir-s3-project"),
          tags$h6("Sources"),
          tags$ul(class = "sources-list",
                  tags$li("WHO - European Health Information Gateway", tags$br(),
                          tags$a("Alcohol abuse (including alcoholic psychosis), number of deaths, by sex",
                                 href="https://gateway.euro.who.int/en/indicators/hfamdb_87-deaths-alcohol-abuse-including-alcoholic-psychosis/")),
                  tags$li("World Food Programme (UN agency) via opendatasoft", tags$br(),
                          tags$a("World Administrative Boundaries - Countries and Territories", 
                                 href = "https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/information/")),
                  tags$li("UN Department of Economic and Social Affairs - Population Division", tags$br(),
                          tags$a("World Population Prospects 2022 - Total Population on 01 July.", 
                                 href = "https://population.un.org/wpp/Download/Standard/CSV/")))
        )
      )
    )
    
    return (about_html)
}

  # FONCTION POUR 'TABLEAU' ---- 
draw_table <- function(stats = FALSE, per_million = FALSE) {
  if (stats) {
    
    if (per_million) {
      alcohol_for_table <- alcohol_stats %>%
        select(ends_with("_million"), contains("year"), country_name_fr, max_million) %>% 
        rename(Pays = country_name_fr, `Année max morts/an / million` = highest_year_million, `Max morts/an / million` = max_million,
               `Moy. morts/an / million (H+F)` = deaths_all_million, 
               `Moy. morts/an / million (F)`   = deaths_female_million, 
               `Moy. morts/an / million (H)`   = deaths_male_million)
      
    } else { # if (!per_million)
      alcohol_for_table <- alcohol_stats %>%
        select(matches("^deaths_.{3,6}$"), ends_with("_year"), country_name_fr, max) %>% 
        rename(Pays = country_name_fr, `Année max morts/an` = highest_year, `Max morts/an` = max,
               `Moy. morts/an (H+F)` = deaths_all, 
               `Moy. morts/an (F)` = deaths_female, 
               `Moy. morts/an (H)` = deaths_male)
    }
    
    alcohol_for_table <- alcohol_for_table %>% 
      mutate(`Données dispo` = paste0(alcohol_stats$start_year, "-", alcohol_stats$end_year)) %>% 
      select(-ends_with("_year"))
    
  } else { # if (!stats)

    if (per_million) {
    alcohol_for_table <- alcohol %>%
      select(ends_with("_million"), country_name_fr, year) %>% 
      rename(Pays = country_name_fr, Année = year,
             `Morts / million (H+F)` = deaths_all_million, 
             `Morts / million (F)` = deaths_female_million, 
             `Morts / million (H)` = deaths_male_million)
      
    } else { # if (!per_million)
    alcohol_for_table <- alcohol %>%
      select(matches("^deaths_.{3,6}$"), country_name_fr, year) %>% 
        rename(Pays = country_name_fr, Année = year,
               `Morts (H+F)` = deaths_all, 
               `Morts (F)` = deaths_female, 
               `Morts (H)` = deaths_male)
    }
  }
  
  alcohol_for_table <- alcohol_for_table %>% 
    relocate(`Pays`, .before = 1)

  return(alcohol_for_table)
}

  # (FONCTION TOOLTIP POUR 'GRAPHIQUE') ----
tooltip <- function(series, year, deaths, per_million = FALSE) {
  return(paste0(series,
                "\n  Année : ", year,
                "\n  Morts : ", format(deaths, big.mark=" ")))
}

  # FONCTION POUR 'GRAPHIQUE' (GGPLOTLY) ----
draw_plotly <- function(selected_country_code = "ALL", selected_country_code_2 = "NONE", per_million = FALSE) {
  if (selected_country_code == "ALL" | !(selected_country_code %in% alcohol$country_code)) {
    alcohol_for_plot <- alcohol %>% 
      group_by(year) %>% 
      summarise(across(starts_with("deaths_"), .fns =  sum, na.rm = T)) %>% 
      mutate(country_code = "ALL",
             country_name_fr = "Somme totale") %>% 
      bind_rows(filter(alcohol, country_code == selected_country_code_2))
  } else {
    alcohol_for_plot <- alcohol %>%
      filter(country_code %in% c(selected_country_code, selected_country_code_2)) %>%
      group_by(year)
  }

  y_title <- paste("Morts par intoxications alcoolique par année", if_else(per_million, "par million", ""))
  
  plotly_title <- paste0(
    "Morts par intoxication alcoolique <br>par année ", if_else(per_million, "par million ", ""), "(",
       if_else(selected_country_code == "ALL", "somme totale", 
               country_list$country_name_fr[which(country_list$country_code == selected_country_code)]),
       if_else(selected_country_code_2 == "NONE", "",
               paste0(" et ", country_list$country_name_fr[which(country_list$country_code == selected_country_code_2)])),
       ")"
    )
  
  if (selected_country_code_2 != "NONE") {
    deaths <- if_else(per_million, "deaths_all_million", "deaths_all")
    plot <- alcohol_for_plot %>%
      ggplot(aes(x = year, y = .data[[deaths]], color = country_name_fr, group = 1)) +
      geom_line(aes(text = tooltip(country_name_fr, year, .data[[deaths]], per_million)), size = 1.2) +
      labs(x = "Année", y = y_title, title = "\n\n") +
      scale_colour_discrete(name = "") +
      theme_minimal()
    
  } else {
    deaths_all    <- if_else(per_million, "deaths_all_million", "deaths_all")
    deaths_female <- if_else(per_million, "deaths_female_million", "deaths_female")
    deaths_male   <- if_else(per_million, "deaths_male_million", "deaths_male")
    
    plot <- alcohol_for_plot %>%
      ggplot(aes(x = year, group = 1)) +
        geom_line(aes(y = .data[[deaths_all]],
                      color = "Total",
                      text = tooltip("Total",  year, .data[[deaths_all]], per_million)), size = 1.2) +
        geom_line(aes(y = .data[[deaths_female]],
                      color = "Femmes", 
                      text = tooltip("Femmes", year, .data[[deaths_female]], per_million)), size= 1.2) +
        geom_line(aes(y = .data[[deaths_male]],   
                      color = "Hommes", 
                      text = tooltip("Hommes", year, .data[[deaths_male]], per_million)), size = 1.2) +
        labs(x = "Année", y = y_title, title = "\n\n") +
        scale_colour_discrete(name = "") +
        theme_minimal()
  }
  
  return(ggplotly(plot, tooltip = c("text")) %>% layout(title = list(text = plotly_title)))
}

  # (FONCTION STATS TOOLTIP POUR LEAFLET) ----
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

  # FONCTION POUR 'CARTE' (LEAFLET) ----
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
              labFormat = labelFormat(suffix = if_else(selected_palette == "quantile", "", if_else(per_million, " morts/an/10^6", " morts/an"))),
              opacity = 0.75) else .} %>%
    setView(lng = 18, lat = 55, zoom = 3) %>%
    {if(country_labels) addLabelOnlyMarkers(., label = alcohol_for_leaflet$country_name_fr,
                      data = alcohol_for_leaflet$geometry_centroid,
                      labelOptions = country_labels_options
                      ) else .}

  return(leaflet_map)
}


# SHINY UI ====
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  # CSS STYLES ----
  tags$head(
    tags$style(HTML("
        .tab-pane.active { margin-top: 15px; }
        footer { font-size: 0.85rem; }
        .well { margin-bottom: 20px; }
        .sources-list {
          list-style: none;
          margin-top: 10px;
          padding-left: 10px;
        }
        .sources-list > li { margin-bottom: 10px; }
        .github-img {
          max-width: 15px; 
          margin-right: 10px;
          filter: brightness(5);
          -webkit-filter: brightness(5); 
        }
        .table { --bs-table-striped-bg: #282828; }
        #DataTables_Table_0 tfoot { display: none; }
        #DataTables_Table_0_filter > label { transform: translateX(-60px);}
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

  
  titlePanel("Alcohol Abuse"), # Titre de l'application
  
  # Sous-titre de l'application
  fluidRow(tags$p(paste("Morts par \"abus d'alcool\" par année en Europe de", min(alcohol$year), "à", max(alcohol$year)))),
  
  # UI LAYOUT PRINCIPAL avec sidebar ----
  sidebarLayout(
    
    # UI SIDEBAR ---- 
    sidebarPanel(width = 4,
      
      # Plusieurs conditionalPanel() pour modifier le contenu de la sidebar en fonction du tabset sélectionné
      # SIDEBAR POUR 'A PROPOS'
      conditionalPanel(condition = "input.selected_tab == 'info_tab'",
                       "Vous pouvez sélectionner un onlget pour accéder aux visualisations des données."), 
                        
      
      # SIDEBAR POUR 'TABLEAU'
      conditionalPanel(condition = "input.selected_tab == 'table'",
                       checkboxInput(inputId = "table_stats_select",
                                     label   = "Moyennes sur toutes les années",
                                     value   = FALSE),
                       checkboxInput(inputId = "table_per_million_select",
                                     label   = "Données par million d'habitants",
                                     value   = FALSE)),    
      
      # SIDEBAR POUR 'GRAPHIQUE'
      conditionalPanel(condition = "input.selected_tab == 'plot'",
                       checkboxInput(inputId = "plot_per_million_select",
                                     label = "Données par million d'habitants",
                                     value = FALSE),
                       selectInput(inputId = "country_select", # Choix pays 1
                                   label   = "Pays",
                                   choices = setNames(country_list$country_code, nm = country_list$country_name_fr)),
                       selectInput(inputId = "country_select_2", # Choix pays 2
                                   label   = "Comparer",
                                   choices = setNames(c("NONE", country_list$country_code[2:length(country_list$country_code)]), 
                                                      nm = c("-", country_list$country_name_fr[2:length(country_list$country_code)])))),
      # SIDEBAR POUR 'CARTE'
      conditionalPanel(condition = "input.selected_tab == 'map'",             
                       checkboxInput(inputId = "avg_select",
                                     label = "Moyenne sur toutes les années disponibles",
                                     value = TRUE),
                       tags$p("Sélectionner une année ne fonctionne que si la case 'Moyenne sur toutes les années' n'est pas cochée.",
                              class = "year-select-text"),
                       sliderInput(inputId = "year_select",
                                   label   = "Année",
                                   min     = min(alcohol$year),
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
                                       value = FALSE),
                         useShinyjs())
                       )
    ),

    # UI PANNEAU PRINCIPAL avec tabs (onglets) ----
    mainPanel(
      tabsetPanel( # Onglets
        tabPanel("À propos",  htmlOutput("info_tab"),   value="info_tab"), # APROPOS
        tabPanel("Tableau",   dataTableOutput("table"), value="table"),    # DATATABLE
        tabPanel("Graphique", plotlyOutput("plot"),     value="plot"),     # PLOT
        tabPanel("Carte",     htmlOutput("leaflet_title"), leafletOutput("leaflet"), value="map"), # MAP
        id = "selected_tab" # `id` et les `value` des tabPanel() sont nécessaires aux conditionalPanel() de la sidebar
        )
      )
  )
)

# SHINY SERVER ----
server <- function(input, output) {

  # OUTPUT POUR 'A PROPOS'
  output$info_tab <- renderUI(draw_info_HTML())
  
  # OUTPUT POUR 'TABLEAU'
  output$table <- renderDataTable(draw_table(stats = input$table_stats_select, per_million = input$table_per_million_select),
                                  options = list("pageLength" = 10, 
                                                 "lengthMenu" = list(c(10, 25, 50, -1), c('10', '25', '50', 'Tout'))))
  
  # OUTPUT POUR 'GRAPHIQUE'
  output$plot <- renderPlotly(draw_plotly(selected_country_code   = input$country_select, 
                                          selected_country_code_2 = input$country_select_2,
                                          per_million = input$plot_per_million_select))
  # OUTPUT POUR 'CARTE'
  output$leaflet <- renderLeaflet(draw_leaflet(avg = input$avg_select, 
                                               sex_column = input$sex_select, 
                                               selected_year = input$year_select,
                                               selected_palette = input$palette_select,
                                               country_labels = input$country_labels_select,
                                               legend = input$legend_select,
                                               per_million = input$per_million))
  
  # Désactiver le slider 'années' si 'Moyenne sur toutes les années' est coché
  observeEvent(input$avg_select, {
    if (input$avg_select) {
      shinyjs::disable("year_select")
    } else {
      shinyjs::enable("year_select")
    }
  })
  
  # Output pour le titre en plus dans onglet 'Carte'
  output$leaflet_title <- renderUI(tags$h4(if_else(input$avg_select, 
                                                   "Moyenne par pays", 
                                                   paste0("Données pour l'année ", input$year_select))))
}

# RUN IT ! ====
# Condition if qui vérifie si RStudio est ouvert ou pas, si on lance l'appli depuis le script `run.sh` par exemple...
if (Sys.getenv("RSTUDIO") == "1") {
  shinyApp(ui, server) 
  # runGadget(ui, server, viewer = dialogViewer(dialogName = "Projet PIR", width = 1280, height = 800))
} else {
  runGadget(ui, server, viewer = browserViewer(browser = getOption("browser")))
}
