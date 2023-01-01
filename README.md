# Projet de fin de semestre 2022-2023

**PIR S3 Langage de Traitement des Données**

Robinson Maury (DFGSM3) & César Wapler (DFGSM3)

## Téléchargement

Téléchargez le zip de ce projet sur le [dépôt GitHub](https://github.com/ceswap/pir-s3-project) en cliquant sur le bouton vert "Code" puis "Download ZIP".

-   `app.R` est le fichier principal contenant la totalité du code pour l'application R Shiny.

-   `data/` contient les fichiers de données utilisées (`.csv`, `.shp`)

-   `Rapport.Rmd` est le compte-rendu écrit pour l'évaluation.

## Installation

Les fichiers données nécessaires sont déjà téléchargés et présents dans `data/`, ce qui rend la taille du projet non négligeable. Cependant l'application tente de les télécharger automatiquement si ils ne sont pas présents (à l'exception d'un fichier en particulier sur certains systèmes (problème OpenSSL avec serveur de téléchargement)).

## Sources

Voici les sources d'où proviennent les données nécessaires :

| Organisation                                                | Data set                                                                                                                                                          | Type      |
|--------------------------------|----------------------|------------------|
| WHO European Health Information Gateway                     | [Deaths(\#), Alcohol abuse (incl. alcoholic psychosis)](https://gateway.euro.who.int/en/indicators/hfamdb_87-deaths-alcohol-abuse-including-alcoholic-psychosis/) | CSV       |
| opendatasoft                                                | [World Administrative Boundaries - Countries and Territories](https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/information/)       | Shapefile |
| UN Dept. of Economic and Social Affairs Population Division | [World Population Prospects 2022, Total Pop on Jul 1](https://population.un.org/wpp/Download/Standard/CSV/)                                                       | CSV       |
