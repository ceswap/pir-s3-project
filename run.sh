#! /bin/bash

if [ $(printenv | grep 'RSTUDIO=1') ]; then
  echo 'Fermez RStudio et réessayez depuis le terminal !!'
  exit
fi

Rscript -e 'shiny::runApp('')'