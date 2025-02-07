# load packages --------------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(shinydashboard)
library(DT)
library(lifecontingencies)
library(tidyverse)
library(ggplot2)
library(sass)
library(bslib)

# COMPILE CSS ----
# conflict bslibs on utils
page <- bslib::page

sass(
  input = sass_file("www/styles.scss"),
  output = "www/sass-styles.css",
  options = sass_options(output_style = "compressed") # OPTIONAL, but speeds up page load time by removing white-space & line-breaks that make css files more human-readable
)

# extend shiny theme to plots
# thematic::thematic_shiny()

# source R folder ----
source("R/label_number.R")
source("R/life_insurance_portfolio.R")
source("R/tables.R")
source("R/plots.R")