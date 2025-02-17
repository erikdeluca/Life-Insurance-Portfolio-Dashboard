# load packages --------------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(shinydashboard)
library(DT)
# library(lifecontingencies)
library(tidyverse)
library(ggplot2)
library(sass)
library(bslib)

# COMPILE CSS ----
# conflict bslibs on utils
conflicted::conflict_prefer("page", "bslib")
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("box", "shinydashboard")
# conflicted::conflict_prefer("dataTableOutput", "DT")
# conflicted::conflict_prefer("renderTableOutput", "DT")
conflicted::conflict_prefer_all("DT", "shiny")
# conflicted::conflict_scout()

sass(
  input = sass_file("www/styles.scss"),
  output = "www/sass-styles.css",
  options = sass_options(output_style = "compressed") # OPTIONAL, but speeds up page load time by removing white-space & line-breaks that make css files more human-readable
)

# extend theme to shinyw widgets 
# shinyWidgets:::html_dependency_picker_bs(theme = bslib::bs_theme(version = 5))

# extend shiny theme to plots
# thematic::thematic_shiny()

# source R folder ----
source("R/label_number.R")
source("R/life_insurance_portfolio.R")
source("R/tables.R")
source("R/plots.R")

# load data --------------------------------------------------------------

# HLD data ----
life_tables <- read_csv("data/data_cleaned.csv")

map(
  unique(life_tables$country),
  ~ life_tables |> 
    filter(country %in% .x) |>
    distinct(year) |> 
    pull(year) 
) |> 
  set_names(unique(life_tables$country)) -> mapping_life_tables
# mapping_life_tables |> str()

# SOA data ----
soa_tables <- read_rds("data/soa_tables.rds")

mapping_soa_countries <- names(soa_tables)

soa_tables |> 
  names() |> 
  str_remove("soa_") |> 
  str_replace_all("_", " ") |>
  str_to_title() -> names(mapping_soa_countries)
  # set_names(names(soa_tables)) -> mapping_soa_countries

map(
  names(soa_tables),
  ~ soa_tables[[.x]] |> 
    # select(-age) |> 
    names()
) |> 
  set_names(names(soa_tables)) -> mapping_soa_tables


