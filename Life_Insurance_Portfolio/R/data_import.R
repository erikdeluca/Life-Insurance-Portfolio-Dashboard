# HLD --------------------------------------------------------------------------------------------
data <- read_csv(here::here("Life_Insurance_Portfolio", "data", "data_original.csv"))


data |> glimpse()

# data |> distinct(Country) |> print(n = 132)

data |> 
  # filter(Country %in% c("ITA", "ESP")) |> 
  filter(Year1 == max(Year1), .by = Country) |>  
  filter(TypeLT == 1, if_all(c(Residence, Ethnicity, Region), \(x) x == 0)) |> 
  # mutate(across(everything(), as.factor)) |>  
  # distinct(Country)
  # print(n = 200)
  # distinct(Region)
  summarise(
    n_rows = n(),
    across(everything(), \(x) length(unique(x)), .names = "n_{.col}"),
  )


data |>
  # take the most recent table for each country 
  filter(Year1 %in% c(2000:2025), .by = Country) |> 
  # take only the overall life tables, right now I don't take into account the other types
  filter(TypeLT == 1, if_all(c(Residence, Ethnicity, Region), \(x) x == 0)) |> 
  # take only the columns necessary for the analysis
  select(Country, Year1, Sex, Age, `q(x)`, `l(x)`, `d(x)`) |>
  rename(year = Year1) |> 
  janitor::clean_names() |>
  mutate(id = paste(country, year, sep = "_"), .before = 1) -> data_cleaned

write_csv(data_cleaned, here::here("Life_Insurance_Portfolio", "data", "data_cleaned.csv"))


## ATTENTION: some life tables are wrong, e.g. they finish in 80s and in the last year d(x) is 23000 of 100000 starting people
# life_tables |> filter(country == "BRA", year == 2000, sex ==1) |> print(n = 100)
# life_tables |> filter(country == "ITA", year == 2000, sex ==1) |> print(n = 110)


# Lifecontingencies package --------------------------------------------------------------
# library(lifecontingencies)
# map(
#   data(package = "lifecontingencies")$results[, "Item"],
#   ~ get(.x) |> str()
# 
#   )
# 
# get("demoUsa")

# SOA Life Tables -----------------------------------------------------------------------
# https://mort.soa.org/

# function ----
read_soa_tables <- function(file) {
  # read full file as plain text and find the second time with a blank line
  start_index <- which(readLines(file) == "")[2] + 1
  
  # separate metadata and data
  metadata <- read_csv(file = file, col_names = FALSE, n_max = start_index - 1) |> 
    mutate(across(X1, janitor::make_clean_names)) |> 
    pivot_wider(names_from = X1, values_from = X2)
  
  table_name <- pull(metadata, table_name) |> janitor::make_clean_names()
  
  data <- read_csv(file = file, col_names = c("age", table_name), skip = start_index)
  
  list(
    metadata = metadata,
    data = data
  )
}

# pipeline ----

# get the names into SOA folder
countries_zip <- list.files(here::here("Life_Insurance_Portfolio", "data", "SOA"), full.names = FALSE, pattern = "\\.zip$")
countries <- str_remove(countries_zip, "\\.zip$")

# unzip the files
map2(
  countries_zip,
  countries,
  ~ unzip(here::here("Life_Insurance_Portfolio", "data", "SOA", .x), exdir = here::here("Life_Insurance_Portfolio", "data", "SOA", .y))
)

# for each country get the files
soa_files <- map(
  countries,
  ~ list.files(here::here("Life_Insurance_Portfolio", "data", "SOA", .x), pattern = "\\.csv$", full.names = TRUE)
)
names(soa_files) <- countries

# read_csv(csv_files[9])
# 
# read_soa_tables(csv_files[8])$metadata |> mutate(across(X1, janitor::make_clean_names)) |> pivot_wider(names_from = X1, values_from = X2)
# read_soa_tables(csv_files[9])

# read the tables for each coutry and join tables by age and country
map(
  countries,
  ~ map(
    soa_files[[.x]],
    ~ read_soa_tables(.x)$data
  ) |> 
    reduce(full_join, by = "age") |> 
    mutate(
      # if is the last year set q_x to 1
      across(everything(), \(x) ifelse(is.na(x) & !is.na(lag(x)), 1, x)),
      across(everything(),
             \(x)
             if(mean(x, na.rm = T) < 1){
                accumulate(x, ~ .x * (1 - ifelse(is.na(.y), 0, .y)), .init = 1e5) |>
                tail(-1) |>
                lag(default = 1e5) |>
                round(0)
             }
        )
      )
) -> soa_tables
# soa_tables[[2]] |> print(n = 150)

names(soa_tables) <- countries

map_dfr(
  countries,
  ~ map(
    soa_files[[.x]],
    ~ read_soa_tables(.x)$metadata
  )
) -> soa_metadata

# save files
map(
  countries,
  ~ write_csv(soa_tables[[.x]], here::here("Life_Insurance_Portfolio", "data", "SOA", paste0(.x, ".csv")))
)

write_rds(soa_tables, here::here("Life_Insurance_Portfolio", "data", "soa_tables.rds"))

write_csv(soa_metadata, here::here("Life_Insurance_Portfolio", "data", "SOA", "soa_metadata.csv"))




# soa_tables[[1]] |> 
#   mutate(
#     # if is the last year set q_x to 1 
#     across(everything(), \(x) ifelse(is.na(x) & !is.na(lag(x)), 1, x)),
#     across(everything(),
#            \(x)
#            if(mean(x, na.rm = T) < 1){
#               accumulate(x, ~ .x * (1 - ifelse(is.na(.y), 0, .y)), .init = 1e5) |> 
#               tail(-1) |> 
#               lag(default = 1e5) |> 
#               round(0)
#            }
#       )
#     ) |> print(n = 150)

  
