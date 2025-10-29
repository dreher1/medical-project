# global.R - Loads once when app starts
library(shiny)
library(markdown)
library(leaflet)
library(reactable)
library(sf)
library(tigris)
library(dplyr)
library(readxl)

options(tigris_use_cache = TRUE, tigris_class = "sf")

# Load your cleaned melanoma data
melanoma_table <- read.csv("cleaned_melanoma_table.csv", stringsAsFactors = FALSE)

# Ensure FIPS is 5-digit character with leading zeros
melanoma_table$fips_melanoma <- sprintf("%05d", as.numeric(melanoma_table$fips_melanoma))

# Convert to numeric
##don't think need code below
#melanoma_table$avg_annual_ct <- as.numeric(melanoma_table$avg_annual_ct)

# Get county shapes (we'll filter by state when needed)
counties_sf <- tigris::counties(cb = TRUE, year = 2023) |> 
  sf::st_transform(4326)


# Get state shapes
states_sf <- tigris::states(cb = TRUE, year = 2023) |> 
  sf::st_transform(4326)
keep_names <- setdiff(state.name, "Alaska")
states_sf <- states_sf[states_sf$NAME %in% c(keep_names, "District of Columbia"), ]


uv_table <- read.csv("cleaned_uv_table.csv", stringsAsFactors = FALSE)
uv_table$state_uv <- trimws(uv_table$state_uv)
#uv_table$fips_uv <- sprintf("%05d", as.numeric(uv_table$fips_uv))
uv_table$uv_value <- as.numeric(uv_table$uv_whm2)
View(uv_table)