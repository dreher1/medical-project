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


#Code to load population dataset 
county_population <- read_excel("co-est2024-pop (1).xlsx")
colnames(county_population)[1] <- "county_state_pop"
#county_population$county_state_pop <- str_remove(county_population$county_state_pop, "^\\.+\\s*")

# Load cleaned demographics data
county_demographics <- read.csv("county_demographics_cleaned.csv", stringsAsFactors = FALSE)

# Remove " County", " Parish", " Borough", etc. from county_demo to match counties_sf format
library(stringr)
county_demographics <- county_demographics %>%
  mutate(
    county_clean = toupper(str_remove(county_demo, "\\s+(County|Parish|Borough|Census Area|Municipality|City and Borough|City)$")),
    state_clean = toupper(trimws(state_abr_demo))
  )

# Create lookup from counties_sf
county_lookup <- counties_sf %>%
  st_drop_geometry() %>%
  mutate(
    county_clean = toupper(trimws(NAME)),
    state_clean = STUSPS
  ) %>%
  select(GEOID, county_clean, state_clean)

# Join to get FIPS
county_demographics <- county_demographics %>%
  left_join(county_lookup, by = c("county_clean", "state_clean")) %>%
  rename(fips_demo = GEOID)

# Check results
cat("Matched:", sum(!is.na(county_demographics$fips_demo)), "out of", nrow(county_demographics), "counties\n")