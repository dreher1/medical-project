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
counties_sf <- tigris::counties(cb = TRUE, year = 2020) |> 
  sf::st_transform(4326)


# Get state shapes
states_sf <- tigris::states(cb = TRUE, year = 2020) |> 
  sf::st_transform(4326)
keep_names <- setdiff(state.name, "Alaska")
states_sf <- states_sf[states_sf$NAME %in% c(keep_names, "District of Columbia"), ]


uv_table <- read.csv("cleaned_uv_table.csv", stringsAsFactors = FALSE)
uv_table$state_uv <- trimws(uv_table$state_uv)
uv_table$uv_value <- as.numeric(uv_table$uv_whm2)
View(uv_table)

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


# Calculate US-wide quantile breaks for bivariate maps
# This ensures consistent color classification across all states

# Get all counties with both UV and melanoma data
us_wide_data <- melanoma_table %>%
  left_join(
    uv_table %>% 
      mutate(fips_uv = sprintf("%05d", as.numeric(fips_uv))) %>%
      select(fips_uv, uv_value),
    by = c("fips_melanoma" = "fips_uv")
  ) %>%
  filter(!is.na(age_adj_inc_rate) & !is.na(uv_value))

# Calculate US-wide breaks for UV
us_uv_breaks <- quantile(us_wide_data$uv_value, 
                         probs = c(0, 1/3, 2/3, 1), 
                         na.rm = TRUE)

# Calculate US-wide breaks for melanoma rate
us_melanoma_breaks <- quantile(us_wide_data$age_adj_inc_rate, 
                               probs = c(0, 1/3, 2/3, 1), 
                               na.rm = TRUE)

# Calculate US-wide breaks for melanoma per white population
us_wide_data_weighted <- us_wide_data %>%
  left_join(
    county_demographics %>% select(fips_demo, white_not_h_or_l_pct),
    by = c("fips_melanoma" = "fips_demo")
  ) %>%
  filter(!is.na(white_not_h_or_l_pct), white_not_h_or_l_pct >= 10) %>%
  mutate(melanoma_per_white = age_adj_inc_rate / (white_not_h_or_l_pct / 100))

us_melanoma_per_white_breaks <- quantile(us_wide_data_weighted$melanoma_per_white, 
                                         probs = c(0, 1/3, 2/3, 1), 
                                         na.rm = TRUE)

# Display the breaks for reference
cat("\n=== US-Wide Bivariate Breaks ===\n")
cat("UV breaks (W/m²):", round(us_uv_breaks), "\n")
cat("Melanoma rate breaks (per 100k):", round(us_melanoma_breaks, 1), "\n")
cat("Melanoma per white breaks (per 100k white):", round(us_melanoma_per_white_breaks, 1), "\n")


