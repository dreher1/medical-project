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


# Get county shapes (we'll filter by state when needed)
counties_sf <- tigris::counties(cb = TRUE, year = 2020) |> 
  sf::st_transform(4326)


# Get state shapes
states_sf <- tigris::states(cb = TRUE, year = 2020) |> 
  sf::st_transform(4326)
states_sf <- states_sf[states_sf$NAME %in% c(state.name, "District of Columbia"), ]


uv_table <- read.csv("cleaned_uv_table.csv", stringsAsFactors = FALSE)
uv_table$state_uv <- trimws(uv_table$state_uv)
uv_table$uv_value <- as.numeric(uv_table$uv_whm2)


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

# ==============================================================================
# FIX VIRGINIA INDEPENDENT CITIES AND OTHER SPECIAL CASES
# ==============================================================================

# Manual fixes for known mismatches
manual_fixes <- data.frame(
  demo_county_clean = c("ALEXANDRIA", "BEDFORD", "BRISTOL", "BUENA VISTA",
                        "CHARLOTTESVILLE", "CHESAPEAKE", "COLONIAL HEIGHTS",
                        "COVINGTON", "DANVILLE", "EMPORIA", "FAIRFAX",
                        "FALLS CHURCH", "FRANKLIN", "FREDERICKSBURG",
                        "GALAX", "HAMPTON", "HARRISONBURG", "HOPEWELL",
                        "LEXINGTON", "LYNCHBURG", "MANASSAS", "MANASSAS PARK",
                        "MARTINSVILLE", "NEWPORT NEWS", "NORFOLK", "NORTON",
                        "PETERSBURG", "POQUOSON", "PORTSMOUTH", "RADFORD",
                        "RICHMOND", "ROANOKE", "SALEM", "STAUNTON",
                        "SUFFOLK", "VIRGINIA BEACH", "WAYNESBORO",
                        "WILLIAMSBURG", "WINCHESTER",
                        "BALTIMORE", "CARSON", "DONA ANA", "LA SALLE", "ST. LOUIS"),
  state_abbr = c(rep("VA", 39), "MD", "NV", "NM", "LA", "MO"),
  fips = c("51510", "51515", "51520", "51530", "51540", "51550", "51570",
           "51580", "51590", "51595", "51600", "51610", "51620", "51630",
           "51640", "51650", "51660", "51670", "51678", "51680", "51683",
           "51685", "51690", "51700", "51710", "51720", "51730", "51735",
           "51740", "51750", "51760", "51770", "51775", "51790", "51800",
           "51810", "51820", "51830", "51840",
           "24510", "32510", "35013", "22059", "29510"),
  stringsAsFactors = FALSE
)

# Apply manual fixes - match on the base name without " CITY" suffix
for(i in 1:nrow(manual_fixes)) {
  # Remove " CITY" from county_clean for matching
  base_name <- sub(" CITY$", "", county_demographics$county_clean)
  
  county_demographics$fips_demo[
    base_name == manual_fixes$demo_county_clean[i] &
      county_demographics$state_clean == manual_fixes$state_abbr[i]
  ] <- manual_fixes$fips[i]
}

cat("Applied", nrow(manual_fixes), "manual FIPS fixes\n")

# Verify improvement
cat("Counties with FIPS after manual fixes:", sum(!is.na(county_demographics$fips_demo)), "\n")

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


