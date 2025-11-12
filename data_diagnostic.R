# diagnostic_check.R
suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
})

# Scope of the map (adjust if your app includes AK or territories)
target_states <- c(setdiff(state.abb, "AK"), "DC")

# 1) Polygons on the map that have NO demographic value (holes in choropleth)
shape_no_demo <- counties_sf %>%
  filter(STUSPS %in% target_states) %>%
  left_join(
    county_demographics %>% select(fips_demo, white_not_h_or_l_pct),
    by = c("GEOID" = "fips_demo")
  ) %>%
  filter(is.na(white_not_h_or_l_pct)) %>%
  st_drop_geometry() %>%
  select(NAME, STUSPS, GEOID) %>%
  arrange(STUSPS, NAME)

cat("\n=== SHAPES WITH NO JOINED DEMOGRAPHICS ===\n")
print(shape_no_demo, n = Inf)
cat(sprintf("\nCount: %d\n", nrow(shape_no_demo)))

# 2) Demographic rows that never matched any polygon (name/FIPS issues)
demo_no_shape <- county_demographics %>%
  filter(is.na(fips_demo)) %>%
  select(county_demo, state_abr_demo, county_clean, state_clean) %>%
  arrange(state_clean, county_clean)

cat("\n=== DEMOGRAPHICS ROWS WITH NO MATCHED SHAPE ===\n")
print(head(demo_no_shape, 50))
cat(sprintf("\nTotal rows with no match: %d\n\n", nrow(demo_no_shape)))

# Optional CSVs for offline review
# write.csv(shape_no_demo, "shape_no_demo.csv", row.names = FALSE)
# write.csv(demo_no_shape, "demo_no_shape.csv", row.names = FALSE)

