# DATA MATCHING DIAGNOSTIC SCRIPT
# Run this separately to check your data quality
# Instructions: Source this file or run line-by-line in console

cat("========================================\n")
cat("  MELANOMA DATA DIAGNOSTIC TOOL\n")
cat("========================================\n\n")

# Load required libraries
library(dplyr)
library(tigris)
library(sf)

# Set working directory (change this to your project folder if needed)
# setwd("path/to/your/project")

# Load your data
cat("Loading melanoma data...\n")
melanoma_table <- read.csv("cleaned_melanoma_table.csv", stringsAsFactors = FALSE)

# DON'T trim - keep data exactly as it is in the file
melanoma_table$fips_melanoma <- sprintf("%05d", as.numeric(melanoma_table$fips_melanoma))

# Get county shapes
cat("Downloading county shapefiles (this may take a moment)...\n")
counties_sf <- tigris::counties(cb = TRUE, year = 2023) %>% sf::st_transform(4326)

cat("\n========================================\n")
cat("  OVERALL DATA SUMMARY\n")
cat("========================================\n")
cat("Total rows in melanoma data:", nrow(melanoma_table), "\n")
cat("Total US counties in map:", nrow(counties_sf), "\n")
cat("Unique states in your data:", length(unique(melanoma_table$state)), "\n\n")

# Check state name consistency
cat("========================================\n")
cat("  STATE NAMES IN YOUR DATA (AS STORED)\n")
cat("========================================\n")
state_list <- sort(unique(melanoma_table$state))
state_list <- state_list[!is.na(state_list)]  # Remove NA if present
for (i in 1:length(state_list)) {
  count <- sum(melanoma_table$state == state_list[i], na.rm = TRUE)
  # Show with quotes to reveal whitespace
  cat(sprintf("%2d. '%-24s' (%3d counties)\n", i, state_list[i], count))
}

# Function to check a specific state
check_state <- function(state_name, verbose = TRUE) {
  
  # Trim the input state_name to match against state.name, but keep data as-is
  state_name_trimmed <- trimws(state_name)
  
  # Get state abbreviation using trimmed name
  state_abbr <- state.abb[match(state_name_trimmed, state.name)]
  if (is.na(state_abbr) && state_name_trimmed == "District of Columbia") {
    state_abbr <- "DC"
  }
  
  if (is.na(state_abbr)) {
    if (verbose) cat("ERROR: Cannot find state abbreviation for '", state_name, "'\n", sep = "")
    return(list(state = state_name, error = "No abbreviation found"))
  }
  
  # Get counties from map
  state_counties_map <- counties_sf[counties_sf$STUSPS == state_abbr, ]
  
  # Get counties from your data - use EXACT state name as stored
  state_counties_data <- melanoma_table[melanoma_table$state == state_name & !is.na(melanoma_table$state), ]
  
  # Find mismatches
  map_fips <- state_counties_map$GEOID
  data_fips <- state_counties_data$fips_melanoma
  
  in_map_not_data <- setdiff(map_fips, data_fips)
  in_data_not_map <- setdiff(data_fips, map_fips)
  
  # Calculate match statistics
  matched_count <- length(intersect(map_fips, data_fips))
  match_rate <- if (length(map_fips) > 0) matched_count / length(map_fips) * 100 else 0
  
  if (verbose) {
    cat("\n========================================\n")
    cat("  CHECKING: '", state_name, "'\n", sep = "")
    cat("========================================\n")
    cat("Counties in map shapefile:", length(map_fips), "\n")
    cat("Counties in your data:", length(data_fips), "\n")
    cat("Counties matched:", matched_count, "\n")
    cat("Match rate:", round(match_rate, 1), "%\n")
    
    if (length(in_map_not_data) > 0) {
      cat("\nMISSING from your data (", length(in_map_not_data), " counties):\n", sep = "")
      missing <- state_counties_map[state_counties_map$GEOID %in% in_map_not_data, c("GEOID", "NAME")]
      print(missing)
    }
    
    if (length(in_data_not_map) > 0) {
      cat("\nEXTRA in your data (", length(in_data_not_map), " counties - not in 2023 map):\n", sep = "")
      extra <- state_counties_data[state_counties_data$fips_melanoma %in% in_data_not_map, c("fips_melanoma", "county")]
      print(extra)
    }
  }
  
  return(list(
    state = state_name,
    abbr = state_abbr,
    map_count = length(map_fips),
    data_count = length(data_fips),
    matched = matched_count,
    match_rate = match_rate,
    missing_from_data = length(in_map_not_data),
    extra_in_data = length(in_data_not_map)
  ))
}

# Check ALL states and DC
cat("\n\n========================================\n")
cat("  CHECKING ALL STATES + DC\n")
cat("========================================\n\n")

# Get all states from your data EXACTLY as stored (with spaces)
all_states <- sort(unique(melanoma_table$state))
all_states <- all_states[!is.na(all_states)]  # Remove NA if present

# Initialize results
results <- list()

# Check each state using the exact name as stored
for (state in all_states) {
  results[[state]] <- check_state(state, verbose = FALSE)
}

# Create summary table
cat("\n========================================\n")
cat("  SUMMARY TABLE - ALL STATES\n")
cat("========================================\n\n")

cat(sprintf("%-27s %8s %8s %8s %10s %10s %10s\n", 
            "State (as stored)", "MapCnty", "DataCnty", "Matched", "Match%", "Missing", "Extra"))
cat(paste(rep("-", 97), collapse = ""), "\n")

for (state in all_states) {
  r <- results[[state]]
  if (is.null(r$error)) {
    # Add quotes to show exact storage
    cat(sprintf("'%-25s' %8d %8d %8d %9.1f%% %10d %10d\n",
                r$state, r$map_count, r$data_count, r$matched, 
                r$match_rate, r$missing_from_data, r$extra_in_data))
  } else {
    cat(sprintf("'%-25s' %s\n", r$state, r$error))
  }
}

# Identify problem states
cat("\n========================================\n")
cat("  PROBLEM STATES (Match Rate < 100%)\n")
cat("========================================\n\n")

problem_states <- c()
for (state in all_states) {
  r <- results[[state]]
  if (!is.null(r$match_rate) && r$match_rate < 100) {
    problem_states <- c(problem_states, state)
    cat(sprintf("'%-25s': %5.1f%% match (%d missing, %d extra)\n",
                state, r$match_rate, r$missing_from_data, r$extra_in_data))
  }
}

if (length(problem_states) == 0) {
  cat("No problems found! All states have 100% match rate.\n")
}

# Overall statistics
cat("\n========================================\n")
cat("  OVERALL STATISTICS\n")
cat("========================================\n")

total_map <- sum(sapply(results, function(x) if (is.null(x$error)) x$map_count else 0))
total_data <- sum(sapply(results, function(x) if (is.null(x$error)) x$data_count else 0))
total_matched <- sum(sapply(results, function(x) if (is.null(x$error)) x$matched else 0))
overall_match_rate <- if (total_map > 0) total_matched / total_map * 100 else 0

cat("Total counties in all maps:", total_map, "\n")
cat("Total counties in your data:", total_data, "\n")
cat("Total matched:", total_matched, "\n")
cat("Overall match rate:", round(overall_match_rate, 2), "%\n")

cat("\n========================================\n")
cat("  DETAILED CHECK FOR PROBLEM STATES\n")
cat("========================================\n")

if (length(problem_states) > 0) {
  cat("\nRunning detailed diagnostics for states with issues...\n")
  for (state in problem_states) {
    check_state(state, verbose = TRUE)
  }
} else {
  cat("\nNo problem states to check in detail!\n")
}

cat("\n========================================\n")
cat("  DIAGNOSTIC COMPLETE\n")
cat("========================================\n")
cat("\nNOTE: Your state names have trailing spaces (e.g., 'Alabama ')\n")
cat("This diagnostic accounts for that when matching.\n\n")
cat("To check a specific state in detail, use the EXACT name from your data:\n")
cat('  check_state("Alabama ", verbose = TRUE)\n\n')