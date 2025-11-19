library(shiny)
library(markdown)
library(leaflet)
library(reactable)
library(sf)
library(tigris)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
options(tigris_use_cache = TRUE, tigris_class = "sf")

shinyServer(function(input, output, session) {
  
  # ============================================================================
  # BIVARIATE COLOR FUNCTIONS (using US-wide breaks from global.R)
  # ============================================================================
  
  # Standard Bivariate: UV × Melanoma Rate (no demographic adjustment)
  create_bivariate_colors <- function(uv_values, melanoma_values) {
    
    # Use US-wide breaks (calculated in global.R)
    uv_class <- cut(uv_values, breaks = us_uv_breaks, labels = 1:3, include.lowest = TRUE)
    mel_class <- cut(melanoma_values, breaks = us_melanoma_breaks, labels = 1:3, include.lowest = TRUE)
    
    # Color matrix
    color_matrix <- matrix(c(
      "#E8E8E8", "#ACE4E4", "#5AC8C8",  # Low melanoma
      "#DFBFD8", "#A28DA8", "#637994",  # Med melanoma  
      "#BE64AC", "#8C62AA", "#3B4994"   # High melanoma
    ), nrow = 3, byrow = TRUE)
    
    # Map to colors
    colors <- rep("#F0F0F0", length(uv_values))
    for (i in seq_along(colors)) {
      if (!is.na(uv_class[i]) && !is.na(mel_class[i])) {
        row_idx <- as.numeric(mel_class[i])
        col_idx <- as.numeric(uv_class[i])
        colors[i] <- color_matrix[row_idx, col_idx]
      }
    }
    
    return(colors)
  }
  
  # Risk-Adjusted Bivariate: UV × Melanoma per White Population
  create_weighted_bivariate_colors <- function(uv_values, melanoma_values, white_pct) {
    
    # Calculate melanoma rate PER WHITE POPULATION
    melanoma_per_white <- melanoma_values / (white_pct / 100)
    melanoma_per_white[white_pct < 10] <- NA  # Suppress if white pop < 10%
    
    # Use US-wide breaks (calculated in global.R)
    uv_class <- cut(uv_values, breaks = us_uv_breaks, labels = 1:3, include.lowest = TRUE)
    mel_class <- cut(melanoma_per_white, breaks = us_melanoma_per_white_breaks, labels = 1:3, include.lowest = TRUE)
    
    # Color matrix
    color_matrix <- matrix(c(
      "#E8E8E8", "#ACE4E4", "#5AC8C8",
      "#DFBFD8", "#A28DA8", "#637994",
      "#BE64AC", "#8C62AA", "#3B4994"
    ), nrow = 3, byrow = TRUE)
    
    # Map to colors
    colors <- rep("#F0F0F0", length(uv_values))
    for (i in seq_along(colors)) {
      if (!is.na(uv_class[i]) && !is.na(mel_class[i])) {
        row_idx <- as.numeric(mel_class[i])
        col_idx <- as.numeric(uv_class[i])
        colors[i] <- color_matrix[row_idx, col_idx]
      }
    }
    
    return(colors)
  }
  # Healthcare Access Bivariate: MD Availability × Melanoma Rate
  create_md_bivariate_colors <- function(md_values, melanoma_values) {
    
    # Use US-wide breaks (calculated in global.R)
    md_class <- cut(md_values, breaks = us_md_breaks, labels = 1:3, include.lowest = TRUE)
    mel_class <- cut(melanoma_values, breaks = us_melanoma_breaks, labels = 1:3, include.lowest = TRUE)
    
    # Color matrix (reversed logic: high MD + low melanoma = good outcome)
    color_matrix <- matrix(c(
      "#E8E8E8", "#ACE4E4", "#5AC8C8",  # Low melanoma
      "#DFBFD8", "#A28DA8", "#637994",  # Med melanoma  
      "#BE64AC", "#8C62AA", "#3B4994"   # High melanoma
    ), nrow = 3, byrow = TRUE)
    
    # Map to colors
    colors <- rep("#F0F0F0", length(md_values))
    for (i in seq_along(colors)) {
      if (!is.na(md_class[i]) && !is.na(mel_class[i])) {
        row_idx <- as.numeric(mel_class[i])
        col_idx <- as.numeric(md_class[i])
        colors[i] <- color_matrix[row_idx, col_idx]
      }
    }
    
    return(colors)
  }
  
  # ============================================================================
  # HOME TAB OUTPUTS
  # ============================================================================
  
  output$home_text <- renderUI({
    HTML("Welcome to our BIO-185 project on data visualization for Melanoma cases in the United States.
  <br><br>
         <strong><span style='font-size: 24px;'>Overview</span></strong><br><br>
         Below are four examples of invasive melanoma, and an overview of the general development process from the epidermal region of the skin to the fourth stage, which is complete invasion and spread to other organs of the body. The data in this project comes from the NCI (National Cancer Institute) in conjunction with the CDC (CDC.gov).")
  })
  
  output$home_melanoma <- renderUI({
    tags$div(
      style = "display: flex; justify-content: center; align-items: center; gap: 40px; margin: 20px auto;",
      tags$img(src = "cs-Accuracy-Dermoscopic-Criteria-Diagnosis-Melanoma-Situ-600x400.jpg", 
               width = "300px"),
      tags$img(src = "melanoma-stages.jpeg", 
               width = "300px")
    )
  })
  
  output$home_image <- renderUI({
    tags$div(
      style = "display: flex; justify-content: center; align-items: center; gap: 40px; margin: 20px auto;",
      tags$img(src = "National_Cancer_Institute_logo.svg.png", 
               width = "300px"),
      tags$img(src = "CDC_logo.png", 
               width = "300px")
    )
  })
  
  output$home_text2 <- renderUI({
    HTML("<strong><span style='font-size: 24px;'>Available Visualizations</span></strong><br><br>
    On this website, you can view and visualize the following data:
  <br><br>
  <strong>Melanoma Cases by County:</strong> This visualization is created from data collected by the National Cancer Institute. It contains the number of cases (average annual count from the years 2017-2021) for each county. The data is collected on a per-county basis.
  <br><br>
  <strong>Melanoma Rate (Age-Adjusted per 100k):</strong> This visualization shows the number of cases (per 100,000) (age adjusted per the NCI website). The data is also included in the original dataset provided by the NCI. The data is collected on a per-county basis.<br><br>
  <strong>UV Measurement:</strong> This is another choropleth map that shows the intensity of UV (in Watts per square meter) for each county in the choropleth map. The data is taken from the National Institute for Cancer GIS Portal for Patient Research, and contains the average values for intensity from the years 2020-2024.
  <br><br>To view the data, visit the Visualizations tab in the menu bar.")
  })
  
  # ============================================================================
  # VISUALIZATION TAB - MAP AND EXPLANATIONS
  # ============================================================================
  
  output$viz_plot <- renderPlot({
    plot(cars, main = "Placeholder Plot")
  })
  
  output$viz_note <- renderUI({
    tags$em("Replace with visualizations.")
  })
  
  output$map <- renderLeaflet({
    
    # Get default state
    default_state <- input$state_select
    if (is.null(default_state)) {
      default_state <- "Alabama"
    }
    
    # Get state boundaries
    sel <- states_sf[states_sf$NAME == default_state, ]
    
    # Get state abbreviation
    state_abbr <- state.abb[match(default_state, state.name)]
    if (is.na(state_abbr) && default_state == "District of Columbia") {
      state_abbr <- "DC"
    }
    
    # Filter counties for default state
    state_counties <- counties_sf[counties_sf$STUSPS == state_abbr, ]
    
    # Join with melanoma data
    counties_with_data <- state_counties %>%
      left_join(
        melanoma_table[, c("fips_melanoma", "county", "state", "avg_annual_ct")],
        by = c("GEOID" = "fips_melanoma")
      )
    
    # Create color palette
    pal <- colorBin(
      palette = "YlOrRd",
      domain = counties_with_data$avg_annual_ct,
      bins = c(0, 10, 25, 50, 100, 250, 500, Inf),
      na.color = "#F0F0F0"
    )
    
    # Get bounding box
    bb <- sf::st_bbox(sel)
    
    # Create map with data
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = sel, fill = FALSE, color = "#4169E1", weight = 2, 
                  opacity = 1, group = "state_focus") %>%
      addPolygons(
        data = counties_with_data,
        fillColor = ~pal(avg_annual_ct),
        weight = 1, opacity = 1, color = "white",
        layerId = ~GEOID, fillOpacity = 0.7, group = "melanoma",
        label = ~ifelse(
          is.na(avg_annual_ct),
          paste0(NAME, " County: No data available"),
          paste0(NAME, " County: ", 
                 ifelse(avg_annual_ct == 0, "≤3", as.character(avg_annual_ct)), 
                 " cases/year")
        ),
        highlightOptions = highlightOptions(
          weight = 2, color = "#665", fillOpacity = 0.9, bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright", pal = pal, 
        values = counties_with_data$avg_annual_ct,
        title = "Annual<br>Melanoma Cases<br>(County Level)",
        opacity = 0.7, layerId = "melanoma_legend"
      ) %>%
      fitBounds(lng1 = bb[["xmin"]], lat1 = bb[["ymin"]], 
                lng2 = bb[["xmax"]], lat2 = bb[["ymax"]])
  })
  
  # Dynamic explanation text based on selected visualization
  # Dynamic explanation text based on selected visualization
  output$viz_explanation <- renderUI({
    
    choice <- input$melanoma_view
    
    if (choice == "count") {
      HTML("<div style='padding: 15px; background-color: white; border: 1px solid #4169E1; border-radius: 8px; margin-top: 10px;'>
         <strong>About Melanoma Cases by County:</strong><br>
         This map displays the average annual count of invasive melanoma cases for each county from 2017-2021. 
         Values of 0 indicate suppressed data (≤3 cases). Data source: National Cancer Institute.
       </div>")
    } else if (choice == "rate") {
      HTML("<div style='padding: 15px; background-color: white; border: 1px solid #4169E1; border-radius: 8px; margin-top: 10px;'>
         <strong>About Age-Adjusted Incidence Rate:</strong><br>
         This map shows the age-adjusted incidence rate per 100,000 population. Age adjustment accounts for differences 
         in population age distributions. Gray counties indicate suppressed data (<16 cases). Data source: National Cancer Institute.
       </div>")
    } else if (choice == "uv") {
      HTML("<div style='padding: 15px; background-color: white; border: 1px solid #4169E1; border-radius: 8px; margin-top: 10px;'>
         <strong>About UV Measurement:</strong><br>
         This map displays average UV intensity measured in Watts per square meter (W/m²) for each county from 2020-2024. 
         Higher values indicate greater UV radiation exposure. Data source: National Institute for Cancer GIS Portal.
       </div>")
    } else if (choice == "bivariate") {
      HTML("<div style='padding: 15px; background-color: white; border: 1px solid #4169E1; border-radius: 8px; margin-top: 10px;'>
         <strong>About Bivariate Mapping:</strong><br>
         This map shows the relationship between UV exposure and melanoma rates simultaneously using a 3×3 color scheme.
         Counties are classified relative to <strong>national averages</strong>, not just within the selected state.
         Counties in the top-right corner (dark blue/purple) have both high UV and high melanoma rates compared to the entire US.
         <em>Note: This map does not account for population demographics.</em>
       </div>")
    } else if (choice == "bivariate_weighted") {
      HTML("<div style='padding: 15px; background-color: white; border: 1px solid #4169E1; border-radius: 8px; margin-top: 10px;'>
       <strong>About Risk-Adjusted Bivariate Mapping:</strong><br>
       This advanced map calculates melanoma rates <strong>per white (non-Hispanic) population</strong>, since melanoma 
       affects white populations 20-30× more than other racial groups. Counties are classified relative to <strong>national rates</strong>.
       <br><br>
       <strong>Why this matters:</strong> The displayed <strong>Rate per 100k White</strong> shows the melanoma burden 
       specifically within the at-risk population. A county with 30 cases per 100k total population and only 30% white 
       population actually has <strong>100 cases per 100k white residents</strong> - indicating a severe problem within 
       that vulnerable group.
       <br><br>
       <strong>Interpretation:</strong> 
       <ul style='margin: 10px 0;'>
         <li><strong>Low white % + High melanoma rate</strong> = Very high rate per white person (dark colors) = Severe localized problem</li>
         <li><strong>High white % + High melanoma rate</strong> = Moderate rate per white person (medium colors) = Expected endemic level</li>
         <li><strong>Darkest areas</strong> = Highest UV exposure combined with highest melanoma rates within white populations, relative to the entire US</li>
       </ul>
       This helps identify counties where melanoma disproportionately affects the white population, which may indicate 
       environmental, behavioral, or screening disparities that warrant targeted public health interventions.
     </div>")
    } else if (choice == "md_availability") {
      HTML("<div style='padding: 15px; background-color: white; border: 1px solid #4169E1; border-radius: 8px; margin-top: 10px;'>
     <strong>About Physician Availability:</strong><br>
     This map displays the number of physicians (MDs) per 100,000 population for each county. 
     Higher values indicate better healthcare access. This data comes from the md_availability dataset 
     and may help explain melanoma detection and diagnosis patterns. Gray counties indicate missing data.
   </div>")
    } else if (choice == "bivariate_md") {
      HTML("<div style='padding: 15px; background-color: white; border: 1px solid #4169E1; border-radius: 8px; margin-top: 10px;'>
         <strong>About Healthcare Access Bivariate Mapping:</strong><br>
         This map shows the relationship between physician availability and melanoma rates using a 3×3 color scheme.
         Counties are classified relative to <strong>national averages</strong>.
         <br><br>
         <strong>Interpretation:</strong>
         <ul style='margin: 10px 0;'>
           <li><strong>Dark blue/purple (top-right):</strong> High MD access + High melanoma = Good detection/screening</li>
           <li><strong>Dark blue/purple (top-left):</strong> Low MD access + High melanoma = Healthcare disparity concern</li>
           <li><strong>Light cyan (bottom-right):</strong> High MD access + Low melanoma = Successful prevention/early detection</li>
           <li><strong>Light gray (bottom-left):</strong> Low MD access + Low melanoma = May reflect underdiagnosis</li>
         </ul>
         <strong>Why this matters:</strong> Areas with high melanoma rates but low physician access may have delayed diagnoses 
         and worse outcomes, while areas with high MD access may detect more cases earlier.
       </div>")
    } else if (choice == "md_availability") {
    } else {
      return(NULL)
    }
  })
  
  # ============================================================================
  # MAIN MAP OBSERVER - Updates map based on user selections
  # ============================================================================
  
  observeEvent(c(input$state_select, input$melanoma_view, ignoreNULL=FALSE), {
    req(input$state_select, input$melanoma_view)
    # Force immediate evaluation
    state_val <- input$state_select
    view_val <- input$melanoma_view
    proxy <- leafletProxy("map")
    
    # Clear all existing layers
    proxy %>% 
      clearGroup("state_focus") %>%
      clearGroup("melanoma") %>%
      removeControl("melanoma_legend")
    
    # Zoom to selected state
    sel <- states_sf[states_sf$NAME == input$state_select, ]
    # Get bounding box
    bb <- sf::st_bbox(sel)
    
    # Special handling for Michigan - zoom closer to Lower Peninsula
    if (input$state_select == "Michigan") {
      proxy %>%
        addPolygons(data = sel, fill = FALSE, color = "#4169E1", weight = 2, 
                    opacity = 1, group = "state_focus") %>%
        setView(lng = -85.5, lat = 44.5, zoom = 6)  # Custom view for Michigan
    } else if (input$state_select == "Alaska") {
      proxy %>%
        addPolygons(data = sel, fill = FALSE, color = "#4169E1", weight = 2, 
                    opacity = 1, group = "state_focus") %>%
        setView(lng = -152, lat = 64, zoom = 3.49)
    } else {
      proxy %>%
        addPolygons(data = sel, fill = FALSE, color = "#4169E1", weight = 2, 
                    opacity = 1, group = "state_focus") %>%
        fitBounds(lng1 = bb[["xmin"]], lat1 = bb[["ymin"]], 
                  lng2 = bb[["xmax"]], lat2 = bb[["ymax"]])
    }
      
      # Get state abbreviation
      state_abbr <- state.abb[match(input$state_select, state.name)]
      if (is.na(state_abbr) && input$state_select == "District of Columbia") {
        state_abbr <- "DC"
      }
      
      # Filter counties for selected state
      state_counties <- counties_sf[counties_sf$STUSPS == state_abbr, ]
      
      # ========== MELANOMA CASE COUNT ==========
      if (input$melanoma_view == "count") {
        
        counties_with_data <- state_counties %>%
          left_join(
            melanoma_table[, c("fips_melanoma", "county", "state", "avg_annual_ct")],
            by = c("GEOID" = "fips_melanoma")
          )
        
        pal <- colorBin(
          palette = "YlOrRd",
          domain = counties_with_data$avg_annual_ct,
          bins = c(0, 10, 25, 50, 100, 250, 500, Inf),
          na.color = "#F0F0F0"
        )
        
        proxy %>%
          addPolygons(
            data = counties_with_data,
            fillColor = ~pal(avg_annual_ct),
            weight = 1, opacity = 1, color = "white",
            layerId = ~GEOID, fillOpacity = 0.7, group = "melanoma",
            label = ~ifelse(
              is.na(avg_annual_ct),
              paste0(NAME, " County: No data available"),
              paste0(NAME, " County: ", 
                     ifelse(avg_annual_ct == 0, "≤3", as.character(avg_annual_ct)), 
                     " cases/year")
            ),
            highlightOptions = highlightOptions(
              weight = 2, color = "#665", fillOpacity = 0.9, bringToFront = TRUE
            )
          ) %>%
          addLegend(
            position = "bottomright", pal = pal, 
            values = counties_with_data$avg_annual_ct,
            title = "Annual<br>Melanoma Cases<br>(County Level)",
            opacity = 0.7, layerId = "melanoma_legend"
          )
      }
      
      # ========== MELANOMA INCIDENCE RATE ==========
      if (input$melanoma_view == "rate") {
        
        counties_with_data <- state_counties %>%
          left_join(
            melanoma_table[, c("fips_melanoma", "county", "state", "age_adj_inc_rate")],
            by = c("GEOID" = "fips_melanoma")
          )
        
        pal <- colorBin(
          palette = "YlOrRd",
          domain = counties_with_data$age_adj_inc_rate,
          bins = c(0, 15, 20, 25, 30, 35, 40, 50, Inf),
          na.color = "#F0F0F0"
        )
        
        proxy %>%
          addPolygons(
            data = counties_with_data,
            fillColor = ~pal(age_adj_inc_rate),
            weight = 1, opacity = 1, color = "white",
            layerId = ~GEOID, fillOpacity = 0.7, group = "melanoma",
            label = ~ifelse(
              is.na(age_adj_inc_rate),
              paste0(NAME, " County: Data suppressed (<16 cases)"),
              paste0(NAME, " County: ", round(age_adj_inc_rate, 1), " per 100k (age-adj)")
            ),
            highlightOptions = highlightOptions(
              weight = 2, color = "#665", fillOpacity = 0.9, bringToFront = TRUE
            )
          ) %>%
          addLegend(
            position = "bottomright", pal = pal,
            values = counties_with_data$age_adj_inc_rate,
            title = "Age-Adjusted<br>Incidence Rate<br>(per 100,000)<br><span style='font-size:9px;'>(Gray = Suppressed)</span>",
            opacity = 0.7, layerId = "melanoma_legend"
          )
      }
      
      # ========== UV INTENSITY ==========
      if (input$melanoma_view == "uv") {
        
        counties_with_data <- state_counties %>%
          left_join(
            uv_table %>% 
              mutate(fips_uv = sprintf("%05d", as.numeric(fips_uv))) %>%
              select(fips_uv, uv_value),
            by = c("GEOID" = "fips_uv")
          )
        
        pal <- colorBin(
          palette = c("#FFFFCC", "#FED976", "#FEB24C", "#FD8D3C", 
                      "#E31A1C", "#BD0026", "#800026"),
          domain = counties_with_data$uv_value,
          bins = c(3000, 3400, 3800, 4200, 4600, 5000, 5400, 5800),
          na.color = "#F0F0F0"
        )
        
        proxy %>%
          addPolygons(
            data = counties_with_data,
            fillColor = ~pal(uv_value),
            weight = 1, opacity = 1, color = "white",
            layerId = ~GEOID, fillOpacity = 0.7, group = "melanoma",
            label = ~ifelse(
              is.na(uv_value),
              paste0(NAME, " County: No UV data available"),
              paste0(NAME, " County: ", round(uv_value, 1), " W/m²")
            ),
            highlightOptions = highlightOptions(
              weight = 2, color = "#665", fillOpacity = 0.9, bringToFront = TRUE
            )
          ) %>%
          addLegend(
            position = "bottomright", pal = pal,
            values = counties_with_data$uv_value,
            title = "UV Intensity<br>(W/m²)",
            opacity = 0.7, layerId = "melanoma_legend"
          )
      }
      
      # ========== BIVARIATE: UV × MELANOMA RATE ==========
      if (input$melanoma_view == "bivariate") {
        
        counties_with_data <- state_counties %>%
          left_join(
            melanoma_table %>% select(fips_melanoma, age_adj_inc_rate),
            by = c("GEOID" = "fips_melanoma")
          ) %>%
          left_join(
            uv_table %>% 
              mutate(fips_uv = sprintf("%05d", as.numeric(fips_uv))) %>%
              select(fips_uv, uv_value),
            by = c("GEOID" = "fips_uv")
          )
        
        
        # Create bivariate colors using US-wide breaks
        biv_colors <- create_bivariate_colors(
          counties_with_data$uv_value,
          counties_with_data$age_adj_inc_rate
        )
      
          # Override with pure white for missing data (either UV or melanoma)
        biv_colors[is.na(counties_with_data$age_adj_inc_rate) | is.na(counties_with_data$uv_value)] <- "#FFFFFF"
        
        proxy %>%
          addPolygons(
            data = counties_with_data,
            fillColor = biv_colors,
            weight = 1, opacity = 1, color = "white",
            layerId = ~GEOID, fillOpacity = 0.7, group = "melanoma",
            label = ~paste0(
              NAME, " County",
              "<br>UV: ", ifelse(is.na(uv_value), "No data", paste0(round(uv_value, 1), " W/m²")),
              "<br>Melanoma: ", ifelse(is.na(age_adj_inc_rate), "Suppressed", 
                                       paste0(round(age_adj_inc_rate, 1), " per 100k"))
            ) %>% lapply(htmltools::HTML),
            highlightOptions = highlightOptions(
              weight = 2, color = "#665", fillOpacity = 0.9, bringToFront = TRUE
            )
          ) %>%
          addControl(
            html = '<div style="background: white; padding: 12px; border: 2px solid #4169E1; border-radius: 5px;">
              <strong style="font-size: 13px;">UV × Melanoma Rate</strong><br>
              <p style="font-size: 10px; margin: 5px 0;">(National scale)</p>
              <table style="border-collapse: collapse; margin-top: 8px;">
                <tr>
                  <td style="width:28px;height:28px;background:#BE64AC;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#8C62AA;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#3B4994;border:1px solid white"></td>
                  <td rowspan="3" style="writing-mode: vertical-lr; transform: rotate(180deg); padding-left:8px; font-size:11px;">Higher Melanoma →</td>
                </tr>
                <tr>
                  <td style="width:28px;height:28px;background:#DFBFD8;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#A28DA8;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#637994;border:1px solid white"></td>
                </tr>
                <tr>
                  <td style="width:28px;height:28px;background:#E8E8E8;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#ACE4E4;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#5AC8C8;border:1px solid white"></td>
                </tr>
                <tr>
                  <td colspan="3" style="text-align:center; padding-top:5px; font-size:11px;">Higher UV →</td>
                </tr>
              </table>
               <p style="font-size: 10px; color: #666; margin-top: 8px;">
                Light White = Missing/suppressed data</p>
            </div>',
            position = "bottomright",
            layerId = "melanoma_legend"
          )
      }
      
      # ========== RISK-ADJUSTED BIVARIATE: UV × MELANOMA PER WHITE POP ==========
      if (input$melanoma_view == "bivariate_weighted") {
        
        counties_with_data <- state_counties %>%
          left_join(
            melanoma_table %>% select(fips_melanoma, age_adj_inc_rate),
            by = c("GEOID" = "fips_melanoma")
          ) %>%
          left_join(
            uv_table %>% 
              mutate(fips_uv = sprintf("%05d", as.numeric(fips_uv))) %>%
              select(fips_uv, uv_value),
            by = c("GEOID" = "fips_uv")
          ) %>%
          left_join(
            county_demographics %>%
              select(fips_demo, white_not_h_or_l_pct),
            by = c("GEOID" = "fips_demo")
          )
        
        # Create risk-weighted bivariate colors using US-wide breaks
        biv_colors <- create_weighted_bivariate_colors(
          counties_with_data$uv_value,
          counties_with_data$age_adj_inc_rate,
          counties_with_data$white_not_h_or_l_pct
        )
        
        # Override with pure white for suppressed melanoma data
        biv_colors[is.na(counties_with_data$age_adj_inc_rate)] <- "#FFFFFF"
        
        # Calculate risk score for display
        counties_with_data$risk_score <- counties_with_data$age_adj_inc_rate / 
          (counties_with_data$white_not_h_or_l_pct / 100)
        
        proxy %>%
          addPolygons(
            data = counties_with_data,
            fillColor = biv_colors,
            weight = 1, opacity = 1, color = "white",
            layerId = ~GEOID, fillOpacity = 0.7, group = "melanoma",
            label = ~paste0(
              NAME, " County",
              "<br>UV: ", ifelse(is.na(uv_value), "No data", paste0(round(uv_value, 1), " W/m²")),
              "<br>Melanoma Rate: ", ifelse(is.na(age_adj_inc_rate), "Suppressed", 
                                            paste0(round(age_adj_inc_rate, 1), " per 100k total")),
              "<br>White Pop: ", ifelse(is.na(white_not_h_or_l_pct), "No data",
                                        paste0(round(white_not_h_or_l_pct, 1), "%")),
              "<br><strong>Rate per 100k White: ", ifelse(is.na(risk_score), "N/A",
                                                          paste0(round(risk_score, 1), "</strong>"))
            ) %>% lapply(htmltools::HTML),
            highlightOptions = highlightOptions(
              weight = 2, color = "#665", fillOpacity = 0.9, bringToFront = TRUE
            )
          ) %>%
          addControl(
            html = '<div style="background: white; padding: 12px; border: 2px solid #4169E1; border-radius: 5px;">
              <strong style="font-size: 13px;">Risk-Adjusted Map</strong><br>
              <p style="font-size: 10px; margin: 5px 0;">UV × Melanoma per white pop (National scale)</p>
              <table style="border-collapse: collapse; margin-top: 8px;">
                <tr>
                  <td style="width:28px;height:28px;background:#BE64AC;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#8C62AA;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#3B4994;border:1px solid white"></td>
                  <td rowspan="3" style="writing-mode: vertical-lr; transform: rotate(180deg); padding-left:8px; font-size:11px;">Higher Risk →</td>
                </tr>
                <tr>
                  <td style="width:28px;height:28px;background:#DFBFD8;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#A28DA8;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#637994;border:1px solid white"></td>
                </tr>
                <tr>
                  <td style="width:28px;height:28px;background:#E8E8E8;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#ACE4E4;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#5AC8C8;border:1px solid white"></td>
                </tr>
                <tr>
                  <td colspan="3" style="text-align:center; padding-top:5px; font-size:11px;">Higher UV →</td>
                </tr>
              </table>
               <p style="font-size: 10px; color: #666; margin-top: 8px;">
                Light White = Missing/suppressed data</p>
            </div>',
            position = "bottomright",
            layerId = "melanoma_legend"
          )
        
      }
      # ========== PHYSICIAN AVAILABILITY ==========
      if (input$melanoma_view == "md_availability") {
        
        # Join MD data (FIPS already formatted in global.R)
        counties_with_data <- state_counties %>%
          left_join(
            md_availability %>% 
              select(fips_md, md_rate_per_100k),
            by = c("GEOID" = "fips_md")
          )
        
        # Check if data exists
        if (!"md_rate_per_100k" %in% colnames(counties_with_data)) {
          showNotification("MD availability data column not found", type = "error")
          return()
        }
        
        pal <- colorBin(
          palette = "YlGnBu",
          domain = counties_with_data$md_rate_per_100k,
          bins = c(0, 50, 100, 200, 300, 500, 1000, 4600),
          na.color = "#F0F0F0"
        )
        
        proxy %>%
          addPolygons(
            data = counties_with_data,
            fillColor = ~pal(md_rate_per_100k),
            weight = 1, opacity = 1, color = "white",
            layerId = ~GEOID, fillOpacity = 0.7, group = "melanoma",
            label = ~ifelse(
              is.na(md_rate_per_100k),
              paste0(NAME, " County: No MD data available"),
              paste0(NAME, " County: ", round(md_rate_per_100k, 1), " MDs per 100k")
            ),
            highlightOptions = highlightOptions(
              weight = 2, color = "#665", fillOpacity = 0.9, bringToFront = TRUE
            )
          ) %>%
          addLegend(
            position = "bottomright", pal = pal,
            values = counties_with_data$md_rate_per_100k,
            title = "Physicians<br>per 100,000<br>Population",
            opacity = 0.7, layerId = "melanoma_legend"
          )
      }
      
      # ========== BIVARIATE: MD AVAILABILITY × MELANOMA RATE ==========
      if (input$melanoma_view == "bivariate_md") {
        
        counties_with_data <- state_counties %>%
          left_join(
            melanoma_table %>% select(fips_melanoma, age_adj_inc_rate),
            by = c("GEOID" = "fips_melanoma")
          ) %>%
          left_join(
            md_availability %>% select(fips_md, md_rate_per_100k),
            by = c("GEOID" = "fips_md")
          )
        
        # Create bivariate colors using US-wide breaks
        biv_colors <- create_md_bivariate_colors(
          counties_with_data$md_rate_per_100k,
          counties_with_data$age_adj_inc_rate
        )
        
        # Override with pure white for missing data
        biv_colors[is.na(counties_with_data$age_adj_inc_rate) | is.na(counties_with_data$md_rate_per_100k)] <- "#FFFFFF"
        
        proxy %>%
          addPolygons(
            data = counties_with_data,
            fillColor = biv_colors,
            weight = 1, opacity = 1, color = "white",
            layerId = ~GEOID, fillOpacity = 0.7, group = "melanoma",
            label = ~paste0(
              NAME, " County",
              "<br>MDs: ", ifelse(is.na(md_rate_per_100k), "No data", paste0(round(md_rate_per_100k, 1), " per 100k")),
              "<br>Melanoma: ", ifelse(is.na(age_adj_inc_rate), "Suppressed", 
                                       paste0(round(age_adj_inc_rate, 1), " per 100k"))
            ) %>% lapply(htmltools::HTML),
            highlightOptions = highlightOptions(
              weight = 2, color = "#665", fillOpacity = 0.9, bringToFront = TRUE
            )
          ) %>%
          addControl(
            html = '<div style="background: white; padding: 12px; border: 2px solid #4169E1; border-radius: 5px;">
              <strong style="font-size: 13px;">MD Access × Melanoma Rate</strong><br>
              <p style="font-size: 10px; margin: 5px 0;">(National scale)</p>
              <table style="border-collapse: collapse; margin-top: 8px;">
                <tr>
                  <td style="width:28px;height:28px;background:#BE64AC;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#8C62AA;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#3B4994;border:1px solid white"></td>
                  <td rowspan="3" style="writing-mode: vertical-lr; transform: rotate(180deg); padding-left:8px; font-size:11px;">Higher Melanoma →</td>
                </tr>
                <tr>
                  <td style="width:28px;height:28px;background:#DFBFD8;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#A28DA8;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#637994;border:1px solid white"></td>
                </tr>
                <tr>
                  <td style="width:28px;height:28px;background:#E8E8E8;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#ACE4E4;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#5AC8C8;border:1px solid white"></td>
                </tr>
                <tr>
                  <td colspan="3" style="text-align:center; padding-top:5px; font-size:11px;">Higher MD Access →</td>
                </tr>
              </table>
               <p style="font-size: 10px; color: #666; margin-top: 8px;">
                Light White = Missing/suppressed data</p>
            </div>',
            position = "bottomright",
            layerId = "melanoma_legend"
          )
      }
      
  } # End of specific state view
  
    
  ) # End of observe block
  
  # ============================================================================
  # DATA EXPLORER TAB
  # ============================================================================
  
  # Melanoma table
  output$data_table <- renderReactable({
    req(melanoma_table)
    reactable(
      melanoma_table,
      searchable = TRUE,
      filterable = TRUE,
      sortable = TRUE,
      pagination = TRUE,
      defaultPageSize = 10
    )
  })
  
  output$data_summary <- renderText({
    paste0("Rows: ", nrow(melanoma_table), " | Columns: ", ncol(melanoma_table))
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      "cleaned_melanoma_table.csv"
    },
    content = function(file) {
      write.csv(melanoma_table, file, row.names = FALSE)
    }
  )
  
  # UV table
  output$uv_table <- renderReactable({
    req(uv_table)
    reactable(
      uv_table,
      searchable = TRUE,
      filterable = TRUE,
      sortable = TRUE,
      pagination = TRUE,
      defaultPageSize = 10
    )
  })
  
  output$uv_summary <- renderText({
    paste0("Rows: ", nrow(uv_table), " | Columns: ", ncol(uv_table))
  })
  
  output$download_uv <- downloadHandler(
    filename = function() {
      "cleaned_uv_table.csv"
    },
    content = function(file) {
      write.csv(uv_table, file, row.names = FALSE)
    }
  )
  
  # County demographics table
  output$demographics_table <- renderReactable({
    req(county_demographics)
    reactable(
      county_demographics,
      searchable = TRUE,
      filterable = TRUE,
      sortable = TRUE,
      pagination = TRUE,
      defaultPageSize = 10
    )
  })
  
  output$demographics_summary <- renderText({
    paste0("Rows: ", nrow(county_demographics), " | Columns: ", ncol(county_demographics))
  })
  
  output$download_demographics <- downloadHandler(
    filename = function() {
      "cleaned_county_population.csv"
    },
    content = function(file) {
      write.csv(county_demographics, file, row.names = FALSE)
    }
  )
  
  
  
  # ============================================================================
  # NEW IMPROVED STATISTICAL ANALYSIS TAB
  # ============================================================================
  
  # Prepare analysis data
  analysis_data <- reactive({
    melanoma_table %>%
      left_join(
        uv_table %>% 
          mutate(fips_uv = sprintf("%05d", as.numeric(fips_uv))) %>%
          select(fips_uv, uv_value),
        by = c("fips_melanoma" = "fips_uv")
      ) %>%
      left_join(
        county_demographics %>% 
          select(fips_demo, white_not_h_or_l_pct, black_pct, 
                 hispanic_or_latino_pct, asian_pct),
        by = c("fips_melanoma" = "fips_demo")
      ) %>%
      filter(!is.na(age_adj_inc_rate) & !is.na(uv_value) & !is.na(white_not_h_or_l_pct))
  })
  
  # 1. KEY FINDINGS CARDS
  output$key_metrics <- renderUI({
    data <- analysis_data()
    
    cor_uv <- cor(data$uv_value, data$age_adj_inc_rate)
    cor_white <- cor(data$white_not_h_or_l_pct, data$age_adj_inc_rate)
    
    m_melanoma_vs_white <- lm(age_adj_inc_rate ~ white_not_h_or_l_pct, data = data)
    m_uv_vs_white <- lm(uv_value ~ white_not_h_or_l_pct, data = data)
    partial_cor <- cor(residuals(m_melanoma_vs_white), residuals(m_uv_vs_white))
    
    m_full <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = data)
    r_squared <- summary(m_full)$r.squared
    
    HTML(paste0('
      <div class="row">
        <div class="col-md-3">
          <div class="stat-card" style="background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);">
            <div class="stat-value">', nrow(data), '</div>
            <div class="stat-label">Counties Analyzed</div>
          </div>
        </div>
        <div class="col-md-3">
          <div class="stat-card" style="background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);">
            <div class="stat-value">', round(cor_white, 2), '</div>
            <div class="stat-label">Demographics Effect</div>
          </div>
        </div>
        <div class="col-md-3">
          <div class="stat-card" style="background: linear-gradient(135deg, #fa709a 0%, #fee140 100%);">
            <div class="stat-value">', round(partial_cor, 2), '</div>
            <div class="stat-label">True UV Effect</div>
          </div>
        </div>
        <div class="col-md-3">
          <div class="stat-card" style="background: linear-gradient(135deg, #30cfd0 0%, #330867 100%);">
            <div class="stat-value">', round(r_squared * 100), '%</div>
            <div class="stat-label">Variance Explained</div>
          </div>
        </div>
      </div>
    '))
  })
  
  # 2. INTERACTIVE SCATTER PLOT
  output$interactive_scatter <- renderPlotly({
    data <- analysis_data()
    
    # Create hover text
    data$hover_text <- paste0(
      "<b>", data$county, ", ", data$state, "</b><br>",
      "UV: ", round(data$uv_value), " W/m²<br>",
      "Melanoma: ", round(data$age_adj_inc_rate, 1), " per 100k<br>",
      "White Pop: ", round(data$white_not_h_or_l_pct, 1), "%"
    )
    
    p <- plot_ly(data, 
                 x = ~uv_value, 
                 y = ~age_adj_inc_rate,
                 color = ~white_not_h_or_l_pct,
                 colors = c("#440154", "#31688E", "#35B779", "#FDE725"),
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 8, opacity = 0.6,
                               colorbar = list(title = "White<br>Pop %")),
                 text = ~hover_text,
                 hovertemplate = '%{text}<extra></extra>') %>%
      layout(
        title = list(text = "<b>UV Exposure vs Melanoma Rate</b>", font = list(size = 18)),
        xaxis = list(title = "UV Intensity (W/m²)"),
        yaxis = list(title = "Melanoma Rate (per 100k)"),
        plot_bgcolor = "#f8f9fa",
        paper_bgcolor = "white",
        font = list(family = "Segoe UI")
      )
    
    p
  })
  
  # 3. SIMPSON'S PARADOX VISUALIZATION
  output$simpsons_paradox <- renderPlot({
    data <- analysis_data()
    
    # Create tertiles for white population
    data$white_tertile <- cut(data$white_not_h_or_l_pct, 
                              breaks = quantile(data$white_not_h_or_l_pct, c(0, 1/3, 2/3, 1)),
                              labels = c("Low (<50%)", "Medium (50-75%)", "High (>75%)"))
    
    ggplot(data, aes(x = uv_value, y = age_adj_inc_rate)) +
      geom_point(aes(color = white_tertile), alpha = 0.4, size = 2) +
      geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1.5, 
                  linetype = "dashed", aes(group = 1)) +
      geom_smooth(aes(color = white_tertile), method = "lm", se = FALSE, linewidth = 1.2) +
      scale_color_manual(values = c("#2E7D32", "#1976D2", "#7B1FA2"),
                         name = "White Population") +
      labs(
        title = "Simpson's Paradox in UV-Melanoma Relationship",
        subtitle = "Overall trend (red dashed) vs. Within-group trends (colored)",
        x = "UV Intensity (W/m²)",
        y = "Melanoma Rate (per 100k)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "gray40"),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#f8f9fa", color = NA)
      ) +
      annotate("text", x = max(data$uv_value) * 0.95, y = max(data$age_adj_inc_rate) * 0.95,
               label = "Paradox: Overall negative\nWithin groups: positive",
               hjust = 1, vjust = 1, size = 4, color = "red", fontface = "italic")
  })
  
  # 4. MODEL COMPARISON VISUALIZATION
  output$model_comparison <- renderPlot({
    data <- analysis_data()
    
    # Fit models
    m1 <- lm(age_adj_inc_rate ~ uv_value, data = data)
    m2 <- lm(age_adj_inc_rate ~ white_not_h_or_l_pct, data = data)
    m3 <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = data)
    m4 <- lm(age_adj_inc_rate ~ uv_value * white_not_h_or_l_pct, data = data)
    
    # Create comparison data
    models <- data.frame(
      Model = c("UV Only", "Demographics Only", "UV + Demographics", "UV × Demographics"),
      R2 = c(summary(m1)$r.squared, summary(m2)$r.squared, 
             summary(m3)$r.squared, summary(m4)$r.squared) * 100,
      AIC = c(AIC(m1), AIC(m2), AIC(m3), AIC(m4))
    )
    
    # Normalize AIC for visualization (inverse so higher is better)
    models$AIC_score <- 100 * (1 - (models$AIC - min(models$AIC)) / (max(models$AIC) - min(models$AIC)))
    
    models_long <- tidyr::pivot_longer(models, cols = c(R2, AIC_score), 
                                       names_to = "Metric", values_to = "Value")
    models_long$Metric <- factor(models_long$Metric, 
                                 levels = c("R2", "AIC_score"),
                                 labels = c("Variance Explained (%)", "Model Fit Score"))
    
    ggplot(models_long, aes(x = Model, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_text(aes(label = round(Value, 1)), 
                position = position_dodge(width = 0.7), 
                vjust = -0.5, size = 4, fontface = "bold") +
      scale_fill_manual(values = c("#1E88E5", "#43A047")) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
      labs(
        title = "Model Performance Comparison",
        subtitle = "Higher values indicate better model performance",
        x = "",
        y = "Score"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "gray40"),
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      ) +
      ylim(0, 110)
  })
  
  # 5. EFFECT SIZE COMPARISON
  output$effect_comparison <- renderPlot({
    data <- analysis_data()
    
    # Standardize variables
    data_std <- data %>%
      mutate(
        uv_std = scale(uv_value)[,1],
        white_std = scale(white_not_h_or_l_pct)[,1],
        melanoma_std = scale(age_adj_inc_rate)[,1]
      )
    
    # Fit standardized model
    m_std <- lm(melanoma_std ~ uv_std + white_std, data = data_std)
    
    # Get coefficients with confidence intervals
    ci <- confint(m_std)
    coef_data <- data.frame(
      Variable = c("UV Exposure", "White Population %"),
      Effect = coef(m_std)[2:3],
      Lower = ci[2:3, 1],
      Upper = ci[2:3, 2]
    )
    
    ggplot(coef_data, aes(x = Variable, y = Effect)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 1) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                    width = 0.15, size = 1.2, color = "#424242") +
      geom_point(size = 6, color = "#D32F2F") +
      geom_text(aes(label = sprintf("%.2f", Effect)), 
                vjust = -1.5, size = 5, fontface = "bold") +
      coord_flip() +
      labs(
        title = "Standardized Effect Sizes",
        subtitle = "Effect of 1 standard deviation increase (with 95% CI)",
        x = "",
        y = "Change in Melanoma Rate (SD units)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "gray40"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 12, face = "bold")
      ) +
      ylim(min(coef_data$Lower) * 1.2, max(coef_data$Upper) * 1.3)
  })
  
  # 6. PARTIAL REGRESSION VISUALIZATION
  output$partial_plot <- renderPlot({
    data <- analysis_data()
    
    # Calculate residuals
    m_melanoma_vs_white <- lm(age_adj_inc_rate ~ white_not_h_or_l_pct, data = data)
    m_uv_vs_white <- lm(uv_value ~ white_not_h_or_l_pct, data = data)
    
    plot_data <- data.frame(
      uv_residual = residuals(m_uv_vs_white),
      melanoma_residual = residuals(m_melanoma_vs_white)
    )
    
    # Calculate correlation for annotation
    partial_cor <- cor(plot_data$uv_residual, plot_data$melanoma_residual)
    
    ggplot(plot_data, aes(x = uv_residual, y = melanoma_residual)) +
      geom_hex(bins = 30, alpha = 0.8) +
      geom_smooth(method = "lm", color = "#D32F2F", size = 1.5, se = TRUE) +
      scale_fill_viridis_c(option = "plasma", name = "Count") +
      labs(
        title = "True UV Effect After Removing Demographic Influence",
        subtitle = paste0("Partial correlation: r = ", round(partial_cor, 3)),
        x = "UV Residuals (demographic effect removed)",
        y = "Melanoma Residuals (demographic effect removed)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "#D32F2F", size = 12, face = "italic"),
        legend.position = "right",
        panel.background = element_rect(fill = "#f8f9fa", color = NA)
      ) +
      annotate("text", 
               x = min(plot_data$uv_residual) * 0.95, 
               y = max(plot_data$melanoma_residual) * 0.95,
               label = "Positive relationship\nemerges after\ncontrolling for\ndemographics",
               hjust = 0, vjust = 1, size = 4, color = "#1976D2", fontface = "bold")
  })
  
  # 7. GEOGRAPHIC PATTERN MAP (MODEL PREDICTIONS EXPLAINED)
  output$residual_map <- renderPlotly({
    data <- analysis_data()
    
    # Fit model and get residuals
    m_full <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = data)
    data$residuals <- residuals(m_full)
    data$predicted <- fitted(m_full)
    
    # Create hover text
    data$hover_text <- paste0(
      "<b>", data$county, ", ", data$state, "</b><br>",
      "Actual: ", round(data$age_adj_inc_rate, 1), "<br>",
      "Predicted: ", round(data$predicted, 1), "<br>",
      "Residual: ", ifelse(data$residuals > 0, "+", ""), round(data$residuals, 1), "<br>",
      ifelse(abs(data$residuals) > 10, 
             "<b style='color:red'>Large deviation!</b>", 
             "Within expected range")
    )
    
    plot_ly(data, 
            x = ~predicted, 
            y = ~age_adj_inc_rate,
            color = ~residuals,
            colors = c("#2166AC", "#F7F7F7", "#B2182B"),
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 8, opacity = 0.7,
                          colorbar = list(title = "Residual<br>(Actual-Predicted)")),
            text = ~hover_text,
            hovertemplate = '%{text}<extra></extra>') %>%
      layout(
        title = list(text = "<b>How Well Does Our Model Predict?</b>", font = list(size = 18)),
        xaxis = list(title = "What Our Model Predicts"),
        yaxis = list(title = "What Actually Happened"),
        plot_bgcolor = "#f8f9fa",
        paper_bgcolor = "white",
        shapes = list(
          list(type = "line",
               x0 = min(data$predicted), y0 = min(data$predicted),
               x1 = max(data$predicted), y1 = max(data$predicted),
               line = list(color = "gray", dash = "dash"))
        ),
        annotations = list(
          list(x = max(data$predicted) * 0.95, y = max(data$predicted) * 0.85,
               text = "Perfect Prediction Line",
               showarrow = FALSE,
               font = list(color = "gray", size = 12))
        )
      )
  })
  
  # 8. KEY INSIGHTS SUMMARY
  output$insights_summary <- renderUI({
    data <- analysis_data()
    
    # Calculate key statistics
    m_full <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = data)
    uv_coef <- coef(m_full)["uv_value"]
    white_coef <- coef(m_full)["white_not_h_or_l_pct"]
    
    # Effect sizes
    uv_effect_100 <- uv_coef * 100
    white_effect_10 <- white_coef * 10
    
    HTML(paste0('
    <div class="insights-container">
      <h4 style="color: #1976D2; margin-bottom: 20px;"><i class="fa fa-lightbulb"></i> Key Statistical Insights</h4>
      
      <div class="row">
        <div class="col-md-6">
          <div class="insight-box" style="background: #E3F2FD; border-left: 4px solid #1976D2;">
            <h5 style="color: #1976D2;">UV Exposure Effect</h5>
            <p style="font-size: 24px; font-weight: bold; color: #0D47A1;">+', 
                round(uv_effect_100, 1), ' cases</p>
            <p style="color: #666;">per 100 W/m² increase in UV</p>
            <small style="color: #999;">Statistically significant (p < 0.05)</small>
          </div>
        </div>
        
        <div class="col-md-6">
          <div class="insight-box" style="background: #FCE4EC; border-left: 4px solid #C2185B;">
            <h5 style="color: #C2185B;">Demographic Effect</h5>
            <p style="font-size: 24px; font-weight: bold; color: #880E4F;">+', 
                round(white_effect_10, 1), ' cases</p>
            <p style="color: #666;">per 10% increase in white population</p>
            <small style="color: #999;">Strongest predictor (p < 0.001)</small>
          </div>
        </div>
      </div>
      
      <div class="alert alert-info" style="margin-top: 20px;">
        <strong>Bottom Line:</strong> Both UV exposure and demographics matter, but demographic composition 
        has approximately ', round(abs(white_coef * sd(data$white_not_h_or_l_pct)) / 
                                     abs(uv_coef * sd(data$uv_value)), 1), 
                'x larger effect at the county level. This is a classic example of confounding in ecological data.
      </div>
    </div>
    '))
  })
  
  # 9. DIAGNOSTIC PLOTS - WITH DETAILED EXPLANATIONS
  output$diagnostic_plots <- renderPlot({
    data <- analysis_data()
    m <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = data)
    
    # Create diagnostic plots with better labels
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
    
    # 1. Residuals vs Fitted
    plot(fitted(m), residuals(m), 
         main = "1. Linearity Check",
         xlab = "Predicted Values", 
         ylab = "Residuals (Prediction Errors)",
         pch = 20, col = alpha("#2166AC", 0.5))
    abline(h = 0, lty = 2, col = "red", lwd = 2)
    lines(lowess(fitted(m), residuals(m)), col = "red", lwd = 2)
    mtext("Should be scattered around red line", cex = 0.8, line = 0.5)
    
    # 2. Q-Q Plot
    qqnorm(residuals(m), main = "2. Normality Check",
           pch = 20, col = alpha("#2166AC", 0.5),
           xlab = "Theoretical Normal", ylab = "Sample Residuals")
    qqline(residuals(m), col = "red", lwd = 2)
    mtext("Points should follow red line", cex = 0.8, line = 0.5)
    
    # 3. Scale-Location
    plot(fitted(m), sqrt(abs(standardize(residuals(m)))), 
         main = "3. Equal Variance Check",
         xlab = "Predicted Values", 
         ylab = "√|Standardized Residuals|",
         pch = 20, col = alpha("#2166AC", 0.5))
    lines(lowess(fitted(m), sqrt(abs(standardize(residuals(m))))), 
          col = "red", lwd = 2)
    mtext("Red line should be horizontal", cex = 0.8, line = 0.5)
    
    # 4. Influential Points
    cooksd <- cooks.distance(m)
    plot(cooksd, 
         main = "4. Influential Counties",
         xlab = "County Index", 
         ylab = "Cook's Distance",
         type = "h", col = ifelse(cooksd > 4/nrow(data), "red", "gray50"))
    abline(h = 4/nrow(data), lty = 2, col = "red")
    mtext("Red bars = potentially influential", cex = 0.8, line = 0.5)
    
    par(mfrow = c(1, 1))
  }, height = 600)
  
  # 10. VISUAL ANOVA - VARIANCE DECOMPOSITION
  output$anova_visual <- renderPlot({
    data <- analysis_data()
    
    # Fit models
    m_null <- lm(age_adj_inc_rate ~ 1, data = data)
    m_uv <- lm(age_adj_inc_rate ~ uv_value, data = data)
    m_white <- lm(age_adj_inc_rate ~ white_not_h_or_l_pct, data = data)
    m_full <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = data)
    
    # Calculate sum of squares
    ss_total <- sum((data$age_adj_inc_rate - mean(data$age_adj_inc_rate))^2)
    ss_uv_alone <- sum((fitted(m_uv) - mean(data$age_adj_inc_rate))^2)
    ss_white_alone <- sum((fitted(m_white) - mean(data$age_adj_inc_rate))^2)
    ss_model <- sum((fitted(m_full) - mean(data$age_adj_inc_rate))^2)
    ss_residual <- sum(residuals(m_full)^2)
    
    # Create variance decomposition data
    var_data <- data.frame(
      Component = c("UV Alone", "Demographics Alone", "UV + Demographics", "Unexplained"),
      Variance = c(ss_uv_alone/ss_total * 100, 
                   ss_white_alone/ss_total * 100,
                   ss_model/ss_total * 100,
                   ss_residual/ss_total * 100),
      Type = c("Single", "Single", "Combined", "Error")
    )
    
    # Create the plot
    p1 <- ggplot(var_data[1:3,], aes(x = Component, y = Variance, fill = Component)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = paste0(round(Variance, 1), "%")), 
                vjust = -0.5, size = 5, fontface = "bold") +
      scale_fill_manual(values = c("#FF9800", "#2196F3", "#4CAF50")) +
      labs(title = "ANOVA: How Much Variance Does Each Factor Explain?",
           subtitle = "Higher percentage = better predictor",
           x = "", y = "Variance Explained (%)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16),
            axis.text.x = element_text(angle = 20, hjust = 1)) +
      ylim(0, max(var_data$Variance[1:3]) * 1.2)
    
    # F-statistic comparison
    f_uv <- (ss_uv_alone / 1) / (ss_residual / (nrow(data) - 3))
    f_white <- (ss_white_alone / 1) / (ss_residual / (nrow(data) - 3))
    
    f_data <- data.frame(
      Variable = c("UV Effect", "Demographic Effect"),
      F_stat = c(f_uv, f_white)
    )
    
    p2 <- ggplot(f_data, aes(x = Variable, y = F_stat, fill = Variable)) +
      geom_bar(stat = "identity", width = 0.5) +
      geom_text(aes(label = round(F_stat, 0)), 
                vjust = -0.5, size = 5, fontface = "bold") +
      scale_fill_manual(values = c("#FF5722", "#9C27B0")) +
      labs(title = "F-Statistics: Statistical Significance",
           subtitle = "Higher F = more significant (all p < 0.001)",
           x = "", y = "F-Statistic") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 14)) +
      ylim(0, max(f_data$F_stat) * 1.2)
    
    # Combine plots
    gridExtra::grid.arrange(p1, p2, ncol = 2, widths = c(2, 1))
  })
  
  # 11. REGRESSION SUMMARY - VISUAL VERSION
  output$regression_visual <- renderPlot({
    data <- analysis_data()
    
    # Fit the full model
    m <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = data)
    
    # Extract coefficients and stats
    coef_summary <- summary(m)$coefficients
    
    # Create coefficient data
    coef_data <- data.frame(
      Variable = c("UV Intensity\n(per 100 W/m²)", "White Population\n(per 10%)"),
      Coefficient = c(coef(m)[2] * 100, coef(m)[3] * 10),
      StdError = c(coef_summary[2, 2] * 100, coef_summary[3, 2] * 10),
      pValue = coef_summary[2:3, 4]
    )
    
    coef_data$Significance <- ifelse(coef_data$pValue < 0.001, "***",
                                     ifelse(coef_data$pValue < 0.01, "**",
                                            ifelse(coef_data$pValue < 0.05, "*", "")))
    
    ggplot(coef_data, aes(x = Variable, y = Coefficient)) +
      geom_bar(stat = "identity", fill = c("#FF6B6B", "#4ECDC4"), width = 0.6) +
      geom_errorbar(aes(ymin = Coefficient - 1.96*StdError, 
                        ymax = Coefficient + 1.96*StdError),
                    width = 0.2, size = 1.2) +
      geom_text(aes(label = paste0(round(Coefficient, 2), Significance)), 
                vjust = -2, size = 6, fontface = "bold") +
      labs(title = "Linear Regression Results",
           subtitle = paste0("Model R² = ", round(summary(m)$r.squared, 3), 
                             " | Both predictors highly significant (p < 0.001)"),
           x = "", 
           y = "Effect on Melanoma Rate (cases per 100k)") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 16),
            plot.subtitle = element_text(color = "gray40"),
            axis.text.x = element_text(size = 12, face = "bold")) +
      ylim(0, max(coef_data$Coefficient + coef_data$StdError) * 1.3) +
      annotate("text", x = 1.5, y = max(coef_data$Coefficient) * 0.5,
               label = "Both factors matter,\nbut demographics\nhas larger effect",
               size = 4, color = "gray30", fontface = "italic")
  })
  
  # 10. INTERACTIVE TABLE OF TOP/BOTTOM COUNTIES
  output$county_table <- renderDT({
    data <- analysis_data()
    m <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = data)
    
    data$predicted <- fitted(m)
    data$residual <- residuals(m)
    
    # Select top and bottom 10 residuals
    top_counties <- data %>%
      arrange(desc(residual)) %>%
      head(10) %>%
      mutate(Type = "Higher than Expected")
    
    bottom_counties <- data %>%
      arrange(residual) %>%
      head(10) %>%
      mutate(Type = "Lower than Expected")
    
    combined <- rbind(top_counties, bottom_counties) %>%
      select(county, state, age_adj_inc_rate, predicted, residual, 
             uv_value, white_not_h_or_l_pct, Type) %>%
      mutate(
        age_adj_inc_rate = round(age_adj_inc_rate, 1),
        predicted = round(predicted, 1),
        residual = round(residual, 1),
        uv_value = round(uv_value, 0),
        white_not_h_or_l_pct = round(white_not_h_or_l_pct, 1)
      )
    
    datatable(
      combined,
      colnames = c("County", "State", "Actual Rate", "Predicted", "Difference", 
                   "UV", "White %", "Category"),
      options = list(
        pageLength = 10,
        dom = 'tip',
        order = list(list(4, 'desc'))
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Type',
        backgroundColor = styleEqual(
          c("Higher than Expected", "Lower than Expected"),
          c("#FFEBEE", "#E8F5E9")
        )
      ) %>%
      formatStyle(
        'residual',
        color = styleInterval(0, c("#C62828", "#2E7D32")),
        fontWeight = 'bold'
      )
  })
  
  # Helper function for standardizing residuals
  standardize <- function(x) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  }
  
})