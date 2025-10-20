# library(shiny)
# library(leaflet)
# 
# function(input, output, session) {
#   output$map <- renderLeaflet({
#     leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
#       addProviderTiles(providers$CartoDB.Positron) %>%
#       setView(lng = -98.5, lat = 39.8, zoom = 4)
#   })
# }

library(shiny)
library(markdown)
library(leaflet)
library(reactable)

shinyServer(function(input, output, session) {
  
  # Home tab placeholder
  output$home_text <- renderText({
    "Welcome to the Home tab! Replace this text or reference a Markdown file for content."
  })
  
  # Visualizations tab placeholder (still base R plot for now—swap with ggplot if you like)
  output$viz_plot <- renderPlot({
    plot(cars, main = "Placeholder Plot: speed vs. dist")
  })
  
  output$viz_note <- renderUI({
    tags$em("Tip: Replace this with your ggplot or plotly visualizations.")
  })
  
  # ---- INSERT LEAFLET MAP (SERVER) HERE ----
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4)
  })
  # ------------------------------------------
  
  # Data Explorer: interactive table for melanoma_data_cleaned (reactable, not base Shiny tables)
  output$data_table <- renderReactable({
    req(exists("melanoma_data_cleaned", where = .GlobalEnv))
    df <- get("melanoma_data_cleaned", envir = .GlobalEnv)
    
    reactable(
      df,
      searchable = TRUE,
      filterable = TRUE,
      sortable = TRUE,
      pagination = TRUE,
      highlight = TRUE,
      striped = TRUE,
      bordered = TRUE,
      resizable = TRUE,
      defaultPageSize = 10,
      theme = reactableTheme(
        borderColor = "#eee",
        highlightColor = "#f5f5f5"
      )
    )
  })
  
  output$data_summary <- renderText({
    if (!exists("melanoma_data_cleaned", where = .GlobalEnv)) {
      return("melanoma_data_cleaned not found in the environment.")
    }
    df <- get("melanoma_data_cleaned", envir = .GlobalEnv)
    paste0("Rows: ", nrow(df), " | Columns: ", ncol(df))
  })
})