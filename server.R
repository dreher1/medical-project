library(shiny)
library(markdown)
library(leaflet)
library(reactable)

shinyServer(function(input, output, session) {
  
  # Home
  output$home_text <- renderText({
    "Welcome to the Home tab! Replace this text or reference a Markdown file for content."
  })
  
  # Visualizations
  output$viz_plot <- renderPlot({
    plot(cars, main = "Placeholder Plot: speed vs. dist")
  })
  output$viz_note <- renderUI({
    tags$em("Tip: Replace this with your ggplot or plotly visualizations.")
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  # ---- Data Explorer ----
  output$data_table <- renderReactable({
    # Safely fetch from global.R
    df <- get0("melanoma_table", inherits = TRUE)
    req(!is.null(df))  # shows nothing until available
    
    reactable(
      df,
      searchable = TRUE,
      filterable  = TRUE,
      sortable    = TRUE,
      pagination  = TRUE,
      highlight   = TRUE,
      striped     = TRUE,
      bordered    = TRUE,
      resizable   = TRUE,
      defaultPageSize = 10,
      theme = reactableTheme(
        borderColor    = "#eee",
        highlightColor = "#f5f5f5"
      )
    )
  })
  
  output$data_summary <- renderText({
    df <- get0("melanoma_table", inherits = TRUE)
    if (is.null(df)) "melanoma_table not found (check global.R path)."
    else paste0("Rows: ", nrow(df), " | Columns: ", ncol(df))
  })
})