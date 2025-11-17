library(shiny)
library(markdown)
library(leaflet)
library(reactable)
library(biscale)
library(ggplot2)
library(cowplot)

# Define custom colors taken from an online template
royal_blue <- "#4169E1" 
light_blue <- "#E8F0FF" 
dark_blue  <- "#1E3A8A"


#state dropdown choices (includes DC)
state_choices <- c(sort(c(state.name, "District of Columbia")))

#create the user interface establishment
shinyUI(
  navbarPage(
    title = "BIOL-185 Project - Melanoma Case Studies",
    id = "main_navbar",
    inverse = TRUE,  # makes navbar dark-themed by default
    
    # Apply a royal blue background color to the navbar
    header = tags$style(HTML(paste0("
      .navbar {
        background-color: ", royal_blue, ";
        border-color: ", royal_blue, ";
      }
      .navbar-brand, .navbar-nav li a {
        color: white !important;
        font-weight: 500;
      }
      .navbar-nav li a:hover {
        background-color: ", dark_blue, " !important;
      }
      body {
        background-color: ", light_blue, ";
        color: #0A0A0A;
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }
      h2, h3 {
        color: ", dark_blue, ";
      }
      .well, .panel {
        background-color: white !important;
        border: 1px solid ", royal_blue, ";
        border-radius: 10px;
        box-shadow: 0 0 8px rgba(0,0,0,0.05);
      }
    "))),
    
    # home tab
    tabPanel(
      "Home",
      fluidPage(
        h2("Home"),
        uiOutput("home_text"),
        uiOutput("home_melanoma"),
        uiOutput("home_image"),
        uiOutput("home_text2")
      )
    ),
    
    # Visualizations tab
    tabPanel(
      "Visualizations",
      fluidPage(
        h2("Visualizations"),
        h3("Interactive Map"),
        
        # State dropdown
        selectInput(
          inputId = "state_select",
          label   = "Focus state:",
          choices = state_choices,
          selected = "All states (USA)",
          width = "300px"
        ),
        
        #check boxes
        radioButtons(
          inputId = "melanoma_view",  # Changed from "viz_options"
          label   = "Show data layers:",
          choices = c(
            "Melanoma by County" = "count",  # Keep your title, add value
            "Melanoma Rate (Age-Adjusted per 100k)" = "rate",  # Keep your title, add value
            "UV Measurement (wmh2)" = "uv",
            "Bivariate: UV × Melanoma Rate" = "bivariate" ,
            "Risk-Adjusted: UV × Melanoma (White Pop Weighted)" = "bivariate_weighted"  # NEW
          ),
          selected = "count"   
        ),
        
        leafletOutput("map", height = "400px"),
        uiOutput("viz_explanation")
      )
    ),
    
    # Statistical Models tab
    tabPanel(
      "Statistical Models",
      fluidPage(
        h2("Statistical Analysis: UV Exposure and Melanoma Risk"),
        p("This analysis examines the relationship between UV exposure and melanoma incidence rates across US counties, 
      accounting for demographic factors."),
        
        fluidRow(
          column(6,
                 wellPanel(
                   h4("Model Summary"),
                   verbatimTextOutput("regression_summary"),
                   style = paste0("background-color: white; border: 2px solid ", royal_blue, ";")
                 )
          ),
          column(6,
                 wellPanel(
                   h4("Key Findings"),
                   uiOutput("regression_interpretation"),
                   style = paste0("background-color: white; border: 2px solid ", royal_blue, ";")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 wellPanel(
                   h4("Visualizations"),
                   plotOutput("regression_plots", height = "500px"),
                   style = paste0("background-color: white; border: 2px solid ", royal_blue, ";")
                 )
          )
        )
      )
    ),
    
    # Data Explorer tab
    tabPanel(
      "Data Explorer",
      fluidPage(
        h2("Data Explorer"),
        
        # Melanoma Table Section
        h3("Melanoma Data"),
        p("Explore the melanoma_table interactively:"),
        reactableOutput("data_table", height = 500),
        br(),
        textOutput("data_summary"),
        downloadButton("download_data", "Download Melanoma Data (CSV)"),
        br(),
        br(),
        
        # UV Table Section
        h3("UV Data"),
        p("Explore the UV data interactively:"),
        reactableOutput("uv_table", height = 500),
        br(),
        textOutput("uv_summary"),
        downloadButton("download_uv", "Download UV Data (CSV)"),
        br(),
        br(),
        
        # County Demographics Table Section
        h3("County Demographics Data"),
        p("Explore the county population data interactively:"),
        reactableOutput("demographics_table", height = 500),
        br(),
        textOutput("demographics_summary"),
        downloadButton("download_demographics", "Download County Population Data (CSV)"),
        br()
      )
    )
  )
)