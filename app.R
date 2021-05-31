#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes) # find themes here: https://rstudio.github.io/shinythemes/
library(tidyverse)
library(readxl)
library(lubridate)
library(fs)
library(R.utils)
library(Rilostat)
library(plotly)

# https://www.ardata.fr/en/post/2019/04/26/share-reactive-among-shiny-modules/
# https://ilostat.github.io/Rilostat/articles/Rilostat.html
# https://www.ilo.org/ilostat-files/Documents/ILOSTAT_BulkDownload_Guidelines.pdf

# runExample("04_mpg")
# Define UI for application ----
# ui <- fluidPage(theme = shinytheme("yeti"),
ui <- navbarPage("ILO - Project",
  theme = shinytheme("yeti"),
  # App title ----
  # titlePanel("ILO - Project"),


  tabPanel(
    "Overview of data",
    # Sidebar layout with input and output definitions ----
    # sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      helpText("Welcome! Use data provided by ILO to perdict the effect"),

      # Input: Selector for choosing dataset ----
      selectInput(
        inputId = "SelectTopic",
        label = "Choose a Topic:",
        choices = c(
            "Safety and Health" = "EIP_2EET_SEX_RT_A",
            "Child Labour" = "INJ_FATL_SEX_MIG_RT_A",
            "..." = "..."
        )
      ),
      # Input: Selector for choosing country to analysis
      selectInput(
        inputId = "filterCountry",
        label = "Choose a Country:",
        choices = c(
          "All" = "ALL",
          "Denmark" = "DNK",
          "Portugal" = "PRT"
        )
      ),
      
      # Make slider for year
      # sliderInput("yearSlider",
      #   label = h3("Year"), min = 1975,
      #   max = 2019, value = 2010, sep = ""
      # )
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Overall",
          # Output: Formatted text for caption ----
          h3(textOutput("caption")),
          verbatimTextOutput("summary"),
          #plotOutput("mapPlot")
        ),
        
        tabPanel("Fatal Injuries", 
                 # Output: Diagram ----
                 plotOutput("distPlot")
                 ),
        
        tabPanel("Non-Fatal Injuries", 
                 tableOutput("table"),
                 tableOutput("Table_data")
                 ),
        
        tabPanel("Days lost", 
                 tableOutput("table"),
                 tableOutput("Table_data")
        )
      )
    )
  ),
  tabPanel(
    "Data comparison",
  ),
  tabPanel("Component 3")
)
# )

# Define server logic ----
server <- function(input, output) {

  datasetChoice <- reactive({
      paste("DataSet ~", input$SelectTopic)
  })

  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
      datasetChoice()
  })
  
  datasetInput <- reactive({
      dataset <- get_ilostat(id = input$SelectTopic, 
                             segment = 'indicator',
                             time_format = "num",
                             filters = list(
                               #classif1 = "ECO_AGGREGATE_TOTAL",
                               #time = input$yearSlider, 
                               #sex = "T"
                             ),
                            cache = FALSE) %>% 
          filter(str_sub(ref_area,1,1) != 'X') %>%
          select(ref_area, obs_value, time, classif1) %>%
          left_join(Rilostat:::ilostat_ref_area_mapping %>%
                        select(ref_area) %>%
                        label_ilostat(code = 'ref_area'),
                    by = "ref_area")
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  # output$mapPlot <- renderPlot({
  #     dataset <- datasetInput()
  #     dataset %>%   plot_geo( 
  #         #width = 900, 
  #         #height = 600
  #         ) %>%
  #         add_trace(
  #             z = ~obs_value, 
  #             color = ~obs_value, 
  #             colors=c("green", "blue"), 
  #             text = ~ref_area.label, 
  #             locations = ~ref_area_plotly, 
  #             marker = list(line = list(color = toRGB("grey"), width = 0.5)),
  #             showscale = TRUE) %>%
  #         colorbar(title = '(%)', len = 0.5, ticksuffix="%") %>%
  #         layout(
  #             title = list(   text = "Share of youth not in employment, education or training (NEET) in 2018",
  #                             font = list(size = 18)),
  #             font = (size = 1),
  #             geo = list( showframe = TRUE,
  #                         showcoastlines = TRUE,
  #                         projection = list(type = 'Mercator'),
  #                         showCountries = TRUE,
  #                         resolution = 110), 
  #             annotations = 
  #                 list(   x = 1, y = 1,
  #                         text = "Source: ilostat", 
  #                         showarrow = F, xref='paper', yref='paper', 
  #                         xanchor='right', yanchor='auto', xshift=0, yshift=0,
  #                         font=list(size=15, color="blue"))
  #         )
  # })

  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$Table_data <- renderTable ({
      datasetInput()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
