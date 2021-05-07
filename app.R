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
library(Rilostat)
library(plotly)

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
      helpText("Create demographic maps with information from ILO."),

      # Input: Selector for choosing dataset ----
      selectInput(
        inputId = "dataSelect",
        label = "Choose a metric:",
        choices = c(
            "Youth Unemployment" = "EIP_2EET_SEX_RT_A",
            "Fatalities" = "INJ_FATL_SEX_MIG_RT_A",
            "Injuries" = "INJ_NFTL_SEX_MIG_RT_A"
        )
      ),
      # Make buttons for Gender
      radioButtons("genderRadio",
        label = h3("Gender"),
        choices = list("Total" = "T", 
                       "Male" = "M", 
                       "Female" = "F"),
        selected = "T"
      ),
      
      # Make slider for year
      sliderInput("yearSlider",
        label = h3("Year"), min = 1975,
        max = 2019, value = 2010, sep = ""
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Plot",
          # Output: Formatted text for caption ----
          h3(textOutput("caption")),
          
          plotOutput("mapPlot")
        ),
        
        tabPanel("Summary", verbatimTextOutput("summary"),
                 # Output: Diagram ----
                 plotOutput("distPlot")
                 ),
        
        tabPanel("Table", tableOutput("table"),
                 tableOutput("Table_data")
                 )
      )
    )
  ),
  tabPanel(
    "Data comparison",
    # Input: vearible used to change the visulization ----
    sliderInput("bins",
      "Number of bins:",
      min = 5,
      max = 50,
      value = 30
    ),

    # Input: Checkbox for whether outliers should be included ----
    checkboxInput("outliers", "Show outliers", TRUE)
  ),
  tabPanel("Component 3")
)
# )

# Define server logic ----
server <- function(input, output) {

  datasetChoice <- reactive({
      paste("DataSet ~", input$dataSelect)
  })

  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
      datasetChoice()
  })
  
  datasetInput <- reactive({
      dataset <- get_ilostat(id = input$dataSelect, segment = 'indicator', filters = list(time = input$yearSlider, sex = "T")) %>% 
          filter(str_sub(ref_area,1,1) != 'X') %>%
          select(ref_area, obs_value) %>%
          left_join(Rilostat:::ilostat_ref_area_mapping %>%
                        select(ref_area, ref_area_plotly) %>%
                        label_ilostat(code = 'ref_area'),
                    by = "ref_area") %>%
          filter(!obs_value %in% NA) %>%
          mutate(tot_obs_value = cut(obs_value, 
                                     quantile(obs_value, na.rm = TRUE), 
                                     include.lowest = TRUE))
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$mapPlot <- renderPlot({
      dataset <- datasetInput()
      dataset %>%   plot_geo( 
          #width = 900, 
          #height = 600
          ) %>%
          add_trace(
              z = ~obs_value, 
              color = ~obs_value, 
              colors=c("green", "blue"), 
              text = ~ref_area.label, 
              locations = ~ref_area_plotly, 
              marker = list(line = list(color = toRGB("grey"), width = 0.5)),
              showscale = TRUE) %>%
          colorbar(title = '(%)', len = 0.5, ticksuffix="%") %>%
          layout(
              title = list(   text = "Share of youth not in employment, education or training (NEET) in 2018",
                              font = list(size = 18)),
              font = (size = 1),
              geo = list( showframe = TRUE,
                          showcoastlines = TRUE,
                          projection = list(type = 'Mercator'),
                          showCountries = TRUE,
                          resolution = 110), 
              annotations = 
                  list(   x = 1, y = 1,
                          text = "Source: ilostat", 
                          showarrow = F, xref='paper', yref='paper', 
                          xanchor='right', yanchor='auto', xshift=0, yshift=0,
                          font=list(size=15, color="blue"))
          )
  })

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "darkgray", border = "orange")
  })

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
