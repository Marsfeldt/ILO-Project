# This is the R shiny application for the ILO-Project. In which we try to visulize and compare the data on Health and safty

library(shiny) # How to build R Shiny application here: http://shiny.rstudio.com/
library(shinythemes) # find themes here: https://rstudio.github.io/shinythemes/
library(tidyverse)
library(readxl)
library(lubridate)
library(fs)
library(R.utils)
library(Rilostat)
library(plotly)
library(hrbrthemes)
library(GGally)
library(viridis)

# shiny.fullstacktrace: prints full error messages from R Shiny for debugging.
options(shiny.fullstacktrace = TRUE)

# https://www.ardata.fr/en/post/2019/04/26/share-reactive-among-shiny-modules/
# https://ilostat.github.io/Rilostat/articles/Rilostat.html
# https://www.ilo.org/ilostat-files/Documents/ILOSTAT_BulkDownload_Guidelines.pdf

# Import Data ----
# Data for Fatilities ----
Fatilities <- get_ilostat(
  id = "SDG_F881_SEX_MIG_RT_A",
  segment = "indicator",
  time_format = "num",
  filters = list(
    sex = "SEX_T",
    classif1 = "MIG_STATUS_TOTAL"
  ),
  cache = FALSE
) %>%
  filter(str_sub(ref_area, 1, 1) != "X") %>%
  select(ref_area, obs_value, time, classif1) %>%
  left_join(Rilostat:::ilostat_ref_area_mapping %>%
              select(ref_area) %>%
              label_ilostat(code = "ref_area"),
            by = "ref_area"
  ) %>%
  rename(TotalFatilitiesNormP100K = obs_value)

# Data for Injuries ------
Injuries <- get_ilostat(
  id = "SDG_N881_SEX_MIG_RT_A",
  segment = "indicator",
  time_format = "num",
  filters = list(
    sex = "SEX_T",
    classif1 = "MIG_STATUS_TOTAL"
    # year = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
  ),
  cache = FALSE
) %>%
  filter(str_sub(ref_area, 1, 1) != "X") %>%
  select(ref_area, obs_value, time, classif1) %>%
  left_join(Rilostat:::ilostat_ref_area_mapping %>%
              select(ref_area) %>%
              label_ilostat(code = "ref_area"),
            by = "ref_area"
  ) %>%
  rename(TotalInjuriesNormP100K = obs_value)

# Data for Labor Inspectors per 10K worker -------
LaborInspectors <- get_ilostat(
  id = "LAI_INDE_NOC_RT_A",
  segment = "indicator",
  time_format = "num",
  # filters = list(sex = "SEX_T"),
  cache = FALSE
) %>%
  filter(str_sub(ref_area, 1, 1) != "X") %>%
  select(ref_area, obs_value, time) %>%
  left_join(Rilostat:::ilostat_ref_area_mapping %>%
              select(ref_area) %>%
              label_ilostat(code = "ref_area"),
            by = "ref_area"
  ) %>%
  rename(LaborInspectorsNormP10K = obs_value)

# Data for number inspections per inspector ------
InspectionsPerInspector <- get_ilostat(
  id = "LAI_VDIN_NOC_RT_A",
  segment = "indicator",
  time_format = "num",
  # filters = list(sex = "SEX_T"),
  cache = FALSE
) %>%
  filter(str_sub(ref_area, 1, 1) != "X") %>%
  select(ref_area, obs_value, time) %>%
  left_join(Rilostat:::ilostat_ref_area_mapping %>%
              select(ref_area) %>%
              label_ilostat(code = "ref_area"),
            by = "ref_area"
  ) %>%
  rename(InspectionsPerInspector = obs_value)

# Combine the Data -----
HealtySafetyCombinedData <- Fatilities %>%
  dplyr::full_join(Injuries, by = c("ref_area", "time", "ref_area.label", "classif1"), copy = FALSE, keep = FALSE) %>%
  dplyr::full_join(LaborInspectors, by = c("ref_area", "time", "ref_area.label"), copy = FALSE, keep = FALSE) %>%
  dplyr::full_join(InspectionsPerInspector, by = c("ref_area", "time", "ref_area.label"), copy = FALSE, keep = FALSE)

col_order <- c("ref_area", "ref_area.label", "classif1", "time", "TotalFatilitiesNormP100K", "TotalInjuriesNormP100K", "LaborInspectorsNormP10K", "InspectionsPerInspector")

HealtySafetyCombinedData <- HealtySafetyCombinedData[, col_order]

HealtySafetyCombinedData <- HealtySafetyCombinedData %>% drop_na() 

HealtySafetyCombinedData <- HealtySafetyCombinedData[HealtySafetyCombinedData$ref_area %in% names(which(table(HealtySafetyCombinedData$ref_area) > 4)), ] 

HealtySafetyCombinedData <- HealtySafetyCombinedData %>%
  rename(Country = ref_area.label,
         Year = time) %>%
  select(-c(ref_area, classif1)) %>%
  mutate(Year = round(Year, digits = 0))

HealtySafetyCombinedDataMean <- HealtySafetyCombinedData %>%
  group_by(Country) %>%
  dplyr::summarize(mTotalFatilitiesNormP100K = mean(TotalFatilitiesNormP100K),
                   meanTotalInjuriesNormP100K = mean(TotalInjuriesNormP100K),
                   meanLaborInspectorsNormP10K = mean(LaborInspectorsNormP10K),
                   meanInspectionsPerInspector = mean(InspectionsPerInspector))

# Define UI for application ----
ui <- navbarPage("ILO - Project",
  theme = shinytheme("yeti"),

  # Overview tap where the user can get an overview of the data -----
  tabPanel(
    "Overview and perdictions of data",
    # Sidebar layout with input and output definitions 
    sidebarLayout(
      # Sidebar panel for inputs 
      sidebarPanel(
        helpText("Welcome! The International Labor Organization (ILO) provides data on a long list of topics. For this prototype we have decided to focus on Healt and Safety - with the possiblity to add more topics later. This side provides you with an oveverview of the data on the specific topic"),

        # Input: Selector for choosing dataset 
        selectInput(
          inputId = "SelectTopic",
          label = "Choose a Topic:",
          choices = c(
            "Health and Safety" = "HealthSafetyData",
            "Child Labour" = "ChildLabourData",
            "..." = "..."
          )
        ),
      ),

      # Main panel for displaying outputs ----
      mainPanel(
        tabsetPanel(
          tabPanel("Average Data over time per country",
                   # Output: Diagram ----
                   verbatimTextOutput("summaryFatility"),
                   plotOutput("ParallePlotMean"),
                   tableOutput("TableDataMean")
          ),
          tabPanel("Raw data per country",
            # Output: Formatted text for caption ----
            #h3("Paralle plot"),
            plotOutput("ParallePlot"),
            tableOutput("TableData")
          )
        )
      ),
    ),
  ),
  # Combine tap where the user can get perdictions based on the topic -----
  tabPanel(
    "Detailed Data Perdiction",
    # Sidebar layout with input and output definitions 
    sidebarLayout(
      sidebarPanel(
        helpText("Welcome! Use data provided by ILO to perdict the effect"),

        # Input: Selector for choosing dataset 
        selectInput(
          inputId = "SelectTopic",
          label = "Choose a Topic:",
          choices = c(
            "Safety and Health" = "HealthSaftyData",
            "Child Labour" = "ChildLabourData",
            "..." = "..."
          )
        ),
      ),
      mainPanel(
        tabsetPanel(
        tabPanel("Perdict Fatilities",
                 # Output: Formatted text for caption ----
                 #h3("Paralle plot"),
                 #verbatimTextOutput("summaryFatility"),
                 plotOutput("CompareFatilitiesInspectors"),
                 plotOutput("CompareFatilitiesInspections")
                 #plotOutput("ParallePlot"),
                 #tableOutput("TableData")
        ),
        tabPanel("Perdict Injuries",
                 # Output: Formatted text for caption ----
                 #h3("Paralle plot"),
                 verbatimTextOutput("summaryInjury"),
                 plotOutput("CompareInjuriesInspectors"),
                 plotOutput("CompareInjuriesInspections")
                 #plotOutput("ParallePlot"),
                 #tableOutput("TableData")
        )
      ))
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  datasetInput <- reactive({
    if (input$SelectTopic == "HealthSafetyData") {
      return(HealtySafetyCombinedData)
    }

    if (input$SelectTopic == "ChildLabourData") {

    }
  })
  
  datasetInputMean <- reactive({
    if (input$SelectTopic == "HealthSafetyData") {
      return(HealtySafetyCombinedDataMean)
    }
    
    if (input$SelectTopic == "ChildLabourData") {
      
    }
  })

  output$ParallePlot <- renderPlot({
    ggparcoord(datasetInput(),
               columns = 3:6, 
               groupColumn = 1,
               scale="center",
               showPoints = TRUE, 
               alphaLines = 0.3
    )+
      theme_ipsum()
  })
  
  output$ParallePlotMean <- renderPlot({
    ggparcoord(datasetInputMean(),
               columns = 2:5, 
               groupColumn = 1,
               scale="center",
               showPoints = TRUE, 
               alphaLines = 0.3
    )+
      theme_ipsum()
  })

  output$CompareFatilitiesInspections <- renderPlot({
    ggplot(datasetInput(),
           aes(y = TotalFatilitiesNormP100K, 
               x = InspectionsPerInspector,
               color = Year,
               alpha = 0.5)
    ) + 
      geom_point() +
      geom_smooth(method=lm,
                  se = F) +
      facet_wrap(~Country) +
      theme_bw()
  })
  
  
  output$CompareFatilitiesInspectors <- renderPlot({
    ggplot(datasetInput(),
           aes(y = TotalFatilitiesNormP100K, 
               x = LaborInspectorsNormP10K,
               color = Year,
               alpha = 0.5)
    ) + 
      geom_point() +
      geom_smooth(method=lm,
                  se = F) +
      facet_wrap(~Country) +
      theme_bw()
  })
  
  output$CompareInjuriesInspections <- renderPlot({
    ggplot(datasetInput(),
           aes(y = TotalInjuriesNormP100K, 
               x = InspectionsPerInspector,
               color = Year,
               alpha = 0.5)
    ) + 
      geom_point() +
      geom_smooth(method=lm,
                  se = F) +
      facet_wrap(~Country) +
      theme_bw()
  })
  
  output$CompareInjuriesInspectors <- renderPlot({
    ggplot(datasetInput(),
           aes(y = TotalInjuriesNormP100K, 
               x = LaborInspectorsNormP10K,
               color = Year,
               alpha = 0.5)
    ) + 
      geom_point() +
      geom_smooth(method=lm,
                  se = F) +
      facet_wrap(~Country) +
      theme_bw()
  })
  
  output$summaryFatility <- renderPrint({
    dataset <- datasetInput()
    summary(lm(TotalFatilitiesNormP100K ~ LaborInspectorsNormP10K * InspectionsPerInspector, data = dataset))
  })
  
  output$summaryInjury <- renderPrint({
    dataset <- datasetInput()
    summary(lm(TotalInjuriesNormP100K ~ LaborInspectorsNormP10K * InspectionsPerInspector, data = dataset))
  })

  output$TableData <- renderTable({
    datasetInput()
  })
  output$TableDataMean <- renderTable({
    datasetInputMean()
  })
  

  
}

# Run the application
shinyApp(ui = ui, server = server)
