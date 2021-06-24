# This is the R shiny application for the ILO-Project. In which we try to visulize and compare the data on Health and safty

library(shiny) # How to build R Shiny application here: http://shiny.rstudio.com/
#library(shinythemes) # find themes here: https://rstudio.github.io/shinythemes/
library(tidyverse)
library(magrittr)
library(readxl)
library(lubridate)
library(fs)
library(R.utils)
library(Rilostat)
library(plotly)
library(hrbrthemes)
library(GGally)
library(viridis)
library(broom)

# shiny.fullstacktrace: prints full error messages from R Shiny for debugging.
options(shiny.fullstacktrace = TRUE)

# https://www.ardata.fr/en/post/2019/04/26/share-reactive-among-shiny-modules/
# https://ilostat.github.io/Rilostat/articles/Rilostat.html
# https://www.ilo.org/ilostat-files/Documents/ILOSTAT_BulkDownload_Guidelines.pdf

# import modules:
source("modules/Import_Data_module.R", local = T)
source("modules/Overview_Main_module.R", local = T)
source("modules/Summary_of_Data_module.R", local = T)
source("modules/Summary_of_Data_Raw_module.R", local = T)
source("modules/Table_Data_module.R", local = T)
source("modules/Table_Data_Mean_module.R", local = T)
source("modules/plot_Paralle_module.R", local = T)
source("modules/plot_Paralle_Mean_module.R", local = T)
source("modules/Predict_All_Country_module.R", local = T)
source("modules/Country_Analysis_module.R", local = T)

# # Define UI for application ----
ui <- navbarPage("ILO - Project",
  theme = shinytheme("yeti"),

  # Overview tap where the user can get an overview of the data -----
  tabPanel(
    "Overview and perdictions of data",
    # Sidebar layout with input and output definitions
    sidebarLayout(
      # Sidebar panel for inputs
      sidebarPanel(
        width = 3,
        helpText("Welcome! The International Labor Organization (ILO) provides data on a long list of topics. For this prototype we have decided to focus on Healt and Safety - with the possiblity to add more topics later. This side provides you with an oveverview of the data on the specific topic"),

        # Input: Selector for choosing dataset
        Import_Data_UI("ImportData")
      ),

      # Main panel for displaying outputs
      mainPanel(
        tabsetPanel(
          tabPanel(
            width = 10,
            "Average Data over time per country",
            Summary_of_Data_UI("Sum"),
            plot_Paralle_Mean_UI("Paralle_Plot_Mean"),
            Table_Data_Mean_UI("Table_With_Mean_Data")
          ),
          tabPanel(
            width = 10,
            "Predictions Per Country",
            Country_Analysis_UI("Country_Analysis")
          ),
          tabPanel(
            "Overview of Raw Data",
            Summary_of_Data_Raw_UI("SumRaw"),
            plot_Paralle_UI("Paralle_Plot"),
            Table_Data_UI("Table_With_Data")
          ),
          tabPanel(
            "Overview of Raw Data Per Countries",
            Predict_All_Country_UI("Predict_Country")
          )
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  r <- reactiveValues(
    df = NULL,
    dfMean = NULL
  )

  datasetInput <- callModule(Import_Data, "ImportData")

  observeEvent(datasetInput$trigger, {
    req(datasetInput$trigger > 0)
    r$df <- datasetInput$datasetInput
    r$dfMean <- datasetInput$datasetInputMean
  })

  callModule(Summary_of_Data, "Sum", reactive(r$df))
  callModule(Summary_of_Data_Raw, "SumRaw", reactive(r$df))
  callModule(plot_Paralle, "Paralle_Plot", reactive(r$df))
  callModule(plot_Paralle_Mean, "Paralle_Plot_Mean", reactive(r$dfMean))
  callModule(Table_Data, "Table_With_Data", reactive(r$df))
  callModule(Table_Data_Mean, "Table_With_Mean_Data", reactive(r$dfMean))
  callModule(Predict_All_Country, "Predict_Country", reactive(r$df))
  callModule(Country_Analysis, "Country_Analysis", reactive(r$df))
}

# Run the application ----
shinyApp(ui = ui, server = server)
