# Plot the overview 

Overview_Main_UI <- function(id) {
  ns = NS(id)
  mainPanel(
            tabsetPanel(
              tabPanel(
                "Average Data over time per country",
                # Output: Diagram ----
                # verbatimTextOutput("summaryFatility"),
                # plotOutput("ParallePlotMean"),
                # tableOutput("TableDataMean")
                Summary_of_Data_UI(ns("Summary_of_Data")),
                plot_Paralle_Mean_UI(ns("Paralle_Plot_Mean")),
                Table_Data_Mean_UI(ns("Table_Data_Mean"))
              ),
              tabPanel(
                "Raw data per country",
                # Output: Formatted text for caption ----
                # h3("Paralle plot"),
                #Summary_of_Data_UI(ns("Summary_of_Data")),
                plot_Paralle_UI(ns("Paralle_Plot_Mean")),
                Table_Data_UI(ns("Table_Data_Mean"))
              )
            )
  )
}

Overview_Main <- function(input, output, session, df, meta) {
  ns <- session$ns
  
  callModule(Summary_of_Data, "summary", df)
  

  output$more_info <- renderUI({
    #validate() ensures that our code is only executed if the dataframe
    # is available, otherwise showing a fallback message.
    # See another example in plot_timeline_module.R
    validate(need(df(), "No data yet."))
    
    df_data <- df()
    
    ui <- HTML(paste(
      "<p>",
      "Number of rows in data:",
      nrow(df_data),
      "</p>"
    ))
    return(ui)
  })
  
}