#plot_Paralle
#plot_Paralle_Mean

plot_Paralle_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
      htmlOutput(ns("Explanation_Paralle_Plot")),
      plotlyOutput(ns("Paralle_Plot")),
    )
  )
}

plot_Paralle <- function(input, output, session, df) {
  ns <- session$ns
  
  output$Paralle_Plot <- renderPlotly({
    
    plot <- df() %>% 
      ggparcoord(
        columns = 3:6,
        groupColumn = 1,
        scale = "globalminmax",
        boxplot = FALSE,
        showPoints = TRUE,
        alphaLines = 0.3,
      ) +
      theme_bw()
    
    plotly <- ggplotly(plot, tooltip = c("Country")) %>%
      highlight(on = "plotly_hover", color = "red" )
    
    return(plotly)
  })
  
  output$Explanation_Paralle_Plot <- renderUI({
    str0 <- paste(h3("Parallel Plot with Raw Data"))
    str1 <- paste("In the following plot the 'Health and Safty' data has been normalized between 0 and 1")
    Explanation_Paralle_Plot <- HTML(paste(str1, sep = '<br/>'))
    
    return(Explanation_Paralle_Plot)
  })
}