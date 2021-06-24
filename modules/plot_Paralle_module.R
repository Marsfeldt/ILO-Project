#plot_Paralle

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
        scale = "uniminmax",
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
    str1 <- paste("In the following plot we have only included countries that have reported on the each variable for a minimum of 5 years. The data has been normalized between 0 and 1. Where 1 is the country with the highes number in the given variable, and 0 is the country with the lowst number in the given variable. The other countries are then normalized between these two extremes.")
    Explanation_Paralle_Plot <- HTML(paste(str1, sep = '<br/>'))
    
    return(Explanation_Paralle_Plot)
  })
}