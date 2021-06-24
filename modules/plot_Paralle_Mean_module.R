#plot_Paralle_Mean

plot_Paralle_Mean_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
      htmlOutput(ns("Explanation_Paralle_Plot_Mean")),
      plotlyOutput(ns("Paralle_Plot_Mean")),
    )
  )
}

plot_Paralle_Mean <- function(input, output, session, dfMean) {
  ns <- session$ns
  
  output$Paralle_Plot_Mean <- renderPlotly({
    
    plot_Mean <- dfMean() %>% 
     ggparcoord(
              columns = 2:5,
              groupColumn = 1,
              scale = "uniminmax",
              boxplot = FALSE,
              showPoints = TRUE,
              alphaLines = 0.3,
    ) +
     theme_bw()
    
    plotly_Mean <- ggplotly(plot_Mean, tooltip = c("Country")) %>%
      highlight(on = "plotly_hover", color = "red" )
    
    return(plotly_Mean)
  })
  
  output$Explanation_Paralle_Plot_Mean <- renderUI({
    str1 <- paste(h3("Normalized Parallel Plot"))
    str2 <- paste("In the following plot the 'Health and Safty' data has been normalized between 0 and 1. Where 1 is the country with the highes number in the given variable, and 0 is the country with the lowst number in the given variable. The other countries are then normalized between these two extremes. We have only included countries that have reported on the each variable for a minimum of 5 years.")
    Explanation_Paralle_Plot_Mean <- HTML(paste(str1, str2, sep = '<br/>'))
    
    #browser()
    return(Explanation_Paralle_Plot_Mean)
  })
}