#Table_Data_Mean

Table_Data_Mean_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
      htmlOutput(ns("Explanation_Table_Mean")),
      tableOutput(ns("Table_With_Mean_Data"))
    )
  )
}

Table_Data_Mean <- function(input, output, session, dfMean) {
  ns <- session$ns
  
    output$Table_With_Mean_Data <- renderTable({
      #validate(need(dfMean(), "Waiting for data."), errorClass = "vis")
      tableMean <- dfMean()
      #browser()
      return(tableMean)
    })
    
    output$Explanation_Table_Mean <- renderUI({
      str1 <- paste(h3("Table with mean data"))
      str2 <- paste("The following table shows the average data each country have collected over a period of minimum 5 years. This is the data displaied in the paralle plot above.")
      Explanation_Table_Mean <- HTML(paste(str1, str2, sep = '<br/>'))
      
      #browser()
      return(Explanation_Table_Mean)
    })
}
  