#Table_Data

Table_Data_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
      htmlOutput(ns("Explanation_Table")),
      tableOutput(ns("Table_With_Data")),
    )
  )
}

Table_Data <- function(input, output, session, df) {
  ns <- session$ns
  
  output$Table_With_Data <- renderTable({
    table <- df()
    return(table)
  })
  
  output$Explanation_Table <- renderUI({
    str1 <- paste(h3("Table with mean data"))
    str2 <- paste("The following table shows the average data each country which have collected over a period of minimum 5 years. This is the data displaied in the paralle plot above.")
    Explanation_Table <- HTML(paste(str1, str2, sep = '<br/>'))
    
    return(Explanation_Table)
  })
}