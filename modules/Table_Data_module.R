#Table_Data

Table_Data_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
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
}