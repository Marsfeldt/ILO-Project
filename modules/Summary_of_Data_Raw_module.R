# in this module we will return the summary and perdictions about the dataset  

Summary_of_Data_Raw_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
      radioButtons(ns("DependentVariableButton"),
                   label = h3("Dependent variable"),
                   choices = list(
                     "Fatalities" = 1,
                     "Injuries" = 2
                   ), inline = TRUE,
                   selected = 1
      ),
      htmlOutput(ns("Explanation_Summary")),
      verbatimTextOutput(ns("SumRaw")),
    )
  )
}

Summary_of_Data_Raw <- function(input, output, session, df) {
  ns <- session$ns
  
  var <- reactiveValues(DepVariable = NULL,
                        IndepVariable1 = NULL,
                        IndepVariable2 = NULL,
                        Name = NULL,
                        Norm = NULL)
  
  observeEvent(input$DependentVariableButton, {
    req(!is.null(input$DependentVariableButton))
    DependentVariableInput <- input$DependentVariableButton
    
    if (DependentVariableInput == 1) {
      var$DepVariable <- "Fatalities_per_100K_workers"
      var$IndepVariable1 <- "Labor_Inspectors_Per_10K_workers"
      var$IndepVariable2 <- "Inspections_Per_Inspector"
      var$Name <- "Fatalities"
      var$Norm <- "per 100.000 workers"
    }
    
    if (DependentVariableInput == 2) {
      var$DepVariable <- "Injuries_Per_100K_workers"
      var$IndepVariable1 <- "Labor_Inspectors_Per_10K_workers"
      var$IndepVariable2 <- "Inspections_Per_Inspector"
      var$Name <- "Injuries"
      var$Norm <- "per 100.000 workers"
    }
  })
  
  output$SumRaw <- renderPrint({
    
    DepVariable <- var$DepVariable
    IndepVariable1 <- var$IndepVariable1 
    IndepVariable2 <- var$IndepVariable2 
    
    dataset <- df()
    
    result <- lm(dataset[[DepVariable]] ~ dataset[[IndepVariable1]] * dataset[[IndepVariable2]])
    
    SumRaw <- summary(result)
      
    return(SumRaw)
  })
  
  output$Explanation_Summary <- renderUI({
    str0 <- paste(h3("Predictions for ", var$Name))
    str1 <- paste("Based on the data provided by ILO, we have only included countries that have reported on the each variable for a minimum of 5 years.")
    str2 <- paste("The following show the full prediction for", var$Name)
    str3 <- paste("Where dataset[[DepVariable]] = ", var$Name)
    str4 <- paste("Where dataset[[IndepVariable1]] = 'number of labor inspectors per 10.000 workers'")
    str5 <- paste("Where dataset[[IndepVariable2]] = number of inspections per labor inspector")
    Explanation_Summary <- HTML(paste(str0, str1, str2, str3, str4, str5, sep = '<br/>'))
    
    return(Explanation_Summary)
  })
}