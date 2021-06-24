# in this module we will return the summary and perdictions about the dataset  

Summary_of_Data_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
      radioButtons(ns("DependentVariableButton"),
                   label = h3("Dependent variable"),
                   choices = list(
                     "Fatilities" = 1,
                     "Injuries" = 2
                   ), inline = TRUE,
                   selected = 1
      ),
      htmlOutput(ns("Sum")),
    )
  )
}

Summary_of_Data <- function(input, output, session, df) {
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
      var$DepVariable <- "Fatilities_per_100K_workers"
      var$IndepVariable1 <- "Labor_Inspectors_Per_10K_workers"
      var$IndepVariable2 <- "Inspections_Per_Inspector"
      var$Name <- "Fatilities"
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
  
  output$Sum <- renderUI({
    
    DepVariable <- var$DepVariable
    IndepVariable1 <- var$IndepVariable1 
    IndepVariable2 <- var$IndepVariable2 
    
    dataset <- df()

    result <- lm(dataset[[DepVariable]] ~ dataset[[IndepVariable1]] * dataset[[IndepVariable2]])

    tableSummary <- tidy(result)
    
    tableSummary[c(2,3,4),1] <- c("number of labor inspectors per 10.000 workers","number of inspections per labor inspector","combined")
    
    tableSummary.num <- sapply(tableSummary, is.numeric)
    
    tableSummary[tableSummary.num] <- lapply(tableSummary[tableSummary.num], round, 2)
    
    Rsquared <- round(summary(result)$r.squared, 4)*100
    
    str0 <- paste(h3("Predictions for ", var$Name))
    str1 <- paste("Based on the data provided by ILO. Using a linear regression, it is estimated that there will be ", tableSummary[1, 2], var$Name, var$Norm, ". However, if we increase the ", tableSummary[2, 1], "by 1, we predict that ", var$Name, "will change by ", tableSummary[2, 2], ". If we increase the", tableSummary[3, 1], "by 1, we predict that ", var$Name, "will change by ", tableSummary[3, 2])
    str2 <- paste("If we combint the two, meaning both increasing ", tableSummary[2, 1], "and", tableSummary[3, 1], "by 1, we predict that ", var$Name, "will change by ", tableSummary[4, 2])
    str3 <- paste("However, this only explanes ", Rsquared, "% of the variance in the data, meaning other factors must play a role in the number of ", var$Name, "aswell.")
    sum <- HTML(paste(str0, str1, str2, str3, sep = '<br/>'))
    
    return(sum)
  })
}