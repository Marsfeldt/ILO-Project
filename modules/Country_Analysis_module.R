# Country_Analysis_module

Country_Analysis_UI <- function(id) {
  ns <- NS(id)
  list(
    fluidRow(
      radioButtons(ns("DependentVariableRadioButton"),
        label = h3("Dependent variable"),
        choices = list(
          "Fatilities" = 1,
          "Injuries" = 2
        ), inline = TRUE,
        selected = 1
      ),
      selectInput(ns("SelectCountry"),
        "Country",
        choices = NULL,
        selected = NULL
      ),
      htmlOutput(ns("sumCountry")),
      htmlOutput(ns("Explanation_IndependentVariable1")),
      plotlyOutput(ns("CompareInspectionsCountry")),
      htmlOutput(ns("Explanation_IndependentVariable2")),
      plotlyOutput(ns("InspectorsP10kCountry"))
    )
  )
}

Country_Analysis <- function(input, output, session, df) {
  ns <- session$ns

  var <- reactiveValues(
    DepVariable = NULL,
    IndepVariable1 = NULL,
    IndepVariable2 = NULL,
    Name = NULL,
    Norm = NULL
  )

  observeEvent(input$DependentVariableRadioButton, {
    req(!is.null(input$DependentVariableRadioButton))
    DependentVariableInput <- input$DependentVariableRadioButton

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

    dataCountry <- df()

    dataCountry <- dataCountry[!duplicated(dataCountry$Country), ]

    selectCountry <- dataCountry$Country

    updateSelectInput(session,
      "SelectCountry",
      label = "Select Country",
      choices = selectCountry
    )
  })

  output$CompareInspectionsCountry <- renderPlotly({
    DepVariable <- var$DepVariable
    IndepVariable2 <- var$IndepVariable2

    SelectedCountry <- input$SelectCountry

    datasetInput <- df() %>% filter(Country == SelectedCountry)

    plot <- ggplot(
      datasetInput,
      aes(
        y = datasetInput[[DepVariable]],
        x = datasetInput[[IndepVariable2]],
        color = Year,
        alpha = 0.5
      )
    ) +
      geom_point() +
      geom_smooth(
        method = lm,
        se = F
      ) +
      theme(axis.title = element_blank()) +
      # facet_wrap(~Country) +
      theme_bw()

    plotly <- ggplotly(plot, tooltip = c("Year")) %>%
      highlight(on = "plotly_hover", color = "red")

    return(plotly)
  })

  output$InspectorsP10kCountry <- renderPlotly({
    DepVariable <- var$DepVariable
    IndepVariable1 <- var$IndepVariable1

    SelectedCountry <- input$SelectCountry

    datasetInput <- df() %>% filter(Country == SelectedCountry)

    plot <- ggplot(
      datasetInput,
      aes(
        y = datasetInput[[DepVariable]],
        x = datasetInput[[IndepVariable1]],
        color = Year,
        alpha = 0.5
      )
    ) +
      geom_point() +
      geom_smooth(
        method = lm,
        se = F
      ) +
      theme(axis.title = element_blank()) +
      # facet_wrap(~Country) +
      theme_bw()

    plotly <- ggplotly(plot, tooltip = c("Year")) %>%
      highlight(on = "plotly_hover", color = "red")

    return(plotly)
  })



  output$sumCountry <- renderUI({
    DepVariable <- var$DepVariable
    IndepVariable1 <- var$IndepVariable1
    IndepVariable2 <- var$IndepVariable2

    SelectedCountry <- input$SelectCountry

    dataset <- df() %>% filter(Country == SelectedCountry)

    result <- lm(dataset[[DepVariable]] ~ dataset[[IndepVariable1]] * dataset[[IndepVariable2]])

    tableSummary <- tidy(result)

    tableSummary[c(2, 3, 4), 1] <- c("number of labor inspectors per 10.000 workers", "number of inspections per labor inspector", "combined")

    tableSummary.num <- sapply(tableSummary, is.numeric)

    tableSummary[tableSummary.num] <- lapply(tableSummary[tableSummary.num], round, 2)

    Rsquared <- round(summary(result)$r.squared, 4) * 100

    str0 <- paste(h3("Predicted ", var$Name, "for ", SelectedCountry))
    str1 <- paste("Based on the data provided by ILO. It is estimated that there will be ", tableSummary[1, 2], var$Name, var$Norm, ". However, if we increase the ", tableSummary[2, 1], "by 1, we predict that ", var$Name, "will change by ", tableSummary[2, 2], ". If we increase the", tableSummary[3, 1], "by 1, we predict that ", var$Name, "will change by ", tableSummary[3, 2])
    str2 <- paste("If we combint the two, meaning both increasing ", tableSummary[2, 1], "and", tableSummary[3, 1], "by 1, we predict that ", var$Name, "will change by ", tableSummary[4, 2])
    str3 <- paste("However, this only explanes ", Rsquared, "% of the variance in the data, meaning other factors must play a role in the number of ", var$Name, "aswell.")
    sumCountry <- HTML(paste(str0, str1, str2, str3, sep = "<br/>"))

    # browser()
    return(sumCountry)
  })


  output$Explanation_IndependentVariable1 <- renderUI({
    str1 <- paste(h3("Plot over ", var$Name, "and", var$IndepVariable1))
    str2 <- paste("The following plot shows ", var$Name, "(y-axis) and ", var$IndepVariable1, "(x-axis). A linear reggersion line is shown.")
    Explanation_IndependentVariable1 <- HTML(paste(str1, str2, sep = "<br/>"))

    # browser()
    return(Explanation_IndependentVariable1)
  })

  output$Explanation_IndependentVariable2 <- renderUI({
    str1 <- paste(h3("Plot over ", var$Name, "and", var$IndepVariable2))
    str2 <- paste("The following plot shows ", var$Name, "(y-axis) and ", var$IndepVariable2, "(x-axis). A linear reggersion line is shown.")
    Explanation_IndependentVariable2 <- HTML(paste(str1, str2, sep = "<br/>"))

    # browser()
    return(Explanation_IndependentVariable2)
  })
}
