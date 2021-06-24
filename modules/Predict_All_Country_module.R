# Predict_All_Country_module

Predict_All_Country_UI <- function(id) {
  ns <- NS(id)
  list(
    fluidRow(
      radioButtons(ns("DependentVariableRadioButton"),
        label = h3("Dependent variable"),
        choices = list(
          "Fatalities" = 1,
          "Injuries" = 2
        ), inline = TRUE,
        selected = 1
      ),
      plotlyOutput(ns("CompareInspections")),
      plotlyOutput(ns("InspectorsP10k"))
    )
  )
}

Predict_All_Country <- function(input, output, session, df) {
  ns <- session$ns

  var <- reactiveValues(Variable = NULL)

  observeEvent(input$DependentVariableRadioButton, {
    req(!is.null(input$DependentVariableRadioButton))
    DependentVariableInput <- input$DependentVariableRadioButton

    if (DependentVariableInput == 1) {
      var$Variable <- "Fatalities_per_100K_workers"
    }

    if (DependentVariableInput == 2) {
      var$Variable <- "Injuries_Per_100K_workers"
    }
  })

  output$CompareInspections <- renderPlotly({
    Variable <- var$Variable

    datasetInput <- df()

    plot <- ggplot(
      datasetInput,
      aes(
        y = datasetInput[[Variable]],
        x = Inspections_Per_Inspector,
        color = Year,
        alpha = 0.5
      )
    ) +
      geom_point() +
      geom_smooth(
        method = lm,
        se = F
      ) +
      facet_wrap(~Country) +
      theme_bw()

    plotly <- ggplotly(plot, tooltip = c("Year")) %>%
      highlight(on = "plotly_hover", color = "red")

    return(plotly)
  })
  
  output$InspectorsP10k <- renderPlotly({
    Variable <- var$Variable
    
    datasetInput <- df()
    
    plot <- ggplot(
      datasetInput,
      aes(
        y = datasetInput[[Variable]],
        x = Labor_Inspectors_Per_10K_workers,
        color = Year,
        alpha = 0.5
      )
    ) +
      geom_point() +
      geom_smooth(
        method = lm,
        se = F
      ) +
      facet_wrap(~Country) +
      theme_bw()
    
    plotly <- ggplotly(plot, tooltip = c("Year")) %>%
      highlight(on = "plotly_hover", color = "red")
    
    return(plotly)
  })
}
