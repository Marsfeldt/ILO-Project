# Server.R - Bastian I. Hougaard
# Server.R's main responsibility is to initiate  your data import modules, 
# and distribute the data to your other modules. Server.R can render
# your data to a graph or a table, but usually you want to delegate this
# responsiblity to a dedicated plotting module, which takes care of
# plotting and associated calculations. See "plot_timeline_module.R".

shinyServer(function(input, output, session) {
  
  # 1) REACTIVE DATA STRUCTURE
  # Dataframes does not react to user input in R by default. Therefore we need to
  # hold our variables inside a special data structure called "reactiveValues".
  # In this example, we declare a dataframe 'df' and a string 'source'
  # which we initialize to NULL.
  #
  # To make calculations with 'df', we need to call it within a "reactive context"
  # ObserveEvent({ }), observe({ }), renderTable({ }), renderPlot({ }) etc.
  # The server function itself is not "reactive", so avoid making calculations
  # inside it.
  r <- reactiveValues(df = NULL, source = NULL)
  
  # 2) CALL YOUR MODULES
  # We need to initialize the main modules of our R shiny application.
  # If your main modules contain "submodules" (fx a plot inside the frontpage)
  # you initialize the submodules within your modules.
  #
  # callModule(module_function, "module_id", args)
  #   module_function: this is the name of the module server function.
  #   module_id: this is a unique ID you choose. Use the same ID in ui.R.
  #   args: any number of REACTIVE variables you wish to pass to the module, fx 'df'.
  #   we must encapsulate these variables with "reactive()".
  # 
  # If you need to send data from your module to the app, use a return value.
  csv_data <- callModule(data_import, "import_game_csv")
  syn_data <- callModule(data_add_synthetic, "addSyntheticData", reactive(r$df))
  callModule(page_data_investigator, "data_investigator", reactive(r$df))
  
  # 3) OBSERVE INCOMING DATA
  # To delegate the data to the rest of the application, server.R needs to 
  # observe incoming data, process it and pass it on as a data frame.
  # We do this by observing a "trigger", which is just an integer that
  # we increase every time a new change happens.
  observeEvent(csv_data$trigger, {
    req(csv_data$trigger > 0)
    r$df <- csv_data$df
    r$source <- csv_data$filename
  })
  
  # This is another example of observation
  # in this case, we observe our module "data_add_synthetic"
  # which reads the dataframe and adds extra data to it.
  observeEvent(syn_data$trigger, {
    req(syn_data$trigger > 0)
    r$df <- syn_data$df
  })
  
})