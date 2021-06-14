# Here we define the design and layout of our R Shiny application (Bootstrap).
# ui.R. will mainly have the bigger elements, such as navigation panels
# headers and footers. The rest we delegate to our modules, which we
# call in the respective places.
shinyUI(
  fluidPage(
    
    includeCSS("custom.css"), #include your own css styling
    useShinyjs(), #see shinyjs package for reference.
    
    # 1) Create a header with logo and buttons.
    # Inside the header we define a "fluidRow" with columns in it.
    # In Bootstrap, you can define 1-12 columns, with variable widths.
    # Fx column(1,...) creates a column that takes 1/12th of the webpage.
    tags$header(fluidRow(
      column(1,div(class="text-center",img(src='whack_icon.svg', id="whack-logo"))),
      column(2,data_add_synthetic_UI("addSyntheticData")),
      column(8,data_import_UI("import_game_csv")),
    )),
    
    # 2) Define a navigation panel.
    # This is the navigation bar in our application. Within each tabPanel
    # assign a label to the menu item and then we call our module UI code.
    # This way, we minimize UI code to ui.R. itself.
    navlistPanel(id = "pageChooser", well= FALSE, widths=c(2,10),
                 tabPanel("Data Investigator", page_data_investigator_UI("data_investigator")),
                 tabPanel("Secondary Page", HTML("<p>Under construction.</p>"))
    ),
    
    # 3) Insert a footer at the bottom (Optional)
    tags$footer()
  ))