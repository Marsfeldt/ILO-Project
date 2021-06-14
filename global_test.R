# 1) IMPORT LIBRARIES
# When you import libraries below, they become available through-out your
# whole R Shiny application. Importing your libraries here also means
# you have a single location to watch what packages your application uses.
library(tidyverse)
library(lubridate)
library(Rilostat)
library(plotly)
library(shiny)
library(shinyjs)

# 2) SET R OPTIONS
# Sometimes R defaults are not ideal for the app we use.
# shiny.fullstacktrace: prints full error messages from R Shiny for debugging.
# digits.secs: Show milliseconds in timestamps.
# shiny.maxRequestSize: increase maximum file upload size to 50MB.
options(shiny.fullstacktrace=TRUE)
options("digits.secs"=6)
options(shiny.maxRequestSize=50*1024^2)

# 3) IMPORT MODULES
# Whenever you create a new module, add it to the list below.
# Importing your module makes them callable in your R Shiny application.
source("modules/Import_Data_HealthSafty_module.R", local = T)

#source("modules/plot_timeline_module.R", local = T)
#source("modules/page_data_investigator_module.R", local = T)
#source("modules/data_add_synthetic_module.R", local=T)