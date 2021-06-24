# Import, clean, and merged data

Import_Data_UI <- function(id) {
  ns <- NS(id)
  list(
    selectInput(
      inputId = ns("SelectTopic"),
      label = "Choose a Topic:",
      choices = c(
        "Health and Safety" = "HealthSafetyData",
        "Child Labour" = "ChildLabourData",
        "..." = "..."
      ),
    )
  )
}

# https://ilostat.github.io/Rilostat/articles/Rilostat.html
# https://www.ilo.org/ilostat-files/Documents/ILOSTAT_BulkDownload_Guidelines.pdf

Import_Data <- function(input, output, session) {
  ns <- session$ns
  #print("hi it works")
  toReturn <- reactiveValues(
    datasetInput = NULL,
    datasetInputMean = NULL,
    trigger = 0
  )

  observeEvent(input$SelectTopic, {
    req(!is.null(input$SelectTopic))
    topic <- input$SelectTopic
    #browser()
    if (topic == "HealthSafetyData") {
      
      df <- GetHealthData(df)

      dfMean <- df %>%
        #drop_na(Fatilities_per_100K_workers) %>%
        #subset(ave(Country, Country, FUN = length) > 4) #%>%
        #drop_na() #%>%
        group_by(Country) %>%
        dplyr::summarize(
         mean_Fatilities_per_100K_workers = mean(Fatilities_per_100K_workers),
         mean_Injuries_Per_100K_workers = mean(Injuries_Per_100K_workers),
         mean_Labor_Inspectors_Per_10K_worker = mean(Labor_Inspectors_Per_10K_workers),
         mean_Inspections_Per_Inspector = mean(Inspections_Per_Inspector)
        )
      
      # dfMeanDep2 <- df1 %>%
      #   drop_na(Injuries_Per_100K_workers) %>%
      #   group_by(Country) %>%
      #   dplyr::summarize(
      #     #mean_Fatilities_per_100K_workers = mean(Fatilities_per_100K_workers),
      #     mean_Injuries_Per_100K_workers = mean(Injuries_Per_100K_workers),
      #     mean_Labor_Inspectors_Per_10K_worker = mean(Labor_Inspectors_Per_10K_workers),
      #     mean_Inspections_Per_Inspector = mean(Inspections_Per_Inspector)
      #   )
      
      # dfMean <- dfMeanDep1 %>%
      #   dplyr::full_join(dfMeanDep2, by = c("Country", "mean_Labor_Inspectors_Per_10K_worker","mean_Inspections_Per_Inspector"), copy = FALSE, keep = FALSE)
      # 
      # col_order <- c("Country", "mean_Fatilities_per_100K_workers", "mean_Injuries_Per_100K_workers", "mean_Labor_Inspectors_Per_10K_worker", "mean_Inspections_Per_Inspector")
      
      #dfMean <- dfMean[, col_order]
      
      toReturn$datasetInput <- df
      toReturn$datasetInputMean <- dfMean
      toReturn$trigger <- toReturn$trigger + 1
      #browser()
    }
  })

  return(toReturn)
}

GetHealthData <- function(df, dfMean) {

  # Import Data ----
  # Data for Fatilities ----
  Fatilities <- get_ilostat(
    id = "SDG_F881_SEX_MIG_RT_A",
    segment = "indicator",
    time_format = "num",
    filters = list(
      sex = "SEX_T",
      classif1 = "MIG_STATUS_TOTAL"
    ),
    cache = FALSE
  ) %>%
    filter(str_sub(ref_area, 1, 1) != "X") %>%
    select(ref_area, obs_value, time, classif1) %>%
    left_join(Rilostat:::ilostat_ref_area_mapping %>%
      select(ref_area) %>%
      label_ilostat(code = "ref_area"),
    by = "ref_area"
    ) %>%
    rename(Fatilities_per_100K_workers = obs_value)

  # Data for Injuries ------
  Injuries <- get_ilostat(
    id = "SDG_N881_SEX_MIG_RT_A",
    segment = "indicator",
    time_format = "num",
    filters = list(
      sex = "SEX_T",
      classif1 = "MIG_STATUS_TOTAL"
    ),
    cache = FALSE
  ) %>%
    filter(str_sub(ref_area, 1, 1) != "X") %>%
    select(ref_area, obs_value, time, classif1) %>%
    left_join(Rilostat:::ilostat_ref_area_mapping %>%
      select(ref_area) %>%
      label_ilostat(code = "ref_area"),
    by = "ref_area"
    ) %>%
    rename(Injuries_Per_100K_workers = obs_value)

  # Data for Labor Inspectors per 10K worker -------
  LaborInspectors <- get_ilostat(
    id = "LAI_INDE_NOC_RT_A",
    segment = "indicator",
    time_format = "num",
    cache = FALSE
  ) %>%
    filter(str_sub(ref_area, 1, 1) != "X") %>%
    select(ref_area, obs_value, time) %>%
    left_join(Rilostat:::ilostat_ref_area_mapping %>%
      select(ref_area) %>%
      label_ilostat(code = "ref_area"),
    by = "ref_area"
    ) %>%
    rename(Labor_Inspectors_Per_10K_workers = obs_value)
  
  LaborInspectors <- LaborInspectors %>% filter(Labor_Inspectors_Per_10K_workers < 30)

  # Data for number inspections per inspector ------
  Inspections_Per_Inspector <- get_ilostat(
    id = "LAI_VDIN_NOC_RT_A",
    segment = "indicator",
    time_format = "num",
    cache = FALSE
  ) %>%
    filter(str_sub(ref_area, 1, 1) != "X") %>%
    select(ref_area, obs_value, time) %>%
    left_join(Rilostat:::ilostat_ref_area_mapping %>%
      select(ref_area) %>%
      label_ilostat(code = "ref_area"),
    by = "ref_area"
    ) %>%
    rename(Inspections_Per_Inspector = obs_value)

  # Combine the Data -----
  df <- Fatilities %>%
    dplyr::full_join(Injuries, by = c("ref_area", "time", "ref_area.label", "classif1"), copy = FALSE, keep = FALSE) %>%
    dplyr::full_join(LaborInspectors, by = c("ref_area", "time", "ref_area.label"), copy = FALSE, keep = FALSE) %>%
    dplyr::full_join(Inspections_Per_Inspector, by = c("ref_area", "time", "ref_area.label"), copy = FALSE, keep = FALSE)

  col_order <- c("ref_area", "ref_area.label", "classif1", "time", "Fatilities_per_100K_workers", "Injuries_Per_100K_workers", "Labor_Inspectors_Per_10K_workers", "Inspections_Per_Inspector")

  df <- df[, col_order]
  
  # RemoveIndependentVariableNA <- function(data, desiredCols) {
  #   completeVec <- complete.cases(data[, desiredCols])
  #   return(data[completeVec, ])
  # }
  # 
  # df <- RemoveIndependentVariableNA(df, c("Labor_Inspectors_Per_10K_workers", "Inspections_Per_Inspector"))

  df <- df %>%
    rename(
      Country = ref_area.label,
      Year = time
    ) %>%
    select(-c(ref_area, classif1)) %>%
    mutate(Year = round(Year, digits = 0))
  
  df <- df %>% drop_na()
  
  df <- df[df$Country %in% names(which(table(df$Country) > 4)), ]

  #browser()
  # Return Data -----

  return(df)
  # return(dfMean)
}
