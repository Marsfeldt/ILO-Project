#Import, clean, and merged data

library(tidyverse)
library(readxl)
#library(readbulk)
library(lubridate)
library(fs)
library(R.utils)
#library(zoo)
library(Rilostat)
library(plotly)

# https://ilostat.github.io/Rilostat/articles/Rilostat.html
# https://www.ilo.org/ilostat-files/Documents/ILOSTAT_BulkDownload_Guidelines.pdf

Fatilities <- get_ilostat(id = 'INJ_FATL_ECO_RT_A', 
                          segment = 'indicator', 
                          time_format = "num",
                          filters = list(classif1 = "ECO_AGGREGATE_TOTAL"), 
                          cache = FALSE) %>% 
  filter(str_sub(ref_area,1,1) != 'X') %>%
  select(ref_area, obs_value, time, classif1) %>%
  left_join(Rilostat:::ilostat_ref_area_mapping %>%
              select(ref_area) %>%
              label_ilostat(code = 'ref_area'),
            by = "ref_area") %>%
  rename(TotalFatilitiesNormP100K = obs_value)


LaborInspectorsNormP10K <- get_ilostat(id = 'LAI_INDE_NOC_RT_A', 
                              segment = 'indicator', 
                              time_format = "num",
                              #filters = list(sex = "SEX_T"), 
                              cache = FALSE) %>% 
  filter(str_sub(ref_area,1,1) != 'X') %>%
  select(ref_area, obs_value, time) %>%
  left_join(Rilostat:::ilostat_ref_area_mapping %>%
              select(ref_area) %>%
              label_ilostat(code = 'ref_area'),
            by = "ref_area") %>%
  rename(LaborInspectorsNormP10K = obs_value)

NubLaborInspections <- get_ilostat(id = 'LAI_VIST_NOC_NB_A', 
                               segment = 'indicator', 
                               time_format = "num",
                               #filters = list(sex = "SEX_T"), 
                               cache = FALSE) %>% 
  filter(str_sub(ref_area,1,1) != 'X') %>%
  select(ref_area, obs_value, time) %>%
  left_join(Rilostat:::ilostat_ref_area_mapping %>%
              select(ref_area) %>%
              label_ilostat(code = 'ref_area'),
            by = "ref_area") %>%
  rename(LaborInspections = obs_value)

InspectionsPerInspector <- get_ilostat(id = 'LAI_VDIN_NOC_RT_A', 
                                segment = 'indicator', 
                                time_format = "num",
                                #filters = list(sex = "SEX_T"), 
                                cache = FALSE) %>% 
  filter(str_sub(ref_area,1,1) != 'X') %>%
  select(ref_area, obs_value, time) %>%
  left_join(Rilostat:::ilostat_ref_area_mapping %>%
              select(ref_area) %>%
              label_ilostat(code = 'ref_area'),
            by = "ref_area") %>%
  rename(InspectionsPerInspector = obs_value)

CombinedData <- Fatilities %>%
  dplyr::full_join(LaborInspectorsNormP10K, by = NULL, copy = FALSE, keep = FALSE) %>%
  dplyr::full_join(NubLaborInspections, by = NULL, copy = FALSE, keep = FALSE) %>%
  dplyr::full_join(InspectionsPerInspector, by = NULL, copy = FALSE, keep = FALSE)

col_order <- c("ref_area", "ref_area.label", "classif1", "time", "TotalFatilitiesNormP100K", "LaborInspectorsNormP10K", "InspectionsPerInspector", "LaborInspections")

CombinedData <- CombinedData[, col_order]

CombinedDataNoNA <- CombinedData %>% drop_na()

summary(lm(TotalFatilitiesNormP100K ~ LaborInspectorsNormP10K * InspectionsPerInspector, data = CombinedDataNoNA))

CombinedDataNoNA %>% ggplot(aes(x = TotalFatilitiesNormP100K, 
               y = InspectionsPerInspector,
               color = factor(time)),
               alpha(0.5),
) + 
  geom_point() +
  geom_smooth(method=lm,
              se = T) +
  facet_wrap(~ref_area.label) +
  theme_bw()

skimmedDown <- CombinedDataNoNA[CombinedDataNoNA$ref_area %in% names(which(table(CombinedDataNoNA$ref_area) > 4)), ]

# ----------------------------


tocAll <- get_ilostat_toc(lang = 'en')
toc <- get_ilostat_toc(segment = 'ref_area', lang = 'en')
tocAll %>% separate(size,c("size", "unit")," ") %>% 
  mutate(size=as.numeric(size)) %>% 
  group_by(unit) %>% 
  summarize(sum(size))

AllData <- map(tocAll, ~ get_ilostat(id = id, segment = 'indicator') )


toc2 <- get_ilostat_toc(search = 'bargaining')

dat <- get_ilostat(id = 'EIP_2EET_SEX_RT_A', segment = 'indicator') 
dat2 <- get_ilostat(id = 'ARM_A', segment = 'ref_area') 


dat <- get_ilostat(id = c('AFG_A', 'TTO_A'), segment = 'ref_area') 
dplyr::count(dat, ref_area)


# ----------- Vizulize test ---------------









X <- get_ilostat(id = 'EIP_2EET_SEX_RT_A', 
                 segment = 'indicator', 
                 filters = list(time = '2018', sex = 'T'), 
                 cache = FALSE) %>% 
  filter(str_sub(ref_area,1,1) != 'X') %>%
  select(ref_area, obs_value) %>%
  left_join(Rilostat:::ilostat_ref_area_mapping %>%
              select(ref_area, ref_area_plotly) %>%
              label_ilostat(code = 'ref_area'),
            by = "ref_area") %>%
  filter(!obs_value %in% NA) %>%
  mutate(tot_obs_value = cut(obs_value, 
                             quantile(obs_value, na.rm = TRUE), 
                             include.lowest = TRUE))

summary(X)

X %>%   plot_geo( width = 900, height = 600) %>%
  add_trace(
    z = ~obs_value, 
    color = ~obs_value, 
    colors=c("green", "blue"), 
    text = ~ref_area.label, 
    locations = ~ref_area_plotly, 
    marker = list(line = list(color = toRGB("grey"), width = 0.5)),
    showscale = TRUE) %>%
  colorbar(title = '(%)', len = 0.5, ticksuffix="%") %>%
  layout(
    title = list(   text = "Share of youth not in employment, education or training (NEET) in 2018",
                    font = list(size = 18)),
    font = (size = 1),
    geo = list( showframe = TRUE,
                showcoastlines = TRUE,
                projection = list(type = 'Mercator'),
                showCountries = TRUE,
                resolution = 110), 
    annotations = 
      list(   x = 1, y = 1,
              text = "Source: ilostat", 
              showarrow = F, xref='paper', yref='paper', 
              xanchor='right', yanchor='auto', xshift=0, yshift=0,
              font=list(size=15, color="blue"))
  )


Fatilities <- get_ilostat(id = 'INJ_FATL_ECO_RT_A', 
                              segment = 'indicator', 
                              time_format = "num",
                              filters = list(classif1 = "ECO_AGGREGATE_TOTAL"), 
                              cache = FALSE) %>% 
  filter(str_sub(ref_area,1,1) != 'X') %>%
  select(ref_area, obs_value, time, classif1) %>%
  left_join(Rilostat:::ilostat_ref_area_mapping %>%
              select(ref_area) %>%
              label_ilostat(code = 'ref_area'),
            by = "ref_area") %>%
  rename(TotalFatilitiesNormP100K = obs_value)


FatilitiesTime <- get_ilostat(id = 'INJ_FATL_ECO_RT_A', 
                              segment = 'indicator', 
                              time_format = "num",
                              filters = list(classif1 = "ECO_AGGREGATE_TOTAL"), 
                              cache = FALSE) %>% 
  filter(str_sub(ref_area,1,1) != 'X') %>%
  select(ref_area, obs_value, time, classif1) %>%
  left_join(Rilostat:::ilostat_ref_area_mapping %>%
              select(ref_area) %>%
              label_ilostat(code = 'ref_area'),
            by = "ref_area") %>%
  rename(TotalFatilitiesNormP100K = obs_value)


summary(lm(obs_value ~ time, data = FatilitiesTime))

FatilitiesTime %>% ggplot(aes(x = time, 
                              y = obs_value),
                          ) + 
  geom_point() +
  geom_smooth(method=lm,
              se = F) +
  theme_bw()

# ----------- Import: All Data -------------

# make varible with the path to files that needs to be imported 
paths <- "./ILO Data/" 

# Create a vector with the path to all the .gz files - ignore other data types
pathsGz <- dir_ls(paths, glob = "*.gz")

# Unpack the .gz files. Overwrite exsisting files (making it easy to update data later), delete the .gz file to save space
map(pathsGz, ~ gunzip(.x, overwrite = TRUE))

# Create a vector with the path to all the .csv files - ignore other data types
ILO_file_names <- paths %>% 
  list.files(pattern = "\\.csv$") 

# save all the data-sets in one big list
ILO_Data_List <- ILO_file_names %>%
  purrr::map(function(file_name){ # iterate through each file name
    read_csv(paste0(paths, file_name))
  })

table(map_chr(ILO_Data_List, ~ read_lines(.x, n_max = 1)))

# ----------- Import and merge: country, indicators and classifier labels -------------

# Labels found here: https://www.ilo.org/ilostat-files/WEB_bulk_download/html/bulk_dic.html

# path to the label files
pathToLabels <- "./ILO Labels/"

# Create a vector with the path to all the .csv files - ignore other data types
ILO_file_Labels <- pathToLabels %>% 
  list.files(pattern = "\\.csv$") 

# save all the label data-sets in one big list
ILO_Label_List <- ILO_file_Labels %>%
  purrr::map(function(file_name){ # iterate through each file name
    read_csv(paste0(pathToLabels, file_name))
  })  

# Remove unnecessary sorting column 
ILO_Label_List <- ILO_Label_List %>% 
  walk( ~ select(.x, -3))

# Merge labes with the data files



# ----------- Make sublists -------------


MigrantsDataList <- ILO_Data_List %>%
  

unname(map_chr())

# ---------------------------------------------------------------------------------------------------

# ----------- Import: Inspector Data -------------

Inspector_file_path <- "./Data/Statistics on safety and health at work/CleandedData/Inspectors/"

Inspector_file_path %>%
  list.files() %>%
  .[str_detect(., ".csv")] -> csv_Inspectorfile_names

csv_Inspectorfile_names %>%
  purrr::map(function(file_name){ # iterate through each file name
    read_csv2(paste0(Inspector_file_path, file_name))
  }) -> InspectorDataList

csv_Inspectorfile_names %>%
  purrr::map_df(function(file_name) # iterate through each file name
    read_csv2(paste0(Inspector_file_path, file_name))
    %>% mutate(filename=gsub(".csv","",basename(x)))) -> InspectorDataList2

map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>% mutate(filename=gsub(".csv","",basename(x))))


InspectorDataList %>% # https://stackoverflow.com/questions/53800737/how-to-add-corresponding-filename-as-a-new-column-using-tidyverse
  map2(csv_Inspectorfile_names, ~ map(.x[lengths(.x) > 0], cbind, userid = .y)) %>%
  transpose(.names = csv_Inspectorfile_names) %>% 
  map(dplyr::bind_rows)





InspectorDataList %>% #https://stackoverflow.com/questions/46616591/rename-multiple-dataframe-columns-using-purrr
  

# ----------- Import: Migrants Data -------------

Migrants_file_path <- "./Data/Statistics on safety and health at work/CleandedData/Migrants/"

Migrants_file_path %>%
  list.files() %>%
  .[str_detect(., ".csv")] -> csv_Migrantsfile_names

csv_Migrantsfile_names %>%
  purrr::map(function(file_name){ # iterate through each file name
    read_csv2(paste0(Migrants_file_path, file_name))
  }) -> MigrantsDataList


# ----------- Import: Occupation Data -------------

Occupation_file_path <- "./Data/Statistics on safety and health at work/CleandedData/Occupation/"

Occupation_file_path %>%
  list.files() %>%
  .[str_detect(., ".csv")] -> csv_Occupationfile_names

csv_Occupationfile_names %>%
  purrr::map(function(file_name){ # iterate through each file name
    read_csv2(paste0(Occupation_file_path, file_name))
  }) -> OccupationDataList

## ----------- Number of labour inspectors by sex (thousands) ---------

# Labour inspectors are public officials or other authorities who are responsible for three key labour inspection activities: a) securing the enforcement of the legal provisions relating to conditions of work and the protection of workers while engaged in their work, such as provisions relating to hours, wages, safety, health and welfare, the employment of children and young persons, and other connected matters, in so far as such provisions are enforceable by labour inspectors; b) supplying technical information and advice to employers and workers concerning the most effective means of complying with the legal provisions; c) bringing to the notice of the competent authority defects or abuses not specifically covered by existing legal provisions. Labour inspectors have the authority to initiate processes that may lead to legal action. For more information, refer to the concepts and definitions page.

LabourInspectorsSexThousands <- read.csv("./Data/Statistics on safety and health at work/CleandedData/Inspectors/LAI_INSP_SEX_NB_A_EN.csv", sep = ";", dec=",", header = TRUE)

LabourInspectorsSexThousands <- dplyr::rename(LabourInspectorsSexThousands, NumberInspectorsThousands = Total)

InspectorData <- LabourInspectorsSexThousands

## ----------- Inspectors per 10'000 employed persons ---------

# This indicator conveys the average number of labour inspectors per 10,000 employed persons, which provides some 
# indication of the resources available for monitoring and enforncing appropriate work conditions and the 
# corresponding standards. Labour inspectors are public officials or other authorities who are responsible for three 
# key labour inspection activities: a) securing the enforcement of the legal provisions relating to conditions of 
# work and the protection of workers while engaged in their work, such as provisions relating to hours, wages, 
# safety, health and welfare, the employment of children and young persons, and other connected matters, in so far 
# as such provisions are enforceable by labour inspectors; b) supplying technical information and advice to 
# employers and workers concerning the most effective means of complying with the legal provisions; c) bringing to 
# the notice of the competent authority defects or abuses not specifically covered by existing legal provisions. 
# Labour inspectors have the authority to initiate processes that may lead to legal action. For more information, 
# refer to the concepts and definitions page.

InspectorsPer10kEmployed <- read.csv("./Data/Statistics on safety and health at work/CleandedData/Inspectors/LAI_INDE_NOC_RT_A_EN.csv", 
               sep = ";", dec=",", header = TRUE)

InspectorsPer10kEmployed <- dplyr::rename(InspectorsPer10kEmployed, InspectorsPer10kEmployed = Value)

InspectorData <- dplyr::full_join(LabourInspectorsSexThousands, InspectorsPer10kEmployed, 
                                  by = NULL, copy = FALSE, 
                                  suffix = c(".LabourInspectorsSexThousands", ".InspectorsPer10kEmployed"), keep = FALSE)

## ----------- Labour inspection visits per inspector ---------

# This indicator conveys the average number of labour inspection visits conducted by a labour inspector during the 
# year. Labour inspectors are public officials or other authorities who are responsible for three key labour 
# inspection activities: a) securing the enforcement of the legal provisions relating to conditions of work and the 
# protection of workers while engaged in their work, such as provisions relating to hours, wages, safety, health and 
# welfare, the employment of children and young persons, and other connected matters, in so far as such provisions 
# are enforceable by labour inspectors; b) supplying technical information and advice to employers and workers 
# concerning the most effective means of complying with the legal provisions; c) bringing to the notice of the 
# competent authority defects or abuses not specifically covered by existing legal provisions. Labour inspectors 
# have the authority to initiate processes that may lead to legal action. Labour inspection visits refer to the 
# physical presence of a labour inspector in a work place for the purpose of carrying out a labour inspection and 
# which is duly documented as required by national legislation. For more information, refer to the concepts and 
# definitions page.

AvgVisitsPerInspectorPerYear <- read.csv("./Data/Statistics on safety and health at work/CleandedData/Inspectors/LAI_VDIN_NOC_RT_A_EN.csv", 
                                         sep = ";", dec=",", header = TRUE)

AvgVisitsPerInspectorPerYear <- dplyr::rename(AvgVisitsPerInspectorPerYear, AvgVisitsPerInspectorPerYear = Value)

InspectorData <- dplyr::full_join(InspectorData, AvgVisitsPerInspectorPerYear, 
                                  by = NULL, copy = FALSE, 
                                  suffix = c("InspectorData", "AvgVisitsPerInspectorPerYear"), keep = FALSE)

## ----------- Number of labour inspection visits to workplaces during the year ---------

# Labour inspection visits refer to the physical presence of a labour inspector in a workplace for the purpose of 
# carrying out a labour inspection and which is duly documented as required by national legislation. For more 
# information, refer to the concepts and definitions page.

NumberLabourInspectionVisitsPerYear <- read.csv("./Data/Statistics on safety and health at work/CleandedData/Inspectors/LAI_VIST_NOC_NB_A_EN.csv", 
                                         sep = ";", dec=",", header = TRUE)

NumberLabourInspectionVisitsPerYear <- dplyr::rename(NumberLabourInspectionVisitsPerYear, NumberLabourInspectionVisitsPerYear = Value)

InspectorData <- dplyr::full_join(InspectorData, NumberLabourInspectionVisitsPerYear, 
                                  by = NULL, copy = FALSE, 
                                  suffix = c("InspectorData", "NumberLabourInspectionVisitsPerYear"), keep = FALSE)

## ----------- Registered workplaces that could be selected for labour inspection ---------

# A workplace can be defined as any physical space, whether a physical construction (such as a building or set of 
# buildings) or not, in which at least one employed person carries out their work activities. Only those workplaces 
# that are registered and could potentially be selected for labour inspection are included in the total number. For 
# more information, refer to the concepts and definitions page.

RegisteredWorkplacesForLabourInspection <- read.csv("./Data/Statistics on safety and health at work/CleandedData/Inspectors/LAI_WOPL_NOC_NB_A_EN.csv", 
                                                sep = ";", dec=",", header = TRUE)

RegisteredWorkplacesForLabourInspection <- dplyr::rename(RegisteredWorkplacesForLabourInspection, RegisteredWorkplacesForLabourInspection = Value)

InspectorData <- dplyr::full_join(InspectorData, RegisteredWorkplacesForLabourInspection, 
                                  by = NULL, copy = FALSE, 
                                  suffix = c("InspectorData", "RegisteredWorkplacesForLabourInspection"), keep = FALSE)

# ----------- InspectorData Combined -------------

InspectorData <- dplyr::rename(InspectorData, Country = ï..Reference.area)
InspectorData <- dplyr::rename(InspectorData, SourceType = Source.type)

# ----------- Import: Occupation Data -------------
## ----------- Days lost due to cases of occupational injury with temporary incapacity for work by economic activity ---------

# Days lost due to temporary incapacity refers to the total number of calendar days during which those persons 
# temporarily incapacitated were unable to work, excluding the day of the accident, up to a maximum of one year. 
# Temporary absences from work of less than one day for medical treatment are not included. Data are disaggregated 
# by economic activity according to the latest version of the International Standard Industrial Classification of 
# All Economic Activities (ISIC) available for that year. For more information, refer to the concepts and definitions 
# page.

TotalDaysLostDueToInjury <- read.csv("./Data/Statistics on safety and health at work/CleandedData/Occupation/INJ_DAYS_ECO_NB_A_EN.csv", 
                             sep = ";", header = TRUE)

TotalDaysLostDueToInjury['Classifier'] = 'TotalDaysLost'

TotalDaysLostDueToInjury <- TotalDaysLostDueToInjury %>% relocate(Classifier, .before = Total)

## ----------- Cases of fatal occupational injury by economic activity --------

# A case of occupational injury is the case of a worker incurring an occupational injury as a result of an 
# occupational accident. An occupational injury that is fatal is the result of an occupational accident where 
# death occurred within one year from the day of the accident. Data are disaggregated by economic activity 
# according to the latest version of the International Standard Industrial Classification of All Economic 
# Activities (ISIC) available for that year. For more information, refer to the concepts and definitions page.

FatalInjury <- read.csv("./Data/Statistics on safety and health at work/CleandedData/Occupation/INJ_FATL_ECO_NB_A_EN.csv", 
                                     sep = ";", header = TRUE)

FatalInjury['Classifier'] = 'FatalInjury'

FatalInjury <- FatalInjury %>% relocate(Classifier, .before = Total)

OccupationData <- dplyr::full_join(TotalDaysLostDueToInjury, FatalInjury, 
                                  by = NULL, copy = FALSE, 
                                  suffix = c("TotalDaysLostDueToInjury", "FatalInjury"), keep = FALSE)

## ----------- Fatal occupational injuries per 100'000 workers by economic activity --------

# The incidence rate is the average number of new cases of fatal occupational injury during the calendar year per 
# 100,000 workers in the reference group. Data are presented disaggregated by sex and economic activity, according 
# to the latest version available of the International Standard Industrial Classification of all Economic Activities 
# (ISIC). For more information, refer to the concepts and definitions page.

FatalInjuryPer100k <- read.csv("./Data/Statistics on safety and health at work/CleandedData/Occupation/INJ_FATL_ECO_RT_A_EN.csv", 
                        sep = ";", dec=",", header = TRUE)

FatalInjuryPer100k['Classifier'] = 'FatalInjuryPer100k'

FatalInjuryPer100k <- FatalInjuryPer100k %>% relocate(Classifier, .before = Total)

OccupationData <- dplyr::full_join(OccupationData, FatalInjuryPer100k, 
                                   by = NULL, copy = FALSE, 
                                   suffix = c("OccupationData", "FatalInjuryPer100k"), keep = FALSE)

## ----------- Cases of non-fatal occupational injury by economic activity -------

# A case of non-fatal occupational injury is the case of a worker incurring an occupational injury as a result of 
# an occupational accident not leading to death. The non-fatal occupational injury entails a loss of working time. 
# Data are disaggregated by economic activity according to the latest version of the International Standard 
# Industrial Classification of All Economic Activities (ISIC) available for that year. For more information, refer 
# to the concepts and definitions page.

NonFatalInjury <- read.csv("./Data/Statistics on safety and health at work/CleandedData/Occupation/INJ_NFTL_ECO_NB_A_EN.csv", 
                               sep = ";", dec=",", header = TRUE)

NonFatalInjury['Classifier'] = 'NonFatalInjury'

NonFatalInjury <- NonFatalInjury %>% relocate(Classifier, .before = Total)

OccupationData <- dplyr::full_join(OccupationData, NonFatalInjury, 
                                   by = NULL, copy = FALSE, 
                                   suffix = c("OccupationData", "NonFatalInjury"), keep = FALSE)

## ----------- Non-fatal occupational injuries per 100'000 workers by economic activity -------

# The incidence rate is the average number of new cases of non-fatal occupational injury during the calendar year 
# per 100,000 workers in the reference group. Data are presented disaggregated by sex and economic activity, 
# according to the latest version available of the International Standard Industrial Classification of all Economic 
# Activities (ISIC). For more information, refer to the concepts and definitions page.

NonFatalInjuryPer100k <- read.csv("./Data/Statistics on safety and health at work/CleandedData/Occupation/INJ_NFTL_ECO_RT_A_EN.csv", 
                           sep = ";", dec=",", header = TRUE)

NonFatalInjuryPer100k['Classifier'] = 'NonFatalInjuryPer100k'

NonFatalInjuryPer100k <- NonFatalInjuryPer100k %>% relocate(Classifier, .before = Total)

OccupationData <- dplyr::full_join(OccupationData, NonFatalInjuryPer100k, 
                                   by = NULL, copy = FALSE, 
                                   suffix = c("OccupationData", "NonFatalInjuryPer100k"), keep = FALSE)

# ----------- OccupationData Combined ------------

OccupationData <- dplyr::rename(OccupationData, Country = ï..Reference.area)
OccupationData <- dplyr::rename(OccupationData, SourceType = Source.type)

#OccupationData[with(OccupationData, order(Country)),]

# ----------- Import: Migrants Data -------------

## ----------- Days lost due to cases of occupational injury with temporary incapacity for work by sex and migrant status -----------

# Days lost due to temporary incapacity refers to the total number of calendar days during which those persons 
# temporarily incapacitated were unable to work, excluding the day of the accident, up to a maximum of one year. 
# Temporary absences from work of less than one day for medical treatment are not included. For more information, 
# refer to the concepts and definitions page.

MigrantsDaysLostDueInjury <- read.csv("./Data/Statistics on safety and health at work/CleandedData/Migrants/INJ_DAYS_SEX_MIG_NB_A_EN.csv", 
                               sep = ";", dec=",", header = TRUE)

MigrantsDaysLostDueInjury['Classifier'] = 'MigrantsDaysLost'

MigrantsDaysLostDueInjury <- MigrantsDaysLostDueInjury %>% relocate(Classifier, .before = Total)

MigrantsData <- MigrantsDaysLostDueInjury

## ----------- Cases of fatal occupational injury by sex and migrant status -------------
 
# A case of occupational injury is the case of a worker incurring an occupational injury as a result of an occupational 
# accident. An occupational injury that is fatal is the result of an occupational accident where death occurred within 
# one year from the day of the accident. For more information, refer to the concepts and definitions page.

MigrantsFatalInjury <- read.csv("./Data/Statistics on safety and health at work/CleandedData/Migrants/INJ_FATL_SEX_MIG_NB_A_EN.csv", 
                                      sep = ";", dec=",", header = TRUE)

MigrantsFatalInjury['Classifier'] = 'MigrantsFatalInjury'

MigrantsFatalInjury <- MigrantsFatalInjury %>% relocate(Classifier, .before = Total)

MigrantsData <- dplyr::full_join(MigrantsData, MigrantsFatalInjury, 
                                   by = NULL, copy = FALSE, 
                                   suffix = c("MigrantsData", "MigrantsFatalInjury"), keep = FALSE)

## ----------- Fatal occupational injuries per 100'000 workers by sex and migrant status -------------
 
# The incidence rate is the average number of new cases of fatal occupational injury during the calendar year per 
# 100,000 workers in the reference group. For more information, refer to the concepts and definitions page.

MigrantsFatalPer100k <- read.csv("./Data/Statistics on safety and health at work/CleandedData/Migrants/INJ_FATL_SEX_MIG_RT_A_EN.csv", 
                                sep = ";", dec=",", header = TRUE)

MigrantsFatalPer100k['Classifier'] = 'MigrantsFatalPer100k'

MigrantsFatalPer100k <- MigrantsFatalPer100k %>% relocate(Classifier, .before = Total)

MigrantsData <- dplyr::full_join(MigrantsData, MigrantsFatalPer100k, 
                                 by = NULL, copy = FALSE, 
                                 suffix = c("MigrantsData", "MigrantsFatalPer100k"), keep = FALSE)

## ----------- Cases of non-fatal occupational injury by sex, type of incapacity and migrant status -------------

# A case of non-fatal occupational injury is the case of a worker incurring a non-fatal occupational injury as a 
# result of an occupational accident, which entailed a loss of working time. Incapacity for work is the inability of 
# the victim of an occupational accident, due to an occupational injury, to perform the normal duties of work in the 
# job or post occupied at the time of the occupational accident. The incapacity for work can be permanent, when the 
# persons injured were never able to perform again the normal duties of work in the job or post occupied at the time 
# of the occupational accident causing the injury, or temporary, when the workers injured were unable to work from
# the day after the day of the accident, but were later able to perform again the normal duties of work in the job or
# post occupied at the time of the occupational accident causing the injury within a period of one year from the day
# of the accident. For more information, refer to the concepts and definitions page.

MigrantsLevelOfIncapacity <- read.csv("./Data/Statistics on safety and health at work/CleandedData/Migrants/INJ_NFTL_SEX_INJ_MIG_NB_A_EN.csv", sep = ";", dec=",", header = TRUE)

MigrantsLevelOfIncapacity['Classifier'] = 'MigrantsLevelOfIncapacity'

MigrantsLevelOfIncapacity <- MigrantsLevelOfIncapacity %>% relocate(Classifier, .before = Total)

MigrantsData <- dplyr::full_join(MigrantsData, MigrantsLevelOfIncapacity, 
                                 by = NULL, copy = FALSE, 
                                 suffix = c("MigrantsData", "MigrantsLevelOfIncapacity"), keep = FALSE)


## ----------- Cases of non-fatal occupational injury by sex and migrant status -------------

# A case of non-fatal occupational injury is the case of a worker incurring an occupational injury as a result of an occupational accident not leading to death. The non-fatal occupational injury entails a loss of working time. For more information, refer to the concepts and definitions page.

MigrantsNonFatalInjury <- read.csv("./Data/Statistics on safety and health at work/CleandedData/Migrants/INJ_NFTL_SEX_MIG_NB_A_EN.csv", sep = ";", dec=",", header = TRUE)

MigrantsNonFatalInjury['Classifier'] = 'MigrantsNonFatalInjury'

MigrantsNonFatalInjury <- MigrantsNonFatalInjury %>% relocate(Classifier, .before = Total)

MigrantsData <- dplyr::full_join(MigrantsData, MigrantsNonFatalInjury, 
                                 by = NULL, copy = FALSE, 
                                 suffix = c("MigrantsData", "MigrantsNonFatalInjury"), keep = FALSE)


## ----------- Non-fatal occupational injuries per 100'000 workers by sex and migrant status -------------

# The incidence rate is the average number of new cases of non-fatal occupational injury during the calendar year per 100,000 workers in the reference group. For more information, refer to the concepts and definitions page.

MigrantsNonFatalInjuryPer100k <- read.csv("./Data/Statistics on safety and health at work/CleandedData/Migrants/INJ_NFTL_SEX_MIG_RT_A_EN.csv", sep = ";", dec=",", header = TRUE)

MigrantsNonFatalInjuryPer100k['Classifier'] = 'MigrantsNonFatalInjuryPer100k'

MigrantsNonFatalInjuryPer100k <- MigrantsNonFatalInjuryPer100k %>% relocate(Classifier, .before = Total)

MigrantsData <- dplyr::full_join(MigrantsData, MigrantsNonFatalInjuryPer100k, 
                                 by = NULL, copy = FALSE, 
                                 suffix = c("MigrantsData", "MigrantsNonFatalInjuryPer100k"), keep = FALSE)

# ----------- MigrantsData Combined ------------

MigrantsData <- dplyr::rename(MigrantsData, Country = ï..Reference.area)
MigrantsData <- dplyr::rename(MigrantsData, SourceType = Source.type)

# ----------- Save Complete Dataframe ------------

ILOSafetyAndHealth <- 

save(ILOSafetyAndHealth, file='ILOSafetyAndHealth.rda', compress=TRUE)