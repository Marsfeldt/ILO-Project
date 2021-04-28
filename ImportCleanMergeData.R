#Import, clean, and merged data

library(tidyverse)
library(readxl)
#library(readbulk)
library(lubridate)
#library(zoo)

# ----------- Import: Inspector Data -------------

Inspector_file_path <- "./Data/Statistics on safety and health at work/CleandedData/Inspectors/"

Inspector_file_path %>%
  list.files() %>%
  .[str_detect(., ".csv")] -> csv_file_names

csv_file_names %>%
  purrr::map(function(file_name){ # iterate through each file name
    read_csv(paste0(Inspector_file_path, file_name))
  }) -> InspectorDataList

InspectorDataList %>% 
  map_dfr(read_csv) %>%
  mutate(sep = ";", dec=",", header = TRUE)

## ----------- Number of labour inspectors by sex (thousands) ---------

# Labour inspectors are public officials or other authorities who are responsible for three key labour inspection activities: a) securing the enforcement of the legal provisions relating to conditions of work and the protection of workers while engaged in their work, such as provisions relating to hours, wages, safety, health and welfare, the employment of children and young persons, and other connected matters, in so far as such provisions are enforceable by labour inspectors; b) supplying technical information and advice to employers and workers concerning the most effective means of complying with the legal provisions; c) bringing to the notice of the competent authority defects or abuses not specifically covered by existing legal provisions. Labour inspectors have the authority to initiate processes that may lead to legal action. For more information, refer to the concepts and definitions page.

LabourInspectorsSexThousands <- read.csv("./Data/Statistics on safety and health at work/CleandedData/Inspectors/LAI_INSP_SEX_NB_A_EN.csv", 
                                         sep = ";", dec=",", header = TRUE)

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