#####
# Code for getting all the .dat files from the PC400 for leaf temperatures
# This code is by Heinz Coners
# 
rm(list=ls(all=TRUE))  #remove all objects (from other sessions)
#library(zoo)
 library(lubridate)
 library(tidyverse)
# #library(broom)
# #library(timetk)
library(plotly)
library(RColorBrewer)
 library(knitr)
# #library(ggpubr)
 library(vroom)
# #library(plantecophys)
allDat <- tibble()

############################################
#####  load data   ########################
############################################
getwd()
filepattern <- paste(glob2rx("T_leaf_*_T10min*.dat"), sep = "")
filelist <- list.files(path=paste("RawData/", sep=""), pattern=filepattern, recursive = TRUE, full.names = TRUE)

Tl <- vroom(filelist,
                    skip = 1, id = "filename", 
                    col_types = c(TIMESTAMP = "T", 
                                  .default = "n")) %>% 
  drop_na(TIMESTAMP) %>%
  rename(
    TS = TIMESTAMP,
battery = BattV_Min) %>% 
  mutate(
    species = str_split_i(filename,"/",-1),
    species = str_split_i(species,"_",3),
  ) %>% 
  select(
    -c(RECORD,
       filename)
  ) %>% 
  pivot_longer(
    cols = -c(TS, 
              species,
              battery,
              PTemp_C_Avg
              ),
    names_to = "Parameter",
    values_to = "Value"
  ) %>% 
  distinct() %>% 
  mutate(
    Parameter = str_replace(Parameter, "\\(", "_"),
    Parameter = str_replace(Parameter, "\\)", ""),
    Parameter = str_replace(Parameter, "Temp_C_", "_"),
    Tno = str_split_i(Parameter, "_", -1),
    Parameter = str_split_i(Parameter, "_", -2),
    speciesTno = paste0(species,Tno)
  ) %>% 
  filter(
    (species == "PM" & TS >= "2024-06-24 16:00") | (species == "PA") | (species == "FS"  & TS >= "2024-07-11 10:00")
  ) %>% 
  filter(
    speciesTno %in% c(
      "PA8",
      "PA9",
      "PA10",
      "PA11",
      "PA12",
      "PA16",
      "PA17",
      "PA18",
      "PA19",
      "PA20",
      "PA28",
      "PA29",
      "PA30",
      "PA31",
      "PA32",
      "PM1",
      "PM6",
      "PM7",
      "PM8",
      "PM9",
      "PM10",
      "PM11",
      "PM12",
      "PM14",
      "PM15",
      "PM16",
      "PM17",
      "PM18",
      "PM19",
      "PM20",
      "FS1",
      "FS2",
      "FS6",
      "FS7",
      "FS8",
      "FS9",
      "FS10",
      "FS11",
      "FS12",
      "FS16",
      "FS17",
      "FS18",
      "FS19",
      "FS20",
      "FS21"

    )
  )


############################################
#####  write to csv  #######################
############################################

write_csv(Tl, "RawData/Tl.csv")

############################################
##### end write to csv  ####################
############################################