## Setup ##
library(DBI)
library(data.table)
library(dplyr)
library(reshape)
library(ROracle)
library(dbplyr)
library(stringr)
library(tidyverse)

source("Oracle_Connection.R") 
options(scipen = 999)

###############################################################################
################################# Survey Data #################################

#read in the cruise data from all cruises
surveys <- tbl(nefscdb1,in_schema("SVDBS", "MSTR_CRUISE")) %>%  
  filter(PURPOSE_CODE==60,
         STATUS_CODE==10)  %>% #filter to scallop dredges
  select(CRUISE6, CRUISE, SVVESSEL, YEAR, PURPOSE_CODE, 
         STATUS_CODE, STARTDATE, ENDDATE, VESSEL_NUM) %>%  #select relevant data
  collect() # Pull cruise data

cruises <- unique(surveys$CRUISE6)

################################# Length Data #################################

# read in the length data
length <- tbl(nefscdb1, in_schema("SVDBS", "UNION_FSCS_SVLEN")) %>% 
  filter(CRUISE6 %in% cruises) %>% 
  filter(SVSPP=="401") %>% #filter to live sea scallops
  select(CRUISE6, STATION, TOW, SVSPP,  #select relevant fields
         LOGGED_SPECIES_NAME, LENGTH, EXPNUMLEN) %>% 
  collect() %>%  # loads 419,974 records
  mutate(CRUISE6 = as.numeric(CRUISE6))

################################ Station Data #################################

station <- tbl(nefscdb1, in_schema("SVDBS", "UNION_FSCS_SVSTA")) %>% 
  select(CRUISE6, CRUISE, SVVESSEL, STATUS_CODE, STATION, TOW, STRATUM, ID, 
         BEGIN_EST_TOWDATE, END_EST_TOWDATE, DECDEG_BEGLAT, DECDEG_BEGLON, 
         DECDEG_ENDLAT, DECDEG_ENDLON, EST_YEAR, STATYPE, SHG, HAUL, GEARCOND) %>% 
  filter(CRUISE6 %in% cruises,
         HAUL %in% c(1:3),  # see HAUL_VALUE table for code defs
         GEARCOND %in% c(1:6), #see GEAR_CONDITION table for code defs
         STATYPE %in% c(1:2)
  ) %>% 
  collect() %>% 
  mutate(CRUISE6 = as.numeric(CRUISE6),
         Station_Type = case_when(STATYPE == 1 ~ "Random Stations",
                                  STATYPE == 2 ~ "Fixed Stations",
                                  TRUE ~ NA))

############################## Biological Data ################################

biological <- tbl(nefscdb1, in_schema("FSCS2", "NEFSC_ORGANISM_VALUES"))  %>% 
  # filter(CRUISE_ID %in% cruises) %>% 
  filter(CENTER_SPECIES_CODE=="401") %>% #filter to live sea scallops
  select(CRUISE_ID, OPERATION_ID, STATION, SPECIES, CONTAINER_SEX, CENTER_SPECIES_CODE,
         ITIS_CODE, SPECIES_GROUP_ID, ACTION_NAME, ORGANISM_ACTION_ID, PARAM_NAME,
         PARAM, VALUE, ROUND_VALUE, UNITS, SAMPLE_ID, SAMPLE_CONTAINER_ID,
         ORGANISM_ID, ORGANISM_PARAMETER_ID, ORGANISM_PARAM_VALUE_ID,
         EXCEPTION_OVERRIDE_FLAG, SAMPLING_LOCATION, SAMPLE_COMMENTS, CONTAINER_COMMENTS,
         ORGANISM_COMMENTS, DATE_TIME, PROTOCOL_RULE_CONTENT_ID, INVESTIGATOR_ID,
         FIRST_NAME, LAST_NAME, ORGANIZATION) %>% 
  collect() # loads 16,873 records

biological <- biological %>%
  filter(
    ACTION_NAME %in% c(
      "Record Length cm",
      "SHELL HEIGHT",
      "Shell Height",
      "Record Length mm",
      "Record Sex",
      "Scallop Meat Weight",
      "Select Organisms Present",
      "Shell Width",
      "Commensal Organisms Present",
      "NR Commensal Organisms Present",
      "NR Disease Present",
      "Disease Present",
      "Gonad Weight",
      "Select All Diseases",
      "NR Gonad Weight",
      "NR Record Sex",
      "NR Scallop Meat Weight",
      "NR Shell Width",
      "NR Scallop Whole Weight",
      "Scallop Whole Weight",
      "NR Select All Diseases",
      "NR Select Organisms Present"
    )
  )  %>%
  mutate(
    ACTION_NAME = case_when(
      ACTION_NAME %in% c(
        "Record Length cm",
        "SHELL HEIGHT",
        "Shell Height",
        "Record Length mm"
      ) ~ "Shell_Length",
      ACTION_NAME %in% c("Record Sex", "NR Record Sex") ~ "Sex",
      ACTION_NAME %in% c("Scallop Meat Weight", "NR Scallop Meat Weight") ~ "Meat_Weight",
      ACTION_NAME %in% c(
        "Select Organisms Present",
        "Commensal Organisms Present",
        "NR Commensal Organisms Present",
        "NR Select Organisms Present"
      ) ~ "Organisms",
      ACTION_NAME %in% c("Shell Width", "NR Shell Width") ~ "Shell_Width",
      ACTION_NAME %in% c(
        "NR Disease Present",
        "Disease Present",
        "Select All Diseases",
        "NR Select All Diseases"
      ) ~ "Disease",
      ACTION_NAME %in% c("Gonad Weight", "NR Gonad Weight") ~ "Gonad_Weight",
      ACTION_NAME %in% c("NR Scallop Whole Weight", "Scallop Whole Weight") ~ "Whole_Weight",
      TRUE ~ ACTION_NAME
    ),
    CRUISE_ID = as.character(CRUISE_ID)
  ) %>%
  filter(CRUISE_ID %in% cruises) 

################################# Catch Data ##################################

catch <- tbl(nefscdb1, in_schema("SVDBS", "UNION_FSCS_SVCAT"))  %>% 
  filter(CRUISE6 %in% cruises) %>% 
  filter(SVSPP=="401") %>% #filter to live sea scallops
  select(CRUISE6, STATION, TOW, SVSPP,  #select relevant fields
         CATCHSEX, EXPCATCHNUM, EXPCATCHWT, LOGGED_SPECIES_NAME) %>% 
  collect() # loads 16,288 records

################################# Vessel Data ##################################

vessels <- tbl(nefscdb1, in_schema("SVDBS", "SV_VESSEL")) %>% 
  select(VESSEL_NAME, VESSEL_ABBREV) %>% 
  collect()

###############################################################################

station <- station %>% 
  left_join(vessels, by = c("SVVESSEL" = "VESSEL_ABBREV")) %>% 
  mutate(VESSEL_NAME = str_to_title(VESSEL_NAME),
         VESSEL_NAME = str_replace_all(VESSEL_NAME, "\\b(I[vx]|V|X|I{1,3})\\b", toupper))

catch_join <- catch %>% 
  mutate(CRUISE6 = as.numeric(CRUISE6)) %>% 
  inner_join(station) %>% 
  mutate(EST_YEAR = as.integer(EST_YEAR),
         REGION = if_else(DECDEG_BEGLON < -71.314566 & DECDEG_BEGLAT < 40.813715, 
                          "Mid-Atlantic", "New England")) 
length_join <- length %>% 
  inner_join(station) %>% 
  mutate(Size_Cat = case_when(LENGTH <= 3.5 ~ "≤ 3.5cm",
                              LENGTH > 3.5 & LENGTH <= 7.5 ~ "3.5cm - 7.5cm",
                              LENGTH > 7.5 & LENGTH <= 8.9 ~ "7.5cm - 8.9cm",
                              LENGTH > 8.9 & LENGTH <= 10 ~ "8.9cm - 10cm",
                              LENGTH >= 10 ~ "≥ 10cm",
                              TRUE ~ NA),
         EST_YEAR = as.integer(EST_YEAR),
         REGION = if_else(DECDEG_BEGLON < -71.314566 & DECDEG_BEGLAT < 40.813715, 
                          "Mid-Atlantic", "New England"))

biological_join <- biological %>%
  distinct(CRUISE_ID, STATION, ORGANISM_ID, ACTION_NAME, .keep_all = TRUE) %>% 
  pivot_wider(
    id_cols = c(CRUISE_ID, STATION, ORGANISM_ID),
    names_from = ACTION_NAME, 
    values_from = VALUE
  ) %>% 
  filter(
    !is.na(Whole_Weight),
    !is.na(Meat_Weight)) %>% 
  mutate(
    Shell_Length = as.numeric(Shell_Length),
    Meat_Weight = as.numeric(Meat_Weight),
    Shell_Width = as.numeric(Shell_Width),
    Gonad_Weight = as.numeric(Gonad_Weight),
    Whole_Weight = as.numeric(Whole_Weight),
    Sex = as.character(Sex),
    Organisms = as.character(Organisms),
    Disease = as.character(Disease),
    CRUISE_ID = as.numeric(CRUISE_ID),
    STATION = str_pad(STATION, width = 4, side = "left", pad = "0")
  ) %>% 
  mutate(LENGTH_CM = Shell_Length/10,
         Size_Cat = case_when(LENGTH_CM <= 3.5 ~ "≤ 3.5cm",
                              LENGTH_CM > 3.5 & LENGTH_CM <= 7.5 ~ "3.5cm - 7.5cm",
                              LENGTH_CM > 7.5 & LENGTH_CM <= 8.9 ~ "7.5cm - 8.9cm",
                              LENGTH_CM > 8.9 & LENGTH_CM <= 10 ~ "8.9cm - 10cm",
                              LENGTH_CM >= 10 ~ "≥ 10cm",
                              TRUE ~ NA),
         EST_YEAR = str_extract(CRUISE_ID, "[0-9]{4}") )

station <- station %>% 
  mutate(REGION = if_else(DECDEG_BEGLON < -71.314566 & DECDEG_BEGLAT < 40.813715, 
                          "Mid-Atlantic", "New England"))

surveys <- surveys %>% 
  mutate(EST_YEAR = YEAR)

export_tables <- lst(biological_join, catch_join, length_join, station)

for (table_name in names(export_tables)) {
  
 
  df <- export_tables[[table_name]]
  
  if (!"EST_YEAR" %in% names(df)) next 
  
  prefix <- sub("_.*", "", table_name)
  
  years <- unique(df$EST_YEAR)
  
  for (yr in years) {
    if (is.na(yr)) next 
    
    df_year <- df %>% filter(EST_YEAR == yr)
    
    file_name <- paste0(prefix, "_", yr, ".csv")
    
    write_csv(df_year, paste0("ScallopShinyApp/Data/", file_name))
  }
}




