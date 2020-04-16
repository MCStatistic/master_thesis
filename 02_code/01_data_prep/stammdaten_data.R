# -----------------------------------------------------------------------------
# Project: Masterthesis
# Content: Stammdaten
# Contributors: Marlon Schumacher
# Last update on: 2020-03-01
# -----------------------------------------------------------------------------

# Packages ----
pacman::p_load(dplyr, ggplot2, stringr, tidyr, purrr, lubridate, magrittr, tm, haven, readr, XML)

# FUNCTIONS ----
# TODO: helper_functions.R should be used to store every single function w
# TODO: which is used for the master thesis

# replacing with NA, if value is NULL (e.g. no academic title)
null_replace <- function(x){
  ifelse(is.null(x), NA, x)
}

# function for extracting data from stammdaten
extract_stammdaten <- function(stammliste){
  
  # creating empty matrix  
  output <- matrix(ncol = 10, nrow = length(stammliste))
  
  # iterating over every element of the list and extract information
  for(i in 1:length(stammliste)){
    output[i, 1] <- null_replace(stammliste[i]$MDB$ID)
    output[i, 2] <- null_replace(stammliste[i]$MDB$NAMEN$NAME$NACHNAME)
    output[i, 3] <- null_replace(stammliste[i]$MDB$NAMEN$NAME$VORNAME)
    output[i, 4] <- null_replace(stammliste[i]$MDB$NAMEN$NAME$ANREDE_TITEL)
    output[i, 5] <- null_replace(stammliste[i]$MDB$NAMEN$NAME$AKAD_TITEL)
    output[i, 6] <- null_replace(stammliste[i]$MDB$BIOGRAFISCHE_ANGABEN$GEBURTSORT)
    output[i, 7] <- null_replace(stammliste[i]$MDB$BIOGRAFISCHE_ANGABEN$GEBURTSDATUM)
    output[i, 8] <- null_replace(stammliste[i]$MDB$BIOGRAFISCHE_ANGABEN$GESCHLECHT)
    output[i, 9] <- null_replace(stammliste[i]$MDB$BIOGRAFISCHE_ANGABEN$PARTEI_KURZ)
    output[i, 10] <- null_replace(stammliste[i]$MDB$BIOGRAFISCHE_ANGABEN$BERUF)
    
  }
  
  # transform matrix to dataframe and keep strings as strings
  output <- data.frame(output, stringsAsFactors = FALSE) %>% 
    dplyr::rename(ID = X1, 
                  NACHNAME = X2,
                  VORNAME = X3,
                  ANREDE_TITEL = X4,
                  AKAD_TITEL = X5,
                  GEBURTSORT = X6,
                  GEBURTSDATUM = X7,
                  GESCHLECHT = X8,
                  PARTEI = X9,
                  BERUF = X10) %>% 
    dplyr::filter(!is.na(ID)) %>% 
    as_tibble()
  
  return(output)
  
}

# RAW DATA ----
# transform XML-Stammdaten to list object
stamdaten <- XML::xmlToList("/Users/marlonschumacher/Documents/Master/MdB-Stammdaten-data/MDB_STAMMDATEN.XML")

# extract data from list object with defined function
stamm_df <- extract_stammdaten(stamdaten)
stamm_df <- stamm_df %>%
  
  # creating full name and trimming white spaces
  dplyr::mutate(f_name = trimws(paste0(VORNAME, " ", NACHNAME))) %>% 
  
  # reshaping party naming
  dplyr::mutate(PARTEI = case_when(
    PARTEI == "GRÜNE" ~ "Grüne",
    PARTEI == "BÜNDNIS 90/DIE GRÜNEN" ~ "Grüne",
    PARTEI == "DIE GRÜNEN/BÜNDNIS 90" ~ "Grüne",
    PARTEI == "PDS" ~ "Linke",
    PARTEI == "DIE LINKE." ~ "Linke",
    PARTEI == "CDU" ~ "CDU/CSU",
    PARTEI == "CSU" ~ "CDU/CSU",
    PARTEI == "PDS/LL" ~ "Linke",
    TRUE ~ PARTEI
  ))

# SAVING DATA ----
saveRDS("01_data/stammdaten_14_19_20191025.RDS")