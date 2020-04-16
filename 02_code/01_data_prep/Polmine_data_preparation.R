# -----------------------------------------------------------------------------
# Project: Masterthesis
# Content: GerParl Preprocessing data
# Contributors: Marlon Schumacher
# Last update on: 2020-03-01
# -----------------------------------------------------------------------------

# PACKAGES ----
library(tidyverse)
library(lubridate)
library(magrittr)
library(stringr)
library(stringi)

# reading data
df_polmine <- readRDS("01_data/02_prepared/polimeData_20200222.RDS")

# Defining regex pattern
# check the last occurence of " auf:" or "...punkt"
# presidency mostly introducing a new topic by "Ich rufe nun [...] auf:"
pattern_last_occ <- paste0(c("(?!.*(?<=punkt).* auf:)", 
                             "(?!.*(?<=tagesordnungspunkt).*.)",
                             "(?!.*(?<=zusatzpunkt).*.)"), 
                           collapse = "|")

# Extract everything which is between LAST and Drucksache|Antrag|Wort.
# NOTE: could lead to long strings due to the fact that also the 
#       ministery is specfied...
general_pattern <- c("(?<=RLAST).*(?=Drucksache|Antrag|Wort|Aussprache)")


# Using Regex:
df_polmine_with_topic <- df_polmine %>% 
  
  # rounding session if decimal...
  dplyr::mutate(session = floor(session)) %>% 
  
  # filtering for presidency speeches
  # only presidency introduces speeches
  dplyr::filter(role == "presidency") %>% 
  
  # filtering for presidency speeches where a topic is probably introduced
  dplyr::filter(stringr::str_detect(tolower(word), "punkt |tagesordnungspunkt|zusatzpunkt")) %>%
  
  # group data to agenda_item level (debate level)
  dplyr::group_by(date, session, agenda_item) %>% 
  
  # take the first speech of presidency for each agenda_item
  # reason: first speech of presidency should contain agenda topic
  #         because interjection were already removed
  dplyr::summarise(word = dplyr::first(word)) %>% 
  
  # ungrouping data to avoid errors
  dplyr::ungroup()

# specify new column
df_polmine_with_topic$top_content <- "NULL"

# iterating over every row of the data
for(i in 1:nrow(df_polmine_with_topic)){
  
  
  df_polmine_with_topic$top_content[i] <- df_polmine_with_topic$word[i] %>% 
    
    # replacing last occ. of defined pattern
    stringi::stri_replace_last(., regex = pattern_last_occ, replacement = "RLAST") %>%
    
    # extracting everything between RLAST and defined pattern
    stringi::stri_extract(., regex = general_pattern) %>% 
    
    # low cases and squishing string
    tolower() %>% 
    stringr::str_squish()
  
  # progress while the loop is running
  svMisc::progress(i, max.value = nrow(df_polmine_with_topic), progress.bar = F)
}

# checking how much topics could be extracted
length(df_polmine_with_topic$top_content[!is.na(df_polmine_with_topic$top_content)])/length(df_polmine_with_topic$top_content)
# first run
# [1] 0.6479893
# after adjustments
# [1] 0.7447922
# NOTE: 100% match could lead to very long strings
# TODO: sequentiel extracting?

# SAVING DATA ----
saveRDS(df_polmine_with_topic, "01_data/02_prepared/polimeData_topic_labels_20200229.RDS")
