# -----------------------------------------------------------------------------
# Project: Masterthesis
# Content: Extracting Speeches of GermaParl Corpus
# Contributors: Marlon Schumacher
# Last update on: 2020-30-01
# -----------------------------------------------------------------------------

# IMPORTANT NOTE
# R Version < 3.6.0 needed (01-02-2020)!
# GermaParl

# Packages
library(tidyverse)
library(lubridate)
library(polmineR)
library(GermaParl)

# Loading corpus
use("GermaParl")

# check if corpus is listed
corpus()


# extract s_attributes for lp as vector
sessions <- as.numeric(s_attributes("GERMAPARL", s_attribute = "lp"))

# DATA EXTRACTION ----
for (s in sessions){
  
  # extract speeches for specific legislature
  session <- partition("GERMAPARL", lp=s)
  
  # transform data to df
  session <- decode(session) %>% 
    tibble::as_tibble()
  
  # check for first first occurence of interection/speech
  session <- session %>% 
    dplyr::mutate(first_occ = case_when(
      interjection == TRUE & lag(interjection == FALSE) ~ 1,
      interjection == FALSE & lag(interjection == TRUE) ~ 1,
      TRUE ~ 0
    )) %>% 
    dplyr::mutate(turn_id = cumsum(first_occ)) 
  
  # extract speech
  session <- session %>% 
    dplyr::mutate(word_pos = str_c(word, pos, collapse = "_"))
  
  # aggregate to speech and interjection level
  # note: speeches will be still splitted into different parts
  session <- session %>% 
    
    # grouping
    dplyr::group_by(turn_id, lp, year, session, date, speaker, interjection, party, 
                    parliamentary_group, role, agenda_item, agenda_item_type) %>% 
    
    # aggregation for speech
    dplyr::summarise(word = str_c(word, collapse = " ")) %>% 
    
    # ungrouping df
    dplyr::ungroup()
  
  # transform characters to doubles
  session <- session %>% 
    dplyr::mutate(lp = as.double(lp),
                  
                  # session of legislature
                  session = as.double(session), 
                  
                  # date of speech
                  date = ymd(date), 
                  
                  # year of speech
                  year = year(date),
                  
                  # agenda item for possible debate aggregation
                  agenda_item = as.double(agenda_item))

  
  # speaker name for interjections will be deleted
  session <- session %>% 
    dplyr::mutate(speaker = case_when(
      interjection == TRUE ~ NA_character_, 
      TRUE ~ speaker
    ))
  
  # saving data to legislature specific object name
  assign(str_c("lp_", s), session)
  
}

# Create list object
lp_list <- list(lp_13 = lp_13,  
                lp_14 = lp_14,  
                lp_15 = lp_15,  
                lp_16 = lp_16,  
                lp_17 = lp_17,  
                lp_18 = lp_18)

# AGGREGATION SPEAKER LEVEL ----
for (lp_df in 1:length(lp_list)){ 
  
  # for each df the interjections will be deleted
  lp_df[[lp_df]] <- lp_df[[lp_df]] %>% 
    
    # removing rows which presents interjections
    dplyr::filter(interjection != TRUE) %>%
    
    # detect if the speaker has changed compared to 
    # speech-part one row before
    dplyr::mutate(speaker_change = case_when(
      speaker != lag(speaker) ~ 1, 
      TRUE ~ 0
    )) %>% 
    
    # cumsum speaker change and group by it
    dplyr::mutate(speaker_cumsum = cumsum(speaker_change)) %>% 
    dplyr::group_by(speaker_cumsum) %>% 
    
    # aggregation to speaker level
    dplyr::summarise(turn_id = first(turn_id), 
                     lp = mean(lp), 
                     year = mean(year), 
                     speaker = first(speaker),
                     session = mean(session), 
                     date = first(date), 
                     interjection = first(interjection), 
                     party = first(party), 
                     parliamentary_group = first(parliamentary_group), 
                     role = first(role), 
                     agenda_item = first(agenda_item), 
                     agenda_item_type = first(agenda_item_type), 
                     word = str_c(word, collapse = " "))

}

# binding resulting data into one single data frame
df_germa_par_parsed <- dplyr::bind_rows(lp_list)


# frst inspection regarding nchar
# df_germa_par_parsed %>% 
#   dplyr::filter(role != "presidency") %>% nrow()
#   dplyr::mutate(nchar = nchar(word)) %>% 
#   dplyr::filter(nchar < 10000) %>% 
#   ggplot(aes(x = date, y = nchar, fill = party)) +
#   geom_density()

# SAVING DATA ----
saveRDS(df_germa_par_parsed, "01_data/02_prepared/polmineR/polimeData.RDS")
