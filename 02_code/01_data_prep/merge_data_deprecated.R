# -----------------------------------------------------------------------------
# Project: Masterthesis
# Content: Combining Data (deprecated)
# Contributors: Marlon Schumacher
# Last update on: 2020-03-01
# -----------------------------------------------------------------------------

pacman::p_load(dplyr, ggplot2, stringr, tidyr, purrr, lubridate, magrittr, tm, haven, readr, XML)
df_bt_19 <- readRDS("/Users/marlonschumacher/Documents/Master/df_pp19_sent_20191201.RDS")
df_bt_19 %>% head()

df_bt_14_18 <- readRDS("/Users/marlonschumacher/Documents/Master/df_main_fin_20191025.RDS")
df_bt_14_18 %>% colnames()

colnames(df_bt_19)
colnames(df_bt_14_18)

df_bt_14_18_prep <- df_bt_14_18 %>% 
  as_tibble() %>% 
  dplyr::rename(speech = speak, ID = ID.y, sentiment_mean = sentiment_val,
                fraktion= parliamentary_group,
                vorname_st = VORNAME, nachname_st = NACHNAME, geburtsort = GEBURTSORT,
                geburtsdatum = GEBURTSDATUM, sex = GESCHLECHT, beruf = BERUF, mandatsart = MANDATSART,
                anrede_titel = ANREDE_TITEL, akad_titel = AKAD_TITEL) %>%
  dplyr::select(date, legislature, name, fraktion, party, role, position, tagesordnung, topic, top_new, speech, interjection,
                nchar, sentiment, sentiment_mean, sentiment_median, mandatsart,
                nachname_st, vorname_st, anrede_titel, akad_titel, sex, geburtsdatum, beruf, geburtsort)

head(df_bt_14_18_prep)



df_bt_19_prep <- df_bt_19 %>% 
  dplyr::rename(name = f_name1, speech = rede, top_new = inhalt, role = rolle, 
                vorname_st = VORNAME, nachname_st = NACHNAME, geburtsort = GEBURTSORT,
                geburtsdatum = GEBURTSDATUM, sex = GESCHLECHT, beruf = BERUF,
                anrede_titel = ANREDE_TITEL, akad_titel = AKAD_TITEL, party = PARTEI) %>% 
  dplyr::select(date, name, fraktion, party, role, top_new, speech, nchar, sentiment_mean, sentiment_median,
                nachname_st, vorname_st, anrede_titel, akad_titel, sex, geburtsdatum, beruf, geburtsort) %>% 
  dplyr::mutate(legislature = 19)

df_final <- dplyr::bind_rows(df_bt_14_18_prep %>% dplyr::mutate(date = as.Date(date)),
                             df_bt_19_prep %>% dplyr::mutate(date = as.Date(date, "%d.%m.%Y"))) %>% 
  dplyr::mutate(speech = str_squish(speech))

df_final_2 <- dplyr::bind_rows(df_final %>% dplyr::filter(legislature < 19),
                             df_bt_19_prep %>% dplyr::mutate(date = as.Date(date, "%d.%m.%Y"))) %>% 
  dplyr::mutate(speech = str_squish(speech))

df_final_2 %<>%
  dplyr::mutate(fraktion = case_when(str_detect(tolower(fraktion), "cdu") ~ "Union",
                                     str_detect(tolower(fraktion), "spd") ~ "SPD",
                                     str_detect(tolower(fraktion), "linke|pds") ~ "Linke",
                                     str_detect(tolower(party), "linke") ~ "Linke",
                                     str_detect(tolower(fraktion), "fdp") ~ "FDP",
                                     str_detect(tolower(fraktion), "fraktionslos") ~ "fraktionslos",
                                     str_detect(tolower(fraktion), "no infor") ~ "NA",
                                     str_detect(tolower(fraktion), "grüne") ~ "Grüne",
                                     str_detect(fraktion, "F[.]") ~ "FDP",
                                     TRUE ~ fraktion )) %>% 
  dplyr::mutate(opp_gov = case_when(
    legislature == 14 & fraktion %in% c("SPD", "Grüne") ~ "Government",
    legislature == 15 & fraktion %in% c("SPD", "Grüne") ~ "Government",
    legislature == 16 & fraktion %in% c("Union", "SPD") ~ "Government",
    legislature == 17 & fraktion %in% c("Union", "FDP") ~ "Government",
    legislature == 18 & fraktion %in% c("Union", "SPD") ~ "Government",
    legislature == 19 & fraktion %in% c("Union", "SPD") ~ "Government",
    TRUE ~ "Opposition")) 
