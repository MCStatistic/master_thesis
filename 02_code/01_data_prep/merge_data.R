# -----------------------------------------------------------------------------
# Project: Masterthesis
# Content: Combining Data
# Contributors: Marlon Schumacher
# Last update on: 2020-03-01
# -----------------------------------------------------------------------------

# PACKAGES ----
library(magrittr)
library(tm)
library(SnowballC)
library(topicmodels)
library(quanteda)
library(SentimentAnalysis)
library(dplyr)
library(magrittr)
library(lubridate)
library(zoo)
library(tm)
library(readr)
library(stringr)
library(ggplot2)
library(corpus)

# prepared data from GermaParl
df_speeches_14_18 <- readRDS("01_data/02_prepared/polimeData_20200222.RDS")

# Adjust mp names due to differences between data sources
df_14_18_join <- df_speeches_14_18 %>% 
  
  # transform to tibble for easier fast exploration
  as_tibble() %>% 
  
  # removing potential titles like Dr.
  dplyr::mutate(name = str_remove(speaker, "(\\b[A-Z]\\.?+ )|( [A-Z]\\.?+\\b)")) %>% 
  
  # transform legislature into double dtype
  dplyr::mutate(legislature = as.double(lp),
                
                # trim whitespaces
                name = trimws(name),
                
                # change name for identified cases
                name = case_when(name == "Sevim Dağdelen" ~ "Sevim Dagdelen",
                                 name == "Daniela Raab" ~ "Daniela Ludwig", 
                                 name == "Karl-Theodor Freiherr zu Guttenberg" ~ "Karl-Theodor zu Guttenberg",
                                 name == "Kees de Vries" ~ "Kees Vries", 
                                 name == "Detlev von Larcher" ~ "Detlev Larcher", 
                                 name == "Philipp Graf von und zu Lerchenfeld" ~ "Philipp Lerchenfeld",
                                 name == "Hans-Georg von der Marwitz" ~ "Hans-Georg Marwitz",
                                 name == "Carl-Detlev Freiherr von Hammerstein" ~ "Carl-Detlev Hammerstein",
                                 name == "Viola von Cramon-Taubadel" ~ "Viola Cramon-Taubadel",
                                 name == "Jörg von Polheim" ~ "Jörg Polheim",
                                 name == "Alois Georg Josef Rainer" ~ "Alois Rainer",
                                 name == "Sabine Bätzing-Lichtenthäler" ~ "Sabine Bätzing",
                                 name == "Christel Riemann-Hanewinckel" ~ "Christel Hanewinckel",
                                 name == "Michael von Schmude" ~ "Michael Schmude",
                                 name == "Kristina Köhler" ~ "Kristina Schröder",
                                 name == "Margot von Renesse" ~ "Margot Renesse",
                                 name == "Ingrid Marianne Fischbach" ~ "Ingrid Fischbach",
                                 name == "Volker Michael Ullrich" ~ "Volker Ullrich",
                                 name == "Ernst Ulrich von Weizsäcker" ~ "Ernst Ulrich Weizsäcker",
                                 name == "Wolfgang Nešković" ~ "Wolfgang Neskovic",
                                 name == "Christian von Stetten" ~ "Christian Stetten",
                                 name == "Ulla Lötzer" ~ "Ursula Lötzer",
                                 name == "Gabi Molitor" ~ "Gabriele Molitor",
                                 name == "Dorothee Mantel" ~ "Dorothee Bär",
                                 name == "Ursula Schauws" ~ "Ulle Schauws",
                                 name == "Ursula „Ulla“ Schmidt" ~ "Ulla Schmidt",
                                 name == "Michael Georg Link" ~ "Michael Link",
                                 name == "Grietje Staffelt" ~ "Grietje Bettin",
                                 TRUE ~ name)) %>% 
  
  # squishing resulting string for safety
  dplyr::mutate(name = stringr::str_squish(name))

# reading stammdaten
stamm_df <- readRDS("01_data/stammdaten_14_18_20191025.RDS")

# adjust names of 
stamm_df2 <- stamm_df %>%
  dplyr::mutate(VORNAME = case_when(
    str_detect(VORNAME, "Alexander") & NACHNAME == "Lambsdorff" ~ "Alexander Graf",
    str_detect(VORNAME, "Wolfgang") & NACHNAME == "Schäuble" ~ "Dr. Wolfgang",
    str_detect(VORNAME, "Katrin Dagmar") & NACHNAME == "Göring-Eckardt" ~ "Katrin",
    str_detect(VORNAME, "Beatrix") & NACHNAME == "Storch" ~ "Beatrix von",
    str_detect(VORNAME, "Sonja") & NACHNAME == "Steffen" ~ "Sonja Amalie",
    str_detect(VORNAME, "Konstantin") & NACHNAME == "Notz" ~ "Konstantin von",
    str_detect(VORNAME, "Johann") & NACHNAME == "Wadephul" ~ "Johann David",
    str_detect(VORNAME, "Michael") & NACHNAME == "Link" ~ "Michael Georg",
    str_detect(VORNAME, "Matern") & NACHNAME == "Marschall von Bieberstein" ~ "Matern von",
    str_detect(VORNAME, "Ottmar") & NACHNAME == "Holtz" ~ "Ottmar von",
    str_detect(VORNAME, "Wilhelm") & NACHNAME == "Gottberg" ~ "Wilhelm von",
    str_detect(VORNAME, "Jörg-Diether") & NACHNAME == "Dehm-Desoi" ~ "Diether",
    str_detect(VORNAME, "Christoph") & NACHNAME == "Vries" ~ "Christoph de",
    str_detect(VORNAME, "Volkmar") & NACHNAME == "Vogel" ~ "Volkmar",
    str_detect(VORNAME, "Albert") & NACHNAME == "Weiler" ~ "Albert H.",
    str_detect(VORNAME, "Michael") & NACHNAME == "Abercron" ~ "Michael von",
    str_detect(VORNAME, "Agnes") & NACHNAME == "Malczak" ~ "Agnieszka",
    TRUE ~ VORNAME
  ),
  NACHNAME = case_when(
    str_detect(VORNAME, "Matern von") & NACHNAME == "Marschall von Bieberstein" ~ "Marschall",
    str_detect(VORNAME, "Olaf") & NACHNAME == "In der Beek" ~ "in der Beek",
    str_detect(VORNAME, "Diether") & NACHNAME == "Dehm-Desoi" ~ "Dehm",
    str_detect(VORNAME, "Agnieszka") & NACHNAME == "Malczak" ~ "Brugger",
    str_detect(VORNAME, "Nadine") & NACHNAME == "Müller" ~ "Schön",
    str_detect(VORNAME, "Daniela") & NACHNAME == "Raab" ~ "Ludwig",
    str_detect(VORNAME, "Dorothee") & NACHNAME == "Mantel" ~ "Bär",
    str_detect(VORNAME, "Ronja") & NACHNAME == "Schmitt" ~ "Kemmer",
    str_detect(VORNAME, "Michaela") & NACHNAME == "Tadjadod" ~ "Noll",
    str_detect(VORNAME, "Kersten") & NACHNAME == "Naumann" ~ "Steinke",
    TRUE ~ NACHNAME
  )) %>% 
  dplyr::mutate(f_name = paste0(VORNAME, " ", NACHNAME)) %>% 
  dplyr::mutate(f_name = case_when(
    VORNAME == "Thomas" & NACHNAME == "Maizière" ~ "Thomas de Maizière",
    VORNAME == "Katrin Dagmar" & NACHNAME == "Göring-Eckardt" ~ "Katrin Göring-Eckardt",
    VORNAME == "Petra" & NACHNAME == "Bläss-Rafajlovski" ~ "Petra Bläss",
    VORNAME == "Heinrich L." & NACHNAME == "Kolb" ~ "Heinrich Leonhard Kolb",
    VORNAME == "Eckart" & NACHNAME == "Klaeden" ~ "Eckart von Klaeden",
    VORNAME == "Eva-Maria" & NACHNAME == "Bulling-Schröter" ~ "Eva Bulling-Schröter",
    VORNAME == "Dagmar G." & NACHNAME == "Wöhrl" ~ "Dagmar Wöhrl",
    VORNAME == "Ulrike" & NACHNAME == "Höfken-Deipenbrock" ~ "Ulrike Höfken",
    VORNAME == "Jörg" & NACHNAME == "Essen" ~ "Jörg van Essen",
    VORNAME == "Ursula" & NACHNAME == "Leyen" ~ "Ursula von der Leyen",
    VORNAME == "Konstantin" & NACHNAME == "Notz" ~ "Konstantin von Notz",
    VORNAME == "Marieluise" & NACHNAME == "Beck-Oberdorf" ~ "Marieluise Beck",
    VORNAME == "Matthias W." & NACHNAME == "Birkwald" ~ "Matthias Birkwald",
    VORNAME == "Margareta" & NACHNAME == "Wolf-Mayer" ~ "Margareta Wolf",
    VORNAME == "Inge" & NACHNAME == "Höger-Neuling" ~ "Inge Höger",
    VORNAME == "Jörg-Diether" & NACHNAME == "Dehm-Desoi" ~ "Diether Dehm",
    VORNAME == "Dorothee" & NACHNAME == "Menzner" ~ "Dorothée Menzner",
    VORNAME == "Ursula" & NACHNAME == "Heinen" ~ "Ursula Heinen-Esser",
    VORNAME == "Hans-Werner" & NACHNAME == "Müller" ~ "Werner Müller",
    VORNAME == "Jan" & NACHNAME == "Aken" ~ "Jan van Aken",
    VORNAME == "Sevim" & NACHNAME == "Dağdelen" ~ "Sevim Dagdelen",
    VORNAME == "Andreas G." & NACHNAME == "Lämmel" ~ "Andreas Lämmel",
    VORNAME == "Kurt J." & NACHNAME == "Rossmanith" ~ "Kurt Rossmanith",
    TRUE ~ f_name
  )) %>% 
  dplyr::mutate(f_name = str_remove(f_name, "(\\b[A-Z]\\.?+ )|( [A-Z]\\.?+\\b)")) %>% 
  dplyr::mutate(f_name = case_when(
    str_detect(f_name, "Schwaetzer") ~ "Irmgard Schwaetzer",
    str_detect(f_name, "Schäuble") ~ "Wolfgang Schäuble",
    str_detect(f_name, "Kristina") ~ "Kristina Schröder",
    str_detect(f_name, "Winkler") ~ "Josef Winkler",
    str_detect(f_name, "Johanna Müller") ~"Johanna Wanka",
    str_detect(f_name, "Paus") ~ "Elisabeth Paus",
    str_detect(f_name, "Gisela Altmann") ~ "Gila Altmann",
    str_detect(f_name, "Johann Da") ~ "Johann Wadephul",
    str_detect(f_name, "Guttenberg") & GEBURTSDATUM == "1971" ~ "Karl-Theodor zu Guttenberg",
    str_detect(f_name, "Peter H. Carstensen") ~ "Peter Harry Carstensen",
    str_detect(f_name, "Erika Steinbach") ~ "Erika Steinbach",
    str_detect(f_name, "Heidi Lippmann") ~ "Heidi Lippmann",
    str_detect(f_name, "Sonja Amalie") ~ "Sonja Steffen",
    str_detect(f_name, "Patrick Sensburg") ~ "Patrick Ernst Sensburg",
    str_detect(f_name, "Ursula Heinen") ~ "Ursula Heinen",
    str_detect(f_name, "Keskin") ~ "Hakkı Keskin",
    str_detect(f_name, "Memet") ~ "Memet Kılıç",
    str_detect(f_name, "Klaus Wolfgang Müller") ~ "Klaus Müller",
    str_detect(f_name, "Wolfgang Stetten") ~ "Wolfgang von Stetten",
    str_detect(f_name, "Pia Zimmermann") ~ "Pia-Beate Zimmermann",
    str_detect(f_name, "Margot von Renesse") ~ "Margot Renesse",
    str_detect(f_name, "Julia Bartz") ~ "Julia Obermeier",
    str_detect(f_name, "Motschmann") ~ "Elisabeth Charlotte Motschmann",
    str_detect(f_name, "Helfrich") ~ "Mark André Helfrich",
    str_detect(f_name, "Peter Carstensen") ~ "Peter Harry Carstensen",
    str_detect(f_name, "Edmund Peter") ~ "Edmund Geisen",
    str_detect(f_name, "Gisela Schröter") ~ "Gisela Hilbrecht",
    str_detect(f_name, "Brigitte Traupe") ~ "Brigitte Schulte",
    str_detect(f_name, "Ursula Heinen") ~ "Ursula Heinen-Esser",
    str_detect(f_name, "Michael Georg Link") ~ "Michael Link",
    str_detect(f_name, "Ursula Eid-Simon") ~ "Uschi Eid",
    str_detect(f_name, "Max Lehmer") ~ "Maximilian Lehmer",
    str_detect(f_name, "Michaela Engelmeier") ~ "Michaela Engelmeier-Heite",
    # str_detect(f_name, "Grietje Bettin") ~ "Grietje Staffelt",
    TRUE ~ f_name))

df_join <- dplyr::left_join(df_14_18_join, stamm_df2,
                 by = c("name" = "f_name"))

# checking how much missings are still there
df_join %>% 
  dplyr::filter(is.na(GEBURTSDATUM))

df_join <- df_join %>% 
  dplyr::mutate(GEBURTSDATUM =case_when(name == "Johanna Wanka" ~ "1951",
                                        name == "Christine Bergmann" ~ "1939",
                                        name == "Wolfgang Clement" ~ "1940",
                                        name == "Manuela Schwesig" ~ "1974",
                                        name == "Philipp Rösler" ~ "1973",
                                        name == "Michael Naumann" ~ "1941",
                                        name == "Manfred Stolpe" ~ "1936",
                                        name == "Ursula Heinen-Esser" ~ "1965",
                                        name == "Karl-Heinz Funke" ~ "1946",
                                        name == "Vera Lengsfeld" ~ "1952",
                                        TRUE ~ GEBURTSDATUM),
                GESCHLECHT = case_when(name == "Johanna Wanka" ~ "weiblich", 
                                       name == "Christine Bergmann" ~ "weiblich",
                                       name == "Wolfgang Clement" ~ "männlich",
                                       name == "Manuela Schwesig" ~ "weiblich",
                                       name == "Philipp Rösler" ~ "männlich",
                                       name == "Michael Naumann" ~ "männlich",
                                       name == "Manfred Stolpe" ~ "männlich",
                                       name == "Ursula Heinen-Esser" ~ "weiblich",
                                       name == "Karl-Heinz Funke" ~ "männlich",
                                       name == "Vera Lengsfeld" ~ "weiblich",
                                       TRUE ~ GESCHLECHT)) %>% 
  # Christine Weiss = fraktionslos and empty name will be removed
  dplyr::filter(name != "Christina Weiss" & name != "")

# checking how much missings are still there
df_join %>% 
  dplyr::filter(is.na(GEBURTSDATUM)) %>%
  dplyr::group_by(speaker) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::arrange(desc(n))
  
df_14_18 <- df_join
df_14_18_join <- df_14_18 %>% 
  dplyr::select(legislature, year, date, session, agenda_item, agenda_item_type, name, party, parliamentary_group, role,
                word, ID, AKAD_TITEL, GEBURTSDATUM, GESCHLECHT, PARTEI, BERUF) %>% 
  dplyr::rename(fraktion = parliamentary_group, speech = word) %>% 
  dplyr::mutate(party = tolower(party),
                party = case_when(str_detect(party, "fdp") ~ "FDP",
                                  str_detect(party, "spd") ~ "SPD",
                                  str_detect(party, "afd") ~ "AfD",
                                  str_detect(party, "linke|pds") ~ "Linke",
                                  str_detect(party, "gr") ~ "Grüne",
                                  str_detect(party, "cdu") ~ "CDU",
                                  str_detect(party, "csu") ~ "CSU",
                                  TRUE ~ NA_character_),
                fraktion = case_when(str_detect(party, "CDU|CSU") ~ "Union",
                                     TRUE ~ party)) %>% 
  dplyr::rename(st_akad = AKAD_TITEL, st_b_year = GEBURTSDATUM, 
                sex = GESCHLECHT, st_party = PARTEI,
                job = BERUF)

# Joining data ----

# reading data for the 19th Bundestag
df_pp19 <- readRDS(file = "01_data/02_prepared/df_pp19_20200222.RDS")

# preparing data for joining
df_19_join <- df_pp19 %>% 
  # selecting variables of interest
  dplyr::select(date, fraktion, top, inhalt, f_name1, rolle, rede, ID, 
                AKAD_TITEL, GEBURTSDATUM, GESCHLECHT, PARTEI, BERUF) %>% 
  # transform party variable to a standardized format
  dplyr::mutate(party = tolower(PARTEI),
                party = case_when(str_detect(party, "fdp") ~ "FDP",
                                  str_detect(party, "spd") ~ "SPD",
                                  str_detect(party, "afd") ~ "AfD",
                                  str_detect(party, "linke") ~ "Linke",
                                  str_detect(party, "grüne|bündnis") ~ "Grüne",
                                  str_detect(party, "cdu") ~ "CDU",
                                  str_detect(party, "csu") ~ "CSU",
                                  TRUE ~ NA_character_),
                # adding fraktion and considering both parties CDU/CSU as fraction
                fraktion = case_when(str_detect(party, "CDU|CSU") ~ "Union",
                                     TRUE ~ party)) %>% 
  # cleaning speeches 
  dplyr::mutate(rede = stringr::str_replace_all(rede, "\\n\\n", " "),
                rede = stringr::str_squish(rede),
                # trannsforming data character into date format
                date = stringr::str_replace_all(date, "[.]", "-"),
                date = zoo::as.Date(date, "%d-%m-%Y"),
                # adding legislature
                legislature = 19,
                # adding year of speech
                year = year(date)) %>% 
  dplyr::left_join(df_pp19 %>% 
                     # adding agenda_item with left_join
                     dplyr::mutate(rede = stringr::str_replace_all(rede, "\\n\\n", " "),
                                   rede = stringr::str_squish(rede),
                                   date = stringr::str_replace_all(date, "[.]", "-"),
                                   date = zoo::as.Date(date, "%d-%m-%Y")) %>% 
                     # grouping by date and agenda topic
                     dplyr::group_by(date, inhalt) %>% 
                     # summarising data to get only all items
                     dplyr::summarise(n = n()) %>% 
                     # adding id as row_number for each topic
                     dplyr::mutate(agenda_item = row_number()) %>% 
                     dplyr::select(-n),
                   by = c("date", "inhalt"))%>% 
  dplyr::left_join(df_pp19 %>% 
                     # adding session id with the same logic as before
                     dplyr::mutate(rede = stringr::str_replace_all(rede, "\\n\\n", " "),
                                   rede = stringr::str_squish(rede),
                                   date = stringr::str_replace_all(date, "[.]", "-"),
                                   date = zoo::as.Date(date, "%d-%m-%Y")) %>% 
                     dplyr::group_by(date) %>% 
                     dplyr::summarise(n = n()) %>% 
                     dplyr::mutate(session = row_number()) %>% 
                     dplyr::select(-n),
                   by = "date") %>% 
  # renaming variable names
  dplyr::rename(name = f_name1, role = rolle, speech = rede) %>% 
  dplyr::rename(st_akad = AKAD_TITEL, st_b_year = GEBURTSDATUM, 
                sex = GESCHLECHT, st_party = PARTEI,
                job = BERUF, top_content = inhalt)

# checking colnames for both data frames
df_19_join %>% colnames()
df_14_18_join %>% colnames()

# binding both data frames together
df_merged_v2 <- dplyr::bind_rows(df_14_18_join,
                                 df_19_join)

# text pre processing
# additional stop words
umlaute <- as.character(c("dafür", "darüber", "demgegenüber", "demgemäss", "dürfen",
                          "dürft", "früher", "fünf", "fünfte", "fünften", "fünfter", "fünftes",
                          "für", "gegenüber",  "hätten", "hätte", "können", "könnt",
                          "könnte", "möchte", "mögen", "möglich", "mögt", "müssen", 
                          "natürlich", "später", "über", "überhaupt", "übrigens", 
                          "während", "währenddem", "währenddessen", "wäre", "würde",
                          # party related words
                          "fdp", "afd", "spd", "cdu", "csu", "fraktion", "bundestagsfraktion",
                          # high frequent qords which should not be considered for ML
                          "herren", "frauen", "herr", "frau", "prsäident", "kollegen", "kollege", "kolleginnen",
                          # special characters
                          "––", "–", "§", "-"))

stopwords_ger <- readr::read_csv("/Users/marlonschumacher/Documents/Master/stopwords.csv") %>% 
  dplyr::filter(!stringr::str_detect(word, "[^[:alnum:]]")) %>% .$word

df_merged_v2 <- df_merged_read %>% 
  dplyr::mutate(speech = tolower(speech))

df_merged_v2$speech_pre_clean <- df_merged_v2$speech %>% 
  # removing all non-alphanumeric characters
  stringr::str_replace_all("[^[:alnum:]]", " ") %>% 
  # removing other special characters which may be part of speech
  stringr::str_replace_all("[.]", " ") %>% 
  stringr::str_replace_all("[-]", " ") %>% 
  stringr::str_replace_all("[,]", " ") %>% 
  stringr::str_replace_all("[.]", " ") %>% 
  stringr::str_replace_all('“|„|"', "") %>% 
  # remove all numbers
  tm::removeNumbers() %>% 
  # remove punctuations
  tm::removePunctuation() %>% 
  stringr::str_squish()


df_merged_v2$speech_clean <- df_merged_v2$speech_pre_clean %>% 
  # removing german stopwords
  tm::removeWords(stopwords_ger) %>% 
  tm::removeWords(tm::stopwords("german")) %>% 
  tm::removeWords(umlaute) %>% 
  # squishing string
  # multiple whitespaces would be removed to one
  stringr::str_squish()

df_merged_v2$speech_stemmed <- df_merged_v2$speech_clean %>% 
  # stemming words
  SnowballC::wordStem(language = "german") %>% 
  stringr::str_squish()


# Adding information to data
df_merged_v2 <- df_merged_v2 %>% 
  # adding information of government/opposition role
  dplyr::mutate(opp_gov = case_when(
    legislature == 14 & fraktion %in% c("SPD", "Grüne") ~ "Government",
    legislature == 15 & fraktion %in% c("SPD", "Grüne") ~ "Government",
    legislature == 16 & fraktion %in% c("Union", "SPD") ~ "Government",
    legislature == 17 & fraktion %in% c("Union", "FDP") ~ "Government",
    legislature == 18 & fraktion %in% c("Union", "SPD") ~ "Government",
    legislature == 19 & fraktion %in% c("Union", "SPD") ~ "Government",
    TRUE ~ "Opposition")) %>% 
  # there are few cases with high ages
  # TODO: inspecting these cases
  dplyr::mutate(st_b_year = as.double(st_b_year),
                # calculating age for each speaker
                age = year - st_b_year,
                # calculating n characters
                nchar = nchar(speech),
                nchar_clean = nchar(speech_clean))

df_merged_v2 %>% 
  dplyr::select(-ngram_pre_clean,-ngram_stemmed) %>% 
  saveRDS("/Users/marlonschumacher/Documents/master_thesis/01_data/02_prepared/df_merged_20200222.RDS")

df_merged_read <-readRDS("01_data/02_prepared/df_merged_20200222.RDS")

# adding n-words for pre cleaned speeches (without punctuations...)
df_merged_read$nword_pre_clean <- df_merged_read$speech_pre_clean %>% 
  purrr::map(.x = .,
             .f = ngram::wordcount) %>% 
  unlist()

# adding n-words for cleaned speeches (removed stopwords)
df_merged_read$nword_stemmed <- df_merged_read$speech_stemmed %>% 
  purrr::map(.x = .,
             .f = ngram::wordcount) %>% 
  unlist()

df_merged_read %<>% 
  dplyr::mutate(# calculating n characters
                nchar_pre_cleaned = nchar(speech_pre_clean))

df_merged_read <- df_merged_read %>% 
  dplyr::mutate(populism = case_when(
    str_detect(party, "Linke|AfD") ~ "Populismus",
    TRUE ~ "Kein Populismus"))

# cut off digits of sessions and transform double to integer
df_merged_read %<>% 
  dplyr::mutate(session = as.integer(floor(session))) 

# check how many rows the president takes part
df_merged_read %>% 
  dplyr::group_by(legislature) %>% 
  dplyr::summarise(n = n())

df_merged_read %>% 
  dplyr::filter(!str_detect(tolower(role), "presidency|präsident")) %>% 
  dplyr::group_by(legislature) %>% 
  dplyr::summarise(n = n())

df_merged_read %>% 
  saveRDS("01_data/02_prepared/df_main_v2_20200222.RDS")


df_merged_read %>% 
  dplyr::filter(role == "presidency") %>% 
  dplyr::filter(str_detect(speech, "tagesordnung|antrag|punkt")) %>% 
  .$session
  .$speech_pre_clean %>% .[1]
  
  pattern <- paste0("(?<=", left.border, ")[a-z]+(?=", right.border, ")")

# [1] ""                              "debate"                        "interrogation"                
# [4] "question_time"                 "recent_issues"                 "point_of_order"               
# [7] "government_declaration"        "budget"                        "election"                     
# [10] "debate|recent_issues"          "debate|budget"                 "government_declaration|debate"
# [13] NA 

# adding word count for each speech
df_merged_v2$nword <- ngram::wordcount(df_merged_v2$speech_pre_clean)
df_merged_v2$nword_clean <- ngram::wordcount(df_merged_v2$speech_clean)
  



# TODO: renaming and selecting needed variables
df_pp19_selected <- df_pp19 %>% 
  dplyr::mutate(rede = stringr::str_replace_all(rede, "\\n\\n", " "),
                rede = stringr::str_squish(rede),
                date = stringr::str_replace_all(date, "[.]", "-"),
                date = zoo::as.Date(date, "%d-%m-%Y"),
                legislature = 19) %>% 
  dplyr::rename(name = f_name1, speech = rede, agenda_top = inhalt, role = rolle, 
                vorname_st = VORNAME, nachname_st = NACHNAME, geburtsort = GEBURTSORT,
                geburtsdatum = GEBURTSDATUM, sex = GESCHLECHT, beruf = BERUF,
                anrede_titel = ANREDE_TITEL, akad_titel = AKAD_TITEL, party = PARTEI) %>% 
  dplyr::select(date, legislature, name, ID, fraktion, party, role, agenda_top, speech,
                sex, beruf, anrede_titel, akad_titel)


df_14_18 %<>% 
  dplyr::mutate(speak = stringr::str_squish(speak),
                date = zoo::as.Date(date)) %>% 
  dplyr::rename(speech = speak, ID = ID.y, fraktion= parliamentary_group,
                vorname_st = VORNAME, nachname_st = NACHNAME, geburtsort = GEBURTSORT,
                geburtsdatum = GEBURTSDATUM, sex = GESCHLECHT, beruf = BERUF,
                anrede_titel = ANREDE_TITEL, akad_titel = AKAD_TITEL, agenda_top = top_new) %>% 
  dplyr::select(date, legislature, name, ID, fraktion, party, role, position, agenda_top, speech,
                sex, beruf, anrede_titel, akad_titel) %>% 
  dplyr::filter(role != "presidency")

glimpse(df_pp19_selected)
glimpse(df_14_18)

df_combined <- dplyr::bind_rows(df_14_18, df_pp19_selected)
glimpse(df_combined)

df_combined <- df_combined %>% 
  dplyr::mutate(nchar = nchar(speech),
                n_sentence = quanteda::nsentence(speech))

df_combined <- readRDS("01_data/df_14_19_20191222")
# creating df which contains names by 
name_by_id <- df_combined %>% 
  dplyr::filter(legislature < 19 & !is.na(ID)) %>% 
  dplyr::select(name, ID) %>% 
  dplyr::rename(name_14_18 = name) %>%
  dplyr::distinct() 

# joining names of 14 to 18th legislature by ID
df_combined %<>% 
  dplyr::left_join(name_by_id, by = "ID")

# replacing names with names of legislatures 14-18th if existing
df_combined <- df_combined %>% 
  dplyr::mutate(name = case_when(name != name_14_18 & !is.na(name_14_18) ~ name_14_18,
                                 TRUE ~ name))

# checking number of names for the 19th legislature
df_combined %>% 
  dplyr::filter(legislature == 19 & !is.na(ID)) %>% 
  dplyr::select(name, ID) %>% 
  distinct()

# saving data
df_combined %>% 
  saveRDS("01_data/df_14_19_20191223")

