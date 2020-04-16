# -----------------------------------------------------------------------------
# Project: Masterthesis
# Content: Extracting Speeches for 19. Legislature
# Contributors: Marlon Schumacher
# Last update on: 2020-03-01
# -----------------------------------------------------------------------------

# Packages ----
library(magrittr)
library(xml2)
library(tidyverse)
library(dplyr)
library(stringr)
library(tibble)

# READING XML FILE ----
pp19_files <- list.files("01_data/01_raw/pp19_data", full.names = T)

readed_xml <- xml2::read_xml(pp19_files[2])
xml_list2 <- xml2::as_list(readed_xml)

# NOTE
# xml list object is highly nested
# tibble could be also used (df with columns containing list objects...)

# function for TOP extraction ----
get_top <- function(xml_list2){
  
  titel <- c()
  inhalt <- c()
  
  for(i in 1:length(xml_list2$dbtplenarprotokoll$vorspann$inhaltsverzeichnis)){
    
    if(!is.null(xml_list2$dbtplenarprotokoll$vorspann$inhaltsverzeichnis[[i]]$`ivz-block-titel`) &
       !is.list(xml_list2$dbtplenarprotokoll$vorspann$inhaltsverzeichnis[[i]]$`ivz-eintrag`$`ivz-eintrag-inhalt`[[1]])){
      
      titel <- c(titel, xml_list2$dbtplenarprotokoll$vorspann$inhaltsverzeichnis[[i]]$`ivz-block-titel`[[1]])
      inhalt <- c(inhalt, ifelse(is.null(xml_list2$dbtplenarprotokoll$vorspann$inhaltsverzeichnis[[i]]$`ivz-eintrag`$`ivz-eintrag-inhalt`[[1]]), 
                                 "no content", 
                                 xml_list2$dbtplenarprotokoll$vorspann$inhaltsverzeichnis[[i]]$`ivz-eintrag`$`ivz-eintrag-inhalt`[[1]]))
    }
    
    df <- data.frame(titel = titel,
                     inhalt = inhalt,
                     stringsAsFactors = F) %>% 
      as_tibble()
    
    
  }
  df <- df %>% 
    dplyr::mutate(titel = stringr::str_remove(titel, ":"))
  
  return(df)
}

# Testing
# get_top(xml_list2)


# Function for extracting speech ----
extract_speeches <- function(xml_list2){
  
  date_value <- attributes(xml_list2$dbtplenarprotokoll$vorspann$kopfdaten$veranstaltungsdaten$datum)
  date_value <- date_value$date
  speech <- c()
  vorname <- "Vorname"
  nachname <- "Nachname"
  fraktion <- "Frak"
  kommentar <- "kommentar"
  error_g <- "nope"
  
  df_base <- data.frame(vorname = "vorname",
                        nachname = "nachname",
                        fraktion = "fraktion",
                        rolle = "rolle",
                        rolle_lang = "rolle_lang",
                        rede = "rede",
                        top = "top",
                        speech_id = "id",
                        error = "error",
                        stringsAsFactors = F)
  
  for(h in 1:length(xml_list2$dbtplenarprotokoll$sitzungsverlauf)){
    
    attributes_s <- attributes(xml_list2$dbtplenarprotokoll$sitzungsverlauf[h])
    
    if(attributes_s$names == "tagesordnungspunkt"){
      
      
      for (i in seq_along(xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk)){
  
      if(is.null(xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede)){
        next
      } else{
        
        for (j in seq_along(xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede)){
          if(j == 1){
            
            tryCatch({
            vorname <- ifelse(is.null(xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p$redner$name$vorname[[1]]),
                              "no information",
                              xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p$redner$name$vorname[[1]])
            nachname <- ifelse(is.null(xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p$redner$name$nachname[[1]]),
                               "no information",
                               xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p$redner$name$nachname[[1]])
            
            fraktion <- ifelse(is.null(xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p$redner$name$fraktion[[1]]),
                               "no information in speech",
                               xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p$redner$name$fraktion[[1]])
            
            },
            error=function(cond){
              message(paste0("Something went wrong with the speeker information!"))
              message(paste0("h = ", h, " i = ", i, " j = ", j))
              error_g <- "occured"
              vorname <- "error"
              nachname <- "error"
              fraktion <- "error"
            })
            
            tryCatch({
              
              rolle <- ifelse(is.null(xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p$redner$name$rolle$rolle_lang[[1]]) |
                              is.list(xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p$redner$name$rolle$rolle_lang[[1]]),
                              "no role",
                              xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p$redner$name$rolle$rolle_lang[[1]])
              
            },
            error=function(cond) {
              message(paste0("xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p$redner$name$rolle$rolle_lang[[1]] ", "h = ", h, " i = ", i, " j = ", j))
              error_g <- "occured"
              rolle <- "no role"})
            
            tryCatch({
              
              rolle_lang <- ifelse(is.null(xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p$redner$name$rolle$rolle_kurz[[1]]) |
                                   is.list(xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p$redner$name$rolle$rolle_kurz[[1]]),
                                   "no role",
                                   xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p$redner$name$rolle$rolle_kurz[[1]])
              
            },
            error=function(cond) {
              message(paste0("xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p$redner$name$rolle$rolle_kurz[[1]] ", "h = ", h, " i = ", i, " j = ", j))
              error_g <- "occured"
              rolle_lang <- "no role"})
            
            att_speech <- attributes(xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede)
            speech_id <- ifelse(is.null(att_speech$id), "missing", att_speech$id)
            
            tryCatch({
              
              if(!is.null(xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p[[2]])){
                
                speech <- c(xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p[[2]])
                
              } else{speech <- c("No p2:")}
              
            },
            error=function(cond) {
              message(paste0("xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p[[2]]: ", "h = ", h, " i = ", i, " j = ", j))
              error_g <- "occured"
              speech <- c("No p2:")})
            
          } else{
            
              tryCatch({
                if(!is.null(xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunkt[i]$rede[j]$p[[1]])){
                speech <- c(speech, xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk[i]$rede[j]$p[[1]])
                } else if(!is.null(xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunkt[i]$rede[j]$kommentar[[1]])){
                  
                  kommentar <- c(kommentar, xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunkt[i]$rede[j]$kommentar[[1]])
                  
                }
              },
              error=function(cond) {
                message(paste0("xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunkt[i]$rede[j]$p[[1]]:"))
                message(paste0("h = ", h, " i = ", i, " j = ", j))
                error_g <- "occured"
                speech <- c(speech, NA)})
            
            }
            
          }
          
        }
        
        att_top <- attributes(xml_list2$dbtplenarprotokoll$sitzungsverlauf[h]$tagesordnungspunk)
        top <- att_top$`top-id`
        
        speech <- paste(speech, collapse = " ")
        kommentar <- paste(kommentar, collapse = " ")
        
        tryCatch({
          
          df_base <- dplyr::bind_rows(df_base,
                                      data.frame(vorname = vorname,
                                                 nachname = nachname,
                                                 fraktion = fraktion,
                                                 rolle = rolle,
                                                 rolle_lang = rolle_lang,
                                                 rede = speech,
                                                 top = top,
                                                 speech_id = speech_id,
                                                 error = error_g,
                                                 stringsAsFactors = F))
          
        },
        error=function(cond) {
          df_base <- df_base
          message(paste0("something went wrong with df"))
          message(paste0("h = ", h, " i = ", i, " j = ", j))
          message("original error:")
          message(cond)
          error_g <- "occured"})
        
        error_g <- "nope"
        kommentar <- c()
        vorname <- c()
        nachname <- c()
        fraktion <- c()
      }
      
      }
    }
  df_base$date <- date_value
  df_base <- as_tibble(df_base)
  return(df_base)
}

# Test 
# extract_speeches(xml_list2)

# Extract speech for each file
for(i in 1:length(pp19_files)){
  
  readed_xml <- xml2::read_xml(pp19_files[i])
  xml_list <- xml2::as_list(readed_xml)

  df <- extract_speeches(xml_list)
  tops <- get_top(xml_list) %>% 
    dplyr::mutate(titel = stringr::str_replace_all(titel,
                                                   "Zusatztagesordnungspunkt", 
                                                   "Zusatzpunkt"))
  
  df_base <- dplyr::bind_rows(df_base,
                             dplyr::left_join(df, tops, by = c("top" = "titel")))
  
  print(paste0("Status: ", i/length(pp19_files), " i = ", i))
  
}

# labeling for first inspection
df_base %<>% 
  dplyr::mutate(inhalt = case_when(str_detect(top, "Einzelplan") ~ "Haushaltsplan",
                                   str_detect(top, "Geschaeftsordnung") ~ "Geschäftsordnung",
                                   TRUE ~ inhalt)) #%>% 
  
# reading stammdaten
stamm_df3 <- readRDS("stammdaten_14_19_20191025.RDS")
stamm_df_18 <- stamm_df3 %>% 
  #dplyr::filter(str_detect(NACHNAME, "Bentele")) %>% View() %>% 
  dplyr::mutate(VORNAME = case_when(
    str_detect(VORNAME, "Alexander") & NACHNAME == "Lambsdorff" ~ "Alexander Graf",
    str_detect(VORNAME, "Wolfgang") & NACHNAME == "Schäuble" ~ "Wolfgang",
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
  dplyr::mutate(f_name_stamm = paste0(VORNAME, " ", NACHNAME))

# CHECK JOINING!!!
df_fin_19 <- df_base %>% 
  mutate(fraktion = case_when(
    str_detect(fraktion, "GRÜNE|Grüne") ~ "Grüne",
    stringr::str_detect(fraktion, "AfD") ~ "AfD",
    stringr::str_detect(fraktion, "LINKE") ~ "Linke",
    stringr::str_detect(fraktion, "FDP") ~ "FDP",
    stringr::str_detect(fraktion, "CDU") ~ "CDU/CSU",
    stringr::str_detect(fraktion, "CSU") ~ "CDU/CSU",
    # nachname == "Petry" & vorname == "Frauke" ~ "AfD",
    TRUE ~ fraktion)) %>% 
  dplyr::mutate(vorname = str_remove(vorname, "(\\b[A-Z]\\.?+ )|( [A-Z]\\.?+\\b)")) %>% 
  dplyr::mutate(f_name1 = paste0(vorname, " ", nachname)) %>% 
  dplyr::mutate(f_name1 = ifelse(f_name1 == "Dr. Wolfgang Schäuble", "Wolfgang Schäuble", 
                                 ifelse(f_name1 == "Albert Weiler", "Albert H. Weiler", f_name1))) %>% 
  dplyr::mutate(f_name1 = case_when(f_name1 == "Katharina Willkomm" ~ "Katharina Kloke", 
                                    f_name1 == "Alterspräsident Dr. Hermann Otto Solms" ~ "Hermann Otto Solms",
                                    f_name1 == "Hans-Georg von der Marwitz" ~ "Hans-Georg Marwitz",
                                    f_name1 == "Heidrun Bluhm-Förster" ~ "Heidrun Bluhm",
                                    f_name1 == "Kees de Vries" ~ "Kees Vries",
                                    f_name1 == "Thomas de Maizière" ~ "Thomas Maizière",
                                    f_name1 == "Christian Freiherr von Stetten" ~ "Christian Stetten",
                                    f_name1 == "Eberhardt Alexander Gauland" ~ "Alexander Gauland",
                                    f_name1 == "Konstantin Elias Kuhle" ~ "Konstantin Kuhle",
                                    f_name1 == "Joana Eleonora Cotar" ~ "Joana Cotar",
                                    TRUE ~ f_name1)) %>% 
  dplyr::left_join(stamm_df_18, by = c("f_name1" = "f_name_stamm")) %>% 
 # dplyr::left_join(stamm_df_18, by = c("nachname"="NACHNAME", "vorname"="VORNAME")) %>% 
  dplyr::filter(vorname != "vorname" & nachname != "nachname") %>% 
  # dplyr::filter(is.na(PARTEI)) %>% 
  dplyr::mutate(f_name2 = paste0(vorname, " ", nachname)) %>% 
  dplyr::mutate(GEBURTSORT = case_when(f_name1 == "Svenja Schulze" ~ "Düsseldorf",
                                       f_name1 == "Franziska Giffey" ~ "Sülke",
                                       f_name1 == "Isabel Mackensen" ~ "Schwetzingen",
                                       TRUE ~ GEBURTSORT)) %>% 
  dplyr::mutate(GEBURTSDATUM = case_when(f_name1 == "Svenja Schulze" ~ "1968",
                                       f_name1 == "Franziska Giffey" ~ "1978",
                                       f_name1 == "Isabel Mackensen" ~ "1986",
                                       TRUE ~ GEBURTSDATUM)) %>% 
  dplyr::mutate(GESCHLECHT = case_when(f_name1 == "Svenja Schulze" ~ "weiblich",
                                         f_name1 == "Franziska Giffey" ~ "weiblich",
                                       f_name1 == "Isabel Mackensen" ~ "weiblich",
                                         TRUE ~ GESCHLECHT)) # %>% 
  # Checking for NAs
  # dplyr::filter(is.na(GEBURTSDATUM)) %>%
  # dplyr::group_by(f_name1) %>% 
  # dplyr::summarise(n = n()) %>% 
  # dplyr::arrange(desc(n))
  # dplyr::filter(fraktion != "no information in speech" & fraktion != PARTEI) %>% # PARTY RECODEN!!!!
  # saveRDS(file = "/Users/marlonschumacher/Documents/Master/df_pp19_20191201.RDS")

df_fin_19 %>% 
  saveRDS(file = "01_data/02_prepared/df_pp19_20200222.RDS")


saveRDS(df_base, file = "/Users/marlonschumacher/Documents/Master/df_pp19_20191123.RDS")
