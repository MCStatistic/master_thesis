# # -----------------------------------------------------------------------------
# Project: Masterthesis
# Content: Word Frequencies for Election programs
# Contributors: Marlon Schumacher
# Last update on: 2020-03-15
# -----------------------------------------------------------------------------

# Packages
library(tidytext)
library(dplyr)
filepath <- "01_data/01_raw/Wahlprogramme/"
years <- c("2017")

files <- paste0(filepath, years) %>% 
  list.files(full.names = T)

text_results <- purrr::map(files,
                           .f = ~paste0(pdftools::pdf_text(.x), collapse = " ")) %>% 
  unlist()

# check if length equals number of files
length(text_results) == length(files)

get_party <- function(string){
  
  x <- tolower(string)
  if(str_detect(x, "afd")){
    return("AfD")
  }
  if(str_detect(x, "spd")){
    return("SPD")
  }
  if(str_detect(x, "linke")){
    return("Lnke")
  }
  if(str_detect(x, "union")){
    return("Union")
  }
  if(str_detect(x, "gruene")){
    return("Grüne")
  }
  if(str_detect(x, "fdp")){
    return("FDP")
  }
}


# text pre processing
# additional stop words
umlaute <- as.character(c("dafür", "darüber", "demgegenüber", "demgemäss", "dürfen",
                          "dürft", "früher", "fünf", "fünfte", "fünften", "fünfter", "fünftes",
                          "für", "gegenüber",  "hätten", "hätte", "können", "könnt",
                          "könnte", "möchte", "mögen", "möglich", "mögt", "müssen", 
                          "natürlich", "später", "über", "überhaupt", "übrigens", 
                          "während", "währenddem", "währenddessen", "wäre", "würde",
                          "programm", "beschlossen", 
                          
                          # party related words
                          "fdp", "afd", "spd", "cdu", "csu", "fraktion", "bundestagsfraktion",
                          "zukunft mut", "grün wählt", 
                          "wahlprogramm", "www", "de", "bundestagswahl", "bundesparteitag",
                          "bundestagswahlprogramm", "bundesdelegiertenkonferenz", "linke",
                          "langfassung", "bundestagswahlparteitag", "grunen", "grünen", "bündnis",
                          "freie demokraten", "bürgerinnen", "bürger",
                          
                          # high frequent words which should not be considered for ML
                          "herren", "frauen", "herr", "frau", "prsäident", "kollegen", "kollege", "kolleginnen",
                          
                          # special characters
                          "––", "–", "§", "-"))

# additional stopwords
stopwords_ger <- readr::read_csv("/Users/marlonschumacher/Documents/Master/stopwords.csv") %>% 
  dplyr::filter(!stringr::str_detect(word, "[^[:alnum:]]")) %>% .$word

text_results <- text_results %>% 
  tolower() %>% 
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
  stringr::str_squish() %>% 
  tm::removeWords(stopwords_ger) %>% 
  tm::removeWords(tm::stopwords("german")) %>% 
  tm::removeWords(umlaute) %>% 
  SnowballC::wordStem(language = "german") %>% 
  tm::removeWords(umlaute) %>% 

# creating tibble from text document
# all programs can be transformed into one tibble
parties <- unlist(purrr::map(files, get_party))
text_df <- tibble(party = parties, text = text_results)

# Unigrams
unigram_list <-text_df %>% 
  tidytext::unnest_tokens(word, text) %>% 
  dplyr::group_split(party, keep = TRUE) %>% 
  # summarise the word counts
  purrr::map(.x = .,
             .f = ~count(.x, word, sort = TRUE)) %>% 
  # assigning parties to each data frame after summarise
  purrr::map2(.x = .,
              .y = parties,
              .f = ~dplyr::mutate(.x, party = .y))

i <- 1
for (i in 1:length(unigram_list)){
  
  unigram_list[[i]] <- unigram_list[[i]] %>% 
    dplyr::filter(!word %in% c("grune", "innen", "ge", "zudem")) %>% 
    head(15) %>% 
    dplyr::mutate(word = paste0(word, " (", as.character(n), ")")) %>% 
    dplyr::select(1)
  
  colnames(unigram_list[[i]]) <- parties[i]
  
}

# creating Latex table
dplyr::bind_cols(unigram_list) %>% 
  xtable::xtable()


# Bigrams ----
bigram_list <- text_df %>% 
  tidytext::unnest_tokens(output = bigram, input = text, token = "ngrams", n = 2) %>% 
  dplyr::group_split(party, keep = TRUE) %>% 
  # summarise the bigram counts
  purrr::map(.x = .,
             .f = ~count(.x, bigram, sort = TRUE)) %>% 
  # assigning parties to each data frame after summarise
  purrr::map2(.x = .,
              .y = parties,
              .f = ~dplyr::mutate(.x, party = .y)) %>% 
  purrr::map(.x = .,
             .f = ~head(.x, 20))
