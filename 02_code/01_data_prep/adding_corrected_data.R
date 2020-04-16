########################################################################
# 1.0 Packages
######################################################################## 
library(readxl)
library(dplyr)
library(magrittr)
library(lubridate)
library(zoo)
library(tm)
library(readr)
library(stringr)

########################################################################
# 2.0 Reading Data
########################################################################

# paths for data
d_paths <- c("01_data/legislature_14_presidency_row_complete.xlsx",
             "01_data/legislature_15_presidency_row_complete.xlsx",
             "01_data/legislature_16_presidency_row_complete.xlsx",
             "01_data/legislature_17_presidency_row_complete.xlsx",
             "01_data/legislature_18_presidency_row_complete.xlsx")

# Used functions can be found in helper_functions_data.R
speeches_14 <- readxl::read_xlsx("01_data/legislature_14_presidency_row_complete.xlsx")

manual_data_list <- purrr::map(d_paths, readxl::read_xlsx) %>% 
  purrr::map(.x = manual_data_list, .f= ~dplyr::select(.x, who, party, name, speak, 
                                                       tagesordnung, session, ID)) %>% 
  purrr::map(.x = ., .f = ~dplyr::mutate(.x, ID = as.double(ID)))
  
manual_df <- manual_data_list %>% 
  dplyr::bind_rows()

colnames(manual_df) <- paste(colnames(manual_df), "manual", sep = "_") %>% 
  str_replace(., "ID_manual", "ID")

df_speeches_14_18 <- readRDS(file = "01_data/df_all_speeches_14_18_int_top_ID_20190826.rds")

# joining manual data to df
df_speeches_14_18_joined <- df_speeches_14_18 %>% 
  dplyr::left_join(manual_df, by = "ID")

# check for rows of new data
df_speeches_14_18_joined %>% 
  dplyr::filter(!is.na(tagesordnung_manual)) %>% nrow()

df_speeches_14_18_joined %>% 
  dplyr::filter(who != who_manual) %>%
  dplyr::select(who, who_manual, party, party_manual, speak, speak_manual, name, name_manual, ID) %>% 
  View()

# change manual and speak, if its different
df_speeches_14_18_joined %<>% 
  dplyr::mutate(name = case_when(who != who_manual ~ name_manual,
                                 TRUE ~ name),
                speak = case_when(who != who_manual ~ speak_manual,
                                  TRUE ~ speak),
                party = case_when(who != who_manual ~ party_manual,
                                  TRUE ~ party),
                who = case_when(who != who_manual ~ who_manual,
                                TRUE ~ who))

colnames(df_speeches_14_18_joined)

# replacing NA TOPs with TOP by presidency
df_speeches_14_18_joined %<>% 
  dplyr::mutate(top_new = tagesordnung_manual,
                top_new = na.locf(top_new, fromLast = TRUE))

df_speeches_14_18_joined$top_new %>% unique() %>% length()

# saveRDS(df_speeches_14_18_joined, file = "/Users/marlonschumacher/Documents/Master/df_speeches_14_18_20191019.RDS")

############################################# STOPWORDS

stopwords_ger <- read_csv("01_data/stopwords.csv") %>% 
  dplyr::filter(!str_detect(word, "[^[:alnum:]]"))
stopwords_ger_s <- as.character(stopwords_ger$word)
df_speeches_14_18_joined <- readRDS("01_data/df_speeches_14_18_20191019.RDS")

df_main <- df_speeches_14_18_joined %>% 
  dplyr::filter(position != "Präsident") %>% 
  dplyr::mutate(nchar = nchar(speak))

nrow(df_main)

umlaute <- as.data.frame(c("dafür", "darüber", "demgegenüber", "demgemäss", "dürfen",
                           "dürft",
                           "früher", "fünf", "fünfte", "fünften", "fünfter", "fünftes",
                           "für", "gegenüber",  "hätten", "hätte", "können", "könnt",
                           "könnte", "möchte", "mögen", "möglich", "mögt", "müssen", 
                           "natürlich", "später", "über", "überhaupt", "übrigens", 
                           "während", "währenddem", "währenddessen", "wäre", "würde",
                           "würden", "zunächst", "zurück", "zwölf", "––", "–", "§", "derzeit"))
colnames(umlaute) <- "word"
umlaute <- as.character(umlaute$word)

df_main$top_new %<>%
  tolower() 

df_main$speak <- df_main$speak %>% 
  str_remove_all("[\r\n]") %>% 
  removeNumbers() %>% 
  removePunctuation() %>% 
  tolower() %>% 
  removeWords(stopwords_ger_s) %>% 
  removeWords(stopwords("german")) %>% 
  removeWords(umlaute)

# function for splitting df by given n
split_df <- function(.df, n = 10000){
  
  n_df <- nrow(.df)/n
  full_n <- floor(n_df)
  df_list <- list()
  
  for(i in 1:ceiling(n_df)){
    
    start_row <-  ((i - 1)*n)+1
    end_row <- ifelse(i != full_n+1, i*n, nrow(.df))
    
    df_list[[i]] <- .df[start_row:end_row,]
    
  }
  
  return(df_list)
}

# SENTIMENT DF [SentiWS]
library(SentimentAnalysis)
# install.packages("SnowballC")
# sentiment <- analyzeSentiment(df_speeches_14_18_joined$speak[1:10], language = "german")

neg_df <- read_tsv("01_data/SentiWS_v2.0_Negative.txt", col_names = FALSE)
names(neg_df) <- c("Wort_POS", "Wert", "Inflektionen")

neg_df %<>% 
  mutate(Wort = str_sub(Wort_POS, 1, regexpr("\\|", .$Wort_POS)-1),
         POS = str_sub(Wort_POS, start = regexpr("\\|", .$Wort_POS)+1)) %>% 
  select(-Wort_POS)

pos_df <- read_tsv("01_data/SentiWS_v2.0_Positive.txt", col_names = FALSE)
names(pos_df) <- c("Wort_POS", "Wert", "Inflektionen")

pos_df %<>% 
  mutate(Wort = str_sub(Wort_POS, 1, regexpr("\\|", .$Wort_POS)-1),
         POS = str_sub(Wort_POS, start = regexpr("\\|", .$Wort_POS)+1))

sentiment_df <- bind_rows("neg" = neg_df, "pos" = pos_df, .id = "neg_pos") %>% 
  select(neg_pos, Wort, Wert, Inflektionen, -Wort_POS) %>% 
  mutate(Wort = tolower(Wort),
         Inflektionen = tolower(Inflektionen))

# removing duplicates
sentiment_df_2 <- sentiment_df %>% 
  dplyr::group_by(neg_pos, Wort) %>% 
  dplyr::summarise(Wert = mean(Wert)) %>% 
  dplyr::ungroup()

sentiment_df_t <- sentiment_df

for(i in 1:nrow(sentiment_df_t)){
  
  words <- str_split(sentiment_df_t[i,]$Inflektionen, ",")[[1]]
  neg_pos <- rep(sentiment_df_t[i,]$neg_pos, length(words))
  Wert <- rep(sentiment_df_t[i,]$Wert, length(words))
  
  sentiment_df_t <- dplyr::bind_rows(sentiment_df_t,
                                     data.frame(neg_pos = neg_pos,
                                                Wort = words,
                                                Wert = Wert,
                                                Inflektionen = "NA",
                                                stringsAsFactors = FALSE) %>% 
                                       as_tibble())
    
}

nrow(sentiment_df)
nrow(sentiment_df_t)

sentiment_df_t %<>% 
  dplyr::select(-Inflektionen)

neg_words <- sentiment_df_t %>% 
  dplyr::filter(neg_pos == "neg")

pos_words <- sentiment_df_t %>% 
  dplyr::filter(neg_pos != "neg")


sentiment_dictionary <- SentimentDictionaryWeighted(sentiment_df_2$Wort, sentiment_df_2$Wert)
senti_dict_binary <- SentimentDictionaryBinary(pos_words$Wort, neg_words$Wort)

split_df_main <- split_df(df_main, n = 5000)

for(i in 1:length(split_df_main)){
  
  split_df_main[[i]]$sentiment <- analyzeSentiment(split_df_main[[i]]$speak, rules=list("Amplifiers"=list(ruleSentiment,
                                                                                      senti_dict_binary)))[,1]
  
  Sys.sleep(1)
  
}

# CALCULATING SENTIMENT BY GIVEN VALUE OF SentiWS
df_main_fin$sentiment_val <- 999

for(i in 1:nrow(df_main_fin)){
  
  speak_vec <- df_main_fin$speak[i] %>% str_split(., " ") %>% .[[1]] %>% .[. != ""]
  
  df_sent_calc <- data.frame(speak_word = speak_vec,
                             stringsAsFactors = FALSE) %>% 
    as_tibble()
  
  sentiment_val <- dplyr::left_join(df_sent_calc, sentiment_df_t, by = c("speak_word" = "Wort")) %>% 
    .$Wert %>% mean(na.rm = TRUE)
  
  df_main_fin$sentiment_val[i] <- sentiment_val
  
}

saveRDS(df_main_fin, file = "01_data/df_main_fin_sent_1_2_20191023.RDS")

# df_main_fin %>% summary()

df_main_fin %>%
  dplyr::filter(sentiment_val > -100) %>%
  dplyr::filter(nchar > 100 & nchar < 50000) %>% 
  ggplot(aes(x = sentiment_val, y = nchar)) +
  geom_point(alpha = 0.2)

df_main_fin %>%
  dplyr::filter(sentiment_val > -100) %>%
  dplyr::filter(nchar > 100 & nchar < 50000) %>% 
  ggplot(aes(x = sentiment, y = nchar)) +
  geom_point(alpha = 0.2)

test_vec <- df_main$speak[1] %>% str_split(., " ") %>% .[[1]] %>% .[. != ""]

test_df <- data.frame(speak_word = test_vec,
           stringsAsFactors = FALSE) %>% 
  as_tibble()

dplyr::left_join(test_df, sentiment_df_t, by = c("speak_word" = "Wort")) %>% 
  .$Wert %>% mean(na.rm = TRUE)

test_vec[test_vec != ""]

split_df_main %>% 
  bind_rows() %>% 
  colnames()

df_main_fin <- split_df_main %>% 
  dplyr::bind_rows() %>%
  dplyr::mutate(party = tolower(party),
                party = case_when(str_detect(party, "grüne") | str_detect(party, "gruene") ~ "Grüne",
                                  str_detect(party, "cdu") ~ "CDU",
                                  str_detect(party, "csu") ~ "CSU",
                                  str_detect(party, "union") ~ "Union",
                                  str_detect(party, "spd") ~ "SPD",
                                  str_detect(party, "linke") | str_detect(party, "pds") ~ "Linke",
                                  str_detect(party, "afd") ~ "AfD",
                                  str_detect(party, "fdp") ~ "FDP",
                                  TRUE ~ party))

df_main_fin %>% 
  dplyr::filter(role != "presidency") %>% 
  dplyr::filter(nchar > 70) %>% 
  summary()

df_main_fin$party %>% unique()

df_main_fin <- df_main_fin %>% 
  dplyr::mutate(party = case_when(name == "Norbert Barthle" ~ "CDU",
                                  is.na(party) | party == "" | party == "na" ~ "NA", 
                                  legislature == 18 & party == "FDP" ~ "NA",
                                  TRUE ~ party))

df_main_fin %>% 
  dplyr::filter(is.na(party) | party == "" | party == "na") %>% 
  View()

df_main_fin %>% 
  dplyr::filter(nchar > 110) %>% 
  ggplot(aes(x = date, y = sentiment, color = as.factor(party))) +
  geom_point(alpha = 0.4)
  
qplot(df_main_fin$sentiment, df_main_fin$nchar)
  
df_main_fin %>% 
  dplyr::filter(nchar < 1000) %>% 
  ggplot(aes(x = sentiment, y = nchar)) +
  geom_point(alpha = 0.2)

df_main_fin %>% 
  dplyr::filter(legislature == 18 & party == "FDP") %>% .$party %>% length()

saveRDS(df_main_fin, file = "01_data/df_main_fin_20191023.RDS")

# geom_ridges - Vereinbarte Debatte
df_main_fin %>% 
  dplyr::filter(str_detect(tagesordnung, "Vereinbarte Debatte")) %>% 
  dplyr::filter(role != "presidency" & party != "NA" & party != "fraktionslos") %>% 
  dplyr::filter(nchar > 100) %>% 
  dplyr::mutate(party = case_when(party == "CDU" | party == "CSU" ~ "Union",
                                  TRUE ~party)) %>% 
  #dplyr::filter(nchar(speak) > 50) %>% 
  ggplot(data = ., aes(x = sentiment_val, y = party)) +
  ggridges::geom_density_ridges(alpha = 0.6) +
  scale_x_continuous(limits = c(-0.1, 0.3)) +
  facet_wrap(~legislature, scale = "free")

df_main_fin %>% 
  dplyr::filter(str_detect(tagesordnung, "Vereinbarte Debatte")) %>% 
  dplyr::filter(role != "presidency" & party != "NA" & party != "fraktionslos") %>% 
  dplyr::filter(nchar > 100) %>% 
  dplyr::mutate(party = case_when(party == "CDU" | party == "CSU" ~ "Union",
                                  TRUE ~party)) %>% 
  #dplyr::filter(nchar(speak) > 50) %>% 
  ggplot(data = ., aes(x = sentiment, y = party)) +
  ggridges::geom_density_ridges(alpha = 0.6) +
  scale_x_continuous(limits = c(-0.1, 0.3)) +
  facet_wrap(~legislature, scale = "free")
