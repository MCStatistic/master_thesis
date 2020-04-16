# -----------------------------------------------------------------------------
# Project: Masterthesis
# Content: Sentimentcalculation
# Contributors: Marlon Schumacher
# Last update on: 2020-01-17
# -----------------------------------------------------------------------------

# Packages
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
library(stringi)

# DATA ----
# SentiWS & manipulation
txt_files <- c("/Users/marlonschumacher/Documents/Master/SentiWS_v2.0_Negative.txt", 
               "/Users/marlonschumacher/Documents/Master/SentiWS_v2.0_Positive.txt")

for(i in 1:length(txt_files)){
  
  df_neg_pos <- read_tsv(txt_files[i], col_names = FALSE)
  names(df_neg_pos) <- c("Wort_POS", "Wert", "Inflektionen")
  
  df_neg_pos %<>% 
    mutate(Wort = str_sub(Wort_POS, 1, regexpr("\\|", .$Wort_POS)-1),
           POS = str_sub(Wort_POS, start = regexpr("\\|", .$Wort_POS)+1)) 
  
  if(i != 1){
    
    sentiment_df <- dplyr::bind_rows("neg" = sentiment_df, "pos" = df_neg_pos, .id = "neg_pos") %>% 
      dplyr::select(neg_pos, Wort, Wert, Inflektionen, -Wort_POS) %>% 
      dplyr::mutate(Wort = tolower(Wort),
                    Inflektionen = tolower(Inflektionen))
    
  } else{
    
    sentiment_df <- df_neg_pos
    
  }
  
}

# inflection as new word
for(i in 1:nrow(sentiment_df)){
  
  words <- str_split(sentiment_df[i,]$Inflektionen, ",")[[1]]
  neg_pos <- rep(sentiment_df[i,]$neg_pos, length(words))
  Wert <- rep(sentiment_df[i,]$Wert, length(words))
  
  sentiment_df <- dplyr::bind_rows(sentiment_df,
                                     data.frame(neg_pos = neg_pos,
                                                Wort = words,
                                                Wert = Wert,
                                                Inflektionen = "NA",
                                                stringsAsFactors = FALSE) %>% 
                                       as_tibble())
  
}

# checking data again
nrow(sentiment_df)

# removing inflections
sentiment_df %<>% 
  dplyr::select(-Inflektionen)

sentiment_df <- sentiment_df %>% 
  dplyr::group_by(neg_pos, Wort) %>% 
  dplyr::summarise(Wert = mean(Wert)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(Wort != "") %>% 
  dplyr::distinct()


# German Polarity Clues ----
# Load and prepare the dictionary

posterms <- read.table(file = "GermanPolarityClues-2012/GermanPolarityClues-Positive-21042012.tsv", sep = "\t")
negterms <- read.table(file = "GermanPolarityClues-2012/GermanPolarityClues-Negative-21042012.tsv", sep = "\t")

names(posterms) <- c("term", "lemma", "POS", "sentiment", "weights", "noidea")
names(negterms) <- c("term", "lemma", "POS", "sentiment", "weights", "noidea")

# Remove superfluous whitespaces
posterms$term <- str_trim(posterms$term, side = "both")
negterms$term <- str_trim(negterms$term, side = "both")

# Add exactly one left and right 
posterms$term <- paste(" ", posterms$term, " ", sep = "")
negterms$term <- paste(" ", negterms$term, " ", sep = "")

# Ensuring correct encoding
Encoding(posterms$term) <- "UTF-8"
Encoding(negterms$term) <- "UTF-8"

# To lower
posterms$term <- tolower(posterms$term)
negterms$term <- tolower(negterms$term)


#---------------------------------------------------------------
gpc_posterms <- as.data.frame(unique(posterms$term)) %>% 
  dplyr::mutate()
names(gpc_posterms) <- "term"
gpc_posterms$term <- as.character(gpc_posterms$term)
gpc_negterms <- as.data.frame(unique(negterms$term))
names(gpc_negterms) <- "term"
gpc_negterms$term <- as.character(gpc_negterms$term)


# Scoring ----
# Dictonary: SentiWS

# reading df
df_base <- readRDS("01_data/df_complete_20200119.RDS")
df_base_v2 <- readRDS("01_data/02_prepared/df_main_v2_20200222.RDS")
df_base <- df_base_v2 %>% 
  dplyr::filter(!str_detect(tolower(role), "presidency|präsident"))

vec <- c("lol", "q", "hallo", "hello", "g")

vec[nchar(vec) != 1]



sentiment_df <- sentiment_df %>% 
  dplyr::mutate(Wort_negation = paste0("(nicht|nichts|kein|keine|keinen) ", Wort),
                negation = ifelse(neg_pos == "neg", "pos", "neg"),
                replacement = paste0("NOT_", Wort),
                negated_value = Wert*-1)

sent_pos <- sentiment_df %>% 
  dplyr::filter(neg_pos == "pos")

# creating new speech column for negation replacement
df_base$speech_clean_neg <- df_base$speech_new %>% stringr::str_squish()

# Ngations for speech
for (i in 1:nrow(sentiment_df)){
  df_base <- df_base %>% 
    dplyr::mutate(speech_clean_neg = stringr::str_replace_all(speech_clean_neg, 
                                                              sentiment_df$negation[i], 
                                                              sentiment_df$replacement[i]))
  
  svMisc::progress(i, max.value = nrow(sentiment_df), progress.bar = F)
}



# defining variables for each sentiment score
df_base$sentiws_polarity_pre_clean <- 999
df_base$sentiws_ratio_pre_clean <- 999
df_base$sentiws_sent_pre_clean <- 999

start <- Sys.time()
for(i in 1:nrow(df_base)){
  
  # creating vector of with all words of the cleaned speech
  speak_vec <-  stringr::str_split(str_squish(df_base$speech_pre_clean[i]), " ") %>% .[[1]] %>% .[. != ""]
  
  df_sent_calc <- data.frame(speak_word = speak_vec,
                             stringsAsFactors = FALSE) %>% 
    tibble::as_tibble() %>% 
    dplyr::filter(nchar(speak_word) > 1)
  
  # joining sentiment words with speech
  df_joined <- dplyr::left_join(df_sent_calc, sentiment_df, by = c("speak_word" = "Wort")) %>% 
    dplyr::filter(!is.na(neg_pos))
  
  # splitting df for positive and negative words
  df_pos <- df_joined %>% 
    dplyr::filter(neg_pos == "pos")
  df_neg <- df_joined %>% 
    dplyr::filter(neg_pos == "neg")
  
  # sum .$Wert for concidering weighting
  sum_pos <- ifelse(nrow(df_pos) != 0,sum(df_pos$Wert), 0)
  sum_neg <- ifelse(nrow(df_neg) != 0,abs(sum(df_neg$Wert)), 0)
  
  # (positive - negative) / (positive + negative)
  sentiment_polarity <- (sum_pos - sum_neg) / (sum_pos + sum_neg)
  
  # ratio (positive + negative) / all
  sentiment_ratio <- (sum_pos + sum_neg) / length(df_sent_calc$speak_word)
  
  # sentiment: (positive - negative) / all
  sentiment_sent <- (sum_pos - sum_neg) / length(df_sent_calc$speak_word)
  
  df_base$sentiws_polarity_pre_clean[i] <- sentiment_polarity
  df_base$sentiws_ratio_pre_clean[i] <- sentiment_ratio
  df_base$sentiws_sent_pre_clean[i] <- sentiment_sent
  # install.packages("svMisc")
  # svMisc::progress(i, max.value = nrow(afd_df), progress.bar = T)
  svMisc::progress(i, max.value = nrow(df_base), progress.bar = F)
  
}

end <- Sys.time()
end-start

# defining variables for each sentiment score
df_base$sentiws_polarity_pre_clean_neg <- 999
df_base$sentiws_ratio_pre_clean_neg <- 999
df_base$sentiws_sent_pre_clean_neg <- 999

start <- Sys.time()
for(i in 1:nrow(df_base)){
  
  # creating vector of with all words of the cleaned speech
  speak_vec <-  stringr::str_split(str_squish(df_base$speech_pre_clean[i]), " ") %>% .[[1]] %>% .[. != ""]
  
  df_sent_calc <- data.frame(speak_word = speak_vec,
                             stringsAsFactors = FALSE) %>% 
    tibble::as_tibble() %>% 
    dplyr::filter(nchar(speak_word) > 1)
  
  # joining sentiment words with speech
  df_joined <- dplyr::left_join(df_sent_calc, sentiment_df, by = c("speak_word" = "Wort")) %>% 
    dplyr::mutate(Wert = case_when(stringi::stri_detect(str = lag(speak_word, 1), 
                                                         regex = "(nicht|nichts|kein|keine|keinen)") ~ Wert*-1,
                                    TRUE ~ Wert),
                  neg_pos = case_when(stringi::stri_detect(str = lag(speak_word, 1), 
                                                        regex = "(nicht|nichts|kein|keine|keinen)") & neg_pos == "neg" ~ "pos",
                                      stringi::stri_detect(str = lag(speak_word, 1), 
                                                           regex = "(nicht|nichts|kein|keine|keinen)") & neg_pos == "pos" ~ "neeg",
                                   TRUE ~ neg_pos)) %>% 
    dplyr::filter(!is.na(neg_pos))
  
  # splitting df for positive and negative words
  df_pos <- df_joined %>% 
    dplyr::filter(neg_pos == "pos")
  
  df_neg <- df_joined %>% 
    dplyr::filter(neg_pos == "neg")
  
  # sum .$Wert for concidering weighting
  sum_pos <- ifelse(nrow(df_pos) != 0,sum(df_pos$Wert), 0)
  sum_neg <- ifelse(nrow(df_neg) != 0,abs(sum(df_neg$Wert)), 0)
  
  # (positive - negative) / (positive + negative)
  sentiment_polarity <- (sum_pos - sum_neg) / (sum_pos + sum_neg)
  
  # ratio (positive + negative) / all
  sentiment_ratio <- (sum_pos + sum_neg) / length(df_sent_calc$speak_word)
  
  # sentiment: (positive - negative) / all
  sentiment_sent <- (sum_pos - sum_neg) / length(df_sent_calc$speak_word)
  
  df_base$sentiws_polarity_pre_clean_neg[i] <- sentiment_polarity
  df_base$sentiws_ratio_pre_clean_neg[i] <- sentiment_ratio
  df_base$sentiws_sent_pre_clean_neg[i] <- sentiment_sent
  
  svMisc::progress(i, max.value = nrow(df_base), progress.bar = F)
  
}

end <- Sys.time()
end-start


# Plotting Purpose
recs <- df_base %>% 
  dplyr::filter(!is.na(date) & !is.na(fraktion)) %>% 
  dplyr::group_by(legislature, fraktion) %>% 
  dplyr::summarise(min = min(date),
                   max = max(date),
                   opp_gov = min(opp_gov)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(fraktion) %>% 
  dplyr::mutate(min_new = case_when(opp_gov == lag(opp_gov) ~ lag(min),
                                    TRUE ~ min),
                max_new = case_when(opp_gov == lead(opp_gov) ~ lead(max),
                                    TRUE ~ max)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(fraktion, opp_gov, min_new, max_new) %>% 
  dplyr::distinct() %>% 
  dplyr::group_by(fraktion) %>% 
  dplyr::mutate(min_new = case_when(opp_gov == lag(opp_gov) ~ lag(min_new),
                                    TRUE ~ min_new),
                max_new = case_when(opp_gov == lead(opp_gov) ~ lead(max_new),
                                    TRUE ~ max_new)) %>% 
  dplyr::ungroup() %>% 
  dplyr::distinct() %>% 
  dplyr::group_by(fraktion) %>% 
  dplyr::mutate(min_new = case_when(opp_gov == lag(opp_gov) ~ lag(min_new),
                                    TRUE ~ min_new),
                max_new = case_when(opp_gov == lead(opp_gov) ~ lead(max_new),
                                    TRUE ~ max_new)) %>% 
  dplyr::ungroup() %>% 
  dplyr::distinct() %>% 
  dplyr::group_by(fraktion) %>% 
  dplyr::mutate(min_new = case_when(opp_gov == lag(opp_gov) ~ lag(min_new),
                                    TRUE ~ min_new),
                max_new = case_when(opp_gov == lead(opp_gov) ~ lead(max_new),
                                    TRUE ~ max_new)) %>% 
  dplyr::ungroup() %>% 
  dplyr::distinct() %>% 
  dplyr::group_by(fraktion) %>% 
  dplyr::mutate(min_new = case_when(opp_gov == lag(opp_gov) ~ lag(min_new),
                                    TRUE ~ min_new),
                max_new = case_when(opp_gov == lead(opp_gov) ~ lead(max_new),
                                    TRUE ~ max_new)) %>% 
  dplyr::distinct()


# PLOTTING SENTIMENT POLARITY ----
df_base %>% 
  dplyr::filter(!is.na(date)) %>% 
  dplyr::filter(!is.na(fraktion)) %>% 
  dplyr::filter(sentiws_polarity_pre_clean_neg > -0.95 & sentiws_polarity_pre_clean_neg < 0.95) %>% 
  dplyr::filter(nchar > 400) %>% 
  # dplyr::filter(thema != "zSonstiges") %>%
  # dplyr::filter(thema == "Energie") %>% 
  dplyr::mutate(legislature = paste0(as.character(legislature), ". Bundestag")) %>% 
  #dplyr::filter(nchar(speak) > 50) %>% 
  ggplot(data = ., aes(x = date, y = sentiws_polarity_pre_clean_neg, col = fraktion)) + # sentiws_polarity_pre_clean
  geom_smooth(fill = "grey") + #, method = "loess", span = 0.2) +
  gghighlight::gghighlight() +
  geom_rect(data=recs,
            aes(xmin=min_new, xmax=max_new, ymin=-Inf, ymax=Inf, fill = opp_gov), 
            inherit.aes = F, alpha = 0.2) +
  # scale_x_continuous(limits = c(-0.25, 0.2)) +
  scale_color_manual(values = c("Grüne" = "#50822E", "Linke" = "#B61C3E", 
                               "Union" = "#32372C", "SPD" = "#E3000F", "FDP" = "#FFD600",
                               "AfD" = "#009ee0")) +
  scale_fill_manual(values = c("Opposition" = "#F8766D", "Government"= "#00BFC4")) +
  theme_bw() +
  # theme(legend.position = "bottom") +
  labs(x = NULL, y = "Sentiment Polarity", fill = NULL, color = NULL, title = NULL,
       caption = NULL) +
  facet_wrap(~fraktion, scale = "free") +
  theme(legend.position = "bottom",
        text = element_text(size = 12, family = "LM Roman 10")) +
  geom_jitter(alpha = 0.01) 




# Dictonary: GPC [GermanPolarityClues - A Lexical Resource for German Sentiment Analysis]
# Download: http://www.ulliwaltinger.de/sentiment/ 
head(df_base)
df_base$gpc_polarity_pre_clean_neg <- 999
df_base$gpc_ratio_pre_clean_neg <- 999
df_base$gpc_sent_pre_clean_neg <- 999

gpc_negterms <- gpc_negterms %>% 
  dplyr::mutate(neg_pos = "neg",
                term = stringr::str_squish(term)) %>% 
  tibble::as_tibble()

gpc_posterms <- gpc_posterms %>% 
  dplyr::mutate(neg_pos = "pos",
                term = stringr::str_squish(term)) %>% 
  tibble::as_tibble()

# binding rows of positive and negative gpc_df
gpc_terms <- dplyr::bind_rows(gpc_negterms, gpc_posterms) %>% 
  dplyr::mutate(score = ifelse(neg_pos == "neg", -1, 1))

# looping over all rows and calculate sentiment score for each speech
start <- Sys.time()
for(i in 1:nrow(df_base)){
  
  # creating vector of with all words of the cleaned speech
  speak_vec <-  stringr::str_split(str_squish(df_base$speech_pre_clean[i]), " ") %>% .[[1]] %>% .[. != ""]
  
  df_sent_calc <- data.frame(speak_word = speak_vec,
                             stringsAsFactors = FALSE) %>% 
    tibble::as_tibble() 
  
  # joining sentiment words with speech
  df_joined <- dplyr::left_join(df_sent_calc, gpc_terms, by = c("speak_word" = "term")) %>% 
    dplyr::mutate(score = case_when(stringi::stri_detect(str = lag(speak_word, 1), 
                                                         regex = "(nicht|nichts|kein|keine|keinen)") ~ score*-1,
                                    TRUE ~ score)) %>% 
    dplyr::filter(!is.na(neg_pos))
  
  # splitting df for positive and negative words
  df_pos <- df_joined %>% 
    dplyr::filter(score == 1)
  
  df_neg <- df_joined %>% 
    dplyr::filter(score == -1)
  
  # n of rows -> no weighting included
  sum_pos <- nrow(df_pos)
  sum_neg <- nrow(df_neg)
  
  # polarity:  (positive - negative) / (positive + negative)
  sentiment_polarity <- (sum_pos - sum_neg) / (sum_pos + sum_neg)
  
  # ratio:     (positive + negative) / all
  sentiment_ratio <- (sum_pos + sum_neg) / length(df_sent_calc$speak_word)
  
  # sentiment: (positive - negative) / all
  sentiment_sent <- (sum_pos - sum_neg) / length(df_sent_calc$speak_word)
  
  df_base$gpc_polarity_pre_clean_neg[i] <- sentiment_polarity
  df_base$gpc_ratio_pre_clean_neg[i] <- sentiment_ratio
  df_base$gpc_sent_pre_clean_neg[i] <- sentiment_sent
  # install.packages("svMisc")
  # svMisc::progress(i, max.value = nrow(afd_df), progress.bar = T)
  svMisc::progress(i, max.value = nrow(df_base), progress.bar = F)
  
}
end <- Sys.time()
end-start

# saving data
saveRDS(df_base, "01_data/02_prepared/df_main_v2_20200224_3.RDS")
