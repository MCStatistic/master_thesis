# DESCRIPTIVE VISUALIZATION ---------------------------
df_match$speech_new[1]

df_sent <- readRDS(file = "01_data/df_speech_sent_14_19_20191228.RDS")
df_sent_fin <- readRDS(file = "01_data/df_speech_sent_fin_14_19_20191229.RDS")

df_sent <- df_sent %>% 
  dplyr::filter(legislature != 18) %>% 
  dplyr::bind_rows(df_sent %>% 
                     dplyr::filter(legislature == 18 & party != "FDP"))

# SENTIMENT OF ALL LABELED DEBATES
df_base %>% 
  dplyr::filter(!is.na(sentiws_polarity_pre_clean)) %>% 
  # dplyr::filter(role != "presidency" & party != "NA" & party != "fraktionslos" & role != "Präsident") %>% 
  dplyr::filter(nchar > 300) %>% 
  # dplyr::filter(thema != "zSonstiges") %>%
  # dplyr::filter(thema == "Energie") %>% 
  dplyr::mutate(party = case_when(party == "CDU" | party == "CSU" ~ "Union",
                                  TRUE ~party)) %>% 
  dplyr::filter(party %in% c("SPD", "Union", "AfD", "FDP", "Linke", "Grüne")) %>% 
  dplyr::mutate(legislature = paste0(as.character(legislature), ". Bundestag")) %>% 
  #dplyr::filter(nchar(speak) > 50) %>% 
  ggplot(data = ., aes(x = sentiws_polarity_pre_clean, y = fraktion, fill = party)) +
  ggridges::geom_density_ridges(alpha = 0.8) +
  # scale_x_continuous(limits = c(-0.25, 0.2)) +
  scale_fill_manual(values = c("Grüne" = "#50822E", "Linke" = "#B61C3E", 
                               "Union" = "#32372C", "SPD" = "#E3000F", "FDP" = "#FFD600",
                               "AfD" = "#009ee0")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Sentiment", y = "Partei", fill = NULL, title = "Sentiments: All labeled Debates",
       caption = "Sentiment Value by SentiWS") +
  facet_wrap(~legislature, scale = "free")


# SENTIMENT BY TOPIC AND PARTY
df_base %>% 
  dplyr::filter(role != "presidency" & role != "Präsident") %>% 
  # dplyr::filter(nchar > 200 & nchar < 10000) %>% 
  dplyr::filter(!is.na(thema_cap)) %>%
  # dplyr::filter(thema == "Energie") %>% 
  # dplyr::mutate(party = case_when(party == "CDU" | party == "CSU" ~ "Union",
  #                                 TRUE ~party)) %>% 
  dplyr::filter(fraktion %in% c("SPD", "Union", "AfD", "FDP", "Linke", "Grüne")) %>% 
  dplyr::mutate(legislature = paste0(as.character(legislature), ". Bundestag")) %>% 
  #dplyr::filter(nchar(speak) > 50) %>% 
  ggplot(data = ., aes(x = sentiws_ratio_pre_clean, y = fraktion, fill = fraktion)) +
  ggridges::geom_density_ridges(alpha = 0.8) +
  # scale_x_continuous(limits = c(-0.25, 0.2)) +
  scale_fill_manual(values = c("Grüne" = "#50822E", "Linke" = "#B61C3E", 
                               "Union" = "#32372C", "SPD" = "#E3000F", "FDP" = "#FFD600",
                               "AfD" = "#009ee0")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Sentiment", fill = NULL) +
  facet_wrap(~thema_cap, scale = "free")

df_base %>% 
  dplyr::filter(role != "presidency" & role != "Präsident") %>% 
  # dplyr::filter(nchar > 200 & nchar < 10000) %>% 
  dplyr::filter(!is.na(thema_cap)) %>%
  # dplyr::filter(thema == "Energie") %>% 
  # dplyr::mutate(party = case_when(party == "CDU" | party == "CSU" ~ "Union",
  #                                 TRUE ~party)) %>% 
  dplyr::filter(fraktion %in% c("SPD", "Union", "AfD", "FDP", "Linke", "Grüne")) %>% 
  ggplot(data = ., aes(x = fraktion , y = sentiws_polarity_pre_clean, fill = fraktion)) +
  geom_boxplot() +
  # scale_x_continuous(limits = c(-0.25, 0.2)) +
  scale_fill_manual(values = c("Grüne" = "#50822E", "Linke" = "#B61C3E", 
                               "Union" = "#32372C", "SPD" = "#E3000F", "FDP" = "#FFD600",
                               "AfD" = "#009ee0")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Sentiment", fill = NULL) +
  facet_wrap(~thema_cap, scale = "free")

df_base$gamma %>% summary()
# LDA Labels
df_base %>%
  dplyr::filter(gamma > 0.2) %>% 
  dplyr::filter(role != "presidency" & role != "Präsident") %>% 
  dplyr::filter(nchar > 400) %>% 
  dplyr::filter(!is.na(lda_label) & !stringr::str_detect(lda_label, "NA")) %>%
  # dplyr::filter(thema == "Energie") %>% 
  # dplyr::mutate(party = case_when(party == "CDU" | party == "CSU" ~ "Union",
  #                                 TRUE ~party)) %>% 
  dplyr::filter(fraktion %in% c("SPD", "Union", "AfD", "FDP", "Linke", "Grüne")) %>% 
  ggplot(data = ., aes(x = fraktion , y = sentiws_polarity_pre_clean, fill = fraktion)) +
  geom_boxplot(width = 0.8) +
  # scale_x_continuous(limits = c(-0.25, 0.2)) +
  scale_fill_manual(values = c("Grüne" = "#50822E", "Linke" = "#B61C3E", 
                               "Union" = "#32372C", "SPD" = "#E3000F", "FDP" = "#FFD600",
                               "AfD" = "#009ee0")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Sentiment", fill = NULL) +
  facet_wrap(~lda_label, scale = "free")





df_complete_rowID %>% 
dplyr::filter(role != "Präsident" & party != "NA" & party != "fraktionslos" & role != "Präsident") %>% 
  dplyr::filter(nchar > 200 & legislature == 19) %>% 
  dplyr::filter(thema != "zSonstiges") %>%
  # dplyr::filter(thema == "Energie") %>% 
  dplyr::filter(party_2 %in% c("SPD", "Union", "AfD", "FDP", "Linke", "Grüne")) %>% 
  dplyr::mutate(legislature = paste0(as.character(legislature), ". Bundestag")) %>% 
  ggplot(data = ., aes(x = cogproc, y = party_2, fill = party_2)) +
  ggridges::geom_density_ridges(alpha = 0.8) +
  # scale_x_continuous(limits = c(20, 50)) +
  scale_fill_manual(values = c("Grüne" = "#50822E", "Linke" = "#B61C3E", 
                               "Union" = "#32372C", "SPD" = "#E3000F", "FDP" = "#FFD600",
                               "AfD" = "#009ee0")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Sentiment", y = "Partei", fill = NULL, title = "Sentiments: All labeled Debates",
       caption = "Sentiment Value by SentiWS") +
  facet_wrap(~thema, scale = "free")


df_complete_rowID %>% 
  dplyr::filter(role != "Präsident" & party != "NA" & party != "fraktionslos" & role != "Präsident") %>% 
  dplyr::filter(nchar > 200) %>% 
  dplyr::filter(thema != "zSonstiges") %>%
  # dplyr::filter(thema == "Energie") %>% 
  dplyr::filter(party_2 %in% c("SPD", "Union", "AfD", "FDP", "Linke", "Grüne")) %>% 
  dplyr::mutate(legislature = paste0(as.character(legislature), ". Bundestag")) %>% 
  ggplot(data = ., aes(x = cogproc, y = sentiment_mean, col = as.factor(populism))) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = "lm")

# 
df_complete_rowID %>% 
  dplyr::filter(role != "Präsident" & party != "NA" & party != "fraktionslos" & role != "Präsident") %>% 
  dplyr::filter(nchar > 200) %>% 
  dplyr::filter(thema != "zSonstiges") %>%
  # dplyr::filter(thema == "Energie") %>% 
  dplyr::filter(party_2 %in% c("SPD", "Union", "AfD", "FDP", "Linke", "Grüne")) %>% 
  dplyr::mutate(legislature = paste0(as.character(legislature), ". Bundestag")) %>% 
  ggplot(data = ., aes(x = as.factor(populism), y = cogproc)) +
  geom_boxplot() +
  ggpubr::stat_compare_means( aes(label = ..p.signif..), 
                      label.x = 1.5, label.y = 40) +
  ggpubr::stat_compare_means(method = "t.test")
  
# mean for cc, cogproc, sentiment_mean
df_complete_rowID %>% 
    dplyr::filter(role != "Präsident" & party != "NA" & party != "fraktionslos" & role != "Präsident") %>% 
    dplyr::filter(nchar > 200) %>% 
    dplyr::filter(thema != "zSonstiges") %>%
    # dplyr::filter(thema == "Energie") %>% 
    dplyr::filter(party_2 %in% c("SPD", "Union", "AfD", "FDP", "Linke", "Grüne")) %>% 
    dplyr::group_by(populism) %>% 
    dplyr::summarise(cc = mean(cc),
                     cogproc = mean(cogproc),
                     sentiment_mean = mean(sentiment_mean, na.rm = TRUE))


library("tm")
data("crude")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Rgraphviz")

BigramTokenizer <- function(x){
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  }

tdm <- TermDocumentMatrix(crude, control = list(tokenize = BigramTokenizer))

plot(tdm, terms = findFreqTerms(tdm, lowfreq = 6)[1:25], corThreshold = 0.5)

install.packages("tidytext")
tidy_text_data <- tidytext::tidy(tdm)
tidy_text_data %>% 
  count(term, sort = TRUE)

additonalwords <- c("damen", "herren", "präsident", "geehrter", "geehrte", "kollege", "kolleginnen", "kollegin",
                    "redner", "rednerin", "minister", "afd", "grüne", "union", "cdu", "csu")

df_complete_rowID$speech_new <- df_complete_rowID$speech_new %>% 
  removeWords(additonalwords)


df_complete_rowID$thema %>% unique()

df_adjusted_fin <- df_adjusted_fin %>% 
  # dplyr::mutate(thema == ifelse(thema == "Kriminalität & Familienprobleme", "Gesetz & Kriminalität", thema)) %>% 
  dplyr::mutate(topic_id = case_when(
    thema == "Makroökonomie" ~ 1,
    thema == "Bürgerrechte" ~ 2,
    thema == "Gesundheit" ~ 3,
    thema == "Agrarwirtschaft" ~ 4,
    thema == "Arbeit & Beschäftigung" ~ 5,
    thema == "Bildung" ~ 6,
    thema == "Umwelt" ~ 7,
    thema == "Energie" ~ 8,
    thema == "Einwanderung" ~ 9,
    thema == "Transport" ~ 10,
    thema == "Kriminalität & Familienprobleme" ~ 12,
    thema == "Soziale Wohlfahrt" ~ 13,
    thema == "Gemeindeentwicklung & Wohnungsprobleme" ~ 14,
    thema == "Banken, Finanzen & Binnenhandel" ~ 15,
    thema == "Verteidigung" ~ 16,
    thema == "Technologie & Kommunikation" ~ 17,
    thema == "Außenhandel" ~ 18,
    thema == "Internationale Angelegenheiten & Auslandshilfe" ~ 19,
    thema == "Regierungsoperationen" ~ 20,
    thema == "Öffentliche Flächen" ~ 21,
    thema == "Kultur" ~ 23,
    TRUE ~ 99
  )) %>% 
  dplyr::mutate(thema = case_when(topic_id == 12 ~ "Gesetz & Kriminalität",
                                  TRUE ~ thema)) %>% 
  dplyr::mutate(topic_id_chr = ifelse(topic_id < 10, 
                                      paste0("0", as.character(topic_id)),
                                      as.character(topic_id)))

df_adjusted_fin %>% 
  dplyr::select(topic_id, topic_id_chr, thema) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(topic_id) %>% 
  as.data.frame()


agenda_id_df <- df_adjusted_fin %>% 
  dplyr::select(date, agenda_top) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(agenda_id = dplyr::row_number())

df_adjusted_fin <- df_adjusted_fin %>% 
  dplyr::left_join(agenda_id_df, by = c("date", "agenda_top"))

saveRDS(df_adjusted_fin, file = "01_data/df_complete_20200114.RDS")

df_adjusted_fin %>% 
  nrow()

df_adjusted_fin_corrected <- readRDS("01_data/df_complete_20200114_corrected.RDS")
df_adjusted_fin_corrected %>% 
  dplyr::mutate(topic_id = case_when(thema == "Verteidigung" ~ 16,
                                     TRUE ~ topic_id)) %>% 
  dplyr::mutate(topic_id_chr = ifelse(topic_id < 10, 
                                      paste0("0", as.character(topic_id)),
                                      as.character(topic_id))) %>% 
  dplyr::mutate(thema = case_when(topic_id == 19 ~ "Internationale Angelegenheiten & Auslandshilfe",
                                  topic_id == 13 ~ "Soziale Wohlfahrt",
                                  topic_id == 4 ~ "Agrarwirtschaft",
                                  TRUE ~ thema)) %>% 
  dplyr::select(topic_id, topic_id_chr, thema) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(topic_id) %>% 
  as.data.frame()


df_adjusted_fin_corrected %>% 
  dplyr::mutate(topic_id = case_when(thema == "Verteidigung" ~ 16,
                                     TRUE ~ topic_id)) %>% 
  dplyr::mutate(topic_id_chr = ifelse(topic_id < 10, 
                                      paste0("0", as.character(topic_id)),
                                      as.character(topic_id))) %>% 
  dplyr::mutate(thema = case_when(topic_id == 19 ~ "Internationale Angelegenheiten & Auslandshilfe",
                                  topic_id == 13 ~ "Soziale Wohlfahrt",
                                  topic_id == 4 ~ "Agrarwirtschaft",
                                  TRUE ~ thema)) %>% 
  saveRDS("01_data/df_complete_20200114_corrected.RDS")

df_adjusted_fin_corrected <- readRDS("01_data/df_complete_20200114_corrected.RDS")

# checking if there are any duplicates or something else
df_adjusted_fin_corrected %>% 
  dplyr::select(topic_id, topic_id_chr, thema) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(topic_id) %>% 
  as.data.frame()

# final df with word_matching topics and all additional information
df_adjusted_fin_corrected %>% 
  dplyr::group_by(thema, party_2) %>% 
  dplyr::summarise(sentiment = mean(sentiment_mean, na.rm = T),
                   sentiment_median = median(sentiment_mean, na.rm = T)) %>% 
  ggplot(aes(x = thema, y = sentiment, fill = party_2)) +
  geom_bar(stat = "identity", position = "dodge", col = "black") +
  coord_flip() +
  theme_minimal()


df_adjusted_fin_corrected %>% 
  dplyr::select(row_id, speech_new) %>% 
  write.csv(file = "01_data/speeches_by_rowID_20200116.csv")

liwc_new <- read.csv("01_data/LIWC2015_Results_20200116.csv",
                     stringsAsFactors = F)

liwc_new <- liwc_new %>%
  dplyr::rename(row_id = B,
                speech_new = C) %>% 
  dplyr::select(-speech_new) %>% 
  dplyr::mutate(cc_new = as.double(str_replace(cogproc, ",", "."))) %>% 
  dplyr::select(row_id, cc_new)

df_adjusted_fin_corrected <- df_adjusted_fin_corrected %>% 
  dplyr::left_join(liwc_new, by = "row_id")
  
df_adjusted_fin_corrected %>% 
  saveRDS("01_data/df_complete_20200116.RDS")

df_adjusted_fin_corrected <- readRDS("01_data/df_complete_20200116.RDS")

# adding salience
df_adjusted_fin_corrected_sal <- df_adjusted_fin_corrected %>%
  dplyr::mutate(salience = case_when(party_2 == "AfD" & topic_id %in% c(1, 9, 7, 8, 12) ~ 1,
                                     party_2 == "Grüne" & topic_id %in% c(7,8) ~ 1,
                                     party_2 == "Linke" & topic_id %in% c(9,13, 14) ~ 1,
                                     party_2 == "SPD" & topic_id %in% c(13, 14) ~ 1,
                                     party_2 == "Union" & topic_id %in% c(9, 1) ~ 1,
                                     party_2 == "FDP" & topic_id %in% c(17,1,5) ~ 1,
                                     TRUE ~ 0),
                AfD = ifelse(party_2 == "Afd", 1, 0),
                Grüne = ifelse(party_2 == "Grüne", 1,0),
                Linke = ifelse(party_2 == "Linke", 1,0),
                Union = ifelse(party_2 == "Union", 1,0),
                SPD = ifelse(party_2 == "SPD", 1, 0),
                FDP = ifelse(party_2 == "FDP", 1,0),
                abs_sentiment_mean = abs(sentiment_mean))


# Next Steps ------
# TODO: LDA Topic Modeling
# TODO: Bayesian models with current labeld debates (IMPORTANT FOR MEETING IN 1.5 WEEKS!!!)
# TODO: Classification with SVM
# TODO: Classification with RF
# TODO: Classification with NN >> Different Approaches



afd_tidy <- tidytext::tidy(tdm_afd)
afd_tidy %>% 
  count(term, sort = TRUE) %>% 
  head(30) 

afd_wahl <- tm::readPDF("01_data/01_raw/Wahlprogramme/AfD_kurzprogramm.pdf",engine = "pdftools")

afd_text <- pdftools::pdf_text("01_data/01_raw/Wahlprogramme/AfD_kurzprogramm.pdf")

afd_text <- afd_text %>% 
  paste0(collapse = " ") %>% 
  str_remove_all("\n")

afd_text <- afd_text %>% 
  str_squish()


stopwords_ger <- read_csv("01_data/stopwords.csv") %>% 
  dplyr::filter(!str_detect(word, "[^[:alnum:]]"))
stopwords_ger_s <- as.character(stopwords_ger$word)

umlaute <- as.data.frame(c("dafür", "darüber", "demgegenüber", "demgemäss", "dürfen",
                           "dürft",
                           "früher", "fünf", "fünfte", "fünften", "fünfter", "fünftes",
                           "für", "gegenüber",  "hätten", "hätte", "können", "könnt",
                           "könnte", "möchte", "mögen", "möglich", "mögt", "müssen", 
                           "natürlich", "später", "über", "überhaupt", "übrigens", 
                           "während", "währenddem", "währenddessen", "wäre", "würde",
                           "würden", "zunächst", "zurück", "zwölf", "––", "–", "§", "derzeit",
                           '"', '„', '“', '–'))
colnames(umlaute) <- "word"
umlaute <- as.character(umlaute$word)







get_top_freq <- function(pdf_text, words_to_remove = NULL, stopwords = NULL, top_n = 10){
  
  # initial functions 
  BigramTokenizer <- function(x){
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  }
  
  ThreeGramTokenizer <- function(x){
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
  }
  
  x <- pdftools::pdf_text(pdf_text)
  x <- x %>% 
    paste0(collapse = " ") %>% 
    str_remove_all("\n") %>% 
    str_squish() %>% 
    removePunctuation() %>% 
    tolower()
  
  if(!is.null(words_to_remove)){
    x <- x %>% 
      removeWords(words_to_remove)
  }
  
  if(!is.null(stopwords)){
    x <- x %>% 
      removeWords(stopwords)
  }
  
  x <- x %>% 
  str_replace_all("[*]", "") %>% 
    str_replace_all("[[:punct:]]", " ") %>% 
    removeNumbers() %>% 
    removeWords(stopwords("german")) %>% 
    qdapRegex::rm_nchar_words("1,2")
  
  x_one <- tm::termFreq(x) 
  
  x_one <- as.data.frame(x_one) %>% 
    tibble::rownames_to_column() %>% 
    as_tibble() %>% 
    dplyr::rename(freq = x_one) %>% 
    dplyr::arrange(desc(freq)) %>% 
    head(top_n) %>% 
    dplyr::mutate(n_word = 1)
  
  x_bi <- tm::termFreq(x, control = list(tokenize = BigramTokenizer))
  x_bi <-  as.data.frame(x_bi) %>% 
    tibble::rownames_to_column() %>% 
    as_tibble() %>% 
    dplyr::rename(freq = x_bi) %>% 
    dplyr::arrange(desc(freq)) %>% 
    head(top_n) %>% 
    dplyr::mutate(n_word = 2)
  
  x_three <- tm::termFreq(x, control = list(tokenize = ThreeGramTokenizer))
  x_three <-  as.data.frame(x_three) %>% 
    tibble::rownames_to_column() %>% 
    as_tibble() %>% 
    dplyr::rename(freq = x_three) %>% 
    dplyr::arrange(desc(freq)) %>% 
    head(top_n) %>% 
    dplyr::mutate(n_word = 3)
  
  output <- dplyr::bind_rows(x_one, x_bi, x_three)
  
  return(output)
  
}

str(XML::xmlToList("/Users/marlonschumacher/Documents/Master/GermaParlTEI-master/15/BT_15_003.xml"))

afd_top_10 <- get_top_freq(pdf_text = "01_data/01_raw/Wahlprogramme/AfD_Bundestagswahlprogramm_Onlinefassung.pdf",
                           words_to_remove = c("afd", "wahlprogramm", "bundestagswahl"),
                           stopwords = c(stopwords_ger_s, stopwords_de, umlaute))

afd_top_10 %>% 
  dplyr::mutate(row = row_number()) %>% 
  ggplot(aes(x = reorder(rowname, freq), y = factor(freq))) +
  geom_bar(stat = "identity", fill = "#009ee0", alpha = 0.8) +
  labs(y = "Häufigkeit", x = NULL) +
  theme_bw() +
  theme(text = element_text(size = 12, family = "LM Roman 10")) +
  coord_flip()

install.packages("GermaParl")


gruene_text <- pdftools::pdf_text("01_data/01_raw/Wahlprogramme/BUENDNIS_90_DIE_GRUENEN_Bundestagswahlprogramm_2017.pdf")
cat(gruene_text)

gruene_text[15] %>% 
  cat()

gruene_top_10 <- get_top_freq(pdf_text = "01_data/01_raw/Wahlprogramme/BUENDNIS_90_DIE_GRUENEN_Bundestagswahlprogramm_2017.pdf",
                           words_to_remove = c("grüne", "bündnis", "wahlprogramm", "bundestagswahl",
                                               "grünen", "bundestagswahlprogramm", "grün", "wählt", 
                                               "mut", "men", "schen", 
                                               # Seitenkapitel
                                               "umwelt im kopf",
                                               "welt im blick",
                                               "zukunft wird aus mut gemacht",
                                               "zukunft",
                                               "bürgerinnen", "bürger",
                                               "verbraucherinnen", "verbraucher",
                                               "stimmt", "projekt", "bäuerinnen", "bauern",
                                               "freiheit im herzen",
                                               "gerechtigkeit im sinn"),
                           stopwords = c(stopwords_ger_s, stopwords_de, umlaute))

gruene_top_10 %>% as.data.frame() %>% 
  dplyr::mutate(row = row_number()) %>% 
  ggplot(aes(x = reorder(rowname, freq), y = factor(freq))) +
  geom_bar(stat = "identity", fill = "#50822E", alpha = 0.8) +
  labs(y = "Häufigkeit", x = NULL) +
  theme_bw() +
  theme(text = element_text(size = 12, family = "LM Roman 10")) +
  coord_flip()

dplyr::bind_rows(afd_top_10 %>% 
                   dplyr::mutate(party = "AfD"),
                 gruene_top_10 %>% 
                   dplyr::mutate(party = "Grüne")) %>% 
  ggplot(aes(x = reorder(rowname, freq), y = factor(freq), fill = party)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(y = "Häufigkeit", x = NULL) +
  theme_bw() +
  scale_fill_manual(values = c("Grüne" = "#50822E", 
                               "AfD" = "#009ee0")) +
  theme(text = element_text(size = 12, family = "LM Roman 10")) +
  facet_wrap(~party, scale = "free") +
  coord_flip() 

  gruene_top_10[21,1] %>% nchar()

afd_wahl <- tm::readPDF("01_data/01_raw/Wahlprogramme/AfD_kurzprogramm.pdf",engine = "pdftools")

afd_text <- pdftools::pdf_text("01_data/01_raw/Wahlprogramme/AfD_kurzprogramm.pdf")

afd_text <- afd_text %>% 
  paste0(collapse = " ") %>% 
  str_remove_all("\n")

afd_text <- afd_text %>% 
  str_squish()


afd_text <- afd_text %>%  
  removeNumbers() %>% 
  removePunctuation() %>% 
  tolower() %>% 
  removeWords(stopwords_ger_s) %>% 
  removeWords(stopwords("german")) %>% 
  removeWords(umlaute) %>% 
  qdapRegex::rm_nchar_words("1,2") %>% 
  str_remove_all("afd")


afd_freq_one <-   tm::termFreq(afd_text)
afd_freq_bi <-    tm::termFreq(afd_text, control = list(tokenize = BigramTokenizer))
afd_freq_three <- tm::termFreq(afd_text, control = list(tokenize = ThreeGramTokenizer))

as.data.frame(afd_freq_one) %>% 
  tibble::rownames_to_column() %>% 
  as_tibble() %>% 
  dplyr::arrange(desc(afd_freq_one))

as.data.frame(afd_freq_three) %>% 
  tibble::rownames_to_column() %>% 
  as_tibble() %>% 
  dplyr::arrange(desc(afd_freq_three))




