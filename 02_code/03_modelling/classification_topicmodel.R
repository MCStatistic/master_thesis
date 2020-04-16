# -----------------------------------------------------------------------------
# Project: Masterthesis
# Content: Classification with topicmodelling
# Contributors: Marlon Schumacher
# Last update on: 2020-02-01
# -----------------------------------------------------------------------------

# PACKAGES ----
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
library(patchwork)
library(corpus)
library(tidytext)
library(tidyr)

# DATA ----
df_base <- readRDS("01_data/02_prepared/df_main_v2_20200224_3.RDS")
df_base <- df_base %>% 
  dplyr::left_join(grouped_speeches_labeld %>% dplyr::select(-debate, -nchar) %>% 
                     dplyr::rename(topic_lda = topic),
                   by = c("date", "legislature", "agenda_item"))


# removing some of the stemmed words
# reason: these words contains no information about the topic
#         and are part of nearly every speech
df_base$speech_stemmed <- tm::removeWords(df_base$speech_stemmed, 
                                          c("kollegin", "kollege", "geehrte", "geehrter", 
                                            "liebe", "damen", "herren", "herrn", "bitte",
                                            "wort", "prasident", "verehrte",
                                            "herzlich", "danke", "erstens", "zweitens", 
                                            "drittens", "prasidentin", "grunen", "linken", 
                                            "sage", "sagen", "namlich", "vorname", "nachname",
                                            "verehrten", "frage", "fragen", "antwort", 
                                            "gesetzentwurf", "gesetz", "gesetzes", "entwurf", "pds",
                                            "linke", "grune", "spd", "cdu", "csu", "antrag", "bundesregierung")) %>% 
  
  # squishing data again
  stringr::str_squish()

# DATA PREPARATION ----
grouped_speeches <- df_base %>%
  
  # aggregating speeches to debate level
  dplyr::group_by(legislature, date, agenda_item) %>% 
  
  # mutating all speeches to one single string for each debate
  dplyr::summarise(debate = paste0(speech_stemmed, collapse = " ")) %>% ungroup() %>% 
  
  # calculating nchar for each debate
  dplyr::mutate(nchar = nchar(debate))

# Rows of grouped speeches = agenda items
nrow(grouped_speeches)
# [1] 9253

# Histogram of nchar for grouped df
hist(grouped_speeches$nchar)

# Summary statistic for nchar
summary(grouped_speeches$nchar)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    4   16625   22383   29504   36915  520645

# filtering debates for nchar
# Reason: mostly election related topics which should be not part of the modeling
grouped_speeches <- dplyr::filter(grouped_speeches, nchar > 1500)

summary(grouped_speeches$nchar)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1532   16881   22657   29936   37155  520645 

nrow(grouped_speeches)
# [1] 9117
# Diff: -136

# 5 debates with more than 300000 nchar
dplyr::filter(grouped_speeches, nchar > 300000) 

# Histogram of nchar for all single speeches
hist(df_base$nchar)

# Summary statistics for nchar
summary(df_base$nchar)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    2     678    3033    3532    5355   89193

nrow(df_base)
# [1] 141031

df_base <- dplyr::filter(df_base, nchar > 100)

nrow(df_base)
# [1] 137106
# Diff: - 3925

# DTM ----

# Creating DTM of filtered speeches
corpus_grouped <- Corpus(VectorSource(grouped_speeches$debate))

# building VCorpus for bigram tokenization
corpus_grouped_v <- VCorpus(VectorSource(grouped_speeches$debate))

dtm_grouped <- DocumentTermMatrix(corpus_grouped)
dtm_sparsed_grouped <- removeSparseTerms(dtm_grouped, sparse = 0.98)
dtm_sparsed_grouped
# <<DocumentTermMatrix (documents: 9117, terms: 11805)>>
# Non-/sparse entries: 10510715/97115470
# Sparsity           : 90%
# Maximal term length: 30
# Weighting          : term frequency (tf)

# check for zero value rows
rowTotals <- apply(dtm_sparsed_grouped , 1, sum)
# extract empty row ids
empty_rows_grouped <- dtm_sparsed_grouped[rowTotals == 0, ]$dimnames[1][[1]]
# NULL
# Removing rows
# corpus_grouped <- corpus_grouped[-as.numeric(empty_rows_grouped)]

NLP_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = "_"), use.names = FALSE)
}

# dtm with bigrams
dtm_grouped_bi <- DocumentTermMatrix(corpus_grouped_v,
                                     control = list(tokenize=NLP_tokenizer,
                                                    weighting=weightTf))

dtm_sparsed_grouped_bi <- removeSparseTerms(dtm_grouped_bi, sparse = 0.99)

list_grouped_dtm <- list(grouped_speeches = grouped_speeches,
                         dtm_sparsed_grouped = dtm_sparsed_grouped,
                         dtm_sparsed_grouped_bi = dtm_sparsed_grouped_bi)

saveRDS(list_grouped_dtm, file = "01_data/02_prepared/grouped_dtm_20200229.RDS")
listed_data <- readRDS("01_data/02_prepared/grouped_dtm_20200229.RDS")

# Creating DTM of all speeches
corpus_single <- Corpus(VectorSource(df_base$speech_stemmed))
dtm_single <- DocumentTermMatrix(corpus_single)
dtm_sparsed_single <- removeSparseTerms(dtm_single, sparse = 0.99)
dtm_sparsed_single
# <<DocumentTermMatrix (documents: 137106, terms: 2788)>>
# Non-/sparse entries: 12164517/370087011
# Sparsity           : 97%
# Maximal term length: 26
# Weighting          : term frequency (tf)

# check for zero value rows
rowTotals <- apply(dtm_sparsed_single , 1, sum)
# extract empty row ids
empty_rows_single <- dtm_sparsed_single[rowTotals == 0, ]$dimnames[1][[1]]
# NULL
# Removing rows
ui = unique(dtm_sparsed_single$i)
dtm_sparsed_single = dtm_sparsed_single[ui,]
# <<DocumentTermMatrix (documents: 136982, terms: 2788)>>
# Non-/sparse entries: 12164517/369741299
# Sparsity           : 97%
# Maximal term length: 26
# Weighting          : term frequency (tf)

# LDA ----
# just short testing
control_list_gibbs <- list(
  # define burnin
  burnin = 0,
  # define number of itereations
  iter = 200,
  nstart = 1,
  best = TRUE
)

para <- tibble(k = c(18,19,20,21,22)) 

# looping threw all k's
# looping threw all ks for dtm
start <- Sys.time()
system.time(
  lda_grouped <- para %>%
    dplyr::mutate(lda = purrr::map(k, 
                                   function(k) topicmodels::LDA(k=k, 
                                                                x=dtm_sparsed_grouped, 
                                                                method="Gibbs", 
                                                                control=control_list_gibbs)
    )
    )
)
end <- Sys.time()
runtime_grouped <- end-start
# Time difference of 56.4308 mins

# system.time output
# User          System verstrichen 
# 3350.520      13.570    3383.776 

gamma_per_doc <- tidytext::tidy(lda_grouped$lda[[4]], matrix = "gamma")

bind_df <- purrr::map2(.x = lda_grouped$k,
                       .y = lda_grouped$lda,
                       .f = ~tidytext::tidy(.y, matrix = "gamma") %>% 
                         dplyr::group_by(document) %>%
                         dplyr::arrange(desc(gamma)) %>%
                         slice(1) %>%
                         # top_n(1, gamma) %>%
                         dplyr::ungroup() %>% 
                         dplyr::mutate(document = as.double(document),
                                       k = .x)) %>% 
  dplyr::bind_rows()


# plotting gamma distribution for different k's 
plot_1 <- bind_df %>% 
  ggplot(aes(x=gamma, y = factor(k))) +
  ggridges::geom_density_ridges(alpha = 0.2, fill = "skyblue4") +
  xlab("Gamma") +
  labs(y = "Anzahl der Themen (k)") +
  theme_bw() +
  theme(text = element_text(size = 12, family = "LM Roman 10")) 

# final LDA
control_list_gibbs_fin <- list(
  # define burnin
  burnin = 2500,
  # define number of itereations
  iter = 5000,
  nstart = 1,
  best = TRUE
)

system.time(
  lda_g_fin <- topicmodels::LDA(k=21, 
                                x=dtm_sparsed_grouped, 
                                method="Gibbs", 
                                control=control_list_gibbs_fin)
)

# User          System verstrichen 
# 13779.10        6.46    13789.09 

# save LDA result
saveRDS(lda_g_fin, file = "01_data/02_prepared/lda_result_k21.RDS")

df_fin_lda <- tidytext::tidy(lda_uni, matrix = "gamma") %>% 
  dplyr::group_by(document) %>%
  dplyr::arrange(desc(gamma)) %>%
  # take first topic which is the topic with the highest gamma
  slice(1) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(document = as.double(document),
                k = 21)

# Plotting Gamma distribution for different topics
plot_2 <- df_fin_lda %>% 
  ggplot(aes(x=gamma, y = as.factor(topic))) +
  ggridges::geom_density_ridges(alpha = 0.2, fill = "skyblue4") +
  scale_x_continuous(limits = c(0.15, 0.65)) +
  labs(x = "Gamma",
       fill = NULL, y = NULL) +
  theme_bw() +
  labs(y = "Topic") +
  theme(text = element_text(size = 12, family = "LM Roman 10"))

# combining different plots
plot_1 + plot_2 +
  plot_annotation(tag_levels = 'A')



# EXTRACTING GAMMA AND TOPIC PER DOCUMENT
gamma_per_doc <- tidytext::tidy(lda_grouped$lda[[4]], matrix = "gamma")
top_per_doc <- gamma_per_doc %>% 
  dplyr::group_by(document) %>%
  dplyr::arrange(desc(gamma)) %>%
  slice(1) %>%
  # top_n(1, gamma) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(document = as.double(document))

grouped_speeches_labeld <- grouped_speeches %>% 
  dplyr::mutate(document = row_number()) %>% 
  dplyr::left_join(top_per_doc, by = "document")

saveRDS(grouped_speeches_labeld, "01_data/02_prepared/labeld_grouped_speeches.RDS")
grouped_speeches_labeld <- readRDS("01_data/02_prepared/labeld_grouped_speeches.RDS")

grouped_speeches_labeld <- grouped_speeches_labeld %>% 
  dplyr::mutate(lda_label = case_when(
    topic == 1 ~ "Gesundheit & Rente",
    topic == 2 ~ "Kultur",
    topic == 3 ~ "Wirtschaft",
    topic == 4 ~ "Familienpolitik",
    topic == 5 ~ "Energie und Umwelt",
    topic == 6 ~ "Bildung und Forschung",
    topic == 7 ~ "Haushalt",
    topic == 8 ~ "Eropäische Union",
    topic == 9 ~ "NA",
    topic == 10 ~ "Migration",
    topic == 11 ~ "Gesetz und Kriminalität",
    topic == 12 ~ "NA",
    topic == 13 ~ "Verteidigung",
    topic == 14 ~ "NA",
    topic == 15 ~ "NA",
    topic == 16 ~ "Agrarwirtschaft und Vebraucherschutz",
    topic == 17 ~ "Arbeit und Beschäftigung",
    topic == 18 ~ "Verfassungsrecht",
    topic == 19 ~ "NA",
    topic == 20 ~ "Investition",
    topic == 21 ~ "Internationale Angelegenheiten",
    TRUE ~ "NA"
  ))


ap_topics <- tidytext::tidy(lda_uni , matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda <- ap_top_terms %>% 
  dplyr::mutate(labeld = ifelse(topic %in% c(9,12,14,15,19), "NO", "YES"))

lda_list_df_1 <- terms_lda %>% 
  dplyr::filter(labeld == "YES") %>%
  dplyr::filter(!topic %in% c(1:8)) %>% 
  dplyr::select(-labeld, -beta) %>% 
  dplyr::group_split(topic)

lda_list_df_2 <- terms_lda %>% 
  dplyr::filter(labeld == "YES") %>%
  dplyr::filter(topic %in% c(1:8)) %>% 
  dplyr::select(-labeld, -beta) %>% 
  dplyr::group_split(topic) 

# not classified
lda_list_df_2 <- terms_lda %>% 
  dplyr::filter(labeld == "NO") %>%
  dplyr::select(-labeld, -beta) %>% 
  dplyr::group_split(topic) 


for(i in 1:length(lda_list_df_2)){
  
  topic_id <- lda_list_df_2[[i]]$topic %>% unique() %>% as.character()
  lda_list_df_2[[i]] <- lda_list_df_2[[i]] %>% 
    dplyr::select(-topic)
  
  colnames(lda_list_df_2[[i]]) <- paste0("Thema ", topic_id)
  
}

# Latex Table
lda_list_df_2 %>% 
  dplyr::bind_cols() %>% 
  xtable::xtable()


# LDA TUNING ----
# detecting number of CPU cores on machine
parallel::detectCores() # 12

start_time <- Sys.time()

# Find best topic numbers
# NOTE: Results imply high number of topics
# Number of topics will be based on CAP
lda_tuned_98_p <- ldatuning::FindTopicsNumber(dtm_sparsed_965_new, 
                                              # defining number of topics and step
                                              topics = c(seq(from = 10, to = 80, by = 10)),
                                              # seq(30, 50, 5)),
                                              # metrics
                                              metrics = c("Arun2010", 
                                                          "Deveaud2014", 
                                                          "CaoJuan2009"),
                                              method = "Gibbs", 
                                              # using defined controls
                                              control = control_list_gibbs,
                                              # using 11 from 12 CPU cores
                                              mc.cores = 11L, 
                                              verbose = TRUE)

end_time <-Sys.time()
runtime <- end_time-start_time

# Plotting result
ldatuning::FindTopicsNumber_plot(lda_tuned_98_p)
