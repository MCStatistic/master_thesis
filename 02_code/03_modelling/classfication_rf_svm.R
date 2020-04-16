# -----------------------------------------------------------------------------
# Project: Masterthesis
# Content: Classification RF & SVM
# Contributors: Marlon Schumacher
# Last update on: 2020-03-28
# -----------------------------------------------------------------------------

# text preprocessing -------------------------------
library(tm)
library(SnowballC)
library(topicmodels)
library(quanteda)
library(dplyr)
library(magrittr)
library(lubridate)
library(zoo)
library(tm)
library(readr)
library(stringr)
library(ggplot2)
library(corpus)
library(tidytext)
library(e1071)
library(randomForest)
library(magrittr)

# data
df_base <- readRDS("01_data/02_prepared/df_word_labeld_20200302.RDS")
df_match <- readRDS("01_data/02_prepared/df_match_20200228.RDS")

df_match %>% 
  dplyr::filter(int_aus == 1 & matches == 1) %>% 
  .$agenda_top %>% 
  .[1:10]

df_match %>% 
  dplyr::mutate(row_id = row_number()) %>% 
  dplyr::filter(matches == 2) %>% 
  dplyr::select(row_id, agenda_item, agenda_top, thema) %>% 
  write.csv(file = "01_data/02_prepared/final_data/matches_2.csv")

df_match %<>% 
  dplyr::select(date, agenda_item, thema, matches) %>% 
  dplyr::rename(thema_cap = thema)


# joining cap topics to df_base
df_base %<>% 
  dplyr::select(-thema_cap) %>% 
  dplyr::left_join(df_match, by = c("date", "agenda_item"))

df_base %<>% 
  dplyr::mutate(thema_cap = case_when(
    thema_cap == "Kriminalität & Familienprobleme" ~ "Gesetz & Kriminalität",
    thema_cap == "Internationale Angelegenheiten & Auslandshilfe" ~ "Internationale Angelegenheiten",
    TRUE ~ thema_cap
  ))

# checking distribution on speaker level
df_base %>% 
  dplyr::mutate(thema_cap = ifelse(matches > 1 | is.na(thema_cap), 
                                   "zSonstiges", thema_cap)) %>% 
  dplyr::filter(thema_cap != "zSonstiges") %>% 
  sjmisc::frq(thema_cap) %>% 
  # transforming to tibble
  .[[1]] %>% as_tibble() %>% 
  dplyr::mutate(val = as.character(val),
                freq_perc = paste0(as.character(frq),
                                   " (",
                                   as.character(raw.prc),
                                   "%)")) %>% 
  dplyr::arrange(desc(frq)) %>% 
  dplyr::select(val, freq_perc) %>% 
  dplyr::left_join(grouped_speeches %>% 
                     dplyr::filter(thema_cap != "zSonstiges") %>% 
                     sjmisc::frq(thema_cap) %>% 
                     # transforming to tibble
                     .[[1]] %>% as_tibble() %>% 
                     dplyr::mutate(val = as.character(val),
                                   freq_perc_d = paste0(as.character(frq),
                                                      " (",
                                                      as.character(raw.prc),
                                                      "%)")) %>% 
                     dplyr::arrange(desc(frq)) %>% 
                     dplyr::select(val, freq_perc_d),
                   by = "val") %>% 
  xtable::xtable()

# checking distribution on debate level
df_match %>% 
  dplyr::mutate(thema_cap = case_when(
    thema_cap == "Kriminalität & Familienprobleme" ~ "Gesetz & Kriminalität",
    thema_cap == "Internationale Angelegenheiten & Auslandshilfe" ~ "Internationale Angelegenheiten",
    TRUE ~ thema_cap
  )) %>% 
  dplyr::filter(matches <= 1) %>% 
  sjmisc::frq(thema_cap)

# saving joined data
saveRDS(df_base, "01_data/02_prepared/final_data/df_ml_base.RDS")

# DATA PREPARATION ----
grouped_speeches <- df_base %>%
  
  dplyr::mutate(thema_cap = ifelse(matches > 1 | is.na(thema_cap), 
                                   "zSonstiges", thema_cap)) %>% 

  # aggregating speeches to debate level
  dplyr::group_by(legislature, date, agenda_item) %>% 
  
  # mutating all speeches to one single string for each debate
  dplyr::summarise(debate = paste0(speech_stemmed, collapse = " "),
                   thema_cap = min(thema_cap)) %>% ungroup() %>% 
  
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

test_grouped <- grouped_speeches %>% 
  dplyr::filter(!is.na(thema_cap) & thema_cap != "zSonstiges") %>% 
  # dplyr::filter(thema_cap != "Öffentliche Flächen" & thema_cap != "Regierungsoperationen") %>% 
  dplyr::rename(thema_debatte = thema_cap) %>% 
  # transforming labels into factor
  dplyr::mutate(thema_debatte = as.factor(thema_debatte))

# filtering df 
df_base_ml <- dplyr::filter(df_base, nchar > 300)



# creating test df for test classfication
test_single <- df_base_ml %>% 
  
  # filtering out zSonstiges for testing classification
  dplyr::filter(thema_cap != "zSonstiges" & !is.na(thema_cap)) %>% 
  dplyr::rename(thema_debatte = thema_cap) %>% 
  
  # transforming labels into factor
  dplyr::mutate(thema_debatte = as.factor(thema_debatte))

# checking for frequencies in each category
test_single %>% 
  sjmisc::frq(thema_debatte)

test_grouped %>% 
  sjmisc::frq(thema_debatte) 


# undersampling data for random forest (major class bias)
sampler <- function(data, min_perc = 2, max_prc = 10, max_frq = NULL, min_frq = NULL, classes = "thema_debatte"){
  
  # creating frequency table from data
  frq_perc_table <- sjmisc::frq(data, classes) 
  frq_perc_table <- tibble::as_tibble(frq_perc_table[[1]]) %>% 
    dplyr::filter(!is.na(val)) 
  
  # extracting max percantage value
  max_prc_data <- max(frq_perc_table$raw.prc)
  min_perc_data <- min(frq_perc_table$raw.prc)
  
  while(max_prc_data >= max_prc | min_perc_data <= min_perc){
    
    max_frq <- frq_perc_table$frq %>% max()
    diff_max <- sum(frq_perc_table$frq) - max_frq
    mean_goal <- round(diff_max/nrow(frq_perc_table))
    class <- dplyr::filter(frq_perc_table, frq == max_frq) %>% .$val
    
    new_sample <- data %>% 
      dplyr::filter(thema_debatte == as.character(class)) %>% 
      dplyr::sample_n(size = max_frq) %>% 
      head(mean_goal)
    
    data <- data %>% 
      dplyr::filter(thema_debatte != as.character(class)) %>% 
      dplyr::bind_rows(new_sample)
    
    frq_perc_table <- sjmisc::frq(data, classes) 
    frq_perc_table <- tibble::as_tibble(frq_perc_table[[1]]) %>% 
      dplyr::filter(!is.na(val)) 
    
    # extracting max percantage value
    max_prc_data <- max(frq_perc_table$raw.prc)
    min_perc_data <- min(frq_perc_table$raw.prc)
    
  }
  
  return(data)
  
}

# undersampling data
test_single_s <- sampler(test_single, min_perc = 1, max_prc = 8, classes = "thema_debatte")
test_grouped_s <- sampler(test_grouped, min_perc = 1, max_prc = 8, classes = "thema_debatte")
# checking undersampled data
frq_group_s <- test_grouped_s %>% 
  sjmisc::frq(thema_debatte) %>% 
  .[[1]] %>% as_tibble() %>% 
  dplyr::mutate(val = as.character(val),
                group = paste0(as.character(frq),
                                " (",
                                as.character(raw.prc),
                                "%)")) %>% 
  dplyr::arrange(desc(frq)) %>% 
  dplyr::select(val, group)

frq_single_s <- test_single_s %>% 
  sjmisc::frq(thema_debatte) %>% 
  .[[1]] %>% as_tibble() %>% 
  dplyr::mutate(val = as.character(val),
                single = paste0(as.character(frq),
                                " (",
                                as.character(raw.prc),
                                "%)")) %>% 
  dplyr::arrange(desc(frq)) %>% 
  dplyr::select(val, single)

# making latex table
dplyr::left_join(frq_single_s, frq_group_s, by = "val") %>% 
  xtable::xtable()


# DOCUMENT TERM MATRIX --------
# GROUPED DEBATES
corpus <- Corpus(VectorSource(test_grouped_s$debate))
dtm <- DocumentTermMatrix(corpus)
dtm_sparsed_99_g <- removeSparseTerms(dtm, sparse = 0.99)
dtm_sparsed_98_g <- removeSparseTerms(dtm, sparse = 0.98)

# check for zero value rows
rowTotals <- apply(dtm_sparsed_99_g , 1, sum)
# extract empty row ids
empty_rows_single <- dtm_sparsed_99_g[rowTotals == 0, ]$dimnames[1][[1]]
# NULL

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]           #remove all docs without words

corpus_dfm <- corpus(corpus)

# transforming dtm to df
dtm_df_98_g <- as.data.frame(as.matrix(dtm_sparsed_98_g))
dtm_df_99_g <- as.data.frame(as.matrix(dtm_sparsed_99_g))
# add original labels, legislature and agenda_id
dtm_df_g <- cbind(dtm_df_98_g, test_grouped_s %>% dplyr::select(thema_debatte))

# filtering document term matrix
dtm_df_g <- dtm_df_g %>% 
  dplyr::select(colnames(dtm_df_g)[nchar(colnames(dtm_df_g)) > 3])


# dtm with bigrams
# function for bigrams
NLP_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = "_"), use.names = FALSE)
}

# dtm with bigrams
dtm_bigram <- DocumentTermMatrix(Vcorpus,
                                 control = list(tokenize=NLP_tokenizer,
                                               weighting=weightTf))

dfm_bi <- quanteda::dfm(corpus_dfm, ngrams = 2)
dfm_trim(dfm_bi, sparsity = 0.99)

dtm_bigram_sparsed <- removeSparseTerms(dtm_bigram, sparse = 0.99)

# Creating DTM of all speeches
corpus_single <- Corpus(VectorSource(test_single_s$speech_stemmed))
dtm_single <- DocumentTermMatrix(corpus_single)
dtm_sparsed_single <- removeSparseTerms(dtm_single, sparse = 0.995)
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

dtm_df_single <- as.data.frame(as.matrix(dtm_sparsed_single))
dtm_df_single_labs <- dplyr::bind_cols(dtm_df_single, test_single_s %>% dplyr::select(thema_debatte))

df_base %>% 
  dplyr::select(speech, row_id) %>% 
  write_csv(path = "01_data/02_prepared/df_main_v2_20200224_liwc_speech.csv")

liwc_pre_clean_results <- read.csv("01_data/02_prepared/df_main_v2_20200224_liwc_results_pre_clean.csv",
                                   stringsAsFactors = F)

liwc_speech_raw_results <- read.csv("01_data/02_prepared/df_main_v2_20200224_liwc_speech_results.csv",
                                   stringsAsFactors = F)

liwc_pre_clean_results <- as_tibble(liwc_pre_clean_results) %>% 
  dplyr::select(B, cogproc) %>% 
  dplyr::rename(row_id = B,
                cogproc_pre_clean = cogproc) %>% 
  dplyr::mutate(cogproc_pre_clean = as.double(str_replace_all(cogproc_pre_clean, ",", ".")))

liwc_speech_raw_results <- as_tibble(liwc_speech_raw_results) %>% 
  dplyr::select(B, cogproc) %>% 
  dplyr::rename(row_id = B,
                cogproc_raw = cogproc) %>% 
  dplyr::mutate(cogproc_raw = as.double(str_replace_all(cogproc_raw, ",", ".")))
  


# SVM + RF --------
# split into train and test
sample <- caTools::sample.split(dtm_df_single_labs$thema_debatte, SplitRatio = 0.8)
df_train_s <- subset(dtm_df_single_labs, sample == TRUE)
df_test_s <- subset(dtm_df_single_labs, sample == FALSE)
nrow(df_train_s)
# 10381
nrow(df_test_s)
# 2594

# Grouped Debates
dtm_df_g
sample <- caTools::sample.split(dtm_df_g$thema_debatte, SplitRatio = 0.8)
df_train_g <- subset(dtm_df_g, sample == TRUE)
df_test_g <- subset(dtm_df_g, sample == FALSE)
nrow(df_train_g)
# 761
nrow(df_test_g)
# 191

# RANDOM FOREST ----
# functions for extracting results
extract_label_from_prob_names <- function(x) return(rownames(as.matrix(which.max(x))))
extract_maximum_prob <- function(x) return(x[which.max(x)])
df_train_g <- df_train_g[,nchar(colnames(df_train_g)) > 3]
df_test_g <- df_test_g[,nchar(colnames(df_test_g)) > 3]

system.time(
modfit_rf200_group <- randomForest::randomForest(thema_debatte ~., data = df_train_g,
                                           ntree = 200))

system.time(
modfit_rf500_group <- randomForest::randomForest(thema_debatte ~., data = df_train_g,
                                           ntree = 500)
)

system.time(
modfit_rf1000_group <- randomForest::randomForest(thema_debatte ~., data = df_train_g,
                                                 ntree = 1000)
)

rf_model_to_df <- function(rf, test, tree = "200", original_labels = NULL){
  
  # RF with 200 trees
  pred <- predict(rf, test, type = "prob")
  rf_pred <- apply(pred, 1, extract_label_from_prob_names)
  rf_prob <- apply(pred, 1, extract_maximum_prob)
  results_table <- data.frame(as.character(rf_pred), 
                              rf_prob) %>% 
    dplyr::mutate(ntree = tree)
  
  names(results_table) <- c("rf_label", "rf_prob", "tree")
  
  if(!is.null(original_labels)){
    
    results_table$original_thema <- original_labels
    
  }
  
  return(results_table)
  
}

rf_models <- list(modfit_rf200_group = modfit_rf200_group,
                  modfit_rf500_group = modfit_rf500_group,
                  modfit_rf1000_group = modfit_rf1000_group)

rf_results <- purrr::map2(.x = rf_models,
                          .y = c("200", "500", "1000"),
                          .f = ~rf_model_to_df(.x, df_test_g, tree = .y, 
                                               original_labels = df_test_g$thema_debatte))

rf_results %>% 
purrr::map(.x = ., .f = ~dplyr::mutate(.x, rf_label = as.character(rf_label))) %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate(tree = paste0("RF (", tree, ")"),
                rf_label = case_when(rf_label == "Gemeindeentwicklung & Wohnungsprobleme" ~ "Wohnungsbau",
                                     rf_label == "Banken, Finanzen & Binnenhandel" ~ "Binnenhandel",
                                     rf_label == "Gesetz & Kriminalität" ~ "Kriminalität",
                                     rf_label == "Arbeit & Beschäftigung" ~ "Arbeit", 
                                     TRUE ~ rf_label)) %>% 
  ggplot(aes(y = rf_prob, x = rf_label, fill = rf_label)) +
  geom_boxplot(alpha = 0.1) +
  geom_jitter(alpha = 0.2) +
  coord_flip() +
  labs(fill = NULL, x = NULL, y = "Klassenwahrscheinlichkeit") +
  theme_bw() +
  theme(legend.position = "None",
        text = element_text(size = 12, family = "LM Roman 10")) +
  facet_wrap(~tree) 

summary <- rf_results %>% 
  purrr::map(.x = ., .f = ~dplyr::mutate(.x, rf_label = as.character(rf_label))) %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate(tree_plot = paste0("RF (", tree, ")"),
                rf_label = as.character(rf_label),
                original_thema = as.character(original_thema),
                check = ifelse(rf_label == original_thema, 1, 0))

trees <- c("200", "500", "1000")
p <- c(0, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4)

for(j in 1:length(trees)){
  
  sum_pre <- summary %>% 
    dplyr::filter(tree == trees[j])
  
  total_row <- nrow(sum_pre)
  
  for(i in 1:length(p)){
    
    result <- sum_pre %>% 
      dplyr::filter(rf_prob > p[i])
    
    cases <- nrow(result)
    
    n_correct <- result %>% 
      dplyr::filter(check == 1) %>% 
      nrow()
    
    accuracy <- n_correct/cases
    anteil <- cases/total_row
    
    cat(paste0("Trees: ", trees[j], " \n", 
               "p: ", p[i], " \n",
               "Accuracy: ", round(accuracy, 2), " \n",
               "Anteil: ", round(anteil,2), "\n",
               "------------- \n"))
    
  }
}

total_rows <- nrow(summary)
summary %>% 
  dplyr::filter(rf_prob > 0.1)
  .$check %>% sum()


rf_models_result_g <- list(rf_results = rf_results,
                  rf_models = rf_models)

saveRDS(rf_models_result_g, file = "rf_results_test_g.RDS")

# single speeches
df_train_s <- df_train_s[,nchar(colnames(df_train_s)) > 3]
df_test_s <- df_test_s[,nchar(colnames(df_test_s)) > 3]

system.time(
  modfit_rf200_single <- randomForest::randomForest(thema_debatte ~., data = df_train_s,
                                                    ntree = 200))

system.time(
  modfit_rf500_single <- randomForest::randomForest(thema_debatte ~., data = df_train_s,
                                                    ntree = 500)
)

system.time(
  modfit_rf1000_single <- randomForest::randomForest(thema_debatte ~., data = df_train_s,
                                                     ntree = 1000)
)


rf_models_s <- list(modfit_rf200_single = modfit_rf200_single,
                    modfit_rf500_single = modfit_rf500_single,
                    modfit_rf1000_single = modfit_rf1000_single)

rf_results_s <- purrr::map2(.x = rf_models_s,
                          .y = c("200", "500", "1000"),
                          .f = ~rf_model_to_df(.x, df_test_s, tree = .y, 
                                               original_labels = df_test_s$thema_debatte)) 

rf_models_result_s <- list(rf_results_s = rf_results_s,
                           rf_models_s = rf_models_s)

summary_s <- rf_results_s %>% 
  purrr::map(.x = ., .f = ~dplyr::mutate(.x, rf_label = as.character(rf_label))) %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate(tree_plot = paste0("RF (", tree, ")"),
                rf_label = as.character(rf_label),
                original_thema = as.character(original_thema),
                check = ifelse(rf_label == original_thema, 1, 0))



for(j in 1:length(trees[1:3])){
  
  sum_pre <- summary_s %>% 
    dplyr::filter(tree == trees[j])
  
  total_row <- nrow(sum_pre)
  
  for(i in 1:length(p)){
    
    result <- sum_pre %>% 
      dplyr::filter(rf_prob > p[i])
    
    cases <- nrow(result)
    
    n_correct <- result %>% 
      dplyr::filter(check == 1) %>% 
      nrow()
    
    accuracy <- n_correct/cases
    anteil <- cases/total_row
    
    cat(paste0("Trees: ", trees[j], " \n", 
               "p: ", p[i], " \n",
               "Accuracy: ", round(accuracy, 2), " \n",
               "Anteil: ", round(anteil,2), "\n",
               "------------- \n"))
    
  }
}

rf_results_s %>% 
  purrr::map(.x = ., .f = ~dplyr::mutate(.x, rf_label = as.character(rf_label))) %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate(tree = paste0("RF (", tree, ")"),
                rf_label = case_when(rf_label == "Gemeindeentwicklung & Wohnungsprobleme" ~ "Wohnungsbau",
                                     rf_label == "Banken, Finanzen & Binnenhandel" ~ "Binnenhandel",
                                     rf_label == "Gesetz & Kriminalität" ~ "Kriminalität",
                                     rf_label == "Arbeit & Beschäftigung" ~ "Arbeit", 
                                     TRUE ~ rf_label)) %>% 
  ggplot(aes(y = rf_prob, x = rf_label, fill = rf_label)) +
  geom_boxplot(alpha = 0.1) +
  geom_jitter(alpha = 0.05) +
  coord_flip() +
  labs(fill = NULL, x = NULL, y = "Klassenwahrscheinlichkeit") +
  theme_bw() +
  theme(legend.position = "None",
        text = element_text(size = 12, family = "LM Roman 10")) +
  facet_wrap(~tree) 


saveRDS(rf_models_result_s, file = "rf_results_test_s.RDS")

# checking acurracy 
results_table_500 %>% 
  dplyr::filter(rf_prob > 0.5)

results_table_500 %>% 
  dplyr::mutate(true_pred = ifelse(rf_label == original_thema, 1, 0))



# EVALUATION RF ------
test_lvl <- results_table_500

levels(test_lvl$rf_label) <- levels(test_lvl$original_thema)

test_lvl$agenda_id <- df_test$agenda_id
test_lvl$legislature <- df_test$legislature

# considering only single speeches
test_lvl %>% 
  dplyr::mutate(true_pred = ifelse(rf_label == original_thema, 1, 0),
                prob_group = case_when(
                  rf_prob >= 0 & rf_prob < 0.1 ~ "0 - 10",
                  rf_prob >= 0.1 & rf_prob < 0.2 ~ "10 - 20",
                  rf_prob >= 0.2 & rf_prob < 0.3 ~ "20 - 30",
                  rf_prob >= 0.3 & rf_prob < 0.4 ~ "30 - 40",
                  rf_prob >= 0.4 & rf_prob <= 1.0 ~ "40 - 100"
                )) %>% 
  dplyr::group_by(prob_group, legislature) %>% 
  dplyr::summarise(n = n(),
                   true_sum_abs = sum(true_pred),
                   true_sum_rel = sum(true_pred)/n()) %>% 
  dplyr::filter(legislature == 19)

purrr::map(1:10, stats::runif, n = 10)
purrr::map(.x = 1:10, .f = ~stats::rnorm(min = .x, n = 10))

test_lvl %>% 
  dplyr::mutate(true_pred = ifelse(rf_label == original_thema, 1, 0)) %>% 
  dplyr::group_by(agenda_id) %>% 
  dplyr::summarise(n = n(),
                   mean_prob = mean(rf_prob),
                   min_prob = min(rf_prob),
                   max_prob = max(rf_prob),
                   true = sum(true_pred)/n()) %>% 
  dplyr::filter(true != 0)



# SVM ----
# DTM Object
# GROUPED DEBATES
corpus <- Corpus(VectorSource(test_grouped$debate))
dtm <- DocumentTermMatrix(corpus)
dtm_sparsed_99_g <- removeSparseTerms(dtm, sparse = 0.99)
dtm_sparsed_98_g <- removeSparseTerms(dtm, sparse = 0.98)

# check for zero value rows
rowTotals <- apply(dtm_sparsed_99_g , 1, sum)
# extract empty row ids
empty_rows_single <- dtm_sparsed_99_g[rowTotals == 0, ]$dimnames[1][[1]]
# NULL

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]           #remove all docs without words

# transforming dtm to df
dtm_df_98_g <- as.data.frame(as.matrix(dtm_sparsed_98_g))
dtm_df_99_g <- as.data.frame(as.matrix(dtm_sparsed_99_g))
# add original labels, legislature and agenda_id
dtm_df_g_98 <- cbind(dtm_df_98_g, test_grouped %>% dplyr::select(thema_debatte))
nrow(dtm_df_g_98)

sample <- caTools::sample.split(dtm_df_g$thema_debatte, SplitRatio = 0.8)
df_train_g_98 <- subset(dtm_df_g_98, sample == TRUE)
df_test_g_98 <- subset(dtm_df_g_98, sample == FALSE)

nrow(df_train_g_98)
nrow(df_test_g_98)
ncol(df_test_g_98)
ncol(df_train_g_98)
ncol(df_train_g)
df_train_g_98$thema_debatte %>% levels()

weights <- 1/table(df_train_g_98$thema_debatte)
model_svm_g_w <- e1071::svm(thema_debatte ~., data = df_train_g_98,
                        method = "C-classification", cross = 0, cost = 10, probability = TRUE,
                        # class.weights = weights,
                        kernel = "polynomial") # linear, polynomial, radial...

e1071::probplot(model_svm_g)
plot(model_svm_g, df_train_g_98, formula = thema_debatte ~.)

pred_98_g <- predict(model_svm_g, df_test_g_98, probability=TRUE)
pred_98_g %>% as.character()
svm_prob <- attributes(pred_98_g)

svm_prob_max <- apply(svm_prob$probabilities, 1, max)

svm_df <- data.frame(svm_label = pred_98_g %>% as.character(),
           org_label = as.character(df_test_g_98$thema_debatte),
           svm_prob = svm_prob_max,
           stringsAsFactors = F) %>% 
  as_tibble() %>% 
  dplyr::mutate(check = ifelse(svm_label == org_label, 1, 0))

tot_n <- nrow(svm_df)

for(i in 1:length(p)){
  
  result <- svm_df %>% 
    dplyr::filter(svm_prob > p[i])
  
  cases <- nrow(result)
  
  n_correct <- result %>% 
    dplyr::filter(check == 1) %>% 
    nrow()
  
  accuracy <- n_correct/cases
  anteil <- cases/tot_n
  
  cat(paste0("Trees: ", trees[j], " \n", 
             "p: ", p[i], " \n",
             "Accuracy: ", round(accuracy, 2), " \n",
             "Anteil: ", round(anteil,2), "\n",
             "------------- \n"))
  
}