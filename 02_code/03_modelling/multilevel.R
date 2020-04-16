# -----------------------------------------------------------------------------
# Project: Masterthesis
# Content: Bayesian Multilevel
# Contributors: Marlon Schumacher
# Last update on: 2020-04-15
# -----------------------------------------------------------------------------

# PACKAGES ----
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
library(sjPlot)
library(patchwork)
library(tidybayes)


df_modelling <- readRDS("01_data/02_prepared/df_modelling_20200303.RDS")

# DATA PREP ----
df_modelling <- df_modelling %>% 
  
  # adding weeks to new election for each legislature
  dplyr::mutate(days_to_election = case_when(
    legislature == 14 ~ (date - ymd("2002-09-22")) %>% as.double() %>% abs(),
    legislature == 15 ~ (date - ymd("2005-09-18")) %>% as.double() %>% abs(),
    legislature == 16 ~ (date - ymd("2009-09-27")) %>% as.double() %>% abs(),
    legislature == 17 ~ (date - ymd("2013-09-22")) %>% as.double() %>% abs(),
    legislature == 18 ~ (date - ymd("2017-09-24")) %>% as.double() %>% abs(),
    legislature == 19 ~ (date - ymd("2021-10-24")) %>% as.double() %>% abs()
  )) %>% 
  
  # mutating time difference to week
  dplyr::mutate(weeks_to_election = round(days_to_election/7)) %>%
  
  # adding koalitions information
  dplyr::mutate(koalitionsrolle = case_when(
    legislature %in% c(14,15) & fraktion == "SPD" ~ "Großer Partner",
    legislature %in% c(14,15) & fraktion == "Grüne" ~ "Kleiner Partner",
    legislature %in% c(16,18,19) & fraktion == "Union" ~ "Großer Partner",
    legislature %in% c(16,18,19) & fraktion == "SPD" ~ "Kleiner Partner",
    legislature == 17 & fraktion == "Union" ~ "Großer Partner", 
    legislature == 17 & fraktion == "FDP" ~ "Kleiner Partner",
    TRUE ~ "NA"
  )) %>% 
  
  # adding salience for each party
  dplyr::mutate(salience_cap = case_when(
    fraktion == "Linke" & thema_cap %in% c("Arbeit & Beschäftigung", "Soziale Wohlfahrt", "Verteidigung") ~ 1,
    fraktion == "AfD" & thema_cap %in% c("Einwanderung", "Kriminalität & Familienprobleme", "Makroökonomie") ~ 1,
    fraktion == "Grüne" & thema_cap %in% c("Energie", "Umwelt") ~ 1,
    fraktion == "FDP" & thema_cap %in% c("Bildung", "Makroökonomie") ~ 1,
    fraktion == "SPD" & thema_cap %in% c("Arbeit & Beschäftigung", "Bildung") ~ 1,
    fraktion == "Union" & thema_cap %in% c("Kriminalität & Familienprobleme", "Energie", "Makroökonomie") ~ 1,
    TRUE ~ 0
  )) %>% 
  
  # adding salience for each party
  dplyr::mutate(salience_lda = case_when(
    fraktion == "Linke" & lda_label %in% c("Verteidigung", "Arbeit und Beschäftigung", "Gesundheit & Rente") ~ 1,
    fraktion == "AfD" & lda_label %in% c("Migration", "Gesetz und Kriminalität", "Europäische Union", "Kultur") ~ 1,
    fraktion == "Grüne" & lda_label %in% c("Energie und Umwelt") ~ 1,
    fraktion == "FDP" & lda_label %in% c("Bildung und Forschung", "Wirtschaft") ~ 1,
    fraktion == "SPD" & lda_label %in% c("Arbeit und Beschäftigung", "Bildung und Forschung") ~ 1,
    fraktion == "Union" & lda_label %in% c("Gesetz und Kriminalität", "Energie und Umwelt", "Wirtschaft") ~ 1,
    TRUE ~ 0
  ))

relevant_parties <- c("SPD", "Union", "Grüne", "FDP", "Linke", "AfD")
saveRDS(df_modelling, "01_data/02_prepared/df_modelling_20200303.RDS")

# START OF MODELLING --------------
df_modelling <- readRDS("01_data/02_prepared/df_modelling_20200303.RDS")

df_modelling %>% 
  dplyr::filter(gamma >= 0.2) %>% 
  dplyr::filter(!is.na(age) & age < 95) %>% 
  dplyr::filter(lda_label != "NA" & fraktion != "NA") %>% nrow()
# [1] 99370

df_modelling %>% 
  dplyr::filter(gamma >= 0.2) %>% 
  dplyr::filter(!is.na(age) & age < 95) %>% 
  dplyr::filter(lda_label != "NA" & fraktion != "NA") %>% 
  dplyr::select(date, agenda_item) %>% dplyr::distinct() %>% nrow()
# [1] 7349

# considering nchar
df_modelling_bay %>% 
  dplyr::filter(gamma >= 0.2 & nchar > 300) %>% 
  dplyr::filter(!is.na(age) & age < 95) %>% 
  dplyr::filter(lda_label != "NA" & fraktion != "NA") %>% nrow()
# [1] 89412

df_modelling_bay %>% 
  dplyr::filter(gamma >= 0.2 & nchar > 300) %>% 
  dplyr::filter(!is.na(age) & age < 95) %>% 
  dplyr::filter(lda_label != "NA" & fraktion != "NA") %>% 
  dplyr::select(date, agenda_item) %>% dplyr::distinct() %>% nrow()
# [1] 7349

df_modelling %>% 
  dplyr::mutate(leg_dummy = ifelse(legislature == 19, "19. Bundestag", "14.-18. Bundestag")) %>% 
  dplyr::filter(nchar < 25000 & fraktion != "NA") %>% 
  ggplot(aes(x = fraktion, y = cogproc_pre_clean, fill = fraktion)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("Grüne" = "#50822E", "Linke" = "#B61C3E", 
                                "Union" = "#32372C", "SPD" = "#E3000F", "FDP" = "#FFD600",
                                "AfD" = "#009ee0")) +
  facet_wrap(~leg_dummy, scales = "free_x") +
  theme_bw() +
  labs(fill = NULL, x = NULL) +
  theme(legend.position = "bottom",
        text = element_text(size = 12, family = "LM Roman 10"))

p_2 <- df_modelling %>% 
  dplyr::mutate(leg_dummy = ifelse(legislature == 19, "19. Bundestag", "14.-18. Bundestag")) %>% 
  dplyr::filter(nchar < 25000 & fraktion != "NA") %>% 
  ggplot(aes(x = nchar, y = cogproc_pre_clean)) +
  geom_point(alpha = 0.1, color = "black", size = 0.8) +
  theme_bw() +
  theme(text = element_text(size = 12, family = "LM Roman 10")) +
  labs(x = NULL, y = "Cognitive Complexity") +
  facet_wrap(~leg_dummy)

p_1 <- df_modelling %>% 
  dplyr::mutate(leg_dummy = ifelse(legislature == 19, "19. Bundestag", "14.-18. Bundestag")) %>% 
  dplyr::filter(nchar < 25000 & nchar > 1000 & fraktion != "NA") %>% 
  ggplot(aes(x = nchar, y = cogproc_pre_clean)) +
  geom_point(alpha = 0.1, color = "black", size = 0.8) +
  theme_bw() +
  theme(text = element_text(size = 12, family = "LM Roman 10")) +
  labs(x = "Anzahl Zeichen", y = "Cognitive Complexity") +
  facet_wrap(~leg_dummy)

p_2/p_1 +
  plot_annotation(tag_levels = 'A')
df_modelling$sentiws_polarity_pre_clean_neg

df_modelling %>% 
  dplyr::filter(stringr::str_detect(speech, "zwischenfrage") & legislature == 19 ) %>% .$speech %>% .[1:30]

df_modelling %>% 
  dplyr::filter(nchar > 300 & fraktion != "NA") %>% 
  dplyr::group_by(fraktion, opp_gov) %>% 
  dplyr::summarise(median = median(sentiws_polarity_pre_clean_neg, na.rm = T),
                   mean = mean(sentiws_polarity_pre_clean_neg, na.rm = T))

df_modelling %>% 
  dplyr::filter(nchar > 300 & fraktion != "NA" & opp_gov == "Government") %>% 
  dplyr::select(sentiws_polarity_pre_clean_neg, sentiws_ratio_pre_clean_neg, sentiws_sent_pre_clean_neg) %>% 
  summary()

df_modelling %>% 
  dplyr::filter(nchar > 300 & fraktion != "NA" & opp_gov != "Government") %>% 
  dplyr::select(sentiws_polarity_pre_clean_neg, sentiws_ratio_pre_clean_neg, sentiws_sent_pre_clean_neg) %>% 
  summary()
  

df_modelling %>% 
  dplyr::filter(nchar > 300 & fraktion != "NA") %>% 
  ggplot(aes(x = fraktion, y = sentiws_polarity_pre_clean_neg, fill = fraktion)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("Grüne" = "#50822E", "Linke" = "#B61C3E", 
                               "Union" = "#32372C", "SPD" = "#E3000F", "FDP" = "#FFD600",
                               "AfD" = "#009ee0")) +
  facet_wrap(~opp_gov, scale = "free_x") +
  theme_bw() +
  labs(fill = NULL, x = NULL, y = "Sentiment Polarity") +
  theme(legend.position = "bottom",
        text = element_text(size = 12, family = "LM Roman 10"))

df_modelling %>% 
  dplyr::filter(nchar > 300 & fraktion != "NA") %>% 
  ggplot(aes(x = fraktion, y = sentiws_ratio_pre_clean_neg, fill = fraktion)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("Grüne" = "#50822E", "Linke" = "#B61C3E", 
                               "Union" = "#32372C", "SPD" = "#E3000F", "FDP" = "#FFD600",
                               "AfD" = "#009ee0")) +
  facet_wrap(~opp_gov, scale = "free_x") +
  theme_bw() +
  labs(fill = NULL, x = NULL, y = "Sentiment Ratio") +
  theme(legend.position = "bottom",
        text = element_text(size = 12, family = "LM Roman 10"))


# prepare df for modelling ----
df_modelling_bay <- df_modelling %>% 
  
  # filtering for nchar and gamma (lda probability)
  dplyr::filter(nchar > 300 & gamma >= 0.2) %>% 
  dplyr::filter(!is.na(age) & age < 95) %>% 
  
  # all rows with na values regarding polarity will be removed
  # will effect only few cases with low nchar
  dplyr::filter(!is.nan(sentiws_polarity_pre_clean_neg)) %>% 
  
  # only considering fraktionen
  dplyr::filter(fraktion %in% relevant_parties) %>% 
  
  # remove lda_topics: Haushalt, Verfassungsrecht, Investition
  dplyr::filter(!topic_lda %in% c(18,7,20) & lda_label != "NA" & !is.na(lda_label))

df_modelling_bay %>% 
  dplyr::select(lda_label, topic_lda) %>% 
  dplyr::distinct() %>% 
  as.data.frame()

# Standardizing variables
df_modelling_bay <- df_modelling_bay %>% 
  dplyr::filter(!is.na(age)) %>% 
  
  # std version of age and other variables
  dplyr::mutate(age_std = scale(age, center = TRUE, scale = TRUE),
                cogproc_pre_clean_std = scale(cogproc_pre_clean, center = TRUE, scale = TRUE),
                weeks_t_e_std = scale(weeks_to_election, center = TRUE, scale = TRUE)) %>% 
  
  # dummy variables
  dplyr::mutate(
    
    # defining populism parties
    Populism = case_when(
      fraktion %in% c("AfD", "Linke") ~ 1,
      TRUE ~ 0
    ),
    populism_chr = case_when(
      fraktion %in% c("AfD", "Linke") ~ "Populism",
      TRUE ~ "Nicht populisitisch"
    ),
    
    # opposition dummy
    opposition = case_when(
      opp_gov == "Opposition" ~ 1,
      TRUE ~ 0
    ),
    
    # government dummy
    government = case_when(
      opp_gov == "Government" ~ 1,
      TRUE ~ 0
    ),
    
    # sex dummy: 1 == female
    weiblich = case_when(
      sex == "weiblich" ~ 1,
      TRUE ~ 0
    ),
    koalitionsrole = case_when(
      koalitionsrolle == "Kleiner Partner" ~ 1,
      TRUE ~ 0
    ))

# package for t-test
library(ggpubr)

p1 <- df_modelling_bay %>% 
  dplyr::mutate(leg_dummy = ifelse(legislature == 19, "19. Bundestag", "14.-18. Bundestag")) %>% 
  dplyr::mutate(opp_gov = ifelse(opp_gov == "Government", "Regierung", opp_gov),
                legislature = paste0(as.character(legislature), ". Bundestag")) %>% 
  dplyr::mutate(opp_gov_pop = case_when(
    opp_gov == "Opposition" & Populism == 1 ~ "Populismus",
    opp_gov == "Opposition" & Populism != 1 ~ "Opposition (ex Populismus)",
    TRUE ~ opp_gov
  )) %>% 
  # dplyr::filter(legislature == 19) %>% 
  ggplot(aes(x = opp_gov, y = sentiws_polarity_pre_clean_neg, fill = opp_gov)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  stat_compare_means(method = "t.test", label.y = 1.1, family = "LM Roman 10") +
  theme_bw() +
  labs(x = NULL, y = "Sentiment Polarity", fill = NULL) +
  facet_wrap(~leg_dummy) +
  theme(legend.position = "bottom",
        text = element_text(size = 12, family = "LM Roman 10"))

tri_comparision <- list(c("Opposition \n(ex Populismus)", "Regierung"),
                    c("Populismus", "Regierung"),
                    c("Opposition \n(ex Populismus)", "Populismus"))

p2 <- df_modelling_bay %>% 
  dplyr::mutate(leg_dummy = ifelse(legislature == 19, "19. Bundestag", "14.-18. Bundestag")) %>% 
  dplyr::mutate(opp_gov = ifelse(opp_gov == "Government", "Regierung", opp_gov),
                legislature = paste0(as.character(legislature), ". Bundestag")) %>% 
  dplyr::mutate(opp_gov_pop = case_when(
    opp_gov == "Opposition" & Populism == 1 ~ "Populismus",
    opp_gov == "Opposition" & Populism != 1 ~ "Opposition \n(ex Populismus)",
    TRUE ~ opp_gov
  )) %>% 
  # dplyr::filter(legislature == 19) %>% 
  ggplot(aes(x = opp_gov_pop, y = sentiws_polarity_pre_clean_neg, fill = opp_gov_pop)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values=c("#E69F00", "#999999", "#56B4E9"))+
  stat_compare_means(comparisons = tri_comparision, 
                     label.y = c(1.1, 1.27, 1.3),
                     method = "t.test", 
                     size = 3,
                     #label.y = 1.1, 
                     family = "LM Roman 10") +
  theme_bw() +
  labs(x = NULL, y = "Sentiment Polarity", fill = NULL) +
  facet_wrap(~leg_dummy) +
  theme(legend.position = "bottom",
        text = element_text(size = 12, family = "LM Roman 10"))

p1/p2


df_modelling_bay %>% 
  dplyr::mutate(leg_dummy = ifelse(legislature == 19, "19. Bundestag", "14.-18. Bundestag")) %>% 
  dplyr::mutate(opp_gov = ifelse(opp_gov == "Government", "Regierung", opp_gov),
                legislature = paste0(as.character(legislature), ". Bundestag")) %>% 
  dplyr::mutate(opp_gov_pop = case_when(
    opp_gov == "Opposition" & Populism == 1 ~ "Populismus",
    opp_gov == "Opposition" & Populism != 1 ~ "Opposition \n(ex Populismus)",
    TRUE ~ opp_gov
  )) %>% 
  dplyr::group_by(leg_dummy, opp_gov_pop) %>% 
  dplyr::summarise(median = median(sentiws_polarity_pre_clean_neg))



p2 <- df_modelling_bay %>% 
  dplyr::filter(nchar > 300 & fraktion != "NA") %>% 
  dplyr::mutate(leg_dummy = ifelse(legislature == 19, "19. Bundestag", "14.-18. Bundestag")) %>% 
  dplyr::mutate(opp_gov = ifelse(opp_gov == "Government", "Regierung", opp_gov)) %>% 
  dplyr::mutate(remover = ifelse(legislature == 15 & fraktion == "Linke", 1, 0)) %>% 
  dplyr::filter(remover == 0) %>% 
  dplyr::mutate(opp_gov_pop = case_when(
    opp_gov == "Opposition" & Populism == 1 ~ "Populismus",
    opp_gov == "Opposition" & Populism != 1 ~ "Opposition \n(ex Populismus)",
    TRUE ~ opp_gov
  )) %>% 
  dplyr::group_by(legislature, opp_gov_pop) %>% 
  dplyr::summarise(mean_pol = round(mean(sentiws_polarity_pre_clean_neg, na.rm = T), 2)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(row_id = row_number()) %>% 
  dplyr::filter(row_id != 5) %>% 
  ggplot(aes(x = legislature, y = mean_pol, color = opp_gov_pop, label = mean_pol)) +
  geom_line() +
  geom_point() +
  geom_label(vjust = -0.25, show.legend = F, size = 3) +
  scale_color_manual(values=c("#E69F00", "#999999", "#56B4E9")) +
  theme_bw() +
  scale_x_continuous(limits = c(13.5, 19.5)) +
  scale_y_continuous(limits = c(-0.2, 0.25)) +
  labs(x = "Legislaturperiode", y = NULL, color = NULL) +
  theme(legend.position = "bottom",
        text = element_text(size = 12, family = "LM Roman 10"))


p1 <- df_modelling_bay %>% 
  dplyr::filter(nchar > 300 & fraktion != "NA") %>% 
  dplyr::mutate(leg_dummy = ifelse(legislature == 19, "19. Bundestag", "14.-18. Bundestag")) %>% 
  dplyr::mutate(opp_gov = ifelse(opp_gov == "Government", "Regierung", opp_gov)) %>% 
  dplyr::mutate(remover = ifelse(legislature == 15 & fraktion == "Linke", 1, 0)) %>% 
  dplyr::filter(remover == 0) %>% 
  dplyr::mutate(opp_gov_pop = case_when(
    opp_gov == "Opposition" & Populism == 1 ~ "Populismus",
    opp_gov == "Opposition" & Populism != 1 ~ "Opposition (ex Populismus)",
    TRUE ~ opp_gov
  )) %>% 
  dplyr::group_by(legislature, opp_gov) %>% 
  dplyr::summarise(mean_pol = round(mean(sentiws_polarity_pre_clean_neg, na.rm = T), 2)) %>% 
  ggplot(aes(x = legislature, y = mean_pol, color = opp_gov,label = mean_pol)) +
  geom_line() +
  geom_point() +
  geom_label(vjust = -0.25, show.legend = F, size = 3) +
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  scale_x_continuous(limits = c(13.5, 19.5)) +
  scale_y_continuous(limits = c(-0.2, 0.25)) +
  theme_bw() +
  labs(x = "Legislaturperiode", y = "Sentiment Polarity", color = NULL) +
  theme(legend.position = "bottom",
        text = element_text(size = 12, family = "LM Roman 10"))


p1+p2 +
  plot_annotation(tag_levels = 'A')

df_modelling_bay %>% 
  dplyr::filter(nchar > 300 & fraktion != "NA") %>% 
  dplyr::mutate(leg_dummy = ifelse(legislature == 19, "19. Bundestag", "14.-18. Bundestag")) %>% 
  dplyr::mutate(opp_gov = ifelse(opp_gov == "Government", "Regierung", opp_gov)) %>% 
  dplyr::mutate(remover = ifelse(legislature == 15 & fraktion == "Linke", 1, 0)) %>% 
  dplyr::filter(remover == 0) %>% 
  dplyr::mutate(opp_gov_pop = case_when(
    opp_gov == "Opposition" & Populism == 1 ~ "Populismus",
    opp_gov == "Opposition" & Populism != 1 ~ "Opposition (ex Populismus)",
    TRUE ~ opp_gov
  )) %>% 
  dplyr::group_by(fraktion, opp_gov) %>% 
  dplyr::summarise(mean_pol = round(mean(sentiws_polarity_pre_clean_neg, na.rm = T), 3),
                   median_pol = round(median(sentiws_polarity_pre_clean_neg, na.rm = T), 3))


tri_comparision <- list(c("salientes Thema", "nicht slaientes Thema"))

df_modelling_bay %>% 
  dplyr::filter(nchar > 300 & fraktion != "NA") %>% 
  dplyr::mutate(leg_dummy = ifelse(legislature == 19, "19. Bundestag", "14.-18. Bundestag")) %>% 
  dplyr::mutate(opp_gov = ifelse(opp_gov == "Government", "Regierung", opp_gov)) %>% 
  dplyr::mutate(remover = ifelse(legislature == 15 & fraktion == "Linke", 1, 0)) %>% 
  dplyr::filter(remover == 0) %>% 
  dplyr::mutate(salience_lda_chr = ifelse(salience_lda == 1, "salientes Thema", "nicht slaientes Thema")) %>% 
  dplyr::mutate(opp_gov_pop = case_when(
    opp_gov == "Opposition" & Populism == 1 ~ "Populismus",
    opp_gov == "Opposition" & Populism != 1 ~ "Opposition (ex Populismus)",
    TRUE ~ opp_gov
  )) %>% 
  # ggboxplot(x = "salience_lda_chr", y = "sentiws_polarity_pre_clean_neg",
  #           fill = "opp_gov_pop",
  #           facet.by = "opp_gov_pop", short.panel.labs = FALSE) +
  # stat_compare_means(label = "p.signif", label.y = 1.2) +
  # theme_bw() +
  # labs(x = NULL, y = "Sentiment Polarity", fill = NULL) +
  # theme(legend.position = "bottom",
  #       text = element_text(size = 12, family = "LM Roman 10"))
  ggplot(aes(x = salience_lda_chr, y = sentiws_polarity_pre_clean_neg, fill = opp_gov_pop)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values=c("#E69F00", "#999999", "#56B4E9"))+
  facet_wrap(~opp_gov_pop) +
  theme_bw() +
  labs(x = NULL, y = "Sentiment Polarity", fill = NULL) +
  theme(legend.position = "bottom",
        text = element_text(size = 12, family = "LM Roman 10"))

df_modelling_bay %>% 
  dplyr::filter(nchar > 300 & fraktion != "NA") %>% 
  dplyr::mutate(leg_dummy = ifelse(legislature == 19, "19. Bundestag", "14.-18. Bundestag")) %>% 
  dplyr::mutate(opp_gov = ifelse(opp_gov == "Government", "Regierung", opp_gov)) %>% 
  dplyr::mutate(remover = ifelse(legislature == 15 & fraktion == "Linke", 1, 0)) %>% 
  dplyr::filter(remover == 0) %>% 
  dplyr::mutate(salience_lda_chr = ifelse(salience_lda == 1, "salientes Thema", "nicht slaientes Thema")) %>% 
  dplyr::mutate(opp_gov_pop = case_when(
    opp_gov == "Opposition" & Populism == 1 ~ "Populismus",
    opp_gov == "Opposition" & Populism != 1 ~ "Opposition (ex Populismus)",
    TRUE ~ opp_gov
  )) %>% 
  dplyr::group_by(opp_gov_pop, salience_lda_chr) %>% 
  dplyr::summarise(median = median(sentiws_polarity_pre_clean_neg))


df_modelling_bay_19 <- df_modelling_bay %>% 
  # filtering for 19 legislature
  dplyr::filter(legislature == 19)

# Speeches
df_modelling_bay_19 %>% 
  nrow()
# [1] 7523

# Debates
df_modelling_bay_19 %>% 
  dplyr::group_by(date, agenda_item) %>% 
  dplyr::summarise(n=n()) %>% 
  nrow()
# [1] 655

df_modelling_bay %>% 
  dplyr::filter(legislature == 19) %>% 
  ggplot(aes(x = sentiws_polarity_pre_clean_neg, y = sentiws_sent_pre_clean_neg)) +
  geom_point()

# BRMS ----
library(brms)

# Modelling Polarity
# Model 1 ----
system.time(
bay_model_1_leg_19 <- df_modelling_bay_19 %>% 
  dplyr::mutate(opp_ex_pop = case_when(
    opp_gov == "Opposition" & Populism != 1 ~ 1,
    TRUE ~ 0
  )) %>% 
  brms::brm(formula = sentiws_polarity_pre_clean_neg ~ Populism + weeks_t_e_std + opp_ex_pop + 
              cogproc_pre_clean_std + age_std + weiblich +
              # FIXED SLOPE & RANDOM INTERCEPT WITH TWO LAYERS
              (1|fraktion+lda_label),
            
            # used cores and chains
            cores = 6,
            chains = 5,
            
            # weak prior for b coefficient instead of flat prior as it seems to be implemented
            prior = c(brms::prior(normal(0, 10), class = b)),
            iter = 5000, warmup = 1000,
            
            # increasing alpha due to divergent transitions
            control = list(adapt_delta = 0.99, max_treedepth = 15))
)

# Model 2 ----
system.time(
  bay_model_2_leg_19 <- df_modelling_bay_19 %>% 
    brms::brm(formula = sentiws_polarity_pre_clean_neg ~ weeks_t_e_std*opposition + weiblich + age_std +
                cogproc_pre_clean_std*opposition + salience_lda*opposition +
              # FIXED SLOPE & RANDOM INTERCEPT WITH TWO LAYERS
              (1|fraktion+lda_label),
              
              # used cores and chains
              cores = 6,
              chains = 5,
              
              # weak prior for b coefficient instead of flat prior as it seems to be implemented
              prior = c(brms::prior(normal(0, 10), class = b)),
              iter = 5000, warmup = 1000,
              
              # increasing alpha due to divergent transitions
              control = list(adapt_delta = 0.99, max_treedepth = 15))
)

prior_summary(bay_model_2_leg_19)
# Model 3 ----
# Note: opposition is implemented as Opposition ex Populism
system.time(
  bay_model_3_leg_19_t <- df_modelling_bay_19 %>% 
    dplyr::mutate(opp_ex_pop = case_when(
      opp_gov == "Opposition" & Populism != 1 ~ 1,
      TRUE ~ 0
    )) %>% 
    brms::brm(formula = sentiws_polarity_pre_clean_neg ~ weeks_t_e_std + Populism + 
                cogproc_pre_clean_std*Populism + age_std + weiblich + opp_ex_pop + salience_lda*Populism +
                # FIXED SLOPE & RANDOM INTERCEPT WITH TWO LAYERS
                (1|fraktion+lda_label),
              
              # used cores and chains
              cores = 6,
              chains = 5,
              
              # weak prior for b coefficient instead of flat prior as it seems to be implemented
              prior = c(brms::prior(normal(0, 10), class = b)),
              iter = 5000, warmup = 1000,
              
              # increasing alpha due to divergent transitions
              control = list(adapt_delta = 0.99, max_treedepth = 15))
)


# Model 4 ----
system.time(
  bay_model_5_all_leg_2 <- df_modelling_bay %>% 
    dplyr::mutate(legislature = case_when(
      legislature == 14 ~ 0,
      legislature == 15 ~ 1,
      legislature == 16 ~ 2,
      legislature == 17 ~ 3,
      legislature == 18 ~ 4,
      legislature == 19 ~ 5
    )) %>% 
    dplyr::mutate(opp_gov_pop = case_when(
      opp_ex_pop == "Opposition" & Populism != 1 ~ "Opposition (ex Populismus)",
      TRUE ~ 0
    )) %>% 
    brms::brm(formula = sentiws_polarity_pre_clean_neg ~ weeks_t_e_std + opp_ex_pop + opposition +
                cogproc_pre_clean_std*opposition + age_std + weiblich + salience_lda*opposition +
                
                # FIXED SLOPE & RANDOM INTERCEPT WITH TWO LAYERS
                (1|fraktion+lda_label+legislature),
              
              # used cores and chains
              cores = 6,
              chains = 5,
              
              # weak prior for b coefficient instead of flat prior as it seems to be implemented
              prior = c(brms::prior(normal(0, 10), class = b)),
              iter = 4000, warmup = 1000,
              
              # increasing alpha due to divergent transitions
              control = list(adapt_delta = 0.99, max_treedepth = 15))
)

# Model 5 ----
system.time(
  bay_model_5_all_leg_2 <- df_modelling_bay %>% 
    dplyr::mutate(legislature = case_when(
      legislature == 14 ~ 0,
      legislature == 15 ~ 1,
      legislature == 16 ~ 2,
      legislature == 17 ~ 3,
      legislature == 18 ~ 4,
      legislature == 19 ~ 5
    )) %>% 
    dplyr::mutate(opp_gov_pop = case_when(
      opp_ex_pop == "Opposition" & Populism != 1 ~ "Opposition (ex Populismus)",
      TRUE ~ 0
    )) %>% 
    brms::brm(formula = sentiws_polarity_pre_clean_neg ~ weeks_t_e_std + opp_ex_pop + Populism +
                cogproc_pre_clean_std*Populism + age_std + weiblich + salience_lda*Populism +
                
                # FIXED SLOPE & RANDOM INTERCEPT WITH TWO LAYERS
                (1|fraktion+lda_label+legislature),
              
              # used cores and chains
              cores = 6,
              chains = 5,
              
              # weak prior for b coefficient instead of flat prior as it seems to be implemented
              prior = c(brms::prior(normal(0, 10), class = b)),
              iter = 4000, warmup = 1000,
              
              # increasing alpha due to divergent transitions
              control = list(adapt_delta = 0.99, max_treedepth = 15))
)


# MULTIPLOT ----
# function for mutliplot
multiplot <- function(x) {
  
  names(x) <- NULL
  
  plot_base <- x %>% purrr::map(function(y) { 
    broom::tidy(y, conf.int = TRUE, par_type = "non-varying") %>% 
      dplyr::bind_rows(., broom::tidy(y, conf.int = TRUE, par_type = "hierarchical"))}) %>% 
    dplyr::bind_rows(.id = "model") %>% 
    dplyr::mutate(model = paste0("Model ", model),
                  term = case_when(
                    term == "weeks_t_e_std" ~ "Wochen bis Wahl",
                    term == "weiblich" ~ "Weiblich",
                    term == "cogproc_pre_clean_std" ~ "Cognitive Complexity",
                    term == "opposition" ~ "Opposition",
                    term == "age_std" ~ "Alter",
                    term == "salience_lda" ~ "Themensalienz",
                    term == "opposition:cogproc_pre_clean_std" ~ "Opposition*Cognitive Complexity",
                    term == "opposition:salience_lda" ~ "Opposition*Themensalienz",
                    term == "cogproc_pre_clean_std:Populism" ~ "Populismus*Cognitive Complexity",
                    term == "Populism:salience_lda" ~ "Populismus*Themensalienz",
                    term == "Populism" ~ "Populismus",
                    term == "sd_lda_label__Intercept" ~ "Intercept Thema (SD)",
                    term == "sd_fraktion__Intercept" ~ "Intercept Fraktion (SD)",
                    term == "sd_legislature__Intercept" ~ "Intercept Legislatur (SD)",
                    term == "opp_ex_pop" ~ "Opposition (ex. Populismus)",
                    term == "Populism:cogproc_pre_clean_std" ~ "Populismus*Cognitive Complexity",
                    TRUE ~ term
                  ))
  
  plot_base$term <- factor(plot_base$term,
                           levels = c(unique(plot_base$term[stringr::str_detect(plot_base$term, "Intercept")]),
                                      rev(unique(plot_base$term[!stringr::str_detect(plot_base$term, "Intercept")]))))
  
  library(wesanderson)
  palette <- c(wes_palette("GrandBudapest1"), rev(wes_palette("GrandBudapest2")))
  
  plot_base %>% 
    ggplot(aes(term, estimate, ymin = lower, ymax = upper, color = model)) + 
    # ggsci::scale_color_jco() +
    scale_color_manual(values = palette[1:length(x)]) +
    # scale_color_brewer(palette = "Dark2") +
    geom_pointrange(alpha = 0.8, position = position_dodge(width = 0.7), size = 0.25) + coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom",
          text = element_text(size = 12, family = "LM Roman 10")) +
    labs(y = NULL, x = NULL, color = NULL)
  
}


brms_models <- list(bay_model_1_leg_19= bay_model_1_leg_19,
                    bay_model_2_leg_19 = bay_model_2_leg_19,
                    bay_model_3_leg_19 = bay_model_3_leg_19,
                    bay_model_4_all_leg= bay_model_4_all_leg)

saveRDS(brms_models, "01_data/02_prepared/brms_models/trained_models.RDS")

brms_models <- readRDS("01_data/02_prepared/brms_models/trained_models_fin.RDS")

# reading model 5 which was running on another machine
model_5 <- readRDS("01_data/02_prepared/brms_models/MODEL_5.RDS")

# adding model to list object
brms_models$bay_model_5_all_leg <- model_5

# plotting all models together
multiplot(brms_models)

# plotting specific effects of model 4
p <- sjPlot::plot_model(brms_models$bay_model_4_all_leg,
                   type = "pred",
                   terms = c("cogproc_pre_clean_std", "opposition"))
  

# changing labels
p$data <- p$data %>% as_tibble() %>% 
  dplyr::mutate(group = ifelse(group == 0, "Regierung", "Opposition"),
                group_col = ifelse(group_col == 0, "Regierung", "Opposition"))

# plotting
p_opp_cog <- p + 
  scale_colour_manual(values=c("Regierung" = "#56B4E9", "Opposition" = "#E69F00")) + #"#56B4E9"
  scale_color_manual(values=c("Regierung" = "#56B4E9", "Opposition" = "#E69F00")) +
  scale_fill_manual(values=c("Regierung" = "#56B4E9", "Opposition" = "#E69F00")) +
  theme_bw() +
  labs(title = NULL, x = "Cognitive Complexity (Std)", y = "Sentiment Polarity", col = NULL) +
  theme(legend.position = "bottom",
        text = element_text(size = 12, family = "LM Roman 10"))

# plotting specific effects of model 5
p_cog_pop <- sjPlot::plot_model(brms_models$bay_model_5_all_leg,
                   type = "pred",
                   terms = c("cogproc_pre_clean_std", "Populism"))

# labeling
p_cog_pop$data <- p_cog_pop$data %>% as_tibble() %>% 
  dplyr::mutate(group = ifelse(group == 0, "Kein Populismus", "Populismus"),
                group_col = ifelse(group_col == 0, "Kein Populismus", "Populismus"))

# adjusting plot
p_pop_cog <- p_cog_pop +
  scale_colour_manual(values=c("Kein Populismus" = "#56B4E9",
                               "Populismus" =  "#999999"))+ #"#56B4E9"
  scale_color_manual(values=c("Kein Populismus" = "#56B4E9",
                               "Populismus" =  "#999999"))+ #"#56B4E9"
  scale_fill_manual(values=c("Kein Populismus" = "#56B4E9",
                               "Populismus" =  "#999999"))+ #"#56B4E9"
  theme_bw() +
  labs(title = NULL, x = "Cognitive Complexity (Std)", y = NULL, col = NULL) +
  theme(legend.position = "bottom",
        text = element_text(size = 12, family = "LM Roman 10"))

# combining plots with annotation
p_opp_cog + p_pop_cog + plot_annotation(tag_levels = 'A')
  

# getting output of models in console for interpreting
brms_models$bay_model_1_leg_19
brms_models$bay_model_2_leg_19
brms_models$bay_model_3_leg_19
brms_models$bay_model_4_all_leg
brms_models$bay_model_5_all_leg


# Plot of group specific intercepts
p_frak <- tidybayes::spread_draws(brms_models$bay_model_5_all_leg, r_fraktion[fraktion,Intercept], b_Intercept) %>% 
  tidybayes::median_qi(fraktion_mean = b_Intercept + r_fraktion, .width = c(.95, .66)) %>%
  ggplot(aes(y = fraktion, x = fraktion_mean, xmin = .lower, xmax = .upper, col = fraktion)) +
  tidybayes::geom_pointintervalh(alpha = 0.8) +
  scale_color_manual(values = c("Grüne" = "#50822E", "Linke" = "#B61C3E", 
                                "Union" = "#32372C", "SPD" = "#E3000F", "FDP" = "#FFD600",
                                "AfD" = "#009ee0")) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 12, family = "LM Roman 10")) +
  labs(y = NULL, x = NULL, color = NULL)

p_lda <- tidybayes::spread_draws(brms_models$bay_model_5_all_leg, r_lda_label[lda_label,Intercept], b_Intercept) %>% 
  tidybayes::median_qi(lda_mean = b_Intercept + r_lda_label, .width = c(.95, .66)) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(lda_label = case_when(
    lda_label == "Agrarwirtschaft.und.Vebraucherschutz" ~ "Agrar.Verbraucher",
    lda_label == "Internationale.Angelegenheiten" ~ "Int.Angelegenheiten",
    lda_label == "Gesundheit.&.Rente" ~ "Gesundheit.Rente",
    lda_label == "Arbeit.und.Beschäftigung" ~ "Arbeit.Beschäftigung",
    lda_label == "Energie.und.Umwelt" ~ "Energie.Umwelt",
    lda_label == "Bildung.und.Forschung" ~ "Bildung.Forschung",
    lda_label == "Gesetz.und.Kriminalität" ~ "Gesetz.Kriminalität",
    TRUE ~lda_label
  )) %>% 
  ggplot(aes(y = lda_label, x = lda_mean, xmin = .lower, xmax = .upper)) +
  tidybayes::geom_pointintervalh(color = "skyblue4", alpha = 0.8) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 12, family = "LM Roman 10")) +
  labs(y = NULL, x = NULL, color = NULL) 

p_leg <- tidybayes::spread_draws(brms_models$bay_model_5_all_leg, r_legislature[legislature,Intercept], b_Intercept) %>% 
  tidybayes::median_qi(legislature_mean = b_Intercept + r_legislature, .width = c(.95, .66)) %>%
  ggplot(aes(y = legislature, x = legislature_mean, xmin = .lower, xmax = .upper)) +
  tidybayes::geom_pointintervalh(color = "skyblue4", alpha = 0.8) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 12, family = "LM Roman 10")) +
  labs(y = NULL, x = NULL, color = NULL)
  
# final plot with annotation
p_lda + p_leg / p_frak + plot_annotation(tag_levels = 'A')



