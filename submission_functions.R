library(scutr)
library(keras)


WD <- getwd()
setwd(WD)

source("functions.R")

# tokenizes SONA data-set according to sentences
generate_sona_sentence_tokens <- function(){
  
  sona_df <- import_data()
  
  # SENTENCE TOKENS
  sona_sentence_tokens_unbalanced <- generate_sentence_tokens(sona_df)
  
  sona_sentence_tokens_balanced <- sona_sentence_tokens_unbalanced %>% filter(!pres %in% c('deKlerk', 'Motlanthe'))
  min_president_word_count <- min(table(sona_sentence_tokens_balanced$pres))
  sona_sentence_tokens_balanced <- sona_sentence_tokens_balanced %>% group_by(pres) %>% sample_n(min_president_word_count) %>% ungroup()
  
  return(list("unbalanced" = sona_sentence_tokens_unbalanced, "balanced" = sona_sentence_tokens_balanced))
  
}

generate_sona_bag_of_words <- function(sentences_unbalanced, top_n_words = 200){
  
  # WORD TOKENS
  sona_word_tokens_unbalanced <- generate_word_tokens(sentences_unbalanced)
  
  # BAG OF WORDS
  sona_bag_of_words_unbalanced <- generate_bag_of_words(sona_word_tokens_unbalanced, sentences_unbalanced, top_n_words)
  
  sona_bag_of_words_balanced <- sona_bag_of_words_unbalanced %>% filter(!pres %in% c('deKlerk', 'Motlanthe'))
  min_president_word_count <- min(table(sona_bag_of_words_balanced$pres))
  sona_bag_of_words_balanced <- sona_bag_of_words_balanced %>% group_by(pres) %>% sample_n(min_president_word_count) %>% ungroup()
  
  sona_bag_of_words_over_mandela <- oversample_smote(sona_bag_of_words_unbalanced,"Mandela","pres",max(table(sona_bag_of_words_unbalanced$pres)))
  sona_bag_of_words_over_mbeki <- oversample_smote(sona_bag_of_words_unbalanced,"Mbeki","pres", max(table(sona_bag_of_words_unbalanced$pres)))
  sona_bag_of_words_over_ramaphosa <- oversample_smote(sona_bag_of_words_unbalanced,"Ramaphosa","pres",max(table(sona_bag_of_words_unbalanced$pres)))
  sona_bag_of_words_over_deklerk <- oversample_smote(sona_bag_of_words_unbalanced,"deKlerk","pres", max(table(sona_bag_of_words_unbalanced$pres)))
  sona_bag_of_words_over_motlanthe <- oversample_smote(sona_bag_of_words_unbalanced,"Motlanthe","pres", max(table(sona_bag_of_words_unbalanced$pres)))
  sona_bag_of_words_over_zuma <- oversample_smote(sona_bag_of_words_unbalanced,"Zuma","pres", max(table(sona_bag_of_words_unbalanced$pres)))
  
  sona_bag_of_words_over <- bind_rows(sona_bag_of_words_over_mandela, 
                                      sona_bag_of_words_over_mbeki, 
                                      sona_bag_of_words_over_deklerk,
                                      sona_bag_of_words_over_motlanthe,
                                      sona_bag_of_words_over_ramaphosa,
                                      sona_bag_of_words_over_zuma)
  
  bag_of_words_df_split_unbalanced <- generate_test_train_sets(sona_bag_of_words_unbalanced)
  bag_of_words_df_split_balanced <- generate_test_train_sets(sona_bag_of_words_balanced)
  bag_of_words_df_split_oversampled <- generate_test_train_sets(sona_bag_of_words_over)
  
  return(list("unbalanced" = bag_of_words_df_split_unbalanced, 
              "balanced" = bag_of_words_df_split_balanced,
              "oversampled" = bag_of_words_df_split_oversampled))
}

generate_sona_tfidf <- function(sentences_unbalanced, top_n_words = 200){
  
  # WORD TOKENS
  sona_word_tokens_unbalanced <- generate_word_tokens(sentences_unbalanced)

  # # produce tf-idf data-set from word tokens
  sona_tfidf_unbalanced <- generate_tfidf_dataset(sona_word_tokens_unbalanced, sentences_unbalanced, top_n_words)
  
  # produce under-sampled tf-idf data-set
  sona_tfidf_balanced <- sona_tfidf_unbalanced %>% filter(!pres %in% c('deKlerk', 'Motlanthe'))
  min_president_word_count <- min(table(sona_tfidf_balanced$pres))
  sona_tfidf_balanced <- sona_tfidf_balanced %>% group_by(pres) %>% sample_n(min_president_word_count) %>% ungroup()
  
  # produce over-sampled tf-idf data-set
  sona_tfidf_over_mandela <- oversample_smote(sona_tfidf_unbalanced,"Mandela","pres",max(table(sona_tfidf_unbalanced$pres)))
  sona_tfidf_over_mbeki <- oversample_smote(sona_tfidf_unbalanced,"Mbeki","pres", max(table(sona_tfidf_unbalanced$pres)))
  sona_tfidf_over_ramaphosa <- oversample_smote(sona_tfidf_unbalanced,"Ramaphosa","pres",max(table(sona_tfidf_unbalanced$pres)))
  sona_tfidf_over_deklerk <- oversample_smote(sona_tfidf_unbalanced,"deKlerk","pres", max(table(sona_tfidf_unbalanced$pres)))
  sona_tfidf_over_motlanthe <- oversample_smote(sona_tfidf_unbalanced,"Motlanthe","pres", max(table(sona_tfidf_unbalanced$pres)))
  sona_tfidf_over_zuma <- oversample_smote(sona_tfidf_unbalanced,"Zuma","pres", max(table(sona_tfidf_unbalanced$pres)))
  
  sona_tfidf_over <- bind_rows(sona_tfidf_over_mandela, 
                               sona_tfidf_over_mbeki, 
                               sona_tfidf_over_ramaphosa,
                               sona_tfidf_over_deklerk,
                               sona_tfidf_over_zuma,
                               sona_tfidf_over_motlanthe)
  
  # Shuffle rows
  sona_tfidf_over= sona_tfidf_over[sample(1:nrow(sona_tfidf_over)), ] 
  
  tfidf_df_split_unbalanced <- generate_test_train_sets(as.tibble(sona_tfidf_unbalanced))
  tfidf_df_split_balanced <- generate_test_train_sets(as.tibble(sona_tfidf_balanced))
  tfidf_df_split_over <- generate_test_train_sets(as.tibble(sona_tfidf_over))
  
  return(list("unbalanced" = tfidf_df_split_unbalanced, 
              "balanced" = tfidf_df_split_balanced,
              "oversampled" = tfidf_df_split_over))
  
}

