library(tidyverse)
library(stringr)
library(lubridate)
library(tidytext)
library(keras)
library(rpart)
library(randomForest)


presidents <- c('Mandela', 'Mbeki', 'Ramaphosa', 'Zuma', 'deKlerk', 'Motlanthe')

# Imports and cleans SONA speeches
import_data <- function(){
  
  set.seed(5073)
  
  WD <- getwd()
  setwd(WD)
  
  # read in text data files and organise these into a data frame
  filenames <- c('1994_post_elections_Mandela.txt', '1994_pre_elections_deKlerk.txt', '1995_Mandela.txt', '1996_Mandela.txt', '1997_Mandela.txt', '1998_Mandela.txt', 
                 '1999_post_elections_Mandela.txt', '1999_pre_elections_Mandela.txt', '2000_Mbeki.txt', '2001_Mbeki.txt', '2002_Mbeki.txt', '2003_Mbeki.txt', 
                 '2004_post_elections_Mbeki.txt', '2004_pre_elections_Mbeki.txt', '2005_Mbeki.txt', '2006_Mbeki.txt', '2007_Mbeki.txt', '2008_Mbeki.txt', 
                 '2009_post_elections_Zuma.txt', '2009_pre_elections_ Motlanthe.txt', '2010_Zuma.txt', '2011_Zuma.txt', '2012_Zuma.txt', '2013_Zuma.txt', 
                 '2014_post_elections_Zuma.txt', '2014_pre_elections_Zuma.txt', '2015_Zuma.txt', '2016_Zuma.txt', '2017_Zuma.txt', '2018_Ramaphosa.txt', 
                 '2019_post_elections_Ramaphosa.txt', '2019_pre_elections_Ramaphosa.txt', '2020_Ramaphosa.txt', '2021_Ramaphosa.txt', '2022_Ramaphosa.txt', '2023_Ramaphosa.txt')
  
  # read in each speech and add to vector
  this_speech <- c()
  this_speech[1] <- readChar('./sona-addresses-1994-2023/1994_post_elections_Mandela.txt', nchars = 27050)
  this_speech[2] <- readChar('./sona-addresses-1994-2023/1994_pre_elections_deKlerk.txt', nchars = 12786)
  this_speech[3] <- readChar('./sona-addresses-1994-2023/1995_Mandela.txt', nchars = 39019)
  this_speech[4] <- readChar('./sona-addresses-1994-2023/1996_Mandela.txt', nchars = 39524)
  this_speech[5] <- readChar('./sona-addresses-1994-2023/1997_Mandela.txt', nchars = 37489)
  this_speech[6] <- readChar('./sona-addresses-1994-2023/1998_Mandela.txt', nchars = 45247)
  this_speech[7] <- readChar('./sona-addresses-1994-2023/1999_post_elections_Mandela.txt', nchars = 34674)
  this_speech[8] <- readChar('./sona-addresses-1994-2023/1999_pre_elections_Mandela.txt', nchars = 41225)
  this_speech[9] <- readChar('./sona-addresses-1994-2023/2000_Mbeki.txt', nchars = 37552)
  this_speech[10] <- readChar('./sona-addresses-1994-2023/2001_Mbeki.txt', nchars = 41719)
  this_speech[11] <- readChar('./sona-addresses-1994-2023/2002_Mbeki.txt', nchars = 50544)
  this_speech[12] <- readChar('./sona-addresses-1994-2023/2003_Mbeki.txt', nchars = 58284)
  this_speech[13] <- readChar('./sona-addresses-1994-2023/2004_post_elections_Mbeki.txt', nchars = 34590)
  this_speech[14] <- readChar('./sona-addresses-1994-2023/2004_pre_elections_Mbeki.txt', nchars = 39232)
  this_speech[15] <- readChar('./sona-addresses-1994-2023/2005_Mbeki.txt', nchars = 54635)
  this_speech[16] <- readChar('./sona-addresses-1994-2023/2006_Mbeki.txt', nchars = 48643)
  this_speech[17] <- readChar('./sona-addresses-1994-2023/2007_Mbeki.txt', nchars = 48641)
  this_speech[18] <- readChar('./sona-addresses-1994-2023/2008_Mbeki.txt', nchars = 44907)
  this_speech[19] <- readChar('./sona-addresses-1994-2023/2009_post_elections_Zuma.txt', nchars = 31101)
  this_speech[20] <- readChar('./sona-addresses-1994-2023/2009_pre_elections_Motlanthe.txt', nchars = 47157)
  this_speech[21] <- readChar('./sona-addresses-1994-2023/2010_Zuma.txt', nchars = 26384)
  this_speech[22] <- readChar('./sona-addresses-1994-2023/2011_Zuma.txt', nchars = 33281)
  this_speech[23] <- readChar('./sona-addresses-1994-2023/2012_Zuma.txt', nchars = 33376)
  this_speech[24] <- readChar('./sona-addresses-1994-2023/2013_Zuma.txt', nchars = 36006)
  this_speech[25] <- readChar('./sona-addresses-1994-2023/2014_post_elections_Zuma.txt', nchars = 29403)
  this_speech[26] <- readChar('./sona-addresses-1994-2023/2014_pre_elections_Zuma.txt', nchars = 36233)
  this_speech[27] <- readChar('./sona-addresses-1994-2023/2015_Zuma.txt', nchars = 32860)
  this_speech[28] <- readChar('./sona-addresses-1994-2023/2016_Zuma.txt', nchars = 32464)
  this_speech[29] <- readChar('./sona-addresses-1994-2023/2017_Zuma.txt', nchars = 35981)
  this_speech[30] <- readChar('./sona-addresses-1994-2023/2018_Ramaphosa.txt', nchars = 33290)
  this_speech[31] <- readChar('./sona-addresses-1994-2023/2019_post_elections_Ramaphosa.txt', nchars = 42112)
  this_speech[32] <- readChar('./sona-addresses-1994-2023/2019_pre_elections_Ramaphosa.txt', nchars = 56960)
  this_speech[33] <- readChar('./sona-addresses-1994-2023/2020_Ramaphosa.txt', nchars = 47910)
  this_speech[34] <- readChar('./sona-addresses-1994-2023/2021_Ramaphosa.txt', nchars = 43352)
  this_speech[35] <- readChar('./sona-addresses-1994-2023/2022_Ramaphosa.txt', nchars = 52972)
  this_speech[36] <- readChar('./sona-addresses-1994-2023/2023_Ramaphosa.txt', nchars = 53933)
  
  # Create data frame of speeches
  sona <- data.frame(filename = filenames, speech = this_speech, stringsAsFactors = FALSE)
  
  # Add year and president columns
  sona$speech_year <- str_sub(sona$filename, start = 1, end = 4)
  sona$pres <- str_remove_all(str_extract(sona$filename, "[dA-Z].*\\."), "\\.")
  
  # Add a 'pres_index' column for predictive modelling
  sona$pres_index <-match(sona$pres, presidents)
  
  # clean the data frame by adding the date and removing unnecessary text
  replace_reg <- '(http.*?(\\s|.$))|(www.*?(\\s|.$))|&amp;|&lt;|&gt;|\n'
  
  sona <-sona %>%
    mutate(speech = str_replace_all(speech, replace_reg , ' ')
           ,date = str_sub(speech, start=1, end=30)
           ,date = str_replace_all(date, "February", "02")
           ,date = str_replace_all(date, "June", "06")
           ,date = str_replace_all(date, "Feb", "02")
           ,date = str_replace_all(date, "May", "05")
           ,date = str_replace_all(date, "Jun", "06")
           ,date = str_replace_all(date, "Thursday, ","")
           ,date = str_replace_all(date, ' ', '-')        
           ,date = str_replace_all(date, "[A-z]",'')
           ,date = str_replace_all(date, '-----', '')
           ,date = str_replace_all(date, '----', '')
           ,date = str_replace_all(date, '---', '')
           ,date = str_replace_all(date, '--', '')
    )
  
  # return data frame as tibble
  sona <- as.tibble(sona)
  save(sona, file = "sona.RData")
  return(sona)
  
}

# Tokenizes SONA data-set by sentence
generate_sentence_tokens <- function(sona_df){
  
  replace_reg <- '(http.*?(\\s|.$))|(www.*?(\\s|.$))|&amp;|&lt;|&gt;'
  
  # turn into tidy text 
  tidy_sona_sentences <- sona_df %>% 
    mutate(speech = str_replace_all(speech, replace_reg, '')) %>%  #remove links
    unnest_tokens(sentences, speech, token = 'sentences') %>%  #tokenise by sentence
    dplyr::select(sentences, pres, pres_index, speech_year) #choose requires variables
  
  # Add sentence_id to identify each row
  tidy_sona_sentences$sentence_id <- 1:nrow(tidy_sona_sentences)
  
  return(tidy_sona_sentences)
  
}

# Tokenizes sentence-centric data-set by word
generate_word_tokens <- function(tidy_sentences){
  
  unnest_reg <- "[^\\w_#@'’]"
  
  # turn into tidy text 
  tidy_sona_words <- tidy_sentences %>% 
    unnest_tokens(word, sentences, token = 'regex', pattern = unnest_reg) %>%   #tokenise by word
    filter(!word %in% stop_words$word, str_detect(word, '[A-Za-z]')) %>%  # remove stop words
    dplyr::select(word, pres, pres_index, sentence_id) # select required fields
  
}

# Generates bag-of-words model from sentence tokens and word tokens
generate_bag_of_words <- function(tidy_word_tokens, tidy_sentence_tokens, top_n){
  
  # What are the top_n most commonly used words ?
  word_bag <- tidy_word_tokens %>%
    group_by(word) %>%
    count() %>%
    ungroup() %>%
    top_n(top_n, wt = n) %>%
    select(-n)
  
  # Calculate the number of times each of these words was used in each sentence. 
  tdf <- tidy_word_tokens %>%
    inner_join(word_bag) %>%
    group_by(sentence_id,word) %>% #group by sentence, then word
    count() %>%  
    group_by(sentence_id) %>%
    mutate(total = sum(n)) %>%
    ungroup()
  
  # Reshape the data set to get it into a format for predictive modelling, 
  # using pivot_wider() so that each sentence will be in its own row, and each word in its own column
  bag_of_words <- tdf %>% 
    select(sentence_id, word, n) %>% 
    pivot_wider(names_from = word, values_from = n, values_fill = 0) %>%
    left_join(tidy_sentence_tokens %>% select(sentence_id, pres)) %>%
    select(sentence_id, pres, everything())
  
}

# Generates tf-idf model from sentence tokens and word tokens
generate_tfidf_dataset <- function(tidy_word_tokens, tidy_sentence_tokens, top_n){
  
  word_bag <- tidy_word_tokens %>%
    group_by(word) %>%
    count() %>%
    ungroup() %>%
    top_n(top_n, wt = n) %>%
    select(-n)
  
  tf_idf_tokens <- tidy_word_tokens %>%
    inner_join(word_bag) %>%
    group_by(sentence_id,word) %>%
    count() %>%  
    bind_tf_idf(word, sentence_id, n) 
  
  tfidf <- tf_idf_tokens %>% 
    select(sentence_id, word, tf_idf) %>% 
    pivot_wider(names_from = word, values_from = tf_idf, values_fill = 0) %>%  
    left_join(tidy_sentence_tokens %>% select(sentence_id,pres))

}

# Splits data into test and train
generate_test_train_sets <- function(bag_of_words) {
  
  set.seed(123)
  
  training_ids <- bag_of_words %>% 
    group_by(pres) %>% 
    slice_sample(prop = 0.7) %>% 
    ungroup() %>%
    select(sentence_id)
  
  training <- bag_of_words %>% 
    right_join(training_ids, by = 'sentence_id') %>%
    select(-sentence_id)
  
  test <- bag_of_words %>% 
    anti_join(training_ids, by = 'sentence_id') %>%
    select(-sentence_id)
  
  return(list("training" = training, "test" = test))
  
}

# Classification tree model
perform_predictive_modelling_classification_tree <- function(training, test){
  
  model <- rpart(pres ~ ., training, method = 'class')
  
  #plot(model, main = 'Full Classification Tree')
  #text(model, use.n = TRUE, all = TRUE, cex=.8)
  
  fitted <- predict(model, newdata = test, type = 'class')
  prediction <- table(test$pres, fitted)
  
  return(list(
    "model" = model,
    "prediction" = prediction,
    "accuracy" = round(sum(diag(prediction))/sum(prediction), 3) 
  ))
  
  
}

# Neural network model
perform_predictive_modelling_neural_network <- function(train, test){
  
  training_nn <- train %>% mutate(pres_index = match(pres, presidents))
  test_nn <- test %>% mutate(pres_index = match(pres, presidents))
  
  train_target <- training_nn$pres_index
  test_target <- test_nn$pres_index
  
  train_target <- to_categorical(train_target)
  test_target_original <-test_target
  test_target <- to_categorical(test_target)
  
  train_features <- as.matrix(training_nn %>% select(-pres, -pres_index))
  test_features <- as.matrix(test_nn %>% select(-pres, -pres_index))
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, input_shape = dim(train_features)[2], activation = "relu") %>%
    layer_dropout(rate = 0.1) %>% 
    layer_dense(units = 128, activation = 'relu') %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = dim(train_target)[2], activation = "softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = "adam",
    metrics = "accuracy"
  )
  
  history <- model %>% fit(train_features, train_target, 
                           epochs = 50, 
                           validation_split = 0.1, 
                           shuffle = TRUE, 
                           batch_size = 128, 
                           verbose = 0) 
  plot(history)
  
  model %>% evaluate(test_features, test_target)
  y_test_hat <- model %>% predict(test_features) %>% k_argmax() %>% as.numeric()
  prediction <- table(test_target_original, y_test_hat)
  
  return(list(
    "model" = model,
    "prediction" = prediction,
    "history" = history,
    "accuracy" = round(sum(diag(prediction))/sum(prediction), 3)
  ))
  
}

# Random forest model
perform_predictive_modelling_random_forest <- function(train, test){
  
  training_rf <- train %>% mutate(pres_index = match(pres, presidents))
  training_rf$pres_index <- as.factor(training_rf$pres_index)
  training_rf <- training_rf %>% select(-pres)
  
  test_rf <- test %>% mutate(pres_index = as.factor(match(pres, presidents)))
  test_rf$pres_index <- as.factor(test_rf$pres_index)
  test_rf <- test_rf %>% select(-pres)
  
  model <- randomForest(pres_index ~ .,  
                          data = training_rf,  
                          do.trace = TRUE, 
                          importance = TRUE,
                          ntree = 150,
                          maxnodes = 250,
                          proximity = TRUE) 
  
  yhat_rf = predict(model, newdata = test_rf)
  
  prediction <- table(test_rf$pres_index , yhat_rf)
  
  return(list(
    "model" = model,
    "prediction" = prediction,
    "accuracy" = round(sum(diag(prediction))/sum(prediction), 3) 
  ))
  
}

# CNN model
perform_predictive_modelling_convolutional_neural_network <- function(tidy_sentences, max_features = 200){
  
  # Tokenise words from the sentences
  tokenizer = text_tokenizer(num_words = max_features)
  fit_text_tokenizer(tokenizer, tidy_sentences$sentences)
  sequences = tokenizer$texts_to_sequences(tidy_sentences$sentences)
  
  training_ids <- tidy_sentences %>% 
    slice_sample(prop = 0.7) %>% 
    select(sentence_id)
  
  cnn_y <- tidy_sentences$pres_index
  cnn_training_rows <- which(tidy_sentences$sentence_id %in% training_ids$sentence_id)
  
  cnn_train <- list()
  cnn_test <- list()
  cnn_train$x <- sequences[cnn_training_rows]
  cnn_test$x <-  sequences[-cnn_training_rows]
  
  cnn_train$y <- cnn_y[cnn_training_rows]
  cnn_train$y <- to_categorical(cnn_train$y)
  
  cnn_test$y <-  cnn_y[-cnn_training_rows]
  cnn_test$y_orig <- cnn_test$y
  cnn_test$y <- to_categorical(cnn_test$y)
  
  maxlen <- 32               
  x_train <- cnn_train$x %>% pad_sequences(maxlen = maxlen)
  x_test <- cnn_test$x %>% pad_sequences(maxlen = maxlen)
  
  output_layers <- length(unique(tidy_sentences$pres)) + 1
  
  embedding_dims <- 10
  model <- keras_model_sequential() %>% 
    layer_embedding(max_features, output_dim = embedding_dims, input_length = maxlen) %>%
    layer_dropout(0.2) %>%
    layer_conv_1d(filters = 64, kernel_size = 8, activation = "relu") %>%
    layer_max_pooling_1d(pool_size = 2) %>%
    layer_flatten() %>%
    layer_dense(32, activation = "relu") %>%
    layer_dense(output_layers, activation = "softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = "adam",
    metrics = "accuracy"
  )
  
  history <- model %>% fit(x_train, cnn_train$y, batch_size = 128, epochs = 50, verbose = 0, validation_split = 0.1, shuffle = TRUE)
  results <- model %>% evaluate(x_test, cnn_test$y, batch_size=128, verbose = 2)
  
  y_test_hat <- model %>% predict(x_test) %>% k_argmax() %>% as.numeric()
  prediction <- table(cnn_test$y_orig, y_test_hat)
  
  return(list(
    "results" = results,
    "prediction" = prediction
  ))
  
}