library(scutr)
library(keras)
library(tidyverse)


WD <- getwd()
setwd(WD)


source("functions.R")
source("submission_functions.R")

# Tokenise by sentence, then generate bag-of-words and tf-idf models
sentences <- generate_sona_sentence_tokens()
bag_of_words_splits <- generate_sona_bag_of_words(sentences$unbalanced, 200)
tfidf_splits <- generate_sona_tfidf(sentences$unbalanced, 200)

save(sentences, bag_of_words_splits, tfidf_splits, file = "dataset_splits.RData")

#___________________________________________
# NEURAL NETWORKS
neural_network_results_bag_of_words_unbalanced <- perform_predictive_modelling_neural_network(bag_of_words_splits$unbalanced$train, 
                                                                                              bag_of_words_splits$unbalanced$test)

neural_networks_results_df_type <- c('Bag of words - unbalanced')
neural_networks_results_accuracy <- c(neural_network_results_bag_of_words_unbalanced$accuracy)
neural_networks_bag_of_words_unbalanced_prediction <- neural_network_results_bag_of_words_unbalanced$prediction

neural_network_results_bag_of_words_balanced <- perform_predictive_modelling_neural_network(bag_of_words_splits$balanced$train, 
                                                                                            bag_of_words_splits$balanced$test)

neural_networks_results_df_type <- append(neural_networks_results_df_type, 'Bag of words - under-sampled')
neural_networks_results_accuracy <- append(neural_networks_results_accuracy, neural_network_results_bag_of_words_balanced$accuracy)
neural_networks_bag_of_words_balanced_prediction <- neural_network_results_bag_of_words_balanced$prediction


neural_network_results_bag_of_words_oversampled <- perform_predictive_modelling_neural_network(bag_of_words_splits$oversampled$train, 
                                                                                               bag_of_words_splits$oversampled$test)

neural_networks_results_df_type <- append(neural_networks_results_df_type, 'Bag of words - over-sampled')
neural_networks_results_accuracy <- append(neural_networks_results_accuracy, neural_network_results_bag_of_words_oversampled$accuracy)
neural_networks_bag_of_words_oversampled_prediction <- neural_network_results_bag_of_words_oversampled$prediction

neural_network_results_tfidf_unbalanced <- perform_predictive_modelling_neural_network(tfidf_splits$unbalanced$train, 
                                                                                       tfidf_splits$unbalanced$test)

neural_networks_results_df_type <- append(neural_networks_results_df_type, 'TFIDF - unbalanced')
neural_networks_results_accuracy <- append(neural_networks_results_accuracy, neural_network_results_tfidf_unbalanced$accuracy)
neural_networks_tfidf_unbalanced_prediction <- neural_network_results_tfidf_unbalanced$prediction

neural_network_results_tfidf_balanced <- perform_predictive_modelling_neural_network(tfidf_splits$balanced$train, 
                                                                                     tfidf_splits$balanced$test)


neural_networks_results_df_type <- append(neural_networks_results_df_type, 'TFIDF - under-sampled')
neural_networks_results_accuracy <- append(neural_networks_results_accuracy, neural_network_results_tfidf_balanced$accuracy)
neural_networks_tfidf_balanced_prediction <- neural_network_results_tfidf_balanced$prediction


neural_network_results_tfidf_oversampled <- perform_predictive_modelling_neural_network(tfidf_splits$oversampled$train, 
                                                                                        tfidf_splits$oversampled$test)


neural_networks_results_df_type <- append(neural_networks_results_df_type, 'TFIDF - over-sampled')
neural_networks_results_accuracy <- append(neural_networks_results_accuracy, neural_network_results_tfidf_oversampled$accuracy)
neural_networks_tfidf_oversampled_prediction <- neural_network_results_tfidf_oversampled$prediction

neural_networks_results <- data.frame(neural_networks_results_df_type, neural_networks_results_accuracy)
colnames(neural_networks_results) <- c('Tokenisation model', 'Accuracy')

colnames(neural_networks_bag_of_words_unbalanced_prediction) = c('Mandela', 'Mbeki', 'Ramaphosa', 'Zuma', 'deKlerk', 'Motlanthe')
rownames(neural_networks_bag_of_words_unbalanced_prediction) = c('Mandela', 'Mbeki', 'Ramaphosa', 'Zuma', 'deKlerk', 'Motlanthe')

colnames(neural_networks_bag_of_words_balanced_prediction) = c('Mandela', 'Mbeki', 'Ramaphosa', 'Zuma')
rownames(neural_networks_bag_of_words_balanced_prediction) = c('Mandela', 'Mbeki', 'Ramaphosa', 'Zuma')

colnames(neural_networks_bag_of_words_oversampled_prediction) = c('Mandela', 'Mbeki', 'Ramaphosa', 'Zuma', 'deKlerk', 'Motlanthe')
rownames(neural_networks_bag_of_words_oversampled_prediction) = c('Mandela', 'Mbeki', 'Ramaphosa', 'Zuma', 'deKlerk', 'Motlanthe')

colnames(neural_networks_tfidf_unbalanced_prediction) = c('Mandela', 'Mbeki', 'Ramaphosa', 'Zuma', 'deKlerk', 'Motlanthe')
rownames(neural_networks_tfidf_unbalanced_prediction) = c('Mandela', 'Mbeki', 'Ramaphosa', 'Zuma', 'deKlerk', 'Motlanthe')

colnames(neural_networks_tfidf_balanced_prediction) = c('Mandela', 'Mbeki', 'Ramaphosa', 'Zuma')
rownames(neural_networks_tfidf_balanced_prediction) = c('Mandela', 'Mbeki', 'Ramaphosa', 'Zuma')

colnames(neural_networks_tfidf_oversampled_prediction) = c('Mandela', 'Mbeki', 'Ramaphosa', 'Zuma', 'deKlerk', 'Motlanthe')
rownames(neural_networks_tfidf_oversampled_prediction) = c('Mandela', 'Mbeki', 'Ramaphosa', 'Zuma', 'deKlerk', 'Motlanthe')

save(neural_networks_results, 
     neural_networks_bag_of_words_unbalanced_prediction,
     neural_networks_bag_of_words_balanced_prediction,
     neural_networks_bag_of_words_oversampled_prediction,
     neural_networks_tfidf_unbalanced_prediction,
     neural_networks_tfidf_balanced_prediction,
     neural_networks_tfidf_oversampled_prediction,
     file = "neural_networks_results.RData")


#___________________________________________
# CLASSIFICATION TREE
tree_results_bag_of_words_unbalanced <- perform_predictive_modelling_classification_tree(bag_of_words_splits$unbalanced$train, 
                                                                                         bag_of_words_splits$unbalanced$test)

classification_tree_results_df_type <- c('Bag of words - unbalanced')
classification_tree_results_accuracy <- c(tree_results_bag_of_words_unbalanced$accuracy)
classification_tree_bag_of_words_unbalanced_prediction <- tree_results_bag_of_words_unbalanced$prediction

tree_results_bag_of_words_balanced <- perform_predictive_modelling_classification_tree(bag_of_words_splits$balanced$train, 
                                                                                       bag_of_words_splits$balanced$test)

classification_tree_results_df_type <- append(classification_tree_results_df_type, 'Bag of words - under-sampled')
classification_tree_results_accuracy <- append(classification_tree_results_accuracy, tree_results_bag_of_words_balanced$accuracy)
classification_tree_bag_of_words_balanced_prediction <- tree_results_bag_of_words_balanced$prediction


tree_results_bag_of_words_oversampled <- perform_predictive_modelling_classification_tree(bag_of_words_splits$oversampled$train, 
                                                                                          bag_of_words_splits$oversampled$test)

classification_tree_results_df_type <- append(classification_tree_results_df_type, 'Bag of words - over-sampled')
classification_tree_results_accuracy <- append(classification_tree_results_accuracy, tree_results_bag_of_words_oversampled$accuracy)
classification_tree_bag_of_words_oversampled_prediction <- tree_results_bag_of_words_oversampled$prediction


tree_results_tfidf_unbalanced <- perform_predictive_modelling_classification_tree(tfidf_splits$unbalanced$train, 
                                                                                  tfidf_splits$unbalanced$test)

classification_tree_results_df_type <- append(classification_tree_results_df_type, 'TFIDF - unbalanced')
classification_tree_results_accuracy <- append(classification_tree_results_accuracy, tree_results_tfidf_unbalanced$accuracy)
classification_tree_tfidf_unbalanced_prediction <- tree_results_tfidf_unbalanced$prediction


tree_results_tfidf_balanced <- perform_predictive_modelling_classification_tree(tfidf_splits$balanced$train, 
                                                                                tfidf_splits$balanced$test)

classification_tree_results_df_type <- append(classification_tree_results_df_type, 'TFIDF - under-sampled')
classification_tree_results_accuracy <- append(classification_tree_results_accuracy, tree_results_tfidf_balanced$accuracy)
classification_tree_tfidf_balanced_prediction <- tree_results_tfidf_balanced$prediction

tree_results_tfidf_oversampled <- perform_predictive_modelling_classification_tree(tfidf_splits$oversampled$train, 
                                                                                   tfidf_splits$oversampled$test)

classification_tree_results_df_type <- append(classification_tree_results_df_type, 'TFIDF - over-sampled')
classification_tree_results_accuracy <- append(classification_tree_results_accuracy, tree_results_tfidf_oversampled$accuracy)
classification_tree_tfidf_oversampled_prediction <- tree_results_tfidf_oversampled$prediction


classification_tree_results <- data.frame(classification_tree_results_df_type, classification_tree_results_accuracy)
colnames(classification_tree_results) <- c('Tokenisation model', 'Accuracy')

save(classification_tree_results, 
     classification_tree_bag_of_words_unbalanced_prediction,
     classification_tree_bag_of_words_balanced_prediction,
     classification_tree_bag_of_words_balanced_prediction,
     classification_tree_tfidf_unbalanced_prediction,
     classification_tree_tfidf_balanced_prediction,
     classification_tree_tfidf_oversampled_prediction,
     file = "classification_tree_results.RData")

#___________________________________________
# RANDOM FOREST
random_forest_results_bag_of_words_unbalanced <- perform_predictive_modelling_random_forest(bag_of_words_splits$unbalanced$train, 
                                                                                            bag_of_words_splits$unbalanced$test)

random_forest_results_df_type <- c('Bag of words - unbalanced')
random_forest_results_accuracy <- c(random_forest_results_bag_of_words_unbalanced$accuracy)
random_forest_bag_of_words_unbalanced_prediction <- random_forest_results_bag_of_words_unbalanced$prediction

random_forest_results_bag_of_words_balanced <- perform_predictive_modelling_random_forest(bag_of_words_splits$balanced$train, 
                                                                                          bag_of_words_splits$balanced$test)

random_forest_results_df_type <- append(random_forest_results_df_type, 'Bag of words - under-sampled')
random_forest_results_accuracy <- append(random_forest_results_accuracy, random_forest_results_bag_of_words_balanced$accuracy)
random_forest_bag_of_words_balanced_prediction <- random_forest_results_bag_of_words_balanced$prediction

random_forest_results_bag_of_words_oversampled <- perform_predictive_modelling_random_forest(bag_of_words_splits$oversampled$train, 
                                                                                             bag_of_words_splits$oversampled$test)

random_forest_results_df_type <- append(random_forest_results_df_type, 'Bag of words - over-sampled')
random_forest_results_accuracy <- append(random_forest_results_accuracy, random_forest_results_bag_of_words_oversampled$accuracy)
random_forest_bag_of_words_oversampled_prediction <- random_forest_results_bag_of_words_oversampled$prediction

random_forest_results_tfidf_unbalanced <- perform_predictive_modelling_random_forest(tfidf_splits$unbalanced$train, 
                                                                                     tfidf_splits$unbalanced$test)

random_forest_results_df_type <- append(random_forest_results_df_type, 'TFIDF - unbalanced')
random_forest_results_accuracy <- append(random_forest_results_accuracy, random_forest_results_tfidf_unbalanced$accuracy)
random_forest_tfidf_unbalanced_prediction <- random_forest_results_tfidf_unbalanced$prediction

random_forest_results_tfidf_balanced <- perform_predictive_modelling_random_forest(tfidf_splits$balanced$train, 
                                                                                   tfidf_splits$balanced$test)

random_forest_results_df_type <- append(random_forest_results_df_type, 'TFIDF - under-sampled')
random_forest_results_accuracy <- append(random_forest_results_accuracy, random_forest_results_tfidf_balanced$accuracy)
random_forest_tfidf_balanced_prediction <- random_forest_results_tfidf_balanced$prediction

random_forest_results_tfidf_oversampled <- perform_predictive_modelling_random_forest(tfidf_splits$oversampled$train, 
                                                                                      tfidf_splits$oversampled$test)


random_forest_results_df_type <- append(random_forest_results_df_type, 'TFIDF - over-sampled')
random_forest_results_accuracy <- append(random_forest_results_accuracy, random_forest_results_tfidf_oversampled$accuracy)
random_forest_tfidf_oversampled_prediction <- random_forest_results_tfidf_oversampled$prediction

random_forest_results <- data.frame(random_forest_results_df_type, random_forest_results_accuracy)
colnames(random_forest_results) <- c('Tokenisation model', 'Accuracy')

save(random_forest_results, 
     random_forest_bag_of_words_unbalanced_prediction,
     random_forest_bag_of_words_balanced_prediction,
     random_forest_bag_of_words_oversampled_prediction,
     random_forest_tfidf_unbalanced_prediction,
     random_forest_tfidf_balanced_prediction,
     random_forest_tfidf_oversampled_prediction,
     file = "random_forest_results.RData")

#___________________________________________
# CNN
convolutional_neural_results_unbalanced_100 <- perform_predictive_modelling_convolutional_neural_network(sentences$unbalanced, 100)
convolutional_neural_results_balanced_100 <- perform_predictive_modelling_convolutional_neural_network(sentences$balanced, 100)

convolutional_neural_results_unbalanced_200 <- perform_predictive_modelling_convolutional_neural_network(sentences$unbalanced, 200)
convolutional_neural_results_balanced_200 <- perform_predictive_modelling_convolutional_neural_network(sentences$balanced, 200)

convolutional_neural_results_unbalanced_300 <- perform_predictive_modelling_convolutional_neural_network(sentences$unbalanced, 300)
convolutional_neural_results_balanced_300 <- perform_predictive_modelling_convolutional_neural_network(sentences$balanced, 300)

cnn_results_df_type <- c('Top 100 words - unbalanced data set')
cnn_results_loss <- c(round(convolutional_neural_results_unbalanced_100$results["loss"], 3))
cnn_results_accuracy <- c(round(convolutional_neural_results_unbalanced_100$results["accuracy"], 3))
cnn_results_prediction_unbalanced_100 <- convolutional_neural_results_unbalanced_100$prediction

cnn_results_df_type <- append(cnn_results_df_type, 'Top 100 words - under-sampled data set')
cnn_results_loss <- append(cnn_results_loss, round(convolutional_neural_results_balanced_100$results["loss"], 3))
cnn_results_accuracy <- append(cnn_results_accuracy, round(convolutional_neural_results_balanced_100$results["accuracy"], 3))
cnn_results_prediction_undersampled_100 <- convolutional_neural_results_balanced_100$prediction

cnn_results_df_type <- append(cnn_results_df_type, 'Top 200 words - unbalanced data set')
cnn_results_loss <- append(cnn_results_loss, round(convolutional_neural_results_unbalanced_200$results["loss"], 3))
cnn_results_accuracy <- append(cnn_results_accuracy, round(convolutional_neural_results_unbalanced_200$results["accuracy"], 3))
cnn_results_prediction <- append(cnn_results_prediction, convolutional_neural_results_unbalanced_200$prediction)
cnn_results_prediction_unbalanced_200 <-convolutional_neural_results_unbalanced_200$prediction

cnn_results_df_type <- append(cnn_results_df_type, 'Top 200 words - under-sampled data set')
cnn_results_loss <- append(cnn_results_loss, round(convolutional_neural_results_balanced_200$results["loss"], 3))
cnn_results_accuracy <- append(cnn_results_accuracy, round(convolutional_neural_results_balanced_200$results["accuracy"], 3))
cnn_results_prediction_undersampled_200 <-convolutional_neural_results_balanced_200$prediction

cnn_results_df_type <- append(cnn_results_df_type, 'Top 300 words - unbalanced data set')
cnn_results_loss <- append(cnn_results_loss, round(convolutional_neural_results_unbalanced_300$results["loss"], 3))
cnn_results_accuracy <- append(cnn_results_accuracy, round(convolutional_neural_results_unbalanced_300$results["accuracy"], 3))
cnn_results_prediction <- append(cnn_results_prediction, convolutional_neural_results_unbalanced_300$prediction)
cnn_results_prediction_unbalanced_300 <-convolutional_neural_results_unbalanced_300$prediction

cnn_results_df_type <- append(cnn_results_df_type, 'Top 300 words - under-sampled data set')
cnn_results_loss <- append(cnn_results_loss, round(convolutional_neural_results_balanced_300$results["loss"], 3))
cnn_results_accuracy <- append(cnn_results_accuracy, round(convolutional_neural_results_balanced_300$results["accuracy"], 3))
cnn_results_prediction <- append(cnn_results_prediction, convolutional_neural_results_balanced_300$prediction)
cnn_results_prediction_undersampled_300 <-convolutional_neural_results_balanced_300$prediction

cnn_results <- data.frame(cnn_results_df_type, cnn_results_loss, cnn_results_accuracy)
colnames(cnn_results) <- c('Data Set', 'Loss', 'Accuracy')

save(cnn_results, cnn_results_prediction_unbalanced_100,
     cnn_results_prediction_undersampled_100,
     cnn_results_prediction_unbalanced_200,
     cnn_results_prediction_undersampled_200,
     cnn_results_prediction_unbalanced_300,
     cnn_results_prediction_undersampled_300,
     file = "convolutional_neural_results.RData")










