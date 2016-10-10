library(tm)
library(topicmodels)

library(ggplot2)
library(reshape2)

# All utility functions
set.seed(5) # sample(1:1000, 1)
topics <- NULL
alpha <- NULL

# load data
load('data/dtm/corpus.dtm.RData')
load('data/dtm/corpus_e.dtm.RData')

setGlobalVariables <- function(){
  assign("topics", c(3, 5, 8), envir = .GlobalEnv)
  
  load("results/k_eval_cv/corpus.rda")
  topics_alpha <- as.numeric(rownames(corpus_alpha[["vem"]]))
  index <- which(topics_alpha %in% topics)
  alpha <- as.data.frame(rowMeans(corpus_alpha[["vem"]][index,]))
  
  assign("alpha", alpha, envir = .GlobalEnv)
  
  rm(corpus_alpha)
  rm(corpus_perplexity)
  rm(topics_alpha)
  rm(index)
  
  # training_data - 1:153
  # test_data - 154:170
  # cog_test_data - 171:271
  # pat_test_data - 272:1563
  # merge_test_data - 1564: 3142
}
