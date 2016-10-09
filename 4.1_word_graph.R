library(plyr)
library(dplyr)
library("wordcloud")
library("RColorBrewer")


load("results/models/lda/mc_model_lda.rda")

# Word cloud for LDA
m_names <- c('vem', 'vem_fixed', 'gibbs')

for(m_name in m_names){
  # to extract probability and terms
  model <- mc_model_lda[[5]][[m_name]]
  theta <- model@gamma
  rownames(theta) <- model@documents
  colnames(theta) <- paste("t", 1:ncol(theta), sep = "_")
  phi <- exp(model@beta)
  colnames(phi) <- model@terms
  rownames(phi) <- colnames(theta)
  result <- list(theta = theta, phi = phi)
  
  # term - topic distribution
  # View(result$phi)
  
  # document - topic distribution
  # View(result$theta)
  
  # formatted topic mixture
  topicsMixture <- as.data.frame(t(result$phi))
  variables <- colnames(topicsMixture)
  topicsMixture[,variables] <- lapply(topicsMixture[,variables] , 
                                      function(x) round(x, 4)) # convert them into factors
  
  formattedTopicsMixture <- list()
  pick <- 20
  
  for(i in 1:ncol(topicsMixture)){
    df <- data.frame(terms= character(0), weight = numeric(0), stringsAsFactors = FALSE)
    topicsMixture<- topicsMixture[ order(topicsMixture[,i], decreasing = TRUE), ]
    for(j in 1: pick){
      df[j,] <- c(as.character(rownames(topicsMixture)[j]), weight = topicsMixture[j, i])
    }
    
    formattedTopicsMixture[[colnames(topicsMixture)[i]]] <- df
    
  }
  
  write.csv(formattedTopicsMixture, paste('results/models/lda/terms_', m_name, '.csv', sep=''))
  
  # Create word cloud
  d<- data.frame(terms= character(0), freq = numeric(0), stringsAsFactors = FALSE)
  for(i in 1:length(formattedTopicsMixture)){
    selectedWords <- formattedTopicsMixture[[i]]#subset(formattedTopicsMixture[[i]], formattedTopicsMixture[[i]]$weight >= 0.05)
    d <- rbind(d, data.frame(terms = selectedWords$terms, freq = rep(1, nrow(selectedWords))))
  }
  
  d<- ddply(d, .(terms), summarize, freq= sum(freq))
  
  # word cloud
  wordcloud(words = d$terms, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE,
            scale = c(2, 0.5),
            colors=brewer.pal(8, "Dark2"))
}




# CTM

load("results/models/ctm/mc_model_ctm.rda")

m_names <- c('CTM')
for(m_name in m_names){
  # to extract probability and terms
  model <- mc_model_ctm[[5]][[m_name]]
  theta <- model@gamma
  rownames(theta) <- model@documents
  colnames(theta) <- paste("t", 1:ncol(theta), sep = "_")
  phi <- exp(model@beta)
  colnames(phi) <- model@terms
  rownames(phi) <- colnames(theta)
  result <- list(theta = theta, phi = phi)
  
  # term - topic distribution
  # View(result$phi)
  # View(model$beta) # cannot use this as it is lograthimic # R help
  
  # document - topic distribution
  # View(result$theta)
  # View(model$gamma)
  
  # formatted topic mixture
  topicsMixture <- as.data.frame(t(result$phi))
  variables <- colnames(topicsMixture)
  topicsMixture[,variables] <- lapply(topicsMixture[,variables] , 
                                      function(x) round(x, 4)) # convert them into factors
  
  formattedTopicsMixture <- list()
  pick <- 20
  
  for(i in 1:ncol(topicsMixture)){
    df <- data.frame(terms= character(0), weight = numeric(0), stringsAsFactors = FALSE)
    # df[i,] <- c
    topicsMixture<- topicsMixture[ order(topicsMixture[,i], decreasing = TRUE), ]
    for(j in 1: pick){
      df[j,] <- c(as.character(rownames(topicsMixture)[j]), weight = topicsMixture[j, i])
    }
    
    formattedTopicsMixture[[colnames(topicsMixture)[i]]] <- df
    
  }
  
  write.csv(formattedTopicsMixture, paste('results/models/ctm/terms_', m_name, '.csv', sep=''))
  
  # Create word cloud
  d<- data.frame(terms= character(0), freq = numeric(0), stringsAsFactors = FALSE)
  for(i in 1:length(formattedTopicsMixture)){
    selectedWords <- formattedTopicsMixture[[i]]#subset(formattedTopicsMixture[[i]], formattedTopicsMixture[[i]]$weight >= 0.05)
    d <- rbind(d, data.frame(terms = selectedWords$terms, freq = rep(1, nrow(selectedWords))))
  }
  
  d<- ddply(d, .(terms), summarize, freq= sum(freq))
  
  # word cloud
  wordcloud(words = d$terms, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE,
            scale = c(2, 0.5),
            colors=brewer.pal(8, "Dark2"))
}




# Test case word cloud
load('results/models/lda/mc_model_test_eval.RData')

mc_model_test_list <- list()
mc_model_test_list[[1]] <- posterior(mc_model_test_eval[[1]][[5]]$vem_fixed)$terms
mc_model_test_list[[2]] <- posterior(mc_model_test_eval[[2]][[5]]$vem_fixed)$terms
mc_model_test_list[[3]] <- posterior(mc_model_test_eval[[3]][[5]]$vem_fixed)$terms
mc_model_test_list[[4]] <- posterior(mc_model_test_eval[[4]][[5]]$vem_fixed)$terms


mc_model_test_list[[5]] <- posterior(mc_model_test_eval[[1]][[5]]$gibbs)$terms
mc_model_test_list[[6]] <- posterior(mc_model_test_eval[[2]][[5]]$gibbs)$terms
mc_model_test_list[[7]] <- posterior(mc_model_test_eval[[3]][[5]]$gibbs)$terms
mc_model_test_list[[8]] <- posterior(mc_model_test_eval[[4]][[5]]$gibbs)$terms

save(mc_model_test_list, file = 'results/models/lda/mc_model_test_list.RData')


formatted_word_list <- list()

for(i in 1:length(mc_model_test_list)){
  temp <- t(mc_model_test_list[[i]])
  terms <- c()
  for (k in 1:5){
    temp <- temp[ order(-temp[,k]), ]
    terms <- append(terms, rownames(temp)[1:10])
  }
  
  d <- data.frame(terms = terms, freq = rep(1, length(terms)))
  d<- ddply(d, .(terms), summarize, freq= sum(freq))
  
  # word cloud
  wordcloud(words = d$terms, freq = d$freq, min.freq = 1,
            scale = c(2, 0.5),
            max.words=200, random.order=FALSE,
            colors=brewer.pal(8, "Dark2"))
  
  formatted_word_list[[i]] <- d
}

