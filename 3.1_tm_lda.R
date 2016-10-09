source('util.R')

# corpus.dtm - document term matrix
# training_data - 1:153
# test_data - 154:170
# cog_test_data - 171:271
# pat_test_data - 272:1563
# merge_test_data - 1564: 3142

# Training model function starts
train_model_lda <- function(){
  mc_model_lda <- list()
  # training_data - 1:153
  mc <- corpus.dtm[1:153,]
  
  # 53, 43
  # 14, 0510
  # 32, 14072016, 09102016  - really good
  #14072016, 177,343, 616
  SEED <- sample(1:1000, 1)
  SEED<-616
  
  
  for(k in topics){
    mc_model_lda[[k]] <- list(
      vem = LDA(mc, k = k, control = list(alpha = alpha[as.character(k),], seed = SEED)),
      vem_fixed = LDA(mc, k = k, 
                      control = list(alpha = 1, estimate.alpha = FALSE, seed=SEED)),
      gibbs = LDA(mc, k = k, method = "Gibbs",
                  control = list(alpha = alpha[as.character(k),], seed = SEED, burnin = 1000, thin = 100,
                                 iter = 1000, best = TRUE)))
    
  }
  
  topTenLdaVEM_616 <- terms(mc_model_lda[[5]]$vem_fixed, 10)
  View(topTenLdaVEM_616)
  
  save(mc_model_lda, file = paste0(file_path, "mc_model_lda.rda"))
  return (mc_model_lda)
}

# Training model function ends





# Evaluation functions start
# Evaluate the model on test data

eval_model <- function(data, mc_model, num){
  # corpus.dtm[154:170,], mc_model = mc_model_lda, num = 1, topics = topics
  mc_model_lda_eval <- list()
  for(k in topics){
    mc_model_lda_eval[[k]] <- list(
      vem = LDA(data, model= mc_model[[k]]$vem,
                control = list(estimate.beta = FALSE, seed = SEED)),
      vem_fixed = LDA(data, model= mc_model[[k]]$vem_fixed,
                      control = list(estimate.beta = FALSE, seed = SEED)),
      gibbs = LDA(data, method = "Gibbs", model= mc_model[[k]]$gibbs,
                  control = list(estimate.beta = FALSE,
                                 seed = SEED, burnin = 1000, thin = 100, iter = 1000, best = TRUE)))
    
  }
  
  save(mc_model_lda_eval, file = paste(file_path, "mc_model_lda_eval", num, ".RData", sep=''))
  return (mc_model_lda_eval)
}

# Evaluate the model perplexity on test data
eval_perplexity <- function(data, mc_model_test, num){
  corpus_test_alpha <- corpus_test_perplexity <- as.data.frame(matrix(NA,
                                                            nrow = length(topics), ncol = 3, dimnames = list(topics, methods)))
  
  for (method in methods) {
    for (T in topics) {
      corpus_test_alpha[method][paste(T),1] <- mc_model_test[[T]][[method]]@alpha
      if(!is.null(data)){
        if(method == 'gibbs'){
          corpus_test_perplexity[method][paste(T),1] <- perplexity(mc_model_test[[T]][[method]], data, 
                                                              use_theta = TRUE, estimate_theta = TRUE)
        }
        else{
          corpus_test_perplexity[method][paste(T),1] <- perplexity(mc_model_test[[T]][[method]], data, use_theta = FALSE)
        }
      }
      else{
        if(method == 'gibbs'){
          corpus_test_perplexity[method][paste(T),1] <- perplexity(mc_model_test[[T]][[method]], corpus.dtm[1:153,], 
                                                              use_theta = TRUE, estimate_theta = TRUE)
        }
        else{
          corpus_test_perplexity[method][paste(T),1] <- perplexity(mc_model_test[[T]][[method]], use_theta = FALSE)
        }
      }
    }
  }
  
  corpus_test_alpha$topics <- rownames(corpus_test_alpha)
  corpus_test_alpha <- melt(corpus_test_alpha, id.vars="topics",
                       variable.name="Method", value.name="Alpha")
  
  
  corpus_test_perplexity$topics <- rownames(corpus_test_perplexity)
  corpus_test_perplexity <- melt(corpus_test_perplexity, id.vars="topics",
                            variable.name="Method", value.name="Perplexity")
  
  save(corpus_test_alpha, corpus_test_perplexity, file = paste(file_path, num, "corpus_test.rda", sep=""))
  
  return (list(corpus_test_alpha, corpus_test_perplexity))
}

# Evaluation function end





# Create formatted data frame
graph_formatter <- function(mc_model_test_perplexity){
  df <- as.data.frame(as.matrix(0, nrow = (length(topics) * length(methods) * 5)))
  df$dataset <- NA
  df$topics <- NA
  df$Method <- NA
  df$perplexity <- NA
  df$V1 <- NULL
  index <- 1
  end <- 9
  for(i in 1:length(mc_model_test_perplexity)){
    temp <-mc_model_test_perplexity[[i]][[2]]
    df[index:end,]$dataset <- paste0('Test set ', i)
    df[index:end,]$topics <- temp$topics
    df[index:end,]$Method <- as.character(temp$Method)
    df[index:end,]$perplexity <- temp$Perplexity
    
    index <- 1 + (9 * i)
    end <- index + 9 - 1
  }
  
  df[df$dataset == 'Test set 5', ]$dataset <- 'Train set'
  
  save(df, file = paste0(file_path, "mc_model_test_df.RData"))
  return (df)
}

# end data frame formatter





# Test the model - using test data set start

test_model_lda <- function(mc_model_lda){
  # Store the results in a model
  mc_model_test_eval <- list()
  mc_model_test_perplexity <- list()
  
  
  
  # training_data - 1:153
  # test_data - 154:170
  # cog_test_data - 171:271
  # pat_test_data - 272:1563
  # merge_test_data - 1564: 3142
  
  # Frass data
  mc_model_test_eval[[1]] <- eval_model(corpus.dtm[154:170,], mc_model = mc_model_lda, num = 1)
  mc_model_test_perplexity[[1]] <- eval_perplexity(corpus.dtm[154:170,], mc_model_test_eval[[1]], num = 1)
  
  
  # Cognition data test data
  mc_model_test_eval[[2]] <- eval_model(corpus.dtm[171:271,], mc_model = mc_model_lda, num = 2)
  mc_model_test_perplexity[[2]] <- eval_perplexity(corpus.dtm[171:271,], mc_model_test_eval[[2]], num = 2)
  
  # Pathology test data
  mc_model_test_eval[[3]] <- eval_model(corpus.dtm[272:1563,], mc_model = mc_model_lda, num = 3)
  mc_model_test_perplexity[[3]] <- eval_perplexity(corpus.dtm[272:1563,], mc_model_test_eval[[3]], num = 3)
  
  # Pathology + cog merged
  mc_model_test_eval[[4]] <- eval_model(corpus.dtm[1564:3142,], mc_model = mc_model_lda, num = 4)
  mc_model_test_perplexity[[4]] <- eval_perplexity(corpus.dtm[1564:3142,], mc_model_test_eval[[4]], num = 4)
  
  # original data
  mc_model_test_perplexity[[5]] <- eval_perplexity(NULL, mc_model_lda, num = 5)
  
  save(mc_model_test_eval, file = paste0(file_path, "mc_model_test_eval.RData"))
  save(mc_model_test_perplexity, file = paste0(file_path, "mc_model_test_perplexity.RData"))
  
  df <- graph_formatter(mc_model_test_perplexity)
  
  return(df)
}

# Test the model - using test data set




# Plot results start

plot_result <- function(df){
  # graphs
  
  set1<- c('Test set 1', 'Test set 2', 'Train set')
  set2<- c('Test set 3', 'Test set 4', 'Train set')
  
  lineCol1 = c('#6baed6','#045a8d','#b30000')
  lineCol2 = c('#d95f0e','#993404','#b30000')
  
  for(method in methods){
    corpus_perplexity1 <- df[df$Method == method & df$dataset %in% set1, ]
    plot (c(3, 8), c(min(corpus_perplexity1$perplexity), max(corpus_perplexity1$perplexity)), 
          type="n", 
          xlab = 'Topics', ylab = paste('Perplexity for method', method)) # adds titles to the axes
    
    
    for(i in 1:length(set1)){
      lines(corpus_perplexity1[corpus_perplexity1$dataset == set1[i],]$topics,
            corpus_perplexity1[corpus_perplexity1$dataset == set1[i],]$perplexity, col=lineCol1[i],lwd=1.5) # adds a line for defense expenditures
    }
    
    legend(max(topics) - 0.8, max(corpus_perplexity1$perplexity) - 0.3, # places a legend at the appropriate place
           set1, # puts text in the legend
           lty=1, # gives the legend appropriate symbols (lines)
           lwd=c(2.5,2.5),col=lineCol1,
           title = 'Data')  
    
    corpus_perplexity2 <- df[df$Method == method & df$dataset %in% set2, ]
    plot (c(3, 8), c(min(corpus_perplexity2$perplexity), max(corpus_perplexity2$perplexity)), type="n", 
          xlab = 'Topics', ylab=paste('Perplexity for method', method)) # adds titles to the axes
    
    
    for(i in 1:length(set2)){
      lines(corpus_perplexity2[corpus_perplexity2$dataset == set2[i],]$topics,
            corpus_perplexity2[corpus_perplexity2$dataset == set2[i],]$perplexity, col=lineCol2[i],lwd=1.5) # adds a line for defense expenditures
    }
    
    legend(max(topics) - 0.8, max(corpus_perplexity2$perplexity) -0.3, # places a legend at the appropriate place
           set2, # puts text in the legend
           lty=1, # gives the legend appropriate symbols (lines)
           lwd=c(2.5,2.5),col=lineCol2,
           title = 'Data')
  }
}
  
# Plot results end






# Begin execution
# This file uses LDA techniques to extract topics (vem, vem_fixed and gibbs)
# Run utils.R before this file
setGlobalVariables()


methods <- c('vem', 'vem_fixed', 'gibbs')
file_path <- 'results/models/lda/'

# Train model on train data and save the model file
mc_model_lda <- train_model_lda()
mc_model_lda_df <- test_model_lda(mc_model_lda = mc_model_lda)


plot_result(mc_model_lda_df)








