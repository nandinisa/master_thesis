
source('util.R')

# corpus.dtm - document term matrix
# training_data - 1:153
# test_data - 154:170
# cog_test_data - 171:271
# pat_test_data - 272:1563
# merge_test_data - 1564: 3142


train_model_ctm <- function(){
  # Convergence is governed by the 2 parameters - 
  # iter.max for maximum number of iterations, and tol for tolerance for relative change in the likelihood.
  # Iter.max:
  mc_model_ctm <- list()
  for(k in topics){
    mc_model_ctm[[k]] <- list(
      CTM = CTM(corpus_e.dtm[1:153,], k = k, control = list(seed = SEED, 
                                                            var = list(tol = 10^-6, iter.max = 1000), 
                                                            em = list(tol = 10^-4, iter.max = 10000),
                                                            cg = list(tol = 10^-6, iter.max = -1))))
    
  }
  
  # topTenLdaVEM <- terms(mc_model_ctm[[5]]$CTM, 10)
  # View(topTenLdaVEM)
  # 
  save(mc_model_ctm, file = paste0(file_path, "mc_model_ctm.rda"))
  return(mc_model_ctm)
}






# Evaluation of models on unseen data start

eval_model <- function(data, mc_model, num){
  mc_model_ctm_eval <- list()
  for(k in topics){
    mc_model_ctm_eval[[k]] <- list(
      CTM = CTM(data, model= mc_model[[k]]$CTM, control = list(seed = SEED, 
                                                                   estimate.beta = FALSE, 
                                                                   var = list(tol = 10^-6, iter.max = 20), 
                                                                   em = list(tol = 10^-3, iter.max = 1000),
                                                                   cg = list(tol = 10^-6, iter.max = -1)))
    )
    
  }
  
  save(mc_model_ctm_eval, file = paste(file_path, "mc_model_ctm_eval", num, ".RData", sep=''))
  return (mc_model_ctm_eval)
}

eval_perplexity <- function(data, mc_model_test, num){
  corpus_test_perplexity <- as.data.frame(matrix(NA,
                                            nrow = length(topics), ncol = 1, dimnames = list(topics, c('CTM'))))
  method <- 'CTM'
  for (T in topics) {
    if(is.null(data)){
      corpus_test_perplexity[method][paste(T),1] <- perplexity(mc_model_test[[T]][[method]], corpus_e.dtm[1:153,], use_theta = FALSE)
    }
    else{
      corpus_test_perplexity[method][paste(T),1] <- perplexity(mc_model_test[[T]][[method]], data, use_theta = FALSE)  
    }
  }
  
  corpus_test_perplexity$topics <- rownames(corpus_test_perplexity)
  corpus_test_perplexity <- melt(corpus_test_perplexity, id.vars="topics",
                            variable.name="Method", value.name="Perplexity")
  corpus_test_perplexity$test <- num
  save(corpus_test_perplexity, file = paste(file_path, num, "corpus_test.rda", sep=""))
  return (list(corpus_test_perplexity))
}

# End of evaluation


graph_formatter <- function(mc_model_test_perplexity){
  methods <- ('CTM')
  df <- data.frame()
  df<- rbind(df, mc_model_test_perplexity[[1]][[1]])
  df<- rbind(df, mc_model_test_perplexity[[2]][[1]])
  df<- rbind(df, mc_model_test_perplexity[[3]][[1]])
  df<- rbind(df, mc_model_test_perplexity[[4]][[1]])
  df<- rbind(df, mc_model_test_perplexity[[5]][[1]])
  
  df$dataset <- paste0('Test set ', df$test)
  df[df$dataset == 'Test set 5', ]$dataset <- 'Train set'
  
  # df$test <- NULL
  
  save(df, file = paste0(file_path, "mc_model_test_df.RData"))
  return (df)
}

test_model_ctm <- function(mc_model_ctm){
  # training_data - 1:153
  # test_data - 154:170
  # cog_test_data - 171:271
  # pat_test_data - 272:1563
  # merge_test_data - 1564: 3142
  
  mc_model_test_eval <- list()
  mc_model_test_perplexity <- list()
  
  # Frass data
  mc_model_test_eval[[1]] <- eval_model(corpus.dtm[154:170,], mc_model = mc_model_ctm, num = 1)
  mc_model_test_perplexity[[1]] <- eval_perplexity(corpus.dtm[154:170,], mc_model_test_eval[[1]], num = 1)
  
  
  # Cognition data test data
  mc_model_test_eval[[2]] <- eval_model(corpus.dtm[171:271,], mc_model = mc_model_ctm, num = 2)
  mc_model_test_perplexity[[2]] <- eval_perplexity(corpus.dtm[171:271,], mc_model_test_eval[[2]], num = 2)
  
  # Pathology test data
  mc_model_test_eval[[3]] <- eval_model(corpus.dtm[272:1563,], mc_model = mc_model_ctm, num = 3)
  mc_model_test_perplexity[[3]] <- eval_perplexity(corpus.dtm[272:1563,], mc_model_test_eval[[3]], num = 3)
  
  # Pathology + cog merged
  mc_model_test_eval[[4]] <- eval_model(corpus.dtm[1564:3142,], mc_model = mc_model_ctm, num = 4)
  mc_model_test_perplexity[[4]] <- eval_perplexity(corpus.dtm[1564:3142,], mc_model_test_eval[[4]], num = 4)
  
  # original data
  mc_model_test_perplexity[[5]] <- eval_perplexity(NULL, mc_model_ctm, num = 5)
  
  save(mc_model_test_eval, file = paste0(file_path, "mc_model_test_eval.RData"))
  save(mc_model_test_perplexity, file = paste0(file_path, "mc_model_test_perplexity.RData"))
  
  df <- graph_formatter(mc_model_test_perplexity)
  
  return(df)
}


plot_result <- function(df){
  set1<- c('Test set 1', 'Test set 2', 'Train set')
  set2<- c('Test set 3', 'Test set 4', 'Train set')
  
  lineCol1 = c('#6baed6','#045a8d','#b30000')
  lineCol2 = c('#d95f0e','#993404','#b30000')
  
  corpus_perplexity1 <- df[df$dataset %in% set1, ]
  
  plot (c(3, 8), c(min(corpus_perplexity1$Perplexity), max(corpus_perplexity1$Perplexity)), type="n", 
        xlab = 'Topics', ylab='Perplexity') # adds titles to the axes
  
  
  for(i in 1:length(set1)){
    lines(corpus_perplexity1[corpus_perplexity1$dataset == set1[i],]$topics,
          corpus_perplexity1[corpus_perplexity1$dataset == set1[i],]$Perplexity, col=lineCol1[i],lwd=1.5) # adds a line for defense expenditures
  }
  
  legend(max(topics) - 1, max(corpus_perplexity1$Perplexity) - 0.3, # places a legend at the appropriate place
         set1, # puts text in the legend
         lty=1, # gives the legend appropriate symbols (lines)
         lwd=c(2.5,2.5),col=lineCol1,
         title = 'Data')  
  
  
  corpus_perplexity2 <- df[df$dataset %in% set2, ]
  plot (c(3, 8), c(min(corpus_perplexity2$Perplexity), max(corpus_perplexity2$Perplexity)), type="n", 
        xlab = 'Topics', ylab='Perplexity') # adds titles to the axes
  
  
  for(i in 1:length(set2)){
    lines(corpus_perplexity2[corpus_perplexity2$dataset == set2[i],]$topics,
          corpus_perplexity2[corpus_perplexity2$dataset == set2[i],]$Perplexity, col=lineCol2[i],lwd=1.5) # adds a line for defense expenditures
  }
  
  legend(max(topics) - 1, max(corpus_perplexity2$Perplexity) - 0.3, # places a legend at the appropriate place
         set2, # puts text in the legend
         lty=1, # gives the legend appropriate symbols (lines)
         lwd=c(2.5,2.5),col=lineCol2,
         title = 'Data')
  
}




# Begin execution
# This file uses LDA techniques to extract topics (vem, vem_fixed and gibbs)
# Run utils.R before this file
setGlobalVariables()

file_path <- 'results/models/ctm/'

SEED <- 41 # sample(1:1000, 1)

# Train model on train data and save the model file
mc_model_ctm <- train_model_ctm()
mc_model_ctm_df <- test_model_ctm(mc_model_ctm = mc_model_ctm)


plot_result(mc_model_ctm_df)








