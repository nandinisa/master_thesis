library("topicmodels")
library(tm)
library(ggplot2)
library(lattice)


set.seed(5) #sanmple(1:1000, 1)
topics <- c(2:10, 20, 40, 70)
SEED <- 177 #sanmple(1:1000, 1)
methods <- c("vem", "vem_fixed", "gibbs")

formatMatrix <- function(m){
  sa <- t(as.data.frame(m))
  df <- data.frame(topics = numeric(0), cross_val = numeric(0), x = numeric(0))
  for(i in 1: ncol(sa)){
    rows <- ((i - 1) * 10) + c(1:10)
    df[rows, ]$topics <- topics[i]
    df[rows, ]$cross_val <- c(1:10)
    df[rows, ]$x <- sa[ ,i]
  }
  
  return (df)
}

load("data/dtm/corpus.dtm.RData")

# code ref:topicmodels: An R Package for Fitting Topic Models
# training_data - 1:153
medical_corpus <- corpus.dtm[1:153,]
D <- nrow(medical_corpus)
folding <- sample(rep(seq_len(10), ceiling(D))[seq_len(D)])


# taining
for (k in topics) {
  for (chain in seq_len(10)) {
    FILE <- paste("vem_", k, "_", chain, ".rda", sep = "")
    training <- LDA(medical_corpus[folding != chain,], k = k,
                    control = list(seed = SEED))
    testing <- LDA(medical_corpus[folding == chain,], model = training,
                   control = list(estimate.beta = FALSE, seed = SEED))
    save(training, testing, file = file.path("results/k_eval_cv", FILE))
    
    FILE <- paste("vem_fixed_", k, "_", chain, ".rda", sep = "")
    training <- LDA(medical_corpus[folding != chain,], k = k,
                    control = list(seed = SEED, estimate.alpha = FALSE))
    testing <- LDA(medical_corpus[folding == chain,], model = training,
                   control = list(estimate.beta = FALSE, seed = SEED))
    save(training, testing, file = file.path("results/k_eval_cv", FILE))
    
    FILE <- paste("gibbs_", k, "_", chain, ".rda", sep = "")
    training <- LDA(medical_corpus[folding != chain,], k = k,
                    control = list(seed = SEED, burnin = 1000, thin = 100,
                                   iter = 1000, best = TRUE), method = "Gibbs")
    
    # best_training <- training@fitted[[which.max(logLik(training))]]
    testing <- LDA(medical_corpus[folding == chain,],
                   model = training, control = list(estimate.beta = FALSE,
                                                    seed = SEED, burnin = 1000, thin = 100, iter = 1000, best = TRUE))
    save(training, testing, file = file.path("results/k_eval_cv", FILE))
  }
}




########################
corpus_perplexity <- corpus_alpha <- list()
for (method in methods) {
  corpus_alpha[[method]] <- corpus_perplexity[[method]] <- matrix(NA,
                                                                  nrow = length(topics), ncol = 10, dimnames = list(topics, seq_len(10)))
  for (fold in seq_len(10)) {
    for (i in seq_along(topics)) {
      T <- topics[i]
      FILE <- paste(method, "_", T, "_", fold, ".rda", sep = "")
      load(file.path("results/k_eval_cv", FILE))
      corpus_alpha[[method]][paste(T),fold] <- training@alpha
      # if (is(training, "Gibbs_list")) training@fitted[[1]]@alpha
      # else training@alpha
      
      corpus_perplexity[[method]][paste(T),fold] <- perplexity(testing,
                                                               medical_corpus[folding == fold,], use_theta = FALSE)
    }
  }
}
save(corpus_alpha, corpus_perplexity, file = "results/k_eval_cv/corpus.rda")


# Compute alpha
corpus_alpha_VEM <- formatMatrix(corpus_alpha$vem)
corpus_alpha_VEM_fixed <- formatMatrix(corpus_alpha$vem_fixed)
corpus_alpha_Gibbs <- formatMatrix(corpus_alpha$gibbs)

# Convert to character for plotting
corpus_alpha_VEM$cross_val <- as.character(corpus_alpha_VEM$cross_val)
corpus_alpha_VEM_fixed$cross_val <- as.character(corpus_alpha_VEM_fixed$cross_val)
corpus_alpha_Gibbs$cross_val <- as.character(corpus_alpha_Gibbs$cross_val)


# Compute perplexity
corpus_perplexity_VEM <- formatMatrix(corpus_perplexity$vem)
corpus_perplexity_VEM_fixed <- formatMatrix(corpus_perplexity$vem_fixed)
corpus_perplexity_Gibbs <- formatMatrix(corpus_perplexity$gibbs)

# Convert to character for plotting
corpus_perplexity_VEM$cross_val <- as.character(corpus_perplexity_VEM$cross_val)
corpus_perplexity_VEM_fixed$cross_val <- as.character(corpus_perplexity_VEM_fixed$cross_val)
corpus_perplexity_Gibbs$cross_val <- as.character(corpus_perplexity_Gibbs$cross_val)

write.csv(corpus_perplexity_VEM, 'results/k_eval_cv/corpus_perplexity_vem.csv', row.names = FALSE)
write.csv(corpus_perplexity_VEM_fixed, 'results/k_eval_cv/corpus_perplexity_vem_fixed.csv', row.names = FALSE)
write.csv(corpus_perplexity_Gibbs, 'results/k_eval_cv/corpus_perplexity_gibbs.csv', row.names = FALSE)

# default quick plotting using qplots
# qplot(topics, x, data = corpus_alpha_VEM, group = cross_val, colour = cross_val, geom = "line",
#       xlab = 'Topics', ylab = 'Alpha values')
# qplot(topics, x, data = corpus_alpha_VEM_fixed, group = cross_val, colour = cross_val, geom = "line",
#       xlab = 'Topics', ylab = 'Alpha values')
# qplot(topics, x, data = corpus_alpha_Gibbs, group = cross_val, colour = cross_val, geom = "line",
#       xlab = 'Topics', ylab = 'Alpha values')
# 
# qplot(topics, x, data = corpus_perplexity_VEM, group = cross_val, colour = cross_val, geom = "line",
#       xlab = 'Topics', ylab = 'Perplexity')
# qplot(topics, x, data = corpus_perplexity_VEM_fixed, group = cross_val, colour = cross_val, geom = "line",
#       xlab = 'Topics', ylab = 'Perplexity')
# qplot(topics, x, data = corpus_perplexity_Gibbs, group = cross_val, colour = cross_val, geom = "line",
#       xlab = 'Topics', ylab = 'Perplexity')



# Plotting with legends and other details

# VEM
# plot alpha for VEM
selected <- subset(corpus_alpha_VEM, corpus_alpha_VEM$topics <= 10)
plot (c(2, 10),c(min(selected$x), max(selected$x)), type="n", 
      xlab = 'Topics', ylab='Alpha') # adds titles to the axes

lineCol = c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#c7eae5','#80cdc1','#35978f','#01665e','#003c30')
for(i in 1:10){
  lines(selected[selected$cross_val == i,]$topics,
        selected[selected$cross_val == i,]$x, col=lineCol[i],lwd=1.5) # adds a line for defense expenditures
}
axis(1, at=topics, labels=topics)

legend(9, max(corpus_alpha_VEM$x), # places a legend at the appropriate place 
       c(1:10), # puts text in the legend
       lty=1, # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c('#003c30'),
       title = 'cross-val')


# plot perplexity for VEM
plot (c(2, 70),c(min(corpus_perplexity_VEM$x), max(corpus_perplexity_VEM$x)), type="n", 
      xlab = 'Topics', ylab='Perplexity') # adds titles to the axes

lineCol = c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#c7eae5','#80cdc1','#35978f','#01665e','#003c30')
for(i in 1:10){
  lines(corpus_perplexity_VEM[corpus_perplexity_VEM$cross_val == i,]$topics,
        corpus_perplexity_VEM[corpus_perplexity_VEM$cross_val == i,]$x, col=lineCol[i],lwd=1.5) # adds a line for defense expenditures
}
axis(1, at=topics, labels=topics)

legend(x= max(topics) - 10, y = max(corpus_perplexity_VEM$x) + 1,
       c(1:10), # puts text in the legend
       lty=1, # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c('#003c30'),
       title = 'cross-val')





# Fixed VEM
# plot alpha
selected <- subset(corpus_alpha_VEM_fixed, corpus_alpha_VEM_fixed$topics <= 10)
plot (c(2, 10),c(min(selected$x), max(selected$x)), type="n", 
      xlab = 'Topics', ylab='Alpha') # adds titles to the axes

lineCol = c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#c7eae5','#80cdc1','#35978f','#01665e','#003c30')
for(i in 1:10){
  lines(selected[selected$cross_val == i,]$topics,
        selected[selected$cross_val == i,]$x, col=lineCol[i],lwd=1.5) # adds a line for defense expenditures
}
#axis(1, at=topics, labels=topics)

legend(x= 9, y = max(corpus_alpha_VEM_fixed$x), # places a legend at the appropriate place 
       c(1:10), # puts text in the legend
       lty=1, # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=lineCol,
       title = 'cross-val')


# plot perplexity for fixed vem
plot (c(2, 70),c(min(corpus_perplexity_VEM_fixed$x), max(corpus_perplexity_VEM_fixed$x)), type="n", 
      xlab = 'Topics', ylab='Perplexity') # adds titles to the axes

lineCol = c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#c7eae5','#80cdc1','#35978f','#01665e','#003c30')
for(i in 1:10){
  lines(corpus_perplexity_VEM_fixed[corpus_perplexity_VEM_fixed$cross_val == i,]$topics,
        corpus_perplexity_VEM_fixed[corpus_perplexity_VEM_fixed$cross_val == i,]$x, col=lineCol[i],lwd=1.5) # adds a line for defense expenditures
}
axis(1, at=topics, labels=topics)
legend(x= max(topics) - 10, y = max(corpus_perplexity_VEM_fixed$x) + 1, # places a legend at the appropriate place 
       c(1:10), # puts text in the legend
       lty=1, # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=lineCol,
       title = 'cross-val')


# Gibbs
# plot alpha
selected <- subset(corpus_alpha_Gibbs, corpus_alpha_Gibbs$topics <= 10)
plot (c(2, 10),c(min(selected$x), max(selected$x)), type="n", 
      xlab = 'Topics', ylab='Alpha') # adds titles to the axes

lineCol = c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#c7eae5','#80cdc1','#35978f','#01665e','#003c30')
for(i in 1:10){
  lines(selected[selected$cross_val == i,]$topics,
        selected[selected$cross_val == i,]$x, col=lineCol[i],lwd=1.5) # adds a line for defense expenditures
}
axis(1, at=topics, labels=topics)

legend(9,max(corpus_alpha_Gibbs$x), # places a legend at the appropriate place 
       c(1:10), # puts text in the legend
       lty=1, # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c('#003c30'),
       title = 'cross-val')

# plot perplexity for gibbs
plot (c(2, 70),c(min(corpus_perplexity_Gibbs$x), max(corpus_perplexity_Gibbs$x)), type="n", 
      xlab = 'Topics', ylab='Perplexity') # adds titles to the axes

lineCol = c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#c7eae5','#80cdc1','#35978f','#01665e','#003c30')
for(i in 1:10){
  lines(corpus_perplexity_Gibbs[corpus_perplexity_Gibbs$cross_val == i,]$topics,
        corpus_perplexity_Gibbs[corpus_perplexity_Gibbs$cross_val == i,]$x, col=lineCol[i],lwd=1.5) # adds a line for defense expenditures
}
axis(1, at=topics, labels=topics)

legend(x= max(topics) - 10, y = max(corpus_perplexity_Gibbs$x), # places a legend at the appropriate place 
       c(1:10), # puts text in the legend
       lty=1, # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=lineCol,
       title = 'cross-val')

#save(medical_corpus, file='results/mc.rda')
######################################



# boxplot to show variation in the samples
for(i in methods){
  df<- read.csv(paste('results/k_eval_cv/corpus_perplexity_', i, '.csv', sep=''))
  # df.agg <- ddply(df, .(topics), summarize, mean_value = mean(x))
  
  df<- subset(df, df$topics <= 10)
  df$topics <- sprintf("%02d", df$topics)
  df$topics <- as.character(df$topics)
  # View(df)
  # ggplot(data = df, aes(x=topics, y=x)) + geom_boxplot()
  
  
  fig <- bwplot(x~topics,    
                data=df,
                between=list(y=1),
                #main=paste("Topics perplexity", i, sep = ' '),
                xlab = 'Topics',
                ylab = 'Perplexity')
  
  plot(fig)
  
}


