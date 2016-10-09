library(tm)
library(topicmodels)
library(lattice)

set.seed(5) #sanmple(1:1000, 1)
k <- 5
SEED <- 177 #sanmple(1:1000, 1)


# generateGammaDf <- function(dataset, gammaCsvName)
# {
#   gammaDF <- as.data.frame(dataset)
#   gammaStats <- as.data.frame(cbind(document = row.names(gammaDF), 
#                                     topTopic = apply(gammaDF, 1, function(x) names(gammaDF)[which(x==max(x))]),
#                                     topTopicValue = apply(gammaDF, 1, function(x) max(x)),
#                                     leastTopic = apply(gammaDF, 1, function(x) names(gammaDF)[which(x==min(x))]),
#                                     leastTopicValue = apply(gammaDF, 1, function(x) min(x)),
#                                     sdTopic = apply(gammaDF, 1, function(x) sd(x))))
#   write.csv(gammaDF, paste(gammaCsvName, 'csv', sep='.'))
#   write.csv(gammaStats, paste(gammaCsvName, 'stat.csv', sep='.'))
# }


load("data/dtm/corpus.dtm.RData")

# code ref:topicmodels: An R Package for Fitting Topic Models
# training_data - 1:153
medical_corpus <- corpus.dtm[1:153,]
D <- nrow(medical_corpus)
folding <- sample(rep(seq_len(10), ceiling(D))[seq_len(D)])

corpus_tm <- 
  list(VEM_fixed_alpha_0.1 = LDA(medical_corpus, k = k, 
                                 control = list(estimate.alpha = FALSE, alpha=0.01, seed = SEED)),
       VEM_fixed_alpha_1 = LDA(medical_corpus, k = k, 
                               control = list(estimate.alpha = FALSE, alpha=1, seed = SEED)),
       VEM_fixed_alpha_10 = LDA(medical_corpus, k = k, 
                                control = list(estimate.alpha = FALSE, alpha=10, seed = SEED)),
       VEM_fixed_alpha_100 = LDA(medical_corpus, k = k, 
                                 control = list(estimate.alpha = FALSE, alpha=100, seed = SEED)))


# methods <- c("VEM_fixed_alpha_0.1", "VEM_fixed_alpha_1", "VEM_fixed_alpha_10", "VEM_fixed_alpha_100", "VEM_varying")
methods <- c("VEM_fixed_alpha_0.1", "VEM_fixed_alpha_1", "VEM_fixed_alpha_10", "VEM_fixed_alpha_100")
DF <- data.frame(posterior = unlist(lapply(corpus_tm, function(x) apply(posterior(x)$topics, 1, max))),
                 method = factor(rep(methods, 
                                     each = nrow(posterior(corpus_tm$VEM_fixed_alpha_0.1)$topics)), methods))

print(histogram(~ posterior | method, data = DF, col = "white", as.table = TRUE,
                xlab = "Probability of assignment to the most likely topic",
                ylab = "Percent of total", layout = c(4, 1)))

# generateGammaDf(corpus_tm$VEM_fixed_alpha_0.1@gamma, gammaCsvName = "results/k_eval_alpha/VEM_fixed_alpha_01")
# generateGammaDf(corpus_tm$VEM_fixed_alpha_1@gamma, gammaCsvName = "results/k_eval_alpha/VEM_fixed_alpha_1")
# generateGammaDf(corpus_tm$VEM_fixed_alpha_10@gamma, gammaCsvName = "results/k_eval_alpha/VEM_fixed_alpha_10")
# generateGammaDf(corpus_tm$VEM_fixed_alpha_100@gamma, gammaCsvName = "results/k_eval_alpha/VEM_fixed_alpha_100")
# generateGammaDf(corpus_tm$VEM_varying@gamma, gammaCsvName = "R_graph/Stats/VEM_varying")