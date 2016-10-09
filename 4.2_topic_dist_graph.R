library(ggplot2)
library(reshape2)

load("results/models/lda/mc_model_lda.rda")
load("results/models/ctm/mc_model_ctm.rda")
load("results/models/lda/mc_model_test_eval.RData")
load("results/models/ctm/mc_model_test_eval.RData")

drawSmallMultiples <- function(model, topicNames){
  # Document - topic distribution for LDA K= 5, alpha = 1
  ldaModel <- model
  
  gammaDf <- data.frame(ldaModel@gamma)
  colnames(gammaDf) <- topicNames
  gammaDf$DocID<- as.numeric(rownames(gammaDf))
  
  gtemp <- melt(gammaDf, id.vars=c("DocID"))
  ggplot(data=gtemp, aes(x=DocID, y=value)) +
    geom_area(stat="identity") +
    facet_wrap(~variable) +
    # ggtitle("Topic-wise document probability distribution") +
    theme(plot.title = element_text(face="bold", size=20, hjust=0, color="#555555")) +
    theme(axis.text.x = element_text(angle=0)) +
    theme_bw() + 
    labs(x = 'Documents', y = 'Probability distribution') +
    expand_limits(y=c(0,0.8))  
}

# LDA
drawSmallMultiples(mc_model_lda[[5]]$vem, c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5"))
drawSmallMultiples(mc_model_lda[[5]]$vem_fixed, c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5"))
drawSmallMultiples(mc_model_lda[[5]]$gibbs, c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5"))

# CTM
drawSmallMultiples(mc_model_ctm[[5]]$CTM, c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5"))



# HDP 
# hardcoded
df <- as.data.frame(matrix(nrow = 153))
df$DocID <- c(1:153)
df$variable <- 'Topic 1' # topics
df$value <- 1
# df$V1 <- NULL
# df[26, ]$variable <- 'Topic 2'

ggplot(data=df, aes(x=DocID, y=value)) +
  geom_bar(stat="identity") +
  facet_wrap(~variable) +
  # ggtitle("Topic-wise document probability distribution") +
  theme(plot.title = element_text(face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=0)) +
  theme_bw() + 
  labs(x = 'Documents', y = 'Probability distribution') +
  expand_limits(y=c(0,0.8))  


# test data

drawSmallMultiples(mc_model_test_eval[[1]][[5]]$vem_fixed, c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5"))
drawSmallMultiples(mc_model_test_eval[[2]][[5]]$vem_fixed, c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5"))
drawSmallMultiples(mc_model_test_eval[[3]][[5]]$vem_fixed, c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5"))
drawSmallMultiples(mc_model_test_eval[[4]][[5]]$vem_fixed, c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5"))

drawSmallMultiples(mc_model_test_eval[[1]][[5]]$gibbs, c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5"))
drawSmallMultiples(mc_model_test_eval[[2]][[5]]$gibbs, c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5"))
drawSmallMultiples(mc_model_test_eval[[3]][[5]]$gibbs, c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5"))
drawSmallMultiples(mc_model_test_eval[[4]][[5]]$gibbs, c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5"))



