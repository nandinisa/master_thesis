library(corrplot)

load("results/models/ctm/mc_model_ctm.rda")
topics <- c(3,5,8)
topics <- c(5)

# Correlation plot for topics in CTM
col<- colorRampPalette(c("black"))(1)

mc_model_ctmPlots <- list()

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

for(k in topics){
  mc_model_ctmPlots[[k]] <- list(
    CTMPlot = corrplot(get_lower_tri(round(mc_model_ctm[[k]]$CTM@Sigma, 3)), method = "number", 
                       col = col, number.digits = 5, na.label = '-',
                       number.cex = 1,  rect.col = 'black', rect.lwd = 2,
                       title = 'Topics', 
                       tl.col="black", tl.srt = 90, cl.pos = "n"))
}

# for(k in topics){
#   print(mc_model_ctmPlots[[k]]) 
# }
