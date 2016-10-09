source('util.R')
library(plyr)

# corpus.dtm - document term matrix
# training_data - 1:153
# test_data - 154:170
# cog_test_data - 171:271
# pat_test_data - 272:1563
# merge_test_data - 1564: 3142

convertToBleiFormat <- function(doc){
  docStr <- apply(doc, 2, function(x) paste0(x[1],':',x[2]))
  docBlei <- paste(length(doc)/2, gsub(",","",toString(docStr)))
  return (docBlei);
}

# convert to blei format
lexicalDtm <- dtm2ldaformat(corpus.dtm[1:153,])
allDocBlei <- lapply(lexicalDtm$documents, convertToBleiFormat)
allDocBleiStr <- paste0(allDocBlei)
write(allDocBleiStr, 'data/blei/mc_input.dat')
write(lexicalDtm$vocab, 'data/blei/mc_vocab.dat')

lexicalDtm <- dtm2ldaformat(corpus_e.dtm[1:153,])
allDocBlei <- lapply(lexicalDtm$documents, convertToBleiFormat)
allDocBleiStr <- paste0(allDocBlei)
write(allDocBleiStr, 'data/blei/mc_e_input.dat')
write(lexicalDtm$vocab, 'data/blei/mc_e_vocab.dat')


# convert to blei format test
lexicalDtm <- dtm2ldaformat(corpus_e.dtm[154:170,])
allDocBlei <- lapply(lexicalDtm$documents, convertToBleiFormat)
allDocBleiStr <- paste0(allDocBlei)
write(allDocBleiStr, 'data/blei/mc_input_test1.dat')
write(lexicalDtm$vocab, 'data/blei/mc_vocab_test1.dat')

lexicalDtm <- dtm2ldaformat(corpus_e.dtm[171:271,])
allDocBlei <- lapply(lexicalDtm$documents, convertToBleiFormat)
allDocBleiStr <- paste0(allDocBlei)
write(allDocBleiStr, 'data/blei/mc_input_test2.dat')
write(lexicalDtm$vocab, 'data/blei/mc_vocab_test2.dat')

lexicalDtm <- dtm2ldaformat(corpus_e.dtm[272:1563,])
allDocBlei <- lapply(lexicalDtm$documents, convertToBleiFormat)
allDocBleiStr <- paste0(allDocBlei)
write(allDocBleiStr, 'data/blei/mc_input_test3.dat')
write(lexicalDtm$vocab, 'data/blei/mc_vocab_test3.dat')

lexicalDtm <- dtm2ldaformat(corpus_e.dtm[1564:3142,])
allDocBlei <- lapply(lexicalDtm$documents, convertToBleiFormat)
allDocBleiStr <- paste0(allDocBlei)
write(allDocBleiStr, 'data/blei/mc_input_test4.dat')
write(lexicalDtm$vocab, 'data/blei/mc_vocab_test4.dat')


#RUN THE HDP EXE AS:
#hdp\bin\hdp.exe --algorithm train --data "data\blei\mc_input.dat" --directory "results\models\hdp" --random_seed 177
#hdp\bin\hdp.exe --algorithm train --data "data\blei\mc_e_input.dat" --directory "results\models\hdp_e" --random_seed 177

print.topics <- function(words.fn, vocab.fn, topics.fn, top.n=5) 
{
  words <- as.matrix(read.table(words.fn, header=FALSE))
  vocab <- readLines(vocab.fn, warn=FALSE)
  num.topics <- nrow(words) 
  topics <- NULL 
  head <- ""
  # df <- data.frame(row.names=1:top.n)
  for (k in seq(num.topics))
  {
    prob <- words[k,]
    total <- sum(prob)
    prob <- prob/total
    s <- sort.int(x=prob, decreasing=TRUE, index.return=TRUE)
    top.idx <- s$ix[1:top.n]
    topic.prob <- prob[top.idx]
    topic.words <- vocab[top.idx]
    topics <- cbind(topics, topic.words, topic.prob) 
    head <- paste(head, sprintf("%20d", k), sep="")
    # head <- paste(head, sprintf("%20d", k), sep="")
  }
  
  write(x=head, file=topics.fn)
  
  for (i in seq(top.n))    
  {
    # line <- paste(sprintf("%20s", topics[i,]), collapse=" & ")
    line <- paste(sprintf("%20s", topics[i,]), collapse="")
    write(x=line, file=topics.fn, append=TRUE)
  }
  
  return (topics)
}

print.terms <- function(words_assign.fn) 
{
  df <-  as.data.frame(read.table(words_assign.fn))
  vocab <- readLines(vocab.fn, warn=FALSE)
  
  colnames(df) <- c('d', 'w', 'z', 't')
  df = df[-1,]
  df$t <- NULL
  
  df <- ddply(df, .(d,w,z), nrow)
  colnames(df)[4] <- 'count'
  
  return (df)
}


saveToFileAndLoadCsv <- function(prefix, top.n, modelPath){
  words.fn <- paste0(modelPath, prefix,'-topics.dat')
  topics.fn <- paste0(modelPath, 'display-',prefix,'-topics.dat')
  topics <- print.topics(words.fn, vocab.fn, topics.fn, top.n)
  return (topics)
}


# print.topics(words.fn, vocab.fn, topics.fn, top.n)
vocab.fn <- paste0("data/blei/", vocab)

t_modeTable <- saveToFileAndLoadCsv('mode', 20, 'results/models/hdp/', 'mc_vocab.dat')
t_modeTable1 <- saveToFileAndLoadCsv('mode', 20, 'results/models/hdp_e/', 'mc_e_vocab.dat')

write.csv(t_modeTable, file = 'results/models/hdp/terms_HDP.csv')

doc_distribution <- print.terms('results/models/hdp/mode-word-assignments.dat')
