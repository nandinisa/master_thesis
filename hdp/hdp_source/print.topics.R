print.topics <- function(words.fn, vocab.fn, topics.fn, top.n=5) 
{
   words <- as.matrix(read.table(words.fn, header=FALSE))
   vocab <- readLines(vocab.fn, warn=FALSE)
   num.topics <- nrow(words) 
   topics <- NULL 
   head <- ""
   for (k in seq(num.topics))
   {
       prob <- words[k,]
       total <- sum(prob)
       prob <- prob/total
       s <- sort.int(x=prob, decreasing=TRUE, index.return=TRUE)
       top.idx <- s$ix[1:top.n]
       topic.prob <- prob[top.idx]
       topic.words <- vocab[top.idx]
       topics <- cbind(topics, topic.words) 
       head <- paste(head, sprintf("%20d", k), sep=",")
   }

   write(x=head, file=topics.fn)

   for (i in seq(top.n))    
   {
       line <- paste(sprintf("%20s", topics[i,]), collapse=" & ")
       write(x=line, file=topics.fn, append=TRUE)
   }
}

saveToFileAndLoadCsv <- function(prefix){
  words.fn <- paste0('hdpOutputDir/',prefix,'-topics.dat')
  vocab.fn <- "hdpIn/vocabulary.dat"
  topics.fn <- paste0('hdpOutputDir/PrettyPrint/display-',prefix,'-topics.dat')
  top.n <- 10
  print.topics(words.fn, vocab.fn, topics.fn, top.n)
  topicsTable <- read.csv(paste0('hdpOutputDir/PrettyPrint/display-',prefix,'-topics.dat'), header = TRUE)
  return (topicsTable)
}

t_modeTable <- saveToFileAndLoadCsv('mode')
t_00000Table <- saveToFileAndLoadCsv('00000')
t_00100Table <- saveToFileAndLoadCsv('00100')
t_00200Table <- saveToFileAndLoadCsv('00200')
t_00300Table <- saveToFileAndLoadCsv('00300')
t_00400Table <- saveToFileAndLoadCsv('00400')
t_00500Table <- saveToFileAndLoadCsv('00500')
t_00600Table <- saveToFileAndLoadCsv('00600')
t_00700Table <- saveToFileAndLoadCsv('00700')
t_00800Table <- saveToFileAndLoadCsv('00800')
t_00900Table <- saveToFileAndLoadCsv('00900')



