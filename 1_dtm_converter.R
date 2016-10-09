library(tm)

# this file converts the data frames into a combined document term matrix
# final data points - corpus.dtm and corpus_e.dtm
# training_data - 1:153
# test_data - 154:170
# cog_test_data - 171:271
# pat_test_data - 272:1563
# merge_test_data - 1564: 3142


commaRemoval <- function(s) {
  return (gsub(",*","", s, perl=T)) 
}

# load training data
load("data/raw/3.1_training_data.RData")

# load test data
load("data/raw/3.2_test_data.RData")
load("data/raw/4.1_cog_test_data.RData")
load("data/raw/4.2_pat_test_data.RData")
load("data/raw/4.3_merge_test_data.RData")


# merge all the data
training_data$result_val <- paste(training_data$frass, 
                                  training_data$cognition, 
                                  training_data$pathology, sep=' ')

test_data$result_val <- paste(test_data$frass, 
                              test_data$cognition, 
                              test_data$pathology, sep=' ')

# check for NA and replace with ''
cog_test_data[is.na(cog_test_data)] <- ''
pat_test_data[is.na(pat_test_data)] <- ''
merge_test_data[is.na(merge_test_data)] <- ''


cog_test_data$result_val <- paste(cog_test_data$frass, 
                                  cog_test_data$cognition, 
                                          sep=' ')
pat_test_data$result_val <- paste(pat_test_data$frass, 
                                  pat_test_data$pathology,
                                  sep=' ')
merge_test_data$result_val <- paste(merge_test_data$frass, 
                                    merge_test_data$cognition, 
                                    merge_test_data$pathology,
                                  sep=' ')

# select only required columns
selectVar <- c('age', 'sex', 'marital_status', 'ethnicity', 'result_val')
training_data <- subset(training_data, select = selectVar)
test_data <- subset(test_data, select = selectVar)

cog_test_data <- subset(cog_test_data, select = selectVar)
pat_test_data <- subset(pat_test_data, select = selectVar)
merge_test_data <- subset(merge_test_data, select = selectVar)

# merge the data sets
# training_data - 1:153
# test_data - 154:170
all <- rbind(training_data, test_data)

# cog_test_data - 171:271
all <- rbind(all, cog_test_data)

# pat_test_data - 272:1563
all <- rbind(all, pat_test_data)

# merge_test_data - 1564: 3142
all <- rbind(all, merge_test_data)

save(all, file = 'data/dtm/all.RData')

# save dtm without ethinicity
set1 <- paste(all$marital_status, all$sex, all$result_val, all$age, sep=' ')
corpus <- VectorSource(set1)
corpus <- VCorpus(corpus);
corpus <- tm_map(corpus, content_transformer(tolower)) # convert all text to lower case
corpus <- tm_map(corpus, content_transformer(commaRemoval))
corpus.dtm <- DocumentTermMatrix(corpus)

save(corpus.dtm, file = 'data/dtm/corpus.dtm.RData')


# save dtm with ethinicity
set2 <- paste(all$marital_Status, all$Ethnicity, all$sex, all$result_val, all$age, sep=' ')
corpus <- VectorSource(set2)
corpus <- VCorpus(corpus);
corpus <- tm_map(corpus, content_transformer(tolower)) # convert all text to lower case
corpus <- tm_map(corpus, content_transformer(commaRemoval))
corpus_e.dtm <- DocumentTermMatrix(corpus)

save(corpus_e.dtm, file = 'data/dtm/corpus_e.dtm.RData')

