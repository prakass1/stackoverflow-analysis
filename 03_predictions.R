# library(text2vec)
# library(data.table)
# library(caret)
# library(tm)
# library(quanteda)
# samples_q <- questions[sample(nrow(questions), 80000), ]
# 
# set.seed(100)
# trainNum <- createDataPartition(as.factor(samples_q$Tags),p = .75,list=F)
# train <- samples_q[trainNum,]
# test <- samples_q[-trainNum,]
# 
# (unique(samples_q$Tags))
# 
# dfmGen <- function(samples){
#   toks <- tokens_remove(tokens(samples$Title, 
#                                remove_punct = TRUE),stopwords("english"))
#   ngram <- tokens_ngrams(toks, n = 2)
#   my_dfm <- dfm(ngram, stem = FALSE)
#   topfeatures(my_dfm, 20)  # 20 top words
#   my_dfm1<- dfm_trim(my_dfm, min_termfreq = 100,min_docfreq = 120)
#   set.seed(100)
#   textplot_wordcloud(my_dfm1, min_count = 6, random_order = FALSE,
#                      rotation = .25, 
#                      color = RColorBrewer::brewer.pal(8,"Dark2"))
#   #return(my_dfm1)
# 
#     return(convert(my_dfm1,to = "data.frame"))
# }


library(dplyr)
library(tidytext)

#samples_q <- questions[sample(nrow(questions), 80000), ]
trainNum <- createDataPartition(as.factor(questions$Tags),p = .75,list=F)

samples_q <- questions[trainNum,]
samples_q_test <- questions[-trainNum,]

perfComputationForLDA <- function(samples_q){
  
  posts_words <- samples_q %>%
    unnest_tokens(to_lower = T, strip_numeric=T,strip_punct=T,word, Title) %>%
    count(Tags, word, sort = TRUE) %>%
    ungroup()
  
  
  total_words <- posts_words %>% 
    group_by(Tags) %>% 
    summarize(total = sum(n))
  
  tag_words <- left_join(posts_words, total_words)
  
  tag_words
  library(ggplot2)
  
  #Number of words 'n' occuring in total tags distribution plot #########
  ggplot(tag_words, aes(n/total, fill = Tags)) +
    geom_histogram(show.legend = FALSE) +
    xlim(NA, 0.0009) +
    facet_wrap(~Tags, ncol = 2, scales = "free_y")
  
  
  ####Zip's frequency is inversly proportional to rank
  freq_by_rank <- tag_words %>% 
    group_by(Tags) %>% 
    mutate(rank = row_number(), 
           `term_frequency` = n/total)
  
  
  #####Plot Zip's Law showing inverse proportion relationship #########
  ggplot(data = freq_by_rank,aes(x=rank,y=term_frequency,color=Tags)) + 
    geom_line(size = 0.4, alpha = 0.8, show.legend = T) + scale_x_log10() + scale_y_log10()
  
  
  ###### Apply stopwords and then apply tfidf #########
  mystopwords = data.frame(word = c("5.7","5.6","2.7","1064","3.6"))
  tag_words <- anti_join(tag_words, mystopwords, by = "word")
  
 # tag_words <- anti_join(tag_words, stop_words, by = "word")
  
  
  tag_words <- tag_words %>%
    bind_tf_idf(word,Tags,n)
  
  
  tag_words
  
  tag_words %>% arrange(desc(tf_idf))
  
  ##Select top_15 words of tf_idf ###########
  tag_words %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>% 
    group_by(Tags) %>% 
    top_n(15) %>% 
    ungroup %>%
    ggplot(aes(word, tf_idf, fill = Tags)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf top 15 words") +
    facet_wrap(~Tags, ncol = 5, scales = "free") +
    coord_flip()
  
  #############Cast the DTM #############
  
  title_dtm <- tag_words %>%
    arrange(desc(tf_idf)) %>%
    top_n(50) %>%
    group_by(Tags) %>% 
    cast_dtm(Tags, word, n)
  
  return(title_dtm)
  
}




####### LDA ###############################

library(topicmodels)
library(stats)
library(stringr)

intrain <- perfComputationForLDA(samples_q)

intest <- perfComputationForLDA(samples_q_test)
title_tags_lda <- LDA(intrain, k = 16, control = list(seed = 1234),method = "VEM")
title_tags_lda



title_topics <- tidy(title_tags_lda, matrix = "beta")
title_topics

top_terms <- title_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()




############ Make Predictions #####################
test.topics <- posterior(title_tags_lda,intest)
(test.topics <- apply(test.topics$topics, 1, which.max))


#using tidy with gamma gets document probabilities into topic
#but you only have document, topic and gamma

colnames(samples_q)[9] <- "Title"
colnames(samples_q)[10] <- "Tags"
source_topic_relationship <- tidy(title_tags_lda, matrix = "gamma") %>%
  #join to orig tidy data by doc to get the source field
  inner_join(samples_q, by = "document") %>%
  select(document, topic, gamma) %>%
  group_by(document, topic) %>%
  #get the avg doc gamma value per source/topic
  mutate(mean = mean(gamma)) %>%
  #remove the gamma value as you only need the mean
  select(-gamma) %>%
  #removing gamma created duplicates so remove them
  distinct()

#relabel topics to include the word Topic
source_topic_relationship$topic = paste("Topic", source_topic_relationship$topic, sep = " ")

circos.clear() #very important! Reset the circular layout parameters
#assign colors to the outside bars around the circle
grid.col = c("android" = "#FF5733",
             "c" = "#DAF7A6",
             "c#" = "#FFC300",
             "c++" = "#C70039",
             "html" = "#581845",
             "css" = "#33FFE5",
             "ios" = "#A0FF33",
             "java" = "#4486D2",
             "javascript" = "#1B324C",
             "jquery" = "#B695D0",
             "mysql" = "#250D39",
             "php" = "#09A6F7",
             "python" = "#064115",
             "r" = "#FF0909",
             "sql" = "#996B97",
             "Topic 1" ="grey",
             "Topic 2" ="grey",
             "Topic 3" ="grey",
             "Topic 4" ="grey",
             "Topic 5" ="grey",
             "Topic 6" ="grey",
             "Topic 7" ="grey",
             "Topic 8" ="grey",
             "Topic 9" ="grey",
             "Topic 10" ="grey",
             "Topic 11" ="grey",
             "Topic 12" ="grey",
             "Topic 13" ="grey",
             "Topic 14" ="grey",
             "Topic 15" ="grey",
             "Topic 16" ="grey")


library(tidytext) #text mining, unnesting
library(topicmodels) #the LDA algorithm
library(tidyr) #gather()
library(dplyr) #awesome tools
library(ggplot2) #visualization
library(kableExtra) #create attractive tables
library(knitr) #simple table generator
library(ggrepel) #text and label geoms for ggplot2
library(gridExtra)
library(formattable) #color tile and color bar in `kables`
library(tm) #text mining
library(circlize) #already loaded, but just being comprehensive
library(plotly)
# set the global parameters for the circular layout. Specifically the gap size (15)
#this also determines that topic goes on top half and source on bottom half
circos.par(gap.after = c(rep(5, length(unique(source_topic_relationship[[1]])) - 1), 15,
                         rep(5, length(unique(source_topic_relationship[[2]])) - 1), 15))


#main function that draws the diagram. transparancy goes from 0-1
chordDiagram(source_topic_relationship, grid.col = grid.col, transparency = .2)
title("Relationship Between Topic and Source")



text = "Implement multi class classification using SVM in R"
text1 = "Implement multi class classification using SVM in python"
pData = data.frame("Title"=c(text,text1),"Tags"=c("doc1","doc2"),stringsAsFactors = FALSE)
class(samples_q$Title)
pData$Title
inPred = perfComputationForLDA(pData)

test.topics <- posterior(title_tags_lda,inPred)
(test.topics <- apply(test.topics$topics, 1, which.max))

(inPred.topics <- apply(inPred$topics, 1, which.max))

tag_words <- posts_words_pred %>%
  bind_tf_idf(word,n)

library(shiny)
# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # Copy the line below to make a text input box
  textInput("text", label = h3("Stack Overflow Title"), placeholder = "Stackoverflow Title"),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value")))
  
)

server <- function(input, output) {
  
  
}

shinyApp(ui, server)






title_tags_lda$topics

samples_qFM <- dfmGen(questions)
samples_qFM <- samples_qFM %>% dplyr::select(-document)
Tags = questions$Tags
samples_qFM <- cbind(samples_qFM,Tags)
set.seed(100)
trainNum <- createDataPartition(as.factor(questions$Tags),p = .75,list=F)
train <- samples_qFM[trainNum,]
test <- samples_qFM[-trainNum,]



train <- train %>% dplyr::select(-document)
train <- train %>% dplyr::select(-document)
trainTags = train$Tags
testTags = test$Tags

inTraining <- cbind(trainFM,trainTags)
inTest <- cbind(testFM,testTags)
convert(inTraining,to="dfm")




library(doParallel)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

fitControl <- trainControl(method = 'repeatedcv',
                           number = 2,
                           repeats = 3,
                           allowParallel = TRUE,
                           classProbs = FALSE)


#inTraining$trainTags

#sum(is.na(inTraining))

model <- train(y=train$Tags,x=train[, names(train) != "Tags"],method="rf",trainControl = fitControl)

## When you are done:
stopCluster(cl)

detach("package:caret", unload=TRUE)
pred <- predict(model$finalModel,test)

out <- confusionMatrix(pred, test$Tags, mode = "everything")
out
trainFM$trainTags



unique(samples_q$Tags)

prep_fun = tolower
tok_fun = word_tokenizer
stop_words = stopwords::data_stopwords_smart

it_train = itoken(samples_q$Title, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = samples_q$Id, 
                  progressbar = FALSE)

vocab = create_vocabulary(it_train,stopwords=as.character(stop_words),ngram = c(1L,2L))

vocabDf <- as.data.frame(vocab)


bigram_vectorizer = vocab_vectorizer(vocab)

dtm_train = create_dtm(it_train, bigram_vectorizer)

dim(dtm_train)

tdm2 <- removeSparseTerms(dtm_train, sparse = 0.2)
tdm_Matrix <- as.matrix(tdm2)






# library(h2o)
# h2o.init()
# 
# job.titles.path = "https://raw.githubusercontent.com/h2oai/sparkling-water/rel-1.6/examples/smalldata/craigslistJobTitles.csv"
# 
# job.titles <- h2o.importFile(job.titles.path, destination_frame = "jobtitles",
#                              col.names = c("category", "jobtitle"), col.types = c("Enum", "String"), header = TRUE)
# 
# STOP_WORDS = c("ax","i","you","edu","s","t","m","subject","can","lines","re","what",
#                "there","all","we","one","the","a","an","of","or","in","for","by","on",
#                "but","is","in","a","not","with","as","was","if","they","are","this","and","it","have",
#                "from","at","my","be","by","not","that","to","from","com","org","like","likes","so","using")
# 
# tokenize <- function(sentences, stop.words = STOP_WORDS) {
#   tokenized <- h2o.tokenize(sentences, "\\\\W+")
#   
#   # convert to lower case
#   tokenized.lower <- h2o.tolower(tokenized)
#   # remove short words (less than 2 characters)
#   tokenized.lengths <- h2o.nchar(tokenized.lower)
#   tokenized.filtered <- tokenized.lower[is.na(tokenized.lengths) || tokenized.lengths >= 2,]
#   # remove words that contain numbers
#   tokenized.words <- tokenized.filtered[h2o.grep("[0-9]", tokenized.filtered, invert = TRUE, output.logical = TRUE),]
#   
#   # remove stop words
#   tokenized.words[is.na(tokenized.words) || (! tokenized.words %in% STOP_WORDS),]
# }
# 
# predict <- function(job.title, w2v, gbm) {
#   words <- tokenize(as.character(as.h2o(job.title)))
#   job.title.vec <- h2o.transform(w2v, words, aggregate_method = "AVERAGE")
#   h2o.predict(gbm, job.title.vec)
# }
# 
# questions.hex <- as.h2o(questions)
# #Convert to categorical
# questions.hex["Tags"] <- as.factor(questions.hex["Tags"])
# #Print the types of h2o dataframe
# h2o.getTypes(questions.hex)
# 
# 
# print("Break job titles into sequence of words")
# words <- tokenize(questions.hex$Title)
# 
# print("Build word2vec model")
# w2v.model <- h2o.word2vec(words, sent_sample_rate = 0, epochs = 10)
# 
# print("Sanity check - find synonyms for the word 'teacher'")
# print(h2o.findSynonyms(w2v.model, "java", count = 10 ))
# 
# 
# 
# print("Calculate a vector for each job title")
# title.vecs <- h2o.transform(w2v.model, words, aggregate_method = "AVERAGE")
# 
# print("Prepare training&validation data (keep only titles made of known words)")
# valid.titles <- ! is.na(title.vecs$C1)
# 
# data <- h2o.cbind(questions.hex[valid.titles, "Tags"], title.vecs[valid.titles, ])
# 
# 
# data.split <- h2o.splitFrame(data, ratios = c(0.6,0.2),seed=101)
# 
# train <- h2o.assign(data.split[[1]], "train.hex")  
# 
# names(train)
# 
# ## assign the first result the R variable train
# ## and the H2O name train.hex
# valid <- h2o.assign(data.split[[2]], "valid.hex")   ## R valid, H2O valid.hex
# test <- h2o.assign(data.split[[3]], "test.hex")     ## R test, H2O test.hex
# 
# print("Build a basic GBM model")
# gbm.model <- h2o.gbm(x = names(title.vecs), y = "Tags",
#                      training_frame = train, validation_frame = valid)
# 
# summary(gbm.model)
# 
# print("Predict!")
# finalRf_predictions<-h2o.predict(
#   object = gbm.model
#   ,newdata = test)
# 
# mean(finalRf_predictions$predict == test$Tags)
# 
# h2o.hit_ratio_table(gbm.model,valid = T)[1,2]
# print(predict("Can't change nextArrow and prevArrow in Slick Carousel", w2v.model, gbm.model))
# 
# print(predict("Python IOError: [Errno 22] when creating file", w2v.model, gbm.model))
