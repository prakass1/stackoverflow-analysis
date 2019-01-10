# 04_prediction.R
# build models for tag recommendation



# get most representative words
# group all the words by each tag and get the top relevant words per tag (tf-idf)
top_words <- train_questions %>%
    group_by(Tags) %>%
    unnest_tokens(output = word, input = Text) %>% 
    count(Tags, word) %>%
    bind_tf_idf(term = word, document = Tags, n = n) %>% 
    # sort by descending tf-idf
    arrange(desc(tf_idf)) %>% 
    group_by(Tags) %>%
    slice(1:60)



######### create document term matrix ###########

# remove words from tidy_questions that are not in top_words and 
# create a document term matrix
dtm <- train_questions %>% 
    unnest_tokens(word, Text) %>% 
    filter(word %in% top_words$word) %>% 
    # get count of each token in each document
    count(Id, word) %>% 
    # create a document-term matrix with all features and tf weighting
    cast_dtm(document = Id, term = word, value = n,
             weighting = tm::weightTfIdf)


# get the doc ids and extract their classes to create the class vector y
rows <- dtm$dimnames$Docs
y <- train_questions %>% 
    filter(Id %in% rows) %>% 
    pull(Tags) %>%
    factor()
# cast as a dataframe to feed to classifiers
dtm_tbl <- as_tibble(as.matrix(dtm))



######### train test split ###########

# get random indices to split train and test sets
#indices <- sample(1:nrow(dtm_tbl), floor(0.70*nrow(dtm_tbl)))
indices <- createDataPartition(y=y, p=0.75, list=FALSE)  

X_train <- dtm_tbl[indices,]
y_train <- y[indices]
X_test <- dtm_tbl[-indices,]
y_test <- y[-indices]




############# classification ###############

# train a nearest centroid classifier
nm_classifier <- klaR::nm(x=X_train, grouping=y_train)
# predict
predictions <- predict(nm_classifier, newdata = X_test)$class
# accuracy
paste0('accuracy: ', mean(y_test == predictions))




# train a multinomial naive bayes classifier

#indices <- createDataPartition(y=y, p=0.25, list=FALSE)  
indices <- sample(1:nrow(dtm_tbl), floor(0.70*nrow(dtm_tbl)))

test_dfm <- as.dfm(dtm[indices,])
test_y <- y[indices]

dfm <- as.dfm(dtm)

nb_classifier <- textmodel_nb(dfm, y = y, distribution = "multinomial")

test_dfm <- dfm_select(test_dfm, pattern = dfm, 
                       selection = "keep")

predicted_class <- predict(nb_classifier, newdata = test_dfm)

paste0('accuracy: ', mean(test_y == predicted_class))

class_table <- table(test_y, predicted_class)
confusionMatrix(class_table, mode = "everything")





#################### lsa ######################



tokens = train_questions$Text %>% 
    tolower %>% 
    word_tokenizer
it = itoken(tokens, ids = train_questions$Id, progressbar = FALSE)
v = create_vocabulary(it) %>% 
    prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.4)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer, type = "dgTMatrix")

lda_model = LDA$new(n_topics = 15, doc_topic_prior = 0.15, topic_word_prior = 0.01)
doc_topic_distr = 
    lda_model$fit_transform(x = dtm, n_iter = 1000, 
                            convergence_tol = 0.001, n_check_convergence = 25, 
                            progressbar = FALSE)


barplot(doc_topic_distr[500, ], xlab = "topic", 
        ylab = "proportion", ylim = c(0, 1), 
        names.arg = 1:ncol(doc_topic_distr))

lda_model$get_top_words(n = 10, topic_number = c(1L, 4, 10L), lambda = 1)


lda_model$plot()



new_dtm = itoken("select dplyr group", tolower, word_tokenizer, ids = 1) %>% 
    create_dtm(vectorizer, type = "dgTMatrix")
new_doc_topic_distr = lda_model$transform(new_dtm)



# new data

new_data = "select dplyr group"
new_doc_embeddings <- 
    new_data %>% 
    itoken(preprocessor = CleanText, progressbar = FALSE) %>% 
    create_dtm(vectorizer) %>% 
    # apply exaxtly same scaling wcich was used in train data
    transform(tfidf) %>% 
    # embed into same space as was in train data
    transform(lsa)
dim(new_doc_embeddings)
