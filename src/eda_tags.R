# eda_tags.R
# visualizations


# questions grouped by total answer count, comment count, and sum of scores of tags
tags_summary <- questions %>%
    group_by(Tags) %>%
    summarise(sum_answer_count = sum(AnswerCount),
              sum_comment_count = sum(CommentCount),
              sum_score = sum(Score),
              count = n()) %>% 
    mutate(avg_score = sum_score/count) %>% 
    select(Tags, sum_answer_count, sum_comment_count, sum_score, count, avg_score)



# create a TidyText column for comments that contain preprocessed text
comments <- comments %>% 
    rowwise() %>% 
    mutate(TidyBody = CleanText(Text))

# same as above for questions
questions_sample <- questions_sample %>% 
    rowwise() %>% 
    mutate(TidyBody = RemoveStopwords(Body)) %>% 
    mutate(TidyBody = CleanText(TidyBody))

    


# get only the comments that are in the posts sample
# then clean the text into a new TidyBody column and also create a Sentiment column
comments_of_posts <- comments %>% 
    filter(PostId %in% questions_sample$Id)
    rowwise() %>% 
    mutate(TidyBody = CleanText(Text)) %>% 
    mutate(Sentiment = GetSentiment(TidyText))



# create a SentimentOfComments column for each post
questions_sample["SentimentOfComments"] <- apply(c(comments_of_posts, questions_sample["Id"]), 1, SentimentOfComments)

# group by tags and aggregate by mean of SentimentOfComments
tags_summary <- questions_sample %>%
    group_by(Tags) %>%
    summarise(AverageSentiment = mean(SentimentOfComments, na.rm = TRUE))

# avg sentiment of comments grouped by tag
ggplot(tags_summary, aes(x = Tags, y = AverageSentiment, fill = Tags)) +
    geom_bar(stat="identity") + 
    coord_polar() +
    theme_bw() +
    labs(x = "Tags", y = "Average Sentiment", title = "Average Sentiment towards Tags")




# Plot answer_counts aggregated with Tags
angle <- theme(axis.text.x = element_text(angle=60))
pdf(file="plots_questions.pdf",paper = "a4" )

ggplot(questions_one_tag, aes(x=Tags,fill=Tags)) + 
    geom_bar(stat = "count") + 
    ggtitle("Frequency of Tags") + 
    angle
ggplot(tags_summary, aes(x=Tags,y=sum_answer_count,fill=sum_answer_count)) +
    geom_bar(stat = "identity") +
    ggtitle("Tag Distribution on Answers") +
    angle
ggplot(tags_summary, aes(x=Tags,y=sum_comment_count,fill=sum_comment_count)) + 
    geom_bar(stat = "identity") + 
    ggtitle("Tag Distribution on Comment") +
    angle
ggplot(tags_summary, aes(x=Tags,y=sum_score,fill=sum_score)) + 
    geom_bar(stat = "identity") + 
    ggtitle("Tag Distribution on Score") +
    angle

dev.off()
graphics.off()




# average score grouped by tags
ggplot(tags_summary, aes(x=Tags,y=avg_score,fill=avg_score)) + 
    geom_bar(stat = "identity") + 
    labs(x="Tags", y="Average Score", title="Average Score By Tag")



avg_score_all <- tags_summary %>% 
    pull(avg_score) %>% 
    mean()

ks <- ks.test(x = avg_score, y = rep(avg_score_all, times=nrow(tags_summary)))



###########################


# group all the words by each tag
tags_grouped <- questions %>%
    group_by(Tags) %>%
    dplyr::summarise(WordsByTag=paste(as.character(TidyBody), collapse=" "))



tags_tokens <- tags_grouped %>%
    unnest_tokens(output = word, input = WordsByTag)

tags_dtm <- tags_tokens %>% 
    # get count of each token in each document
    count(Tags, word) %>% 
    # create a document-term matrix with all features and tfidf weighting
    cast_dtm(document = Tags, term = word, value = n,
             weighting = tm::weightTfIdf)

# calculate tfidf for each word in each tag
tags_tfidf <- tags_tokens %>%
    count(Tags, word) %>%
    bind_tf_idf(term = word, document = Tags, n = n)


# plot top 10 words (based on tfidf) for each Tag
tag <- c('r', 'java', 'c#', 'sql', 'ios', 'python', 'html', 'css', 'javascript')
plot_tags <- tags_tfidf %>%
    filter(Tags %in% tag) %>%
    arrange(desc(tf_idf)) %>% 
    group_by(Tags) %>%
    slice(1:10) 


plot_tags %>% 
    ggplot(aes(word, tf_idf, fill = Tags)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Most relevant words by tag"), ylab = NULL, xlab = NULL) +
    coord_flip() +
    facet_wrap(~Tags, scales="free") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x=element_blank(),
          strip.text.x = element_text(size = 10, face="bold"),
          axis.text=element_text(size=10, face="bold"),
          axis.title.x=element_text(size=10),
          plot.title = element_text(size=15, face = "bold"))



# plot a graph of the top cooccuring words by tag
desc_word_pairs <- tags_tfidf %>% 
    pairwise_count(word, Tags, sort = TRUE, upper = FALSE)

desc_word_pairs %>%
    filter(n >= 6) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
    geom_node_point(size = 5) +
    geom_node_text(aes(label = name), repel = TRUE, 
                   point.padding = unit(0.2, "lines")) +
    labs(title="Most Co-occuring Words in Questions of the Same Tag") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))




