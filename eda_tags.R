#eda_tags.R



# apply the filterOneTag function on the Tags column of questions
# this will make the Tags column have only one tag
questions_one_tag <- questions
questions_one_tag["Tags"] <- apply(questions["Tags"], 1, filterOneTag)
questions_one_tag <- questions_one_tag %>% filter(Tags != "")


# questions grouped by total answer count, comment count, and sum of scores of tags
tags_summary <- questions_one_tag %>%
    group_by(Tags) %>%
    summarise(sum_answer_count = sum(AnswerCount),
              sum_comment_count = sum(CommentCount),
              sum_score = sum(Score),
              count = n()) %>% 
    mutate(avg_score = sum_score/count) %>% 
    select(Tags, sum_answer_count, sum_comment_count, sum_score, count, avg_score)





# random sample questions and filter comments that are in the sample
questions_sample <- sample_n(questions_one_tag, 10000)

# create a TidyText column for comments that contains preprocessed text
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






