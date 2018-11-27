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
              sum_score = sum(Score)) %>%
    #top_n(5,avg_answer_count) %>%
    select(Tags, sum_answer_count, sum_comment_count, sum_score)



q <- questions_one_tag
q <- sample_n(q, 2000)
comments_of_posts <- filter(comments, PostId %in% q$Id)
comments_of_posts <- comments_of_posts %>% 
    rowwise() %>% 
    mutate(Sentiment = GetSentiment(Text))

# create a Sentiment column for comments
comments_of_posts["Sentiment"] <- apply(comments_of_posts["Text"], 1, GetSentiment)

# create a SentimentOfComments column for each post
q["SentimentOfComments"] <- apply(q["Id"], 1, SentimentOfComments)

# group by tags and aggregate by mean of SentimentOfComments
tags_summary <- q %>%
    group_by(Tags) %>%
    summarise(AverageSentiment = mean(SentimentOfComments, na.rm = TRUE))

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

