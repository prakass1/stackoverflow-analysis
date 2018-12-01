#eda_tags.R


# apply the filterOneTag function on the Tags column of questions
# this will make the Tags column have only one tag
questions_one_tag <- questions
questions_one_tag["Tags"] <- apply(questions["Tags"], 1, filterOneTag)
questions_one_tag <- questions_one_tag %>% filter(Tags != "")


# questions grouped by total answer count, comment count, and sum of scores of tags
tags_summary <- questions_one_tag %>%
    group_by(Tags) %>%
    summarise(mean_answer = mean(AnswerCount),
              mean_comment = mean(CommentCount),
              mean_score = mean(Score)) %>%
    #top_n(5,avg_answer_count) %>%
    select(Tags, mean_answer, mean_comment, mean_score)



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
ggplot(tags_summary, aes(x=Tags,y=mean_answer,fill=mean_answer)) +
    geom_bar(stat = "identity") +
    ggtitle("Tag Distribution on Answers") +
    angle
ggplot(tags_summary, aes(x=Tags,y=mean_comment,fill=mean_comment)) + 
    geom_bar(stat = "identity") + 
    ggtitle("Tag Distribution on Comment") +
    angle
ggplot(tags_summary, aes(x=Tags,y=mean_score,fill=mean_score)) + 
    geom_bar(stat = "identity") + 
    ggtitle("Tag Distribution on Score") +
    angle

dev.off()
graphics.off()

####### Pie Chart a Distribution#############
Tags_freq <- count(questions,Tags)

calcPercFreq <- function(x){
  return((x/nrow(questions)) * 100)
}

Tags_freq['n'] <- apply(Tags_freq['n'],1,calcPercFreq)

library("plotly")
p <- plot_ly(Tags_freq, labels = ~Tags, values = ~n, type = 'pie') %>%
  layout(title = 'Percentage of Tag Distribution in the Dataset',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p

######## Percentage of 15 tags across months in our dataset #####

tagPMonthlySum <- questions %>% 
  group_by(month = substr(CreationDate,1,7)) %>% 
  count(Tags) %>% summarise(sumTags = sum(n))

tagPMonthly <- questions %>% 
  group_by(month = substr(CreationDate,1,7)) %>% 
  count(Tags) %>%
  mutate(percN = (n/sum(n))*100)

ggplot(tagPMonthly, aes(x = month, y = percN,fill=Tags)) + 
  geom_bar(stat = "identity",position = "stack") + facet_grid(~ Tags) + theme(axis.text.x = element_text(angle=45))