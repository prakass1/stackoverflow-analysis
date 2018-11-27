library("tm")
library("tidytext")
library("sentimentr")
library("wordcloud")

n <- 1000
u <- sample_n(users, n)
c <- sample_n(comments, n)
q <- sample_n(questions, n)

# convert nas to empty strings
users[is.na(users)] <- ""

u <- users
users <- users[1:1000,]
# create a column that stores the sentiment score for a user in their AboutMe
users <- users %>%
    rowwise() %>% 
    mutate(Sentiment = GetSentiment(AboutMe))


# overall sentiment of users AboutMe
ggplot(users, aes(x = Sentiment)) +
    geom_density(fill = "steelblue") +
    scale_x_log10() 
    



# remove the stopwords in the user AboutMe and convert all text to lower case
users["TidyBody"] <- RemoveStopwords(users$AboutMe)

# create a list of words with their frequencies
words <- users %>% 
    select("TidyBody") %>% 
    unnest_tokens(word, TidyBody) %>% 
    group_by(word) %>% 
    summarise(count = n())

# create a wordcloud by passing in each word with its frequency to wordcloud()
pal <- brewer.pal(9,"Set2")
wordcloud(words$word , words$count, 
          scale=c(2.5, 0.5), max.words=70, colors = pal)



