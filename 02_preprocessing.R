library("stringr")
library("gsubfn")
library("ggplot2")
library("stringr")
library("lubridate")
#select all the post ids
post_ids <- posts$Id
#filter comments dataframe to contain rows that are also in posts dataframe
comments <- filter(comments, PostId %in% post_ids)

#separate questions and answers into different dataframes
questions <- filter(posts, PostTypeId == 1)
answers <- filter(posts, PostTypeId == 2)


## Use only selective columns
question_cols_filtered <- c("Id",
                            "PostTypeId",
                            "CreationDate",
                            "Score",
                            "ViewCount",
                            "Body",
                            "LastActivityDate",
                            "Title",
                            "Tags",
                            "AnswerCount",
                            "FavoriteCount")
<<<<<<< HEAD

answers_cols_filtered <- c("Id",
                          "CreationDate",
                          "Score",
                          "Body",
                          "CommentCount",
                          "ParentId")






#Selected Columns for question and answers
questions_sel <- questions[question_cols_filtered]
answers_sel <- answers[answers_cols_filtered]


##Transform Date to yyyy/mm/dd format

class(questions_sel$CreationDate)
=======

##Transform Date to mm/dd/yyyy format



#remove html tags from the body column
#questions <- RemoveHTML(questions)
#answers <- RemoveHTML(answers)
>>>>>>> afa1ddf618c0faa6909a56e2f376e5416319c7f7

questions_sel <- questions_sel %>%
                 transform(CreationDate = as.Date(CreationDate, tryFormats = c("%Y%m%d"))) %>%
                 transform(LastActivityDate = as.Date(LastActivityDate, tryFormats = c("%Y%m%d")))

<<<<<<< HEAD
answers_sel <- answers_sel %>%
  transform(CreationDate = as.Date(CreationDate, tryFormats = c("%Y%m%d")))


### Make all NA cols to 0 for not having any NULLS
questions_sel[is.na(questions_sel)] <- 0


#### Check <code> tag and add a new column called hasCode
# hasCodeTag <- function(x) {
#   
#               if(grapl("<code>",x,fixed=TRUE)){
#                   return(1)
#               }
#               else{
#                 return(0)
#               }
#               }


# Check and add a new column as hasCodeTag to see if there is already a code
questions_sel <- questions_sel %>%
                 mutate(hasCodeTag = case_when(grepl("<code>",Body,fixed=TRUE) == 1 ~ 1, TRUE ~ 0)) %>%
                 mutate(hasLinks = case_when(grepl("http://",Body,fixed=TRUE) == 1 ~ 1, TRUE ~ 0))

answers_sel <- answers_sel %>%
                mutate(hasCodeTag = case_when(grepl("<code>",Body,fixed=TRUE) == 1 ~ 1, TRUE ~ 0)) %>%
                mutate(hasLinks = case_when(grepl("http://",Body,fixed=TRUE) == 1 ~ 1, TRUE ~ 0))
  
### Extract links in the text





#remove html tags from the body column
#questions <- RemoveHTML(questions)
#answers <- RemoveHTML(answers)


###Select only those 20 top tags by count########
# selected_tags_count <- tbl_df(tags) %>%
#                  top_n(15,Count) %>%
#                  select(TagName,Count) 
# 
# selected_tags_count <- arrange(selected_tags_count,desc(Count))
# 
# 
# selected_tags <- selected_tags_count$TagName


=======
###Select only those 20 top tags by count########
# selected_tags_count <- tbl_df(tags) %>%
#                  top_n(15,Count) %>%
#                  select(TagName,Count) 
# 
# selected_tags_count <- arrange(selected_tags_count,desc(Count))
# 
# 
# selected_tags <- selected_tags_count$TagName


>>>>>>> afa1ddf618c0faa6909a56e2f376e5416319c7f7
#15 selected tags
selected_tags <- c("android",
                   "c",
                   "c#",
                   "c++",
                   "html",
                   "css",
                   "ios",
                   "java",
                   "javascript",
                   "jquery",
                   "mysql",
                   "php",
                   "python",
                   "r",
                   "sql")

filterTags <- function(x){
            for(tag in selected_tags){
              testExpr <- paste0("<",tag,">")
            if(grepl(testExpr,x,fixed = TRUE)){
              return(x)
            }

            }
  return("")
}


#### Filter as one tag -- This is purely for Visualizations ####
filterOneTag <- function(x){
  for(tag in selected_tags){
    testExpr <- paste0("<",tag,">")
    if(grepl(testExpr,x,fixed = TRUE)){
      return(tag)
    }
    
  }
  return("")
}

#### Filter to have only the top 25 tags by questions######
## May be we can give our custom tags too ##########
questions["Tags"] <- apply(questions["Tags"],1,filterTags)
#### Filter to only have tags and remove <> tags
question_filtered <- questions %>%
                    filter(grepl("<", Tags))
<<<<<<< HEAD
=======

# Plot the frequency of Tags in the dataset
ggplot(data=question_filtered, aes(x=Tags,fill=Tags)) + geom_bar(stat="count")
>>>>>>> afa1ddf618c0faa6909a56e2f376e5416319c7f7

# Plot the frequency of Tags in the dataset
ggplot(data=question_filtered, aes(x=Tags,fill=Tags)) + geom_bar(stat="count")






##### Do one tag summarization

questions["Tags"] <- apply(questions["Tags"],1,filterOneTag)
questions <- questions %>% filter(Tags!="")

val <- questions %>%
       group_by(Tags) %>%
       summarise(sum_answer_count = sum(AnswerCount),
                 sum_comment_count = sum(CommentCount),
                 sum_score = sum(Score)) %>%
  #top_n(5,avg_answer_count) %>%
       select(Tags,sum_answer_count,sum_comment_count,sum_score)

# Plot answer_counts aggregated with Tags
angle <- theme(axis.text.x = element_text(angle=60))
pdf(file="plots_questions.pdf",paper = "a4" )
ggplot(questions, aes(x=Tags,fill=Tags)) + 
  geom_bar(stat = "count") + 
  ggtitle("Frequency of Tags") + 
  angle
ggplot(val, aes(x=Tags,y=sum_answer_count,fill=sum_answer_count)) +
  geom_bar(stat = "identity") +
  ggtitle("Tag Distribution on Answers") +
  angle
ggplot(val, aes(x=Tags,y=sum_comment_count,fill=sum_answer_count)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Tag Distribution on Comment") +
  angle
ggplot(val, aes(x=Tags,y=sum_score,fill=sum_answer_count)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Tag Distribution on Score") +
  angle

dev.off()
graphics.off()


<<<<<<< HEAD
=======



##### Do one tag summarization

questions["Tags"] <- apply(questions["Tags"],1,filterOneTag)
questions <- questions %>% filter(Tags!="")

val <- questions %>%
       group_by(Tags) %>%
       summarise(sum_answer_count = sum(AnswerCount),
                 sum_comment_count = sum(CommentCount),
                 sum_score = sum(Score)) %>%
  #top_n(5,avg_answer_count) %>%
       select(Tags,sum_answer_count,sum_comment_count,sum_score)

# Plot answer_counts aggregated with Tags
angle <- theme(axis.text.x = element_text(angle=60))
pdf(file="plots_questions.pdf",paper = "a4" )
ggplot(questions, aes(x=Tags,fill=Tags)) + 
  geom_bar(stat = "count") + 
  ggtitle("Frequency of Tags") + 
  angle
ggplot(val, aes(x=Tags,y=sum_answer_count,fill=sum_answer_count)) +
  geom_bar(stat = "identity") +
  ggtitle("Tag Distribution on Answers") +
  angle
ggplot(val, aes(x=Tags,y=sum_comment_count,fill=sum_answer_count)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Tag Distribution on Comment") +
  angle
ggplot(val, aes(x=Tags,y=sum_score,fill=sum_answer_count)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Tag Distribution on Score") +
  angle

dev.off()
graphics.off()


>>>>>>> afa1ddf618c0faa6909a56e2f376e5416319c7f7
