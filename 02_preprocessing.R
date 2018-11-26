# 02_preprocessing.R
# data cleaning and preprocessing

library("stringr")
library("gsubfn")
library("ggplot2")
library("stringr")
library("lubridate")
library("rvest")
library("corrplot")




############ Preprocess Posts ############

# 1. separate question posts and answer posts into different dataframes
# 2. keep only the relevant columns in questions and answers
# 3. remove the time part of the datetime fields in questions and answers
# 4. convert the Nas in questions to zeros
# 5. select the questions having the top 15 selected tags
# 6. extract links and code from posts and move them to separate columns
# 7. add new columns HasCode and HasLinks


# posts contain questions which are PostTypeId 1 and answers are PostTypeId 2,
# so they can be separated into different dataframes
questions <- posts %>% 
    filter(PostTypeId == 1)
answers <- posts %>% 
    filter(PostTypeId == 2)

# remove the posts dataframe to save memory
rm(posts)


# some columns aren't relevant, so only the useful columns need to be kept 
# and the others can be removed
questions_cols <- c("Id", 
                    "PostTypeId", 
                    "CreationDate", 
                    "Score",
                    "ViewCount",
                    "Body",
                    "OwnerUserId",
                    "LastActivityDate",
                    "Title",
                    "Tags",
                    "AnswerCount",
                    "CommentCount",
                    "FavoriteCount")

answers_cols <- c("Id",
                  "CreationDate",
                  "Score",
                  "Body",
                  "OwnerUserId",
                  "CommentCount",
                  "ParentId")

# keep only selective columns
questions <- questions %>% 
    select(questions_cols)
answers <- answers %>% 
    select(answers_cols)

# remove the time part of the datetime columns as time isn't meaningful 
questions <- questions %>%
    transform(CreationDate = as.Date(CreationDate, tryFormats = c("%Y%m%d"))) %>%
    transform(LastActivityDate = as.Date(LastActivityDate, tryFormats = c("%Y%m%d")))
# same as above
answers <- answers %>%
    transform(CreationDate = as.Date(CreationDate, tryFormats = c("%Y%m%d")))


# make all NA cols to 0 for not having any NULLS
questions[is.na(questions)] <- 0



#### Filter to have only the top 15 tags by questions######
## May be we can give our custom tags too ##########

# apply the filterTags function on the values in the Tags column. 
# This will replace the tags with empty strings, if they are not in the top 15 selected tags
questions["Tags"] <- apply(questions["Tags"], 1, filterTags)
# remove the questions that do not have a top 15 tag
questions <- questions %>% 
    filter(Tags != "")



# extract links from questions and answers body and place them in new columns 
questions$Links <- lapply(questions$Body, GetLinks)
answers$Links <- lapply(answers$Body, GetLinks)

# extract code from questions and answers body and place them in new columns 
questions$Code <- lapply(questions$Body, GetCode)
answers$Code <- lapply(answers$Body, GetCode)

# create a new column HasLinks, which contains 1 if the post body contains links, 0 otherwise
questions <- questions %>% 
    mutate(HasLinks = if_else(Links != "", 1, 0))
answers <- answers %>% 
    mutate(HasLinks = if_else(Links != "", 1, 0))

# create a new column HasCode, which contains 1 if the post body contains code, 0 otherwise
questions <- questions %>% 
    mutate(HasCode = if_else(Code != "", 1, 0))
answers <- answers %>% 
    mutate(HasCode = if_else(Code != "", 1, 0))

# this code extracts the <p> tags from the Body column and replaces
# the column with the contents inside the <p> tags. This removes the
# links and code, and keeps only the text
questions$Body <- lapply(questions$Body, function(x){
    body <- read_html(x) %>% 
        html_nodes("p") %>% 
        html_text()
    return (paste(body, collapse=","))
})
# same as above
answers$Body <- lapply(answers$Body, function(x){
    body <- read_html(x) %>% 
        html_nodes("p") %>% 
        html_text()
    return (paste(body, collapse=","))
})



#############################################











############ Preprocess Comments ############

# 1. filter out the comments that are not present in the questions and the answers dataframe
# 2. keep only the relevant columns in comments
# 3. remove the time part of the CreationDate column

# select all the post ids and filter the comments dataframe to contain 
# entries that are also in the questions and answers dataframes
questions_and_answers <- c(pull(questions, Id), pull(answers, Id))
comments <- comments %>% 
    filter(PostId %in% questions_and_answers)

comments_cols <- c("Id",
                   "PostId",
                   "Score",
                   "Text",
                   "CreationDate",
                   "UserId")

# keep only selected columns
comments <- comments %>% 
    select(comments_cols)


# remove the time part of the CreationDate column as time is not meaningful
comments <- comments %>% 
    mutate(CreationDate = as.Date(CreationDate, tryformats=c("Y%m%d")))

#############################################



