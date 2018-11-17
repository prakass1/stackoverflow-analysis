library("stringr")

#select all the post ids
post_ids <- posts$Id
#filter comments dataframe to contain rows that are also in posts dataframe
comments <- filter(comments, PostId %in% post_ids)

#separate questions and answers into different dataframes
questions <- filter(posts, PostTypeId == 1)
answers <- filter(posts, PostTypeId == 2)

#remove html tags from the body column
questions <- RemoveHTML(questions)
answers <- RemoveHTML(answers)