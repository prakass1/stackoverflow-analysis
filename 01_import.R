#01_import.R

library(readr)
library(dplyr)

posts_names <- c("Id",
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
  "ParentId",
  "AcceptedAnswerId",
  "LastEditorUserId",
  "LastEditDate",
  "ClosedDate",
  "FavoriteCount",
  "OwnerDisplayName",
  "LastEditorDisplayName",
  "CommunityOwnedDate"
)

comments_names <- c("Id",
                    "PostId",
                    "Score",
                    "Text",
                    "CreationDate",
                    "UserId",
                    "UserDisplayName")


start_time <- Sys.time()
posts <- read_csv(paste0(csv_loc, "posts.csv"))
comments <- read_csv(paste0(csv_loc, "comments.csv"))
end_time <- Sys.time()
time_taken <- difftime(end_time, start_time, units='mins')
print(paste0("Time Taken for reading dataframe is ", time_taken))


#select all the post ids
post_ids <- posts$Id

#filter comments dataframe to contain rows that are also in posts dataframe
comments <- filter(comments, PostId %in% post_ids)

#separate questions and answers into different dataframes
questions <- filter(posts, PostTypeId == 1)
answers <- filter(posts, PostTypeId == 2)
