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



