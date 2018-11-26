# 01_import.R
# import files into the workspace

library(tidyverse)

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
                    "UserDisplayName"
                    )

tags_names <- c("Id",
                "TagName",
                "Count",
                "ExcerptPostId",
                "WikiPostId"
                )

users_names <- c("Id",
                 "Reputation",
                 "CreationDate",
                 "DisplayName",
                 "LastAccessDate",
                 "WebsiteUrl",
                 "Location",
                 "AboutMe",
                 "Views",
                 "UpVotes",
                 "DownVotes",
                 "ProfileImageUrl",
                 "AccountId"
)


start_time <- Sys.time()

posts <- read_csv(paste0(csv_loc, "posts.csv"),
                  col_names = TRUE,
                  col_types = list(col_integer(), # Id
                                   col_integer(), # PostTypeId
                                   col_datetime(), # CreationDate
                                   col_integer(), # Score
                                   col_integer(), # ViewCount
                                   col_character(), # Body
                                   col_integer(), # OwnerUserId
                                   col_datetime(), # LastActivityDate
                                   col_character(), # Title
                                   col_character(), # Tags
                                   col_integer(), # AnswerCount
                                   col_integer(), # CommentCount
                                   col_integer(), # ParentId
                                   col_integer(), # AcceptedAnswerId
                                   col_integer(), # LastEditorUserId
                                   col_datetime(), # LastEditDate
                                   col_datetime(), # ClosedDate
                                   col_integer(), # FavoriteCount
                                   col_character(), # OwnerDisplayName
                                   col_character(), # LastEditorDisplayName
                                   col_datetime() # CommunityOwnedDate
                                   )
                  )


comments <- read_csv(paste0(csv_loc, "comments.csv"),
                     col_names = TRUE,
                     col_types = list(col_integer(), # Id
                                      col_integer(), # PostId
                                      col_integer(), # Score
                                      col_character(), # Text
                                      col_datetime(), # CreationDate
                                      col_integer(), # UserId
                                      col_character() # UserDisplayName
                                      )
                    )


tags <- read_csv(paste0(csv_loc, "tags.csv"),
                 col_names = TRUE,
                 col_types = list(col_integer(), # Id
                                  col_character(), # TagName
                                  col_integer(), # Count
                                  col_integer(), # ExcerptPostId
                                  col_integer() # WikiPostId
                                  )
                 )


users <- read_csv(paste0(csv_loc, "users.csv"),
                  col_names = TRUE,
                  col_types = list(col_integer(), # Id
                                   col_integer(), # Reputation
                                   col_datetime(), # CreationDate
                                   col_character(), # DisplayName
                                   col_datetime(), # LastAccessDate
                                   col_character(), # WebsiteUrl
                                   col_character(), # Location
                                   col_character(), # AboutMe
                                   col_integer(), # Views
                                   col_integer(), # UpVotes
                                   col_integer(), # DownVotes
                                   col_character(), # ProfileImageUrl
                                   col_integer() # AccountId
                                   )
                  )


end_time <- Sys.time()
time_taken <- difftime(end_time, start_time, units='mins')
print(paste0("Time Taken for reading dataframes: ", time_taken))
