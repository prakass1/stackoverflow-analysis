#01_import.R
Posts_names <- c(#"Index",
                 "Id",
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

Comments_names <- c("Id",
              "PostId",
              "Score",
              "Text",
              "CreationDate",
              "UserId",
              "UserDisplayName")

start_time <- Sys.time()
### Read CSV ###############
posts <- read.csv( paste0(csv_loc, "posts.csv"), 
                   header = T, 
                   col.names = Posts_names,
                   sep = ",",
                   #colClasses=c("variableName"="character"), 
                   stringsAsFactors = F)

comments <- read.csv( paste0(csv_loc, "comments.csv"), 
                   header = T,
                   sep = ",", 
                   col.names = Comments_names,
                   stringsAsFactors = F)

posts %>% head(1)
comments %>% head(1)
end_time <- Sys.time()

time_taken <- difftime(end_time, start_time, units='mins')

print(paste0("Time Taken for creating dataframe is ", time_taken))


#select all the post ids
post_ids <- as.numeric(paste(posts$Id))

#filter comments dataframe to contain rows that are also in posts dataframe
comments <- filter(comments, PostId %in% post_ids)

#separate questions and answers into different dataframes
questions_df <- filter(posts, PostTypeId == 1)
answers_df <- filter(posts, PostTypeId == 2)