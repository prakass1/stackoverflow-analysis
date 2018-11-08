#01_import.R

library(readr)
library(dplyr)

start_time <- Sys.time()
posts <- read_csv(paste0(csv_loc, "posts.csv"))
comments <- read_csv(paste0(csv_loc, "comments.csv"))

posts %>% head(1)
comments %>% head(1)
end_time <- Sys.time()

time_taken <- difftime(end_time, start_time, units='mins')

print(paste0("Time Taken for reading dataframe is ", time_taken))


#select all the post ids
post_ids <- as.numeric(paste(posts$Id))

#filter comments dataframe to contain rows that are also in posts dataframe
comments <- filter(comments, PostId %in% post_ids)

#separate questions and answers into different dataframes
questions <- filter(posts, PostTypeId == 1)
answers <- filter(posts, PostTypeId == 2)
