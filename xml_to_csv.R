library(dplyr)
library(XML)

ReadXMLToDf <- function(filepath, query){
  #load the xml doc
  doc <- xmlParse(filepath)
  #return the top node of the xml file
  rootnode <- xmlRoot(doc)
  #return the set of xml nodes
  nodeset <- getNodeSet(rootnode, query)
  return(XML:::xmlAttrsToDataFrame(nodeset))
}


#import to dataframe
posts_df <- ReadXMLToDf("data/Posts_500k.xml", "//posts/row")
comments_df <- ReadXMLToDf("data/Comments.xml", "//comments/row")
tags_df <- ReadXMLToDf("data/Tags.xml", "//tags/row")
users_df <- ReadXMLToDf("data/Users.xml", "//users/row")

#select all the post ids
post_ids <- as.numeric(paste(posts_df$Id))

#filter comments dataframe to contain rows that are also in posts dataframe
comments_df <- filter(comments_df, PostId %in% post_ids)

#separate questions and answers into different dataframes
questions_df <- filter(posts_df, PostTypeId == 1)
answers_df <- filter(posts_df, PostTypeId == 2)

#export as csv
write.csv(questions_df, "data//Questions.csv")
write.csv(answers_df, "data//Answers.csv")
write.csv(comments_df, "data//Comments.csv")
write.csv(users_df, "data//Users.csv")
write.csv(tags_df, "data//Tags.csv")