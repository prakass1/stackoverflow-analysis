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


#convert to dataframe
tags.df <- ReadXMLToDf("data/Tags.xml", "//tags/row")
#export as csv
write.csv(tags.df, "data//Tags.csv")

#convert to dataframe
comments.df <- ReadXMLToDf("data/Comments.xml", "//comments/row")
#export as csv
write.csv(comments.df, "data//Comments.csv")

#convert to dataframe
users.df <- ReadXMLToDf("data/Users.xml", "//users/row")
#export as csv
write.csv(users.df, "data//Users.csv")

#convert to dataframe
posts.df <- ReadXMLToDf("data/Posts_500k.xml", "//posts/row")

#separate questions and answers into different dataframes
questions.df <- filter(posts.df, PostTypeId == 1)
answers.df <- filter(posts.df, PostTypeId == 2)

write.csv(questions.df, "data//Questions.csv")
write.csv(answers.df, "data//Answers.csv")