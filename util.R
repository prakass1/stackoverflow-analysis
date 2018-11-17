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

WriteToCsv <- function(df, loc){
  write_csv(df, loc)
}


RemoveHTML <- function(df){
    #non-greedy search and replace for HTML tags
    mutate(df, Body = str_replace_all(Body,"<(.*?)>", ""))
}