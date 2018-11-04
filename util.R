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

write2csv <- function(df,loc){
  write.csv(df,loc)
}
