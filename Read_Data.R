require(rvest)
require(magrittr)
doc <- read_xml("F:/Modules_Courses/Semester 3/DataScience with R/datasets/Data/Post1.xml")


# '''
# [1] "Id"                    "PostTypeId"            "CreationDate"          "Score"                 "ViewCount"            
#  [6] "Body"                  "OwnerUserId"           "LastActivityDate"      "Title"                 "Tags"                 
# [11] "AnswerCount"           "CommentCount"          "ParentId"              "AcceptedAnswerId"      "LastEditorUserId"     
# [16] "LastEditDate"          "ClosedDate"            "FavoriteCount"         "OwnerDisplayName"      "LastEditorDisplayName"
# [21] "CommunityOwnedDate" 
# '''

rows <- doc %>% xml_nodes("row")
POSTS<-data.frame(
  Id = rows %>% xml_attr("Id"),
  PostTypeId = rows %>% xml_attr("PostTypeId"),
  CreationDate = rows %>% xml_attr("CreationDate"),
  Score = rows %>% xml_attr("Score"),
  ViewCount = rows %>% xml_attr("ViewCount"),
  Body = rows %>% xml_attr("Body"),
  OwnerUserId = rows %>% xml_attr("OwnerUserId"),
  LastActivityDate = rows %>% xml_attr("LastActivityDate"),
  Title = rows %>% xml_attr("Title"),
  Tags = rows %>% xml_attr("Tags"),
  AnswerCount = rows %>% xml_attr("AnswerCount"),
  CommentCount = rows %>% xml_attr("CommentCount"),
  AcceptedAnswerId = rows %>% xml_attr("AcceptedAnswerId"),
  LastEditorUserId = rows %>% xml_attr("LastEditorUserId"),
  LastEditDate = rows %>% xml_attr("LastEditDate"),
  ClosedDate = rows %>% xml_attr("ClosedDate"),
  FavoriteCount = rows %>% xml_attr("FavoriteCount"),
  OwnerDisplayName = rows %>% xml_attr("OwnerDisplayName")
)
