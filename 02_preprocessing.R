library("stringr")
library("gsubfn")
#select all the post ids
post_ids <- posts$Id
#filter comments dataframe to contain rows that are also in posts dataframe
comments <- filter(comments, PostId %in% post_ids)

#separate questions and answers into different dataframes
questions <- filter(posts, PostTypeId == 1)
answers <- filter(posts, PostTypeId == 2)

#remove html tags from the body column
questions <- RemoveHTML(questions)
answers <- RemoveHTML(answers)

max(tags$Count)

###Select only those 20 top tags by count########
selected_tags_count <- tbl_df(tags) %>%
                 top_n(25,Count) %>%
                 select(TagName,Count) 

selected_tags_count <- arrange(selected_tags,desc(Count))


selected_tags <- selected_tags_count$TagName
X <- "<python><python3><unix>"

X <- "python,python3,unix"

#Choose from top 23 tags and generalize
filterTags <- function(x){
            for(tag in selected_tags){
            if(grepl(tag,x,fixed = TRUE)){
              return(tag)
            }

            }
  return(x)
}

#### Filter to have only the top 25 tags by questions######
## May be we can give our custom tags too ##########
questions["Tags"] <- apply(questions["Tags"],1,filterTags)
#### Filter to only have tags and remove <> tags
question_filtered <- questions %>%
                    filter(!grepl("<", Tags))