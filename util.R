# util.R
# contains utility functions used by other files

library("dplyr")
library("XML")

# input: @filepath: the path with extension of the xml to read, @query: xpath query to select nodes
# returns a dataframw with the parsed xml contents
ReadXMLToDf <- function(filepath, query){
    #load the xml doc
    doc <- xmlParse(filepath)
    #return the top node of the xml file
    rootnode <- xmlRoot(doc)
    #return the set of xml nodes
    nodeset <- getNodeSet(rootnode, query)
    return(XML:::xmlAttrsToDataFrame(nodeset))
}


# writes the dataframe to a .csv file
# input: @df: a dataframe, @loc: filepath to store the csv
WriteToCsv <- function(df, loc){
    write_csv(df, loc)
}




#15 selected tags
selected_tags <- c("android",
                   "c",
                   "c#",
                   "c++",
                   "html",
                   "css",
                   "ios",
                   "java",
                   "javascript",
                   "jquery",
                   "mysql",
                   "php",
                   "python",
                   "r",
                   "sql")

# input: @x: character vector of tags
# returns the tags if at least one tag is in the top 15 selected tags, else returns an empty string
filterTags <- function(x){
    for(tag in selected_tags){
        testExpr <- paste0("<",tag,">")
        if(grepl(testExpr, x, fixed = TRUE)){
            return(x)
        }
    }
    return("")
}

# intput: @x: character vector of tags
# checks if the tags are one of the top 15 selected tags
# returns the tag when a top 15 tag is found
filterOneTag <- function(x){
    for(tag in selected_tags){
        testExpr <- paste0("<",tag,">")
        if(grepl(testExpr, x, fixed = TRUE)){
            return(tag)
        }
        
    }
    return("")
}






# input: @x: a character vector.
# extracts all links using rvest functions
# returns the links as one character vector separated by commas
GetLinks <- function(x){
    # if a link is found
    if (grepl("https://", x, fixed = TRUE) == TRUE){
        #extract contents of href
        link <- read_html(x) %>% 
            html_nodes("a") %>% 
            html_attr("href")
    }
    else{
        link = ""
    }
    # return the links as one character vector
    return (paste(link, collapse=","))
}

# input: @x: a character vector.
# extracts all code texts using rvest functions
# returns the code texts as one character vector separated by commas
GetCode <- function(x){
    # if a code tag is found
    if (grepl("<code>", x, fixed = TRUE) == TRUE){
        # extract contents of the code tag
        code <- read_html(x) %>% 
            html_nodes("code") %>% 
            html_text()
    }
    else{
        code = ""
    }
    # return the code texts as one character vector
    return (paste(code, collapse=","))
}


# input: @x: a character vector.
# extracts all contents inside <p> tags using rvest functions
# returns the contents as one character vector separated by periods
GetParagraphContents <- function (x) {
    if (grepl ("<p>", x, fixed = TRUE) == TRUE) {
        x <- read_html (x) %>% 
            html_nodes ("p") %>% 
            html_text ( ) 
        return (paste(x, collapse="."))
    }
    else {
        return (x)
    }
}



# input: @text: a text with one or many words/sentences
# divides the text into sentences and calculates sentiment of each of them
# returns the average of all the sentiments
GetSentiment <- function(text){
    
    sentiments <- text %>% 
        get_sentences() %>% 
        sentiment() 
    
    sentiment <- sentiments$sentiment %>% 
        mean()
    
    return (sentiment)
}



# input: a date in character format or date format
# if the date is before 2018, returns false, else returns true
JoinedRecently <- function(date){
    if_else(date >= as.Date("2018-01-01"), 1, 0)
}




# input: @x: a character vector
# returns the string with stopwords removed
RemoveStopwords <- function(x){
    try_corpus <- Corpus(VectorSource(x)) %>% 
        tm_map(content_transformer(tolower)) %>% 
        tm_map(removeWords, stopwords("en"))
    return(try_corpus$content)
}


# input: @text: a string
# returns the sentiment of the text
GetSentiment <- function(text){
    sentiments <- text %>% 
        get_sentences() %>% 
        sentiment() 
    
    sentiment <- sentiments$sentiment %>% 
        mean()
    
    return(sentiment)
}




# input: @post_id: id of a post 
# returns the average sentiment of the comments of the post
SentimentOfComments <- function(post_id){
    all_comments <- filter(comments_of_posts, PostId == post_id)
    avg_sentiment <- all_comments$Sentiment %>% 
        mean()
    print(paste('Avg sentiment of post ', post_id, ': ', avg_sentiment))
    return (avg_sentiment)
}
