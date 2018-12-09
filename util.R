# util.R
# contains utility functions used by other files


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



# input: a date in character format or date format
# if the date is before 2018, returns false, else returns true
JoinedRecently <- function(date){
    if_else(date >= as.Date("2018-01-01"), 1, 0)
}




# input: @text: a string
# returns the sentiment of the text
GetSentiment <- function(text){
    # divides the text into separate words
    tokens <- data_frame(text = text) %>% 
        unnest_tokens(word, text)
    
    # gets sentiment score of words and sums the scores to get overall sentiment
    sentiment <- tokens %>%
        inner_join(get_sentiments("afinn")) %>% 
        summarise(sentiment = sum(score)) %>% 
        pull(sentiment)
    
    return (sentiment)
}






# input: @comments dataframe, @post_id: id of a post 
# returns the average sentiment of the comments of the post
SentimentOfComments <- function(comments, post_id){
    all_comments <- filter(comments, PostId == post_id)
    avg_sentiment <- all_comments$Sentiment %>% 
        mean()
    print(paste('Avg sentiment of post ', post_id, ': ', avg_sentiment))
    return (avg_sentiment)
}



# input: @text: a character vector
# removes numbers and punctuation, and then stems and lemmatizes the text
CleanText <- function(text){
    text <- gsub("([0-9]+|[[:punct:]]+)", " ", text) %>% 
        wordStem(language = "english") %>% 
        lemmatize_strings()
    text <- gsub("\\s+", " ", text)
    return (text)
}

# input: @x: a character vector
# returns the text with stopwords removed
RemoveStopwords <- function(x){
    try_corpus <- Corpus(VectorSource(x)) %>% 
        tm_map(content_transformer(tolower)) %>% 
        tm_map(removeWords, stopwords("en"))
    return(try_corpus$content)
}




# input: @df: a dataframe
# returns a Document Term Matrix (to return a Term Document Matrix, change the function name to TermDocumentMatrix)
DocumentTermMatrix <- function(df){
    #corpus <- Corpus (VectorSource(df$TidyBody)) 
    corpus <- Corpus (VectorSource(df)) 
    tdmcorpus <- DocumentTermMatrix(corpus)
    return(tdmcorpus)
}




#######################################




GetLocation <- function(id){
    location <- users %>% 
        filter(Id == id) %>% 
        select(Location)
    return (location)
}


########## Install packages if not present else load them ##########
# ipak function: install and load multiple R packages.
# source: https://gist.github.com/stevenworthington/3178163
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)){
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}