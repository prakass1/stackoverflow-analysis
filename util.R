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


# non-greedy search and replace for HTML tags
# input: @df: a dataframe
RemoveHTML <- function(df){
    mutate(df, Body = str_replace_all(Body,"<(.*?)>", ""))
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

