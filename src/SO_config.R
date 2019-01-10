# SO_config.R
# StackOverflow Configurations

library("tidyr")
library("readr")
library("textclean")
library("dplyr")
library("XML")
library("tm")
library("textmineR")
library("gsubfn")
library("SnowballC")
library("textstem")
library("NLP") 
library("openNLP")
library("text2vec")
library("tidytext")
library("tm")
library("glmnet")
library("ggmap")
library("sentimentr")
library("wordcloud")
library("gender")
library("stringr")
library("gsubfn")
library("ggplot2")
library("stringr")
library("lubridate")
library("rvest")
library("corrplot")
library("magrittr")
library("shiny")
library("leaflet")
library("widyr")
library("ggraph")
library("igraph")
library("udpipe")
library("h2o")
library("RTextTools")
library("rvest")
library("probsvm")
library("klaR")
library("circlize")
library("quanteda")
library("caret")
library("LDAvis")


xml_loc <- "data/"
csv_loc <- "data/"

### For Other OS ############
# xml_loc <- "/data/"
# csv_loc <- "/data/"


#### Used extension #####
ext <- ".csv"

# This will help with automatically processing only those files not processed
files <- c("posts",
           "comments",
           "tags",
           "users")