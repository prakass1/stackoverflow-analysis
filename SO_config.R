# SO_config.R
# StackOverflow Configurations

library("dplyr")
library("XML")
library("tm")
library("textmineR")
library("gsubfn")
library("SnowballC")
library("textstem")
library("tidytext")
library("tm")
library("ggmap")
library("tidytext")
library("sentimentr")
library("wordcloud")
library("gender")
library("corrplot")
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