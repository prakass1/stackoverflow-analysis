#eda_tags.R



# clean users AboutMe
users["TidyBody"] <- users$AboutMe %>% 
    RemoveStopwords() %>% 
    CleanText()


# create a column that stores the sentiment score for a user in their AboutMe
users <- users %>%
    rowwise() %>% 
    mutate(Sentiment = GetSentiment(AboutMe))


# overall sentiment of users AboutMe
ggplot(users, aes(x = Sentiment)) +
    geom_density(fill = "steelblue") +
    scale_x_log10() +
    scale_alpha_discrete(range = c(0,1))
    




# create a list of words with their frequencies
words <- users %>% 
    select("TidyBody") %>% 
    unnest_tokens(word, TidyBody) %>% 
    group_by(word) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count))

words <- Corpus(VectorSource(users$TidyBody))
dtm <- TermDocumentMatrix(words)
m <- as.matrix(dtm)

# remove some terms
words <- words %>% 
    filter( !(word %in% c("li", "http", "https", "s", "com", "www", "h")))

# create a wordcloud by passing in each word with its frequency to wordcloud()
pal <- brewer.pal(8,"Dark2")
wordcloud(words = words$word , freq = words$count,
          min.freq = 1, max.words=150,
          scale=c(3, 0.7), colors = pal)




# predict gender from name
genders <- users$DisplayName %>% 
    gender()

# plot gender distribution
ggplot(genders, aes(x = factor(1), fill = factor(gender))) +
    geom_bar(width = 1) +
    coord_polar(theta = "y") +
    labs(x = "", y = "", title = "Gender Distribution") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    scale_fill_manual(values=c("green", "yellow")) 





#################################### Locations #################################### 


# get a sorted dataframe of user locations
locations <- users %>%
    filter(Location != "") %>% 
    count(Location, sort = TRUE) %>%
    ungroup()



#locations_top100 <- locations[1:5, ]
#c <- apply(locations_top100, 1, GetCoordinates)
#locations_top100['Coordinates'] <- apply(locations_top100, 1, GetCoordinates)

locations1 <- locations[1:2500,]
locations2 <- locations[2501:4130,]

coordinates1 <- geocode(locations1$Location, source="dsk")
coordinates2 <- geocode(locations2$Location, source="dsk")

# plotting locations on the map
map <- df %>%  
    leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    # add a cluster option to the markers
    addCircleMarkers(
        clusterOptions = markerClusterOptions()
    )
map  # Print the map


lat <- rep(df$lat, times=)



# tags by location
q <- questions_one_tag %>% 
    select(OwnerUserId, Tags)

q["Location"] <- GetLocation(q$OwnerUserId)
