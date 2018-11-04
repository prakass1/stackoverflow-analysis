# 00_main.R
# A wrapper to run all R scripts

xml_loc <- "F://xml_locations//"
csv_loc <- "F://csv_location//"
ext <- ".csv"
files <- c("posts",
           "comments")
           #"tags",
           #"users")
xmls <- c()
ne_csvs <- c()

#Check if csvs exist
count <-0

#source util.R
source("util.R")

for(val in files){
  print(paste0(csv_loc, val, ext))
if(file.exists(paste0(csv_loc, val, ext))){
  count=count+1
  #xmls <- append(xmls,val)
}
else{
  ne_csvs <- append(ne_csvs,val)
}
}

######## csv stuff ##################
if(count!=4){
  #call to csvs
  start_time <- Sys.time()
  for(val in ne_csvs){
     #Assign each parsed vals to dataframe
     assign(paste0(val,"_df"),ReadXMLToDf(paste0(xml_loc,val,".xml"), paste0("//",val,"/row")))
    
  }
}

###### Assign each xml parsed to a dataframe ##############

# for(val in xmls){
#        #Assign each parsed vals to dataframe
#        assign(paste0(val,"_df"),ReadXMLToDf(paste0(xml_loc,val,".xml"), paste0("//",val,"/row")))
# }

end_time <- Sys.time()

time_taken <- difftime(end_time, start_time, units='mins')

print(paste0("Time Taken for creating dataframe is ", time_taken))

#export as csv
print(paste0("Begin writing to csv ....."))
for(val in ne_csvs){
  dfs <- eval(parse(text=paste0(val,"_df")))
  write2csv(dfs,paste0(csv_loc,val,ext))
}
print(paste0("Writing to csv is completed ....."))
#About 5min and 25 seconds

#Save some memory
if(exists('dfs')){
  remove(dfs)
}

if(exists('posts_df')){
  remove(posts_df)
}

if(exists('comments_df')){
  remove(comments_df)
}


#import csv to df
source('01_import.R')