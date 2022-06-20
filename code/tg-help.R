

librarian::shelf(dplyr,  jsonlite)


# do this for pro-russian and pro-ukrainian separately 
channels <- scan("lists/telegram-pro-ru-small.txt", what='character', sep="\n", skipNul = TRUE)
channels <- paste(channels,".json", sep = "")

json_file <- paste("MediaChannels/", channels[1], sep = "")
file <- fromJSON(paste(readLines(json_file), collapse=""), flatten=TRUE)
file2 <- file[, c("peer_id.channel_id", "id","date", "message","views", "forwards", "fwd_from.from_id.channel_id", 
                  "media._", "media.webpage.url", "media.webpage.type", "media.webpage.title","media.webpage.description")]
#file2 <- file2[startsWith(file2$date, '2021'),]
file2$channel_name <- channels[1] 

data <- file2

for (channel in channels[2:length(channels)]){
  json_file <- paste("MediaChannels/", channel, sep = "")
  file <- fromJSON(paste(readLines(json_file), collapse=""), flatten=TRUE)
  file2 <- file[, c("peer_id.channel_id", "id","date", "message","views", "forwards", "fwd_from.from_id.channel_id", 
                    "media._", "media.webpage.url", "media.webpage.type", "media.webpage.title","media.webpage.description")]
  #file2 <- file2[startsWith(file2$date, '2022'),]
  file2$channel_name <- channel 
  data <- rbind(data, file2)
  print(channel)
}


#write.csv(data, "datasets/media-tg-pro-ru-small.csv")

