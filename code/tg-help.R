

librarian::shelf(dplyr, ggplot2, ggraph, 
                 rtweet, academictwitteR, quanteda, jsonlite,
                 quanteda.textplots, quanteda.textstats)


#### Organizing the data #### 
channels <- scan("lists/telegram-pro-ua.txt", what='character', sep="\n", skipNul = TRUE)
channels <- paste(channels,".json", sep = "")

json_file <- paste("MediaChannels/", channels[36], sep = "")
file <- fromJSON(paste(readLines(json_file), collapse=""), flatten=TRUE)
file2 <- file[, c("peer_id.channel_id", "id","date", "message","views", "forwards", "fwd_from.from_id.channel_id", 
                  "media._", "media.webpage.url", "media.webpage.type", "media.webpage.title","media.webpage.description")]
file2 <- file2[startsWith(file2$date, '2021'),]
file2$channel_name <- channels[36] 

#data <- file2
for (channel in channels[17:length(channels)]){
  json_file <- paste("MediaChannels/", channel, sep = "")
  file <- fromJSON(paste(readLines(json_file), collapse=""), flatten=TRUE)
  file2 <- file[, c("peer_id.channel_id", "id","date", "message","views", "forwards", "fwd_from.from_id.channel_id", 
                    "media._", "media.webpage.url", "media.webpage.type", "media.webpage.title","media.webpage.description")]
  file2 <- file2[startsWith(file2$date, '2022'),]
  file2$channel_name <- channel 
  data <- rbind(data, file2)
}


#write.csv(data, "datasets/media-tg-pro-ua-2021.csv")



#### Languages ####
text <- unlist(tg_media_ru$message, use.names = FALSE)
text <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", text) %>% as.list()
lang <- lapply(text, detect_language_mixed)

langs <- c()
for (i in 1:nrow(tg_media_ru)){
  new <- lang[[i]][["classification"]][["language"]][1]
  langs <- c(langs, new)
}

tg_media_ua$lang <- langs
tg_media_ru$lang <- langs

write.csv(all, "datasets/media-tg-pro-ru-small-lang-2021.csv")