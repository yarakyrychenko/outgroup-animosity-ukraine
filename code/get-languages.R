
librarian::shelf(dplyr, qdapRegex, cld2)

get_language <- function(text){
  text <- rm_url(text)
  text <- rm_twitter_url(text) %>% as.list()
  lang <- lapply(text, detect_language_mixed)
  
  langs <- c()
  for (i in 1:length(text)){
    new <- lang[[i]][["classification"]][["language"]][1]
    new <- tolower(substr(new,1,2))
    langs <- c(langs, new)
  }
  
  return(langs)
}

#### Twitter Languages ####
tw_media <- read.csv("datasets/media-tw-unique-2022-06-18.csv")
tw_media$lang_twitter <- tw_media$lang
langs <- get_language(tw_media$text)

tw_media$lang <- langs 
write.csv(tw_media, "datasets/media-tw-lang-2022-06.csv")

#### Facebook Languages ####
fb_media <- read.csv("datasets/media-fb-unique-2022-06-16.csv")
langs <- get_language(fb_media$Message)

fb_media$lang <- langs
write.csv(fb_media, "datasets/media-fb-lang-2022-06.csv")

#### Telegram Languages ####
tg_media1 <- read.csv("datasets/media-tg-pro-ua.csv")
tg_media1$ProUA <- TRUE
tg_media2 <- read.csv("datasets/media-tg-pro-ru-small.csv")
tg_media2$ProUA <- FALSE
tg_media <- rbind(tg_media1,tg_media2)
langs <- get_language(tg_media$message)

tg_media$lang <- langs
write.csv(tg_media, "datasets/media-tg-lang-2022-06.csv")
