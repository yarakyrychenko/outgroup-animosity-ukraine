#### Set Up ####

#Packages

#install.packages("librarian")
#XQuartz
librarian::shelf(dplyr, ggplot2, ggraph, 
                 rtweet, academictwitteR,
                 dotwhisker, jtools, plyr, 
                 tidygraph, tidytext,
                 stopwords, sentimentr, lubridate, textfeatures,
                 wordcloud, RColorBrewer, rvest,
                 writexl, quanteda)
lib_startup()

#Twitter Permissions
#https://github.com/cran/rtweet
#https://github.com/cjbarrie/academictwitteR
#set_bearer()

create_token(
  app = "Moral Contagion",
  consumer_key = "tumunPWxLudKoN4fB5uQYMDrf",
  consumer_secret = "wn3MyNCxIbXp9AS9NZkCajdh0pcdLbbZez4BaRWGhl5xXIlr2w",
  access_token = "1365653622432952321-A7gnwEeHhk46w85HbnRcq1JeIKwPFo",
  access_secret = "xFm0lJpHoMxB0EQJoFIezBFz1hafiRMCQUv9SLqFDb3hI",
  set_renv = TRUE
)


#### Getting Media Twitter Profiles #### 

media_csv <- read.csv("lists/UkraineMediaList3.csv")
media_list <- media_csv$Twitter.handle
medias <- media_list[!(is.na(media_list) | media_list=="")]

#main 
df <- get_timeline(medias[1], n = 3200, retryonratelimit = TRUE)
for(media in medias[-1]){
  tweets <- get_timeline(media, n = 3200, retryonratelimit = TRUE)
  df <- rbind(df, tweets)
}

saveRDS(df, "Ukraine-Media-TW-2022-06-18.rds")
rtweet::save_as_csv(x=df, file_name="Ukraine-Media-TW-2022-06-18.csv")


#### Getting Political Parties Twitter Profiles ####

#http://w1.c1.rada.gov.ua/pls/site2/p_deputat_list

sluganarodu_pp <- get_timeline("sluganarodu_pp", n = 3200)
oppo_platform <- get_timeline("oppo_platform", n = 3200)
Batkivshchyna <- get_timeline("Batkivshchyna", n = 3200)
Euro_Solidarity <- get_timeline("Euro_Solidarity", n = 3200)
GolosZmin <- get_timeline("GolosZmin", n = 3200)
ForFutureUA <- get_timeline("ForFutureUA", 3200)

parties <- rbind(sluganarodu_pp, oppo_platform, Batkivshchyna,
                 Euro_Solidarity, GolosZmin, ForFutureUA)

saveRDS(parties, "Ukraine-Political-Parties-TW-2022-06-18.rds")
rtweet::save_as_csv(x=parties, file_name="Ukraine-Political-Parties-TW-2022-06-18.csv")



#read in
parties <- readRDS("datasets/ua_parties_twitter_15-08-21.rds")

#visualize
parties %>%
  #dplyr::filter(created_at > "2021-08-15") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("months", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by Ukrainian political parties",
    subtitle = "Twitter status (tweet) counts aggregated by month from Jan 2020 to Aug 2021",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#### Getting Independence Tweets from Hashtags #### 

indep_words = c("україна", "україна30", "незалежності", "незалежність",
                "деньнезалежності", "день незалежності", 
                "парад незалежності", "парад", "30 років", "30років")

eng_ru_words = c("украина", "независимость",
                 "ukraine", "ukraine30", "ukrainianindependence")

pres_words = c("зеленський", "зе", "слуга народу")

ukraine_indep = "україна OR україна30 OR незалежності OR незалежність OR 
                деньнезалежності OR парад OR 30років"

ukraine_indep_tweets_11 <- search_tweets(ukraine_indep, 
                                      n = 17600,
                                      lang="uk",
                                      max_id = max_id_8,
                                      retryonratelimit=TRUE)
                                    

max_id_1 = "1428356320709677056"
max_id_2 = "1429400402269810695"
max_id_3 = "1430959816042852353"
max_id_4 = "1430284375225245701"
max_id_5 = "1430137224444760099"
max_id_6 ="1430131572523651095"
max_id_7 = "1432756322802487296"
max_id_8 = "1431298790414241796"


ua_ind_tweets_5 <- rbind(ukraine_indep_tweets_09, ukraine_indep_tweets_10,ukraine_indep_tweets_11)
saveRDS(ua_ind_tweets_5, "ukraine_indep_TW_31-08-21.rds")

full_ua_in_tw %>%
  ts_plot("1 days") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of tweets about Ukraine",
    subtitle = "Tweet counts aggregated using day intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )



indep_tweets <-
  get_all_tweets(
    query = indep_words,
    start_tweets = "2021-08-01T00:00:00Z",
    end_tweets = "2021-08-12T00:00:00Z",
    file = "indep01-12",
    n=100
  )


#### Getting Media Networks ####

df_users <- users
no_friends <- c()

network <- get_friends(medias[1], retryonratelimit = TRUE)
users <- lookup_users(network$user_id)
df_friends <- cbind(network,users$screen_name)
for(media in medias[90:97]){
  network <- get_friends(media, retryonratelimit = TRUE)
  if (length(network) == 0){
    no_friends <- c(no_friends, media)
  }else{
    users <- lookup_users(network$user_id)
    df_users <- rbind(df_users,users)
    new <- cbind(network,users$screen_name)
    df_friends <- rbind(df_friends, new)
  }
  }


network <- get_friends("sluganarodu_pp")
followers <- get_followers("sluganarodu_pp")
users <- lookup_users(network$user_id)
users$screen_name <- lookup_users(network$screen_name)

rate_limit(get_friends)

rtweet::save_as_csv(more_from_users, "more_from_users.csv")
saveRDS(df_friends, "ua_media_friends_twitter_17-08-21.rds")
write.csv(df_friends, "ua_media_friends_twitter_17-08-21.csv")

#### Load in the Data ####

surnames_ru <- read.table("surname_ru.txt")
politicians <- cbind(politicians,surnames_ru)

fb_parties <- read.csv("datasets/Ukraine-Political-Parties-FB-2020-08-15--2021-08-15.csv")
fb_media <- read.csv("datasets/Ukraine-Media-FB-2020-08-15--2021-08-15.csv")

tw_parties <- readRDS("datasets/Ukraine-Political-Parties-TW-2021-08-15.rds")
tw_media <- readRDS("datasets/Ukraine-Media-TW-2021-08-15.rds")

politicians <- read.csv("lists/mps_9.csv")

#rtweet::save_as_csv(x=parties , file_name="Ukraine-Political-Parties-TW-2021-08-15")
#rtweet::save_as_csv(x=df, file_name="Ukraine-Media-TW-2021-08-15")

#### Get most retweeted tweets/words ####

dataset <- parties
#Look at most popular tweets 
mostPopular <- dataset %>%
  select(text, retweet_count, screen_name) %>%
  arrange(desc(retweet_count)) 

nGrams <- mostPopular %>%
  unnest_tokens(word, text, token = "ngrams", n = 1) 

nGramSort <- nGrams %>%
  group_by(word) %>%
  dplyr::summarize(n = n(),
                   avg_retweets = mean(retweet_count)) %>%
  filter(n > 10) %>%
  arrange(desc(avg_retweets))

#### Analysis ##### 

# Read in dictionaries from dictionaries folder 
MoralEmotional <- scan("dictionaries/Ukraine-Moral-Emotional.txt", what='character', sep="\n", skipNul = TRUE)

#Polarization <- scan("Dictionaries/Polarization.txt", what='character', sep="\n", skipNul = TRUE)
#TopRepublican <- scan("Dictionaries/MostFamousRepublicans.txt", what='character', sep="\t", skipNul = TRUE)
#TopDemocrat <- scan("Dictionaries/MostFamousDemocrats.txt", what='character', sep="\t", skipNul = TRUE)
#DemocratCongress <- scan("Dictionaries/DemocratCongress.txt", what='character', sep="\t", skipNul = TRUE)
#RepublicanCongress <- scan("Dictionaries/RepublicansCongress.txt", what='character', sep="\t", skipNul = TRUE)

#Another way to enter dictionaries 
#liberalidentity = c("socialist*", "communist*", "marxist*", "leftist*", "liberal*", "left-wing*", "progressive*", "social justice warrior", "antifa", "democrat*", "dem", "dems", "libs")
#conservativeidentity = c("conservative*", "gop", "republican*", "libertarian*", "alt-right", "right-wing", "fascist*", "far-right", "far right", "repub", "repubs", "maga")

#Create list of dictionaries
dictionary = dictionary(list(MoralEmotional = MoralEmotional)) #,
                             #Polarization = Polarization, 
                             #Republican = TopRepublican,
                             #Republican = RepublicanCongress,
                             #Republican = conservativeidentity, 
                             #Democrat = TopDemocrat, 
                             #Democrat = DemocratCongress, 
                             #Democrat = liberalidentity))

#quanteda steps 
df_ukr <- df[df$lang == "uk",]
dataset <- ua_ind_tweets
dataset_corpus <- corpus(dataset)
toks <- tokens(dataset_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
dataset_dict <- dfm(toks, dictionary = dictionary)
dataset_dict_df <- quanteda::convert(dataset_dict, to='data.frame')
datasetcombined = cbind(dataset_dict_df, dataset)
datasetcombined$doc_id <- NULL

#### Predicting Retweets ####

datasetcombined$has_media <- is.na(datasetcombined$media_type) == FALSE
datasetcombined$has_URL <- is.na(datasetcombined$urls_url) == FALSE

#Log Transform
datasetcombined$retweet_count_log <- log(datasetcombined$retweet_count + 1)

#Model  
lm <- glm(retweet_count_log ~ MoralEmotional + has_media + has_URL + followers_count + is_retweet, data=datasetcombined)
lmsumm <- summ(lm, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
lmsumm

#### Crowdtangle Data ####

# download crowdtangle data 
dataset <- read_csv("crowdtangleSampleSmall.csv")

#quanteda steps 
dataset_corpus <- corpus(dataset$Message)
toks <- tokens(dataset_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
dataset_dict <- dfm(toks, dictionary = dictionary)
dataset_dict_df <- quanteda::convert(dataset_dict, to='data.frame')
datasetcombined = cbind(dataset_dict_df, dataset)
datasetcombined$doc_id <- NULL

#Add Variables
datasetcombined$has_URL <- ifelse(datasetcombined$Type == "Link", TRUE, FALSE)
datasetcombined$has_media <- ifelse(datasetcombined$Type != "Link" & datasetcombined$Type != "Status", TRUE, FALSE)
#Conservative Media Analysis
datasetcombined$shares_log <- log(datasetcombined$Shares + 1)
datasetcombined$angry_log <- log(datasetcombined$Angry + 1)


#Model  
lm <- glm(shares_log ~ Democrat + Republican + MoralEmotional + Polarization + has_media + has_URL + `Likes at Posting`, data=datasetcombined)
lmsumm <- summ(lm, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
lmsumm

#### Plotting Models #### 

plot <- dwplot(lm,ci=.95, dot_args = list(size = 1.2),
               whisker_args = list(size = 1), exponentiate=TRUE) 
plot 


#### Extra - TweetScores Package and Scape Congress Data ####

#Below is a loop that gets the timelines of every US congressmember: 

install_github("pablobarbera/twitter_ideology/pkg/tweetscores")
library(tweetscores)
congress <- scrapeCongressData(commit = "master")
congresstweets <- get_timeline(congress$twitter[1], n = 3200)

for (i in 1:537) {
  username <- congress$twitter[i]
  print(username)
  party <- congress$party[i]
  print(party)
  timeline <- tryCatch(get_timeline(username, n = 3200),
                       error = function(error_message) {
                         message(error_message)
                         return(NA)
                       })
  timeline$party <- party
  print(timeline$party)
  congresstweets <- rbind.fill(congresstweets, timeline)
}

#helper functions 
get_title <- function(link){
  cat <- rvest::read_html(link)
  films <- cat %>% rvest::html_elements("head")
  title <- films %>% 
    rvest::html_element("title") %>% 
    rvest::html_text2()
  return(title)
}

get_all_links <- function(tweets){
  link_titles <- c()
  for(number in 1:nrow(tweets)){
    thelink <- tweets$entities[[1]][[number]][[4]][[1]] #need to redo; this is supposed to be something that finds the link
    thetitle <- get_title(thelink) 
    link_titles <- c(link_titles, thetitle)
  }
  return(link_titles)  
}



