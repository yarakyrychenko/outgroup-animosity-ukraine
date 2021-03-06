
# Outgroup Animosity: News Media Analysis 

#### Set Up ####
librarian::shelf(dplyr, ggraph, 
                 dotwhisker, jtools, plyr, stringi,
                 tidygraph, tidytext, RColorBrewer, rvest,
                 quanteda, tm, broom, stringr, meta, cowplot,
                 Hmisc,ggsignif,
                 cld2, quiet=TRUE)

source("code/helper.R")
source("code/load_dicts.R")
options(scipen = 999)

# pro-ru pro-ua lists
media_list_csv <- read.csv("lists/UkraineMediaList3.csv")

media_list_tw <- media_list_csv %>% filter(!is.na(Twitter.handle), Twitter.handle !="")
ua_media_tw <- media_list_tw %>% filter(is.na(Pro.Ukr.Pro.Rus) | Pro.Ukr.Pro.Rus == "")
ru_media_tw <- media_list_tw %>% filter(Pro.Ukr.Pro.Rus == "pro-ru")

media_list_fb <- media_list_csv %>% filter(!is.na(Facebook.Id), Facebook.Id !="") 
ua_media_fb <- media_list_fb %>% filter(is.na(Pro.Ukr.Pro.Rus) | Pro.Ukr.Pro.Rus == "")
ru_media_fb <- media_list_fb %>% filter(Pro.Ukr.Pro.Rus == "pro-ru")

# make quanteda dicts
ua_dict <- make_ua_dict()
ru_dict <- make_ru_dict()

#### Twitter  Media Quanteda   ####
tw_media <- read.csv("datasets/media-tw-lang-2022-06.csv")
tw_media$ProUA <- tolower(tw_media$screen_name) %in% tolower(ua_media_tw$Twitter.handle)
tw_media$ProUA <- tolower(tw_media$screen_name) %in% tolower(ru_media_tw$Twitter.handle)
rm(ua_media_tw,ru_media_tw)

tw_media_ua <- tw_media[tw_media$lang == 'uk',] %>% prep_tw_data_q(ua_dict)
tw_media_ru <- tw_media[tw_media$lang == 'ru',] %>% prep_tw_data_q(ru_dict)
tw_media <- rbind(tw_media_ua,tw_media_ru)
rm(tw_media_ua,tw_media_ru)

tw_media_proua <- tw_media %>% filter(ProUA == TRUE)
tw_media_proru <- tw_media %>% filter(ProRU == TRUE)

tw_sep_by_groups <- filter_groups_by_id(tw_media_proua,tw_media_proru)

tw_media_proua_ua <- add_all(data.frame(tw_sep_by_groups[1][1]))
tw_media_proua_ru <- add_all(data.frame(tw_sep_by_groups[2][1]))
tw_media_proru_ua <- add_all(data.frame(tw_sep_by_groups[3][1]))
tw_media_proru_ru <- add_all(data.frame(tw_sep_by_groups[4][1]))
rm(tw_sep_by_groups)

tw_media_proua <- rbind(tw_media_proua_ua, tw_media_proua_ru)
tw_media_proru <- rbind(tw_media_proru_ua, tw_media_proru_ru)
rm(tw_media_proua_ua, tw_media_proua_ru,tw_media_proru_ua, tw_media_proru_ru)

#### Twitter   ####
tw_id_predict <- list("ingroup", "outgroup", #"disputed",
                      "positive", "negative", "moral_emotional",
                      "has_media", "has_URL", "is_retweet", "total_tokens")
tw_id_predict_fwd <- "engagement_log" 

tw_r_proua_fwd <- run_regression(tw_media_proua, tw_id_predict_fwd, tw_id_predict)
tw_r_proru_fwd <- run_regression(tw_media_proru, tw_id_predict_fwd, tw_id_predict)

tw_mu <- clean_mod(tw_r_proua_fwd,"Twitter")
tw_mr <- clean_mod(tw_r_proru_fwd,"Twitter")
rm(tw_r_proua_fwd,tw_r_proru_fwd)

#### Facebook Media Quanteda   ####
fb_media <- read.csv("datasets/media-fb-lang-2022-06.csv")

fb_media$Facebook.Id.X <- paste("x",fb_media$Facebook.Id, sep="")
fb_media$ProUA <- fb_media$Facebook.Id.X %in% ua_media_fb$Facebook.Id
fb_media$ProRU <- fb_media$Facebook.Id.X %in% ru_media_fb$Facebook.Id
rm(ua_media_fb,ru_media_fb)

fb_media_ua <- fb_media[fb_media$lang == 'uk',] %>%  prep_fb_data_q(ua_dict)
fb_media_ru <- fb_media[fb_media$lang == 'ru',] %>% prep_fb_data_q(ru_dict)
fb_media <- rbind(fb_media_ua,fb_media_ru)
rm(fb_media_ua,fb_media_ru)

fb_media_proua <- fb_media %>% filter(ProUA == TRUE)
fb_media_proru <- fb_media %>% filter(ProRU == TRUE)

fb_sep_by_groups <- filter_groups_by_id(fb_media_proua,fb_media_proru)
fb_media_proua_ua <- add_all(data.frame(fb_sep_by_groups[1][1]))
fb_media_proua_ru <- add_all(data.frame(fb_sep_by_groups[2][1]))
fb_media_proru_ua <- add_all(data.frame(fb_sep_by_groups[3][1]))
fb_media_proru_ru <- add_all(data.frame(fb_sep_by_groups[4][1]))
rm(fb_sep_by_groups)

same <-  names(fb_media_proua_ua) == names(fb_media_proua_ru) 
fb_media_proua <- rbind(fb_media_proua_ua[same], fb_media_proua_ru[same])
fb_media_proru <- rbind(fb_media_proru_ru[same], fb_media_proru_ua[same])
rm(fb_media_proua_ua, fb_media_proua_ru,fb_media_proru_ua, fb_media_proru_ru)

#### Facebook ####
fb_id_predict <- list("ingroup", "outgroup", 
                   "positive", "negative", "moral_emotional", "total_tokens",
                   "has_media", "has_URL", "Likes.at.Posting", "Followers.at.Posting")
fb_id_predict_fwd <- "engagement_log"

fb_id_predict <- unlist(fb_id_predict)
cols <- c(fb_id_predict,fb_id_predict_fwd)
rm(fb_sep_by_groups) <- fb_media_proua[,cols]
fb_media_proru <- fb_media_proru[,cols]

fb_r_proua_fwd <- run_regression(fb_media_proua, fb_id_predict_fwd, fb_id_predict)
rm(fb_media_proua)
fb_r_proru_fwd <- run_regression(fb_media_proru, fb_id_predict_fwd, fb_id_predict)
rm(fb_media_proru)

fb_mu <- clean_mod(fb_r_proua_fwd,"Facebook")
fb_mr <- clean_mod(fb_r_proru_fwd,"Facebook")
rm(fb_r_proua_fwd,fb_r_proru_fwd)


#### Telegram Media Quanteda   ####
tg_media <- read.csv("datasets/media-tg-lang-2022-06.csv")

tg_media_ua <- tg_media[tg_media$lang == 'uk',]
tg_media_ru <- tg_media[tg_media$lang == 'ru',]
tg_media_ua <- prep_tg_data_q(tg_media_ua,ua_dict)
tg_media_ru <- prep_tg_data_q(tg_media_ru,ru_dict)

tg_media <- rbind(tg_media_ua,tg_media_ru)
tg_media_proua <- tg_media %>% filter(ProUA == TRUE)
tg_media_proru <- tg_media %>% filter(ProUA == FALSE)

tg_sep_by_groups <- filter_groups_by_id(tg_media_proua,tg_media_proru)

tg_media_proua_ua <- add_all(data.frame(tg_sep_by_groups[1][1]))
tg_media_proua_ru <- add_all(data.frame(tg_sep_by_groups[2][1]))
tg_media_proru_ua <- add_all(data.frame(tg_sep_by_groups[3][1]))
tg_media_proru_ru <- add_all(data.frame(tg_sep_by_groups[4][1]))

#### Telegram ####
tg_id_predict <- list("ingroup", "outgroup", #"disputed", 
                      "positive", "negative", "moral_emotional",
                      "has_media", "is_fwd", "total_tokens")

same <-  names(tg_media_proua_ua) == names(tg_media_proua_ru) 
tg_media_proua_b <- rbind(tg_media_proua_ua[same], tg_media_proua_ru[same])
tg_media_proru_b <- rbind(tg_media_proru_ru[same], tg_media_proru_ua[same])
tg_media_all <-rbind(tg_media_proua_b,tg_media_proru_b)

tg_id_predict_fwd <- "engagement_log" 

tg_r_proua_fwd <- run_regression(tg_media_proua_b, tg_id_predict_fwd, tg_id_predict)
tg_r_proru_fwd <- run_regression(tg_media_proru_b, tg_id_predict_fwd, tg_id_predict)

tg_mu <- clean_mod(tg_r_proua_fwd,"Telegram")
tg_mr <- clean_mod(tg_r_proru_fwd,"Telegram")

#### All Media Reactions Plots ####
tw_reactions <- list("retweet_count_log","favorite_count_log")
tw_labels <- list("retweet", "favorite")
fb_reactions <- list("shares_log", "comments_log", "likes_log", 
                     "love_log", "haha_log", "wow_log", "sad_log", "angry_log")
fb_labels <- list("share", "comment", "like", "love","haha", "wow","sad", "angry")
tg_reactions <- list("views_log", "forwards_log")
tg_labels <- list("views", "forwards")

tw_r_all <- all_reaction_regressions(tw_media_all,tw_reactions,tw_id_predict,tw_labels)
fb_r_all<- all_reaction_regressions(fb_media_all,fb_reactions,fb_id_predict,fb_labels)
tg_r_all <- all_reaction_regressions(tg_media_all,tg_reactions,tg_id_predict,tg_labels)

r_all <- rbind(tw_r_all, fb_r_all, tg_r_all)

rpall <- reactions_plot(r_all, "All News Media")
ggsave("plots/news_media_reactions.png", plot=rpall, width = 12, height = 9)

#### Combined Engagement Plots no language ####
both_proua <- rbind(tw_mu, fb_mu, tg_mu)
both_proru <- rbind(tw_mr, fb_mr, tg_mr)

p11 <- identity_affect_plot_nolang(both_proua, "Pro-Ukrainian News Media (Engagement)") + theme(legend.position = "none")
p22 <- identity_affect_plot_nolang(both_proru, "Pro-Russian News Media (Engagement)")  + theme(legend.position = "none")

# this is all just to get the plot's legend; the actual plot is discarded 
plot1 <- dwplot(both_proua_ua, confint = .95, dot_args = list(size = 2), whisker_args = list(size = 1.5)) %>%
  relabel_predictors(c(
    ingroup  = "Ingroup",
    outgroup = "Outgroup",
    negative = "Negative",
    positive = "Positive",
    moral_emotional = "Moral Emotional"
  )) 
plot <- plot1 + theme_bw() + xlab("Odds Ratio") + ylab("") +
  ggtitle("whatever") +
  geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
  theme_apa(legend.font.size = 12,
            legend.pos = "bottom") + 
  scale_colour_manual(values=c("Facebook"="dodgerblue4", "Twitter"="deepskyblue", "Telegram"="turquoise"), 
                      labels=c("Facebook", "Twitter", "Telegram"))
leg <- cowplot::get_legend(plot)


both_no_lang <- ggdraw(plot_grid(p11,p22,
                                   nrow = 2, ncol = 2, 
                                   labels = c("A", "B"),
                                   rel_heights = c(1, .2)) + draw_grob(leg, vjust = 0.4))


ggsave("plots/news_media_engagement_no_lang_plots.png", plot=both_no_lang, width = 17, height = 6)


#### Sliding Time Window Analysis ####


fb_media_proua_b$date_num <- as.numeric(as.Date(fb_media_proua_b$Post.Created.Date, format="%Y-%m-%d"))
fb_start_num <- min(fb_media_proua_b$date_num)
fb_start_date <- as.Date(fb_media_proua_b[fb_media_proua_b$date_num == fb_start_num,]$Post.Created.Date[1])
fb_chunks_ua <- run_smoothing_regressions(fb_media_proua_b, fb_id_predict_fwd, fb_id_predict, 
                                          start_num=fb_start_num, smoothing=21)

fb_media_proru_b$date_num <- as.numeric(as.Date(fb_media_proru_b$Post.Created.Date, format="%Y-%m-%d"))
fb_chunks_ru <- run_smoothing_regressions(fb_media_proru_b, fb_id_predict_fwd, fb_id_predict, 
                                          start_num=fb_start_num, smoothing=21)

tw_start_num <- 18000
tw_start_date <- as.Date(tw_media_proua[tw_media_proua$date_num == tw_start_num,]$created_at[1])
tw_media_proua$date <- as.POSIXct(tw_media_proua$created_at, format="%Y-%m-%d")
tw_media_proua$date_num <- as.numeric(as.Date(tw_media_proua$date, format="%Y-%m-%d"))
tw_chunks_ua <- run_smoothing_regressions(tw_media_proua, tw_id_predict_fwd, tw_id_predict, 
                                          start_num=tw_start_num)


tw_start_num_ru <- 18700
tw_start_date_ru <- as.Date(tw_media_proru[tw_media_proru$date_num == tw_start_num_ru,]$created_at[1])
tw_media_proru$date <- as.POSIXct(tw_media_proru$created_at, format="%Y-%m-%d")
tw_media_proru$date_num <- as.numeric(as.Date(tw_media_proru$date, format="%Y-%m-%d"))
tw_chunks_ru <- run_smoothing_regressions(tw_media_proru, tw_id_predict_fwd, tw_id_predict, 
                                          start_num=tw_start_num_ru, smoothing=28)


tw_ua_ts_plot <- make_two_timeseries_plot(tw_chunks_ua,start_num=tw_start_num,first_date=tw_start_date)
tw_ru_ts_plot <- make_two_timeseries_plot(tw_chunks_ru,start_num=tw_start_num_ru,first_date=tw_start_date_ru)
fb_ua_ts_plot <- make_two_timeseries_plot(fb_chunks_ua,start_num=fb_start_num,first_date=fb_start_date)
fb_ru_ts_plot <- make_two_timeseries_plot(fb_chunks_ru,start_num=fb_start_num,first_date=fb_start_date)

#ggsave("news_media_two_plots.png", plot=r_plots, width = 16, height = 9)


