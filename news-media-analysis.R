
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
ru_media_tw <- media_list_tw %>% filter(!is.na(Pro.Ukr.Pro.Rus) & Pro.Ukr.Pro.Rus != "")

media_list_fb <- media_list_csv %>% filter(!is.na(Facebook.Id), Facebook.Id !="") #,Facebook.Id !="x1381568875466933")
ua_media_fb <- media_list_fb %>% filter(is.na(Pro.Ukr.Pro.Rus) | Pro.Ukr.Pro.Rus == "")
ru_media_fb <- media_list_fb %>% filter(!is.na(Pro.Ukr.Pro.Rus) & Pro.Ukr.Pro.Rus != "")

# make quanteda dicts
ua_dict <- make_ua_dict()
ru_dict <- make_ru_dict()

#### Twitter  Media Quanteda   ####
tw_media <- read.csv("datasets/media-tw-unique-2022-01-18.csv")
tw_media$ProUA <- tw_media$screen_name %in% ua_media_tw$Twitter.handle

tw_media_ua <- tw_media[tw_media$lang == 'uk',]
tw_media_ru <- tw_media[tw_media$lang == 'ru',]
tw_media_ua <- prep_tw_data_q(tw_media_ua,ua_dict)
tw_media_ru <- prep_tw_data_q(tw_media_ru,ru_dict)

tw_media2 <- rbind(tw_media_ua,tw_media_ru)

tw_media_proua <- tw_media2 %>% filter(ProUA == TRUE)
tw_media_proru <- tw_media2 %>% filter(ProUA == FALSE)

tw_sep_by_groups <- filter_groups_by_id(tw_media_proua,tw_media_proru)

tw_media_proua_ua <- add_all(data.frame(tw_sep_by_groups[1][1]))
tw_media_proua_ru <- add_all(data.frame(tw_sep_by_groups[2][1]))
tw_media_proru_ua <- add_all(data.frame(tw_sep_by_groups[3][1]))
tw_media_proru_ru <- add_all(data.frame(tw_sep_by_groups[4][1]))

tw_media_proua <- rbind(tw_media_proua_ua, tw_media_proua_ru)
tw_media_proru <- rbind(tw_media_proru_ua, tw_media_proru_ru)
same <-  names(tw_media_proua) == names(tw_media_proru) 
tw_media_all <-rbind(tw_media_proua[same,],tw_media_proru[same,])

#### Twitter  Media Engagement ####
tw_id_predict <- list("ingroup", "outgroup", #"disputed",
                      "positive", "negative", "moral_emotional",
                      "has_media", "has_URL", "is_retweet", "total_tokens")
tw_var <- "retweet_count_log"

tw_proua_ua <- run_regression(tw_media_proua_ua, tw_var, tw_id_predict)
tw_proua_ru <- run_regression(tw_media_proua_ru, tw_var, tw_id_predict)
tw_proru_ua <- run_regression(tw_media_proru_ua, tw_var, tw_id_predict)
tw_proru_ru <- run_regression(tw_media_proru_ru, tw_var, tw_id_predict)

tw_muu <- clean_mod(tw_proua_ua,"Twitter")
tw_mur <- clean_mod(tw_proua_ru,"Twitter")
tw_mru <- clean_mod(tw_proru_ua,"Twitter")
tw_mrr <- clean_mod(tw_proru_ru,"Twitter")

#### Twitter  Media Reactions  ####

tw_reactions <- list("retweet_count_log","favorite_count_log")
tw_labels <- list("retweet", "favorite")

tw_r_proua <- all_reaction_regressions(tw_media_proua,tw_reactions,tw_id_predict,tw_labels)
tw_r_proru <- all_reaction_regressions(tw_media_proru,tw_reactions,tw_id_predict,tw_labels)

#### Twitter  share/retweet no language ####
tw_id_predict_fwd <-"engagement_log" #"retweet_count_log"

tw_r_proua_fwd <- run_regression(tw_media_proua, tw_id_predict_fwd, tw_id_predict)
tw_r_proru_fwd <- run_regression(tw_media_proru, tw_id_predict_fwd, tw_id_predict)

tw_mu <- clean_mod(tw_r_proua_fwd,"Twitter")
tw_mr <- clean_mod(tw_r_proru_fwd,"Twitter")

#### Facebook Media Quanteda   ####
fb_media <- read.csv("datasets/media-fb-fullsent-2021-11-25.csv")
fb_media <- fb_media[-c(45:56)]
fb_media$Facebook.Id.X <- paste("x",fb_media$Facebook.Id, sep="")
fb_media$ProRU <- fb_media$Facebook.Id.X %in% ru_media_fb$Facebook.Id

fb_media_ua <- fb_media[fb_media$lang == 'uk',]
fb_media_ru <- fb_media[fb_media$lang == 'ru',]
fb_media_ua <- prep_fb_data_q(fb_media_ua,ua_dict)
fb_media_ru <- prep_fb_data_q(fb_media_ru,ru_dict)

fb_media2 <- rbind(fb_media_ua,fb_media_ru)
fb_media_proua <- fb_media2 %>% filter(ProRU == FALSE)
fb_media_proru <- fb_media2 %>% filter(ProRU == TRUE)

fb_sep_by_groups <- filter_groups_by_id(fb_media_proua,fb_media_proru)
fb_media_proua_ua <- add_all(data.frame(fb_sep_by_groups[1][1]))
fb_media_proua_ru <- add_all(data.frame(fb_sep_by_groups[2][1]))
fb_media_proru_ua <- add_all(data.frame(fb_sep_by_groups[3][1]))
fb_media_proru_ru <- add_all(data.frame(fb_sep_by_groups[4][1]))

#### Facebook Media Engagement ####
fb_id_predict <- list("ingroup", "outgroup", #"disputed", "in_positive", "in_negative", "out_positive", "out_negative",
                   "positive", "negative", "moral_emotional", "total_tokens",
                   "has_media", "has_URL", "Likes.at.Posting", "Followers.at.Posting")
fb_var <- "shares_log"

same <-  names(fb_media_proua_ua) == names(fb_media_proua_ru) 
fb_media_proua_b <- rbind(fb_media_proua_ua[same], fb_media_proua_ru[same])
fb_media_proru_b <- rbind(fb_media_proru_ru[same], fb_media_proru_ua[same])
fb_media_all <-rbind(fb_media_proua_b,fb_media_proru_b)

proua_ua <- run_regression(fb_media_proua_ua, fb_var, fb_id_predict)
proua_ru <- run_regression(fb_media_proua_ru, fb_var, fb_id_predict)
proru_ua <- run_regression(fb_media_proru_ua, fb_var, fb_id_predict)
proru_ru <- run_regression(fb_media_proru_ru, fb_var, fb_id_predict)

fb_muu <- clean_mod(proua_ua,"Facebook")
fb_mur <- clean_mod(proua_ru,"Facebook")
fb_mru <- clean_mod(proru_ua,"Facebook")
fb_mrr <- clean_mod(proru_ru,"Facebook")


#### Facebook Media Reactions  ####
fb_reactions <- list("shares_log", "comments_log", "likes_log", 
                     "love_log", "haha_log", "wow_log", "sad_log", "angry_log")
fb_labels <- list("share", "comment", "like", "love","haha", "wow","sad", "angry")

fb_r_proua <- all_reaction_regressions(fb_media_proua_b,fb_reactions,fb_id_predict,fb_labels)
fb_r_proru <- all_reaction_regressions(fb_media_proru_b,fb_reactions,fb_id_predict,fb_labels)


#### Facebook share/retweet no language ####
fb_id_predict_fwd <- "engagement_log" #"shares_log"

fb_r_proua_fwd <- run_regression(fb_media_proua_b, fb_id_predict_fwd, fb_id_predict)
fb_r_proru_fwd <- run_regression(fb_media_proru_b, fb_id_predict_fwd, fb_id_predict)

fb_mu <- clean_mod(fb_r_proua_fwd,"Facebook")
fb_mr <- clean_mod(fb_r_proru_fwd,"Facebook")

#### Telegram Media Quanteda   ####
tg_media <- read.csv("datasets/media-tg-2021.csv")

tg_media[tg_media$channel_name == "stranaua.json",c("ProUA")] <- FALSE

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

#### Telegram Media Engagement ####
tg_id_predict <- list("ingroup", "disputed", "outgroup", 
                      "positive", "negative", "moral_emotional",
                      "has_media", "is_fwd", "total_tokens")
tg_var <- "forwards_log"

tg_proua_ua <- run_regression(tg_media_proua_ua, tg_var, tg_id_predict)
tg_proua_ru <- run_regression(tg_media_proua_ru, tg_var, tg_id_predict)
#tg_proru_ua <- run_regression(tg_media_proru_ua, tg_var, tg_id_predict)
tg_proru_ru <- run_regression(tg_media_proru_ru, tg_var, tg_id_predict)

tg_muu <- clean_mod(tg_proua_ua,"Telegram")
tg_mur <- clean_mod(tg_proua_ru,"Telegram")
#tg_mru <- clean_mod(tg_proru_ua,"Telegram")
tg_mrr <- clean_mod(tg_proru_ru,"Telegram")

#### Telegram Media Reactions  ####
same <-  names(tg_media_proua_ua) == names(tg_media_proua_ru) 
tg_media_proua_b <- rbind(tg_media_proua_ua[same], tg_media_proua_ru[same])
tg_media_proru_b <- rbind(tg_media_proru_ru[same], tg_media_proru_ua[same])
tg_media_all <-rbind(tg_media_proua_b,tg_media_proru_b)

tg_reactions <- list("views_log", "forwards_log")
tg_labels <- list("views", "forwards")

tg_r_proua <- all_reaction_regressions(tg_media_proua_b,tg_reactions,tg_id_predict,tg_labels)
tg_r_proru <- all_reaction_regressions(tg_media_proru_b,tg_reactions,tg_id_predict,tg_labels)

#### Telegram engagement no language ####
tg_id_predict_fwd <- "engagement_log" #"forwards_log"

#tg_media_proua_date <- tg_media_proua_b[tg_media_proua_b$date<"2022-02-07T09:34:36+00:00",]
#tg_media_proru_date <- tg_media_proru_b[tg_media_proru_b$date<"2022-02-07T09:34:36+00:00",]

tg_r_proua_fwd <- run_regression(tg_media_proua_b, tg_id_predict_fwd, tg_id_predict)
tg_r_proru_fwd <- run_regression(tg_media_proru_b, tg_id_predict_fwd, tg_id_predict)

tg_mu <- clean_mod(tg_r_proua_fwd,"Telegram")
tg_mr <- clean_mod(tg_r_proru_fwd,"Telegram")

#### All Media Reactions Plots ####
tw_r_all <- all_reaction_regressions(tw_media_all,tw_reactions,tw_id_predict,tw_labels)
fb_r_all<- all_reaction_regressions(fb_media_all,fb_reactions,fb_id_predict,fb_labels)
tg_r_all <- all_reaction_regressions(tg_media_all,tg_reactions,tg_id_predict,tg_labels)

r_all <- rbind(tw_r_all, fb_r_all, tg_r_all)

rpall <- reactions_plot(r_all, "All News Media")
#ggsave("all_news_media.png", plot=rpall, width = 12, height = 9)

#### Combined Sharing Plots ####

both_proua_ua <- rbind(tw_muu, fb_muu, tg_muu)
both_proua_ru <- rbind(tw_mur, fb_mur, tg_mur)
both_proru_ua <- rbind(tw_mru, fb_mru)
both_proru_ru <- rbind(tw_mrr, fb_mrr, tg_mrr)

p1 <- identity_affect_plot(both_proua_ua, "Pro-Ukrainian in Ukrainian") + theme(legend.position = "none")
p2 <- identity_affect_plot(both_proua_ru, "Pro-Ukrainian in Russian")  + theme(legend.position = "none")
p3 <- identity_affect_plot(both_proru_ua, "Pro-Russian in Ukrainian") + theme(legend.position = "none")
p4 <- identity_affect_plot(both_proru_ru, "Pro-Russian in Russian") + theme(legend.position = "none")

plot1 <- dwplot(both_proua_ua, confint = .95, dot_args = list(size = 1), whisker_args = list(size = 0.9)) %>%
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

all_idaf_plots <- ggdraw(plot_grid(p1, p2, p3, p4,
                            nrow = 3, ncol = 2, 
             labels = c("A", "B", "C", "D"),
             rel_heights = c(1, 1, .3)) + draw_grob(leg, vjust = 0.4))


#ggsave("news_media_four_plots.png", plot=all_idaf_plots, width = 16, height = 9)



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


#ggsave("plots/news_media_engagement_no_lang_plots.png", plot=both_no_lang, width = 17, height = 6)

#### Combined Reactions  Plots ####
r_proua <- rbind(fb_r_proua, tw_r_proua, tg_r_proua)
r_proru <- rbind(fb_r_proru, tw_r_proru, tg_r_proru)

# this is all just to get the plot's legend; the actual plot is discarded 
plot1 <- dwplot(r_proua, confint = .95, dot_args = list(size = 1.2), whisker_args = list(size = 0.7)) 
plot <- plot1 + theme_bw() + xlab("Odds Ratio") + ylab("") +
  ggtitle("whatever") +
  geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
  theme_apa(legend.font.size = 12,
            legend.pos = "bottom") + 
  scale_colour_manual(values=c("ingroup"="dodgerblue4", "outgroup"="firebrick"), 
                      labels=c("Ingroup", "Outgroup"))
leg <- cowplot::get_legend(plot)


rp1 <- reactions_plot(r_proua, "Pro-Ukrainian News Media")
rp2 <- reactions_plot(r_proru, "Pro-Russian News Media")

r_plots <- ggdraw(plot_grid(rp1, rp2,
                                   nrow = 2, ncol = 2, 
                                   labels = c("A", "B"),
                                   rel_heights = c(1, .1)) + draw_grob(leg, vjust = 0.4))

#ggsave("news_media_two_plots.png", plot=r_plots, width = 16, height = 9)
 




#### Sliding Time Window Analysis ####


fb_media_proua_b$date_num <- as.numeric(as.Date(fb_media_proua_b$Post.Created.Date, format="%Y-%m-%d"))
fb_start_num <- min(fb_media_proua_b$date_num)
fb_start_date <- as.Date(fb_media_proua_b[fb_media_proua_b$date_num == fb_start_num,]$Post.Created.Date[1])
fb_chunks_ua <- run_smoothing_regressions(fb_media_proua_b, fb_id_predict_fwd, fb_id_predict, 
                                          start_num=fb_start_num, smoothing=21)

fb_media_proru_b$date_num <- as.numeric(as.Date(fb_media_proru_b$Post.Created.Date, format="%Y-%m-%d"))
fb_chunks_ru <- run_smoothing_regressions(fb_media_proru_b, fb_id_predict_fwd, fb_id_predict, 
                                          start_num=fb_start_num, smoothing=21)

tw_start_num <- 18400
tw_start_date <- as.Date(tw_media_proru[tw_media_proru$date_num == tw_start_num,]$created_at[1])
tw_media_proru$date <- as.POSIXct(tw_media_proru$created_at, format="%Y-%m-%d")
tw_media_proru$date_num <- as.numeric(as.Date(tw_media_proru$date, format="%Y-%m-%d"))
tw_chunks_ua <- run_smoothing_regressions(tw_media_proua, tw_id_predict_fwd, tw_id_predict, 
                                          start_num=tw_start_num)

tw_media_proru$date <- as.POSIXct(tw_media_proru$created_at, format="%Y-%m-%d")
tw_media_proru$date_num <- as.numeric(as.Date(tw_media_proru$date, format="%Y-%m-%d"))
tw_chunks_ru <- run_smoothing_regressions(tw_media_proru, tw_id_predict_fwd, tw_id_predict, 
                                          start_num=tw_start_num)


tw_ua_ts_plot <- make_two_timeseries_plot(tw_chunks_ua,start_num=tw_start_num,first_date=tw_start_date)
tw_ru_ts_plot <- make_two_timeseries_plot(tw_chunks_ru,start_num=tw_start_num,first_date=tw_start_date)
fb_ua_ts_plot <- make_two_timeseries_plot(fb_chunks_ua,start_num=fb_start_num,first_date=fb_start_date)
fb_ru_ts_plot <- make_two_timeseries_plot(fb_chunks_ru,start_num=fb_start_num,first_date=fb_start_date)

#ggsave("news_media_two_plots.png", plot=r_plots, width = 16, height = 9)


