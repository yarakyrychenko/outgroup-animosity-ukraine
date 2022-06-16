
# Outgroup Animosity: Parties Analysis 

#### Set Up ####
librarian::shelf(dplyr, ggplot2, ggraph, gridExtra,
                 dotwhisker, jtools, plyr, stringi,
                 tidygraph, tidytext, RColorBrewer, rvest,
                 quanteda, tm, broom, stringr, meta, 
                 cld2, quiet=TRUE)

source("code/helper.R")
source("code/load_dicts.R")
options(scipen = 999)

#dictionaries 
ua_party_dict <- make_ua_party_dict()
ru_party_dict <- make_ru_party_dict()

party_handles <- read.csv("lists/UkrainePartyHandles.csv") 
party_codes <- list("X3025", "X3026", "X3027", "X3028", "X3029")

#### Twitter ####
tw_party <- read.csv("datasets/parties-tw-unique-2022-01-18.csv")

tw_party_ua <- tw_party[tw_party$lang == 'uk',]
tw_party_ru <- tw_party[tw_party$lang == 'ru',]
tw_party_ua <- prep_tw_data_q(tw_party_ua,ua_party_dict)
tw_party_ru <- prep_tw_data_q(tw_party_ru,ru_party_dict)

tw_party2 <- rbind(tw_party_ua,tw_party_ru)
tw_party_r3 <- make_groups(tw_party2)
tw_party_r3 <- tw_party_r3 %>% filter(screen_name != party_handles$Twitter.handle[6])

tw_party_r25 <- tw_party_r3 %>% filter(screen_name != party_handles$Twitter.handle[1])
tw_party_r26 <- tw_party_r3 %>% filter(screen_name != party_handles$Twitter.handle[2])
tw_party_r27 <- tw_party_r3 %>% filter(screen_name != party_handles$Twitter.handle[3])
tw_party_r28 <- tw_party_r3 %>% filter(screen_name != party_handles$Twitter.handle[4])
tw_party_r29 <- tw_party_r3 %>% filter(screen_name != party_handles$Twitter.handle[5])

names(tw_party_r25)[names(tw_party_r25) == "X3025"] <- "ingroup"
names(tw_party_r25)[names(tw_party_r25) == "out25"] <- "outgroup"

names(tw_party_r26)[names(tw_party_r26) == "X3026"] <- "ingroup"
names(tw_party_r26)[names(tw_party_r26) == "out26"] <- "outgroup"

names(tw_party_r27)[names(tw_party_r27) == "X3027"] <- "ingroup"
names(tw_party_r27)[names(tw_party_r27) == "out27"] <- "outgroup"

names(tw_party_r28)[names(tw_party_r28) == "X3028"] <- "ingroup"
names(tw_party_r28)[names(tw_party_r28) == "out28"] <- "outgroup"

names(tw_party_r29)[names(tw_party_r29) == "X3029"] <- "ingroup"
names(tw_party_r29)[names(tw_party_r29) == "out29"] <- "outgroup"

tw_party_predict <- list("ingroup", "outgroup", "ua_identity","ru_identity", "negative", "positive", "moral_emotional", 
                      "has_media", "has_URL", "is_retweet", "total_tokens")
tw_var <- "engagement_log"
tw_var1 <- "retweet_count_log"
tw_var2 <- "favorite_count_log"

tm1 <- run_regression(tw_party_r25, tw_var, tw_party_predict)
tm2 <- run_regression(tw_party_r26, tw_var, tw_party_predict)
tm3 <- run_regression(tw_party_r27, tw_var, tw_party_predict)
tm4 <- run_regression(tw_party_r28, tw_var, tw_party_predict)
tm5 <- run_regression(tw_party_r29, tw_var, tw_party_predict)
tm1 <- clean_mod(tm1,"Government")
tm2 <- clean_mod(tm2,"Opposition Platform 'For Life'")
tm3 <- clean_mod(tm3,"Batkivshchyna")
tm4 <- clean_mod(tm4,"European Solidarity")
tm5 <- clean_mod(tm5,"Golos Zmin")

all <- rbind(tm1,tm2, tm4,tm5)
identity_affect_plot_parties(all, "Twitter engagement with political parties")

#### Twitter Reactions ####

tm1 <- run_regression(tw_party_r25, tw_var1, tw_party_predict)
tm2 <- run_regression(tw_party_r26, tw_var1, tw_party_predict)
tm3 <- run_regression(tw_party_r27, tw_var1, tw_party_predict)
tm4 <- run_regression(tw_party_r28, tw_var1, tw_party_predict)
tm5 <- run_regression(tw_party_r29, tw_var1, tw_party_predict)
tm1 <- clean_mod(tm1,"Servant of the People (Government)")
tm2 <- clean_mod(tm2,"Opposition Platform 'For Life'")
tm3 <- clean_mod(tm3,"Motherland")
tm4 <- clean_mod(tm4,"European Solidarity")
tm5 <- clean_mod(tm5,"Voice of Change")

all <- rbind(tm1,tm2, tm4,tm5)
party_retweets <- identity_affect_plot_parties(all, "Twitter retweets for political parties")
#ggsave("party_retweets.png", plot=party_retweets, width = 16, height = 9)

all <- rbind(tm1,tm2, tm3, tm4,tm5)
party_retweets_all <- identity_affect_plot_parties(all, "Twitter retweets for political parties")
#ggsave("party_retweets_all.png", plot=party_retweets_all, width = 16, height = 9)


tm1 <- run_regression(tw_party_r25, tw_var2, tw_party_predict)
tm2 <- run_regression(tw_party_r26, tw_var2, tw_party_predict)
tm3 <- run_regression(tw_party_r27, tw_var2, tw_party_predict)
tm4 <- run_regression(tw_party_r28, tw_var2, tw_party_predict)
tm5 <- run_regression(tw_party_r29, tw_var2, tw_party_predict)
tm1 <- clean_mod(tm1,"Government")
tm2 <- clean_mod(tm2,"Opposition Platform 'For Life'")
tm3 <- clean_mod(tm3,"Batkivshchyna")
tm4 <- clean_mod(tm4,"European Solidarity")
tm5 <- clean_mod(tm5,"Golos Zmin")

all <- rbind(tm1,tm2, tm4,tm5)
identity_affect_plot_parties(all, "Twitter favorites for political parties")

# Twitter together plot 

tw_party_r25 <- subset(tw_party_r25,select=-c(X3026,X3027,X3028,X3029,X3030, out26, out27, out28, out29, out30))
tw_party_r26 <- subset(tw_party_r26,select=-c(X3025,X3027,X3028,X3029,X3030, out25, out27, out28, out29, out30))
tw_party_r27 <- subset(tw_party_r27,select=-c(X3026,X3025,X3028,X3029,X3030, out26, out25, out28, out29, out30))
tw_party_r28 <- subset(tw_party_r28,select=-c(X3026,X3027,X3025,X3029,X3030, out26, out27, out25, out29, out30))
tw_party_r29 <- subset(tw_party_r29,select=-c(X3026,X3027,X3028,X3025,X3030, out26, out27, out28, out25, out30))

tw_together <- rbind(tw_party_r25, tw_party_r26, tw_party_r27, tw_party_r28, tw_party_r29)
tw_m <- run_regression(tw_together, tw_var1, tw_party_predict)
tw_m <- clean_mod(tw_m,"Twitter")
tw_togetherplot <- identity_affect_plot_parties(tw_m, "Twitter retweets") 
#ggsave("party_retweets_together.png", plot=tw_togetherplot, width = 16, height = 9)



#### Facebook ####
fb_party <- read.csv("datasets/parties-fb-fullsent-2021-09-24.csv")
fb_party <- fb_party[-c(44:61)]

fb_party_ua <- fb_party[fb_party$lang == 'uk',]
fb_party_ru <- fb_party[fb_party$lang == 'ru',]
fb_party_ua <- prep_fb_data_q(fb_party_ua,ua_party_dict)
fb_party_ru <- prep_fb_data_q(fb_party_ru,ru_party_dict)

fb_party2 <- rbind(fb_party_ua,fb_party_ru)
fb_party_r3 <- make_groups(fb_party2)
fb_party_r3 <- fb_party_r3 %>% filter(User.Name != party_handles$FB.handle[6])


fb_party_r25 <- fb_party_r3 %>% filter(User.Name == party_handles$FB.handle[1] )
fb_party_r26 <- fb_party_r3 %>% filter(User.Name == party_handles$FB.handle[2] )
fb_party_r27 <- fb_party_r3 %>% filter(User.Name == party_handles$FB.handle[3] )
fb_party_r28 <- fb_party_r3 %>% filter(User.Name == party_handles$FB.handle[4] )
fb_party_r29 <- fb_party_r3 %>% filter(User.Name == party_handles$FB.handle[5] )

names(fb_party_r25)[names(fb_party_r25) == "X3025"] <- "ingroup"
names(fb_party_r25)[names(fb_party_r25) == "out25"] <- "outgroup"

names(fb_party_r26)[names(fb_party_r26) == "X3026"] <- "ingroup"
names(fb_party_r26)[names(fb_party_r26) == "out26"] <- "outgroup"

names(fb_party_r27)[names(fb_party_r27) == "X3027"] <- "ingroup"
names(fb_party_r27)[names(fb_party_r27) == "out27"] <- "outgroup"

names(fb_party_r28)[names(fb_party_r28) == "X3028"] <- "ingroup"
names(fb_party_r28)[names(fb_party_r28) == "out28"] <- "outgroup"

names(fb_party_r29)[names(fb_party_r29) == "X3029"] <- "ingroup"
names(fb_party_r29)[names(fb_party_r29) == "out29"] <- "outgroup"


fb_party_predict <- list("ingroup", "outgroup", "ua_identity", "ru_identity", 
                      "negative", "positive", "moral_emotional", "has_URL","has_media", "total_tokens")
fb_var <- "shares_log"

m1 <- run_regression(fb_party_r25, fb_var, fb_party_predict)
m2 <- run_regression(fb_party_r26, fb_var, fb_party_predict)
m3 <- run_regression(fb_party_r27, fb_var, fb_party_predict)
m4 <- run_regression(fb_party_r28, fb_var, fb_party_predict)
m5 <- run_regression(fb_party_r29, fb_var, fb_party_predict)

m1 <- clean_mod(m1,"Servant of the People (Government)")
m2 <- clean_mod(m2,"Opposition Platform 'For Life'")
m3 <- clean_mod(m3,"Motherland")
m4 <- clean_mod(m4,"European Solidarity")
m5 <- clean_mod(m5,"Voice of Change")

all <- rbind(m1,m2,m3,m4,m5)
allp <- all[all$p.value< 0.05,]
fb_sig <- identity_affect_plot_parties(allp, "Facebook shares of political parties")
#ggsave("party_shares_sig.png", plot=fb_sig, width = 16, height = 9)

#Together plot 
fb_party_r25 <- subset(fb_party_r25,select=-c(X3026,X3027,X3028,X3029,X3030, out26, out27, out28, out29, out30))
fb_party_r26 <- subset(fb_party_r26,select=-c(X3025,X3027,X3028,X3029,X3030, out25, out27, out28, out29, out30))
fb_party_r27 <- subset(fb_party_r27,select=-c(X3026,X3025,X3028,X3029,X3030, out26, out25, out28, out29, out30))
fb_party_r28 <- subset(fb_party_r28,select=-c(X3026,X3027,X3025,X3029,X3030, out26, out27, out25, out29, out30))
fb_party_r29 <- subset(fb_party_r29,select=-c(X3026,X3027,X3028,X3025,X3030, out26, out27, out28, out25, out30))

together <- rbind(fb_party_r25, fb_party_r26, fb_party_r27, fb_party_r28, fb_party_r29)
m <- run_regression(together, fb_var, fb_party_predict)
m <- clean_mod(m,"Facebook")
#togetherplot <- identity_affect_plot_parties(m, "Facebook shares") 


#Model  
fb_party_not25 <- fb_party_r3 %>% filter(User.Name != party_handles$FB.handle[1] )
#var2 <- "shares_log"
names(fb_party_not25)[names(fb_party_not25) == "out25"] <- "ingroup"
names(fb_party_not25)[names(fb_party_not25) == "X3025"] <- "outgroup"
notm1 <- run_regression(fb_party_not25, fb_var, fb_party_predict)
notm1 <- clean_mod(notm1,"Opposition")

main <- rbind(m1,notm1)
identity_affect_plot_parties(main, "Government versus Opposition")


#### Together ####
together_both <- rbind(m,tw_m)
together_both_leg <- identity_affect_plot_parties(together_both, "") 

ggsave("plots/parties_all_together.png", plot=together_both_leg, width = 8, height = 6)
