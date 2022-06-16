# Functions for quanteda, running regressions and plotting 

#### For All ####
run_regression <- function(data, variable, predictors){
  f <- as.formula(
    paste(variable,
          paste(predictors, collapse = " + "),
          sep = " ~ ")
  )
  model <- glm(formula = f,
               data = data)
  model <- center_mod(
    model,
    binary.inputs = "center",
    center.response = FALSE,
    data = data,
    apply.weighted.contrasts = getOption("jtools-weighted.contrasts", FALSE)
  )
  return(model)
}


all_reaction_regressions <- function(data, variables, predictors, labels){
  plural <- data.frame()
  
  for(i in 1:length(variables)){
    model <- run_regression(data,variables[i], predictors)
    clean_model <- clean_mod(model,labels[i])
    plural <- rbind(plural, clean_model)
  }
  
  plural$termtemp <- plural$term
  plural$modeltemp <- plural$model
  plural$term <- plural$modeltemp
  plural$model <- plural$termtemp
  
  plural <- plural %>% filter(plural$model == "ingroup" | plural$model == "outgroup" )
  
  return(plural)
}

filter_groups_by_lang <- function(media_proua,media_proru){
  media_proua_ua <- media_proua %>% filter(lang == "uk")
  media_proua_ru <- media_proua %>% filter(lang == "ru")
  media_proru_ua <- media_proru  %>% filter(lang == "uk")
  media_proru_ru <- media_proru %>% filter(lang == "ru")
  
  names(media_proua_ua)[names(media_proua_ua) == "ua_ua_identity"] <- "ingroup_identity"
  names(media_proua_ua)[names(media_proua_ua) == "ua_ua_politicians"] <- "ingroup_politicians"
  names(media_proua_ua)[names(media_proua_ua) == "ua_ru_identity"] <- "outgroup_identity"
  names(media_proua_ua)[names(media_proua_ua) == "ua_ru_politicians"] <- "outgroup_politicians"
  
  names(media_proua_ru)[names(media_proua_ru) == "ru_ua_identity"] <- "ingroup_identity"
  names(media_proua_ru)[names(media_proua_ru) == "ru_ua_politicians"] <- "ingroup_politicians"
  names(media_proua_ru)[names(media_proua_ru) == "ru_ru_identity"] <- "outgroup_identity"
  names(media_proua_ru)[names(media_proua_ru) == "ru_ru_politicians"] <- "outgroup_politicians"
  
  names(media_proru_ua)[names(media_proru_ua) == "ua_ua_identity"] <- "outgroup_identity"
  names(media_proru_ua)[names(media_proru_ua) == "ua_ua_politicians"] <- "outgroup_politicians"
  names(media_proru_ua)[names(media_proru_ua) == "ua_ru_identity"] <- "ingroup_identity"
  names(media_proru_ua)[names(media_proru_ua) == "ua_ru_politicians"] <- "ingroup_politicians"
  
  names(media_proru_ru)[names(media_proru_ru) == "ru_ua_identity"] <- "outgroup_identity"
  names(media_proru_ru)[names(media_proru_ru) == "ru_ua_politicians"] <- "outgroup_politicians"
  names(media_proru_ru)[names(media_proru_ru) == "ru_ru_identity"] <- "ingroup_identity"
  names(media_proru_ru)[names(media_proru_ru) == "ru_ru_politicians"] <- "ingroup_politicians"
  
  stuff <- list(media_proua_ua,media_proua_ru,media_proru_ua,media_proru_ru)
  return(stuff)
}


filter_groups_by_id <- function(media_proua,media_proru){
  
  names(media_proua)[names(media_proua) == "ua_identity"] <- "ingroup_identity"
  names(media_proua)[names(media_proua) == "ua_politicians"] <- "ingroup_politicians"
  names(media_proua)[names(media_proua) == "ru_identity"] <- "outgroup_identity"
  names(media_proua)[names(media_proua) == "ru_politicians"] <- "outgroup_politicians"
  
  names(media_proru)[names(media_proru) == "ua_identity"] <- "outgroup_identity"
  names(media_proru)[names(media_proru) == "ua_politicians"] <- "outgroup_politicians"
  names(media_proru)[names(media_proru) == "ru_identity"] <- "ingroup_identity"
  names(media_proru)[names(media_proru) == "ru_politicians"] <- "ingroup_politicians"
  
  media_proua_ua <- media_proua %>% filter(lang == "uk")
  media_proua_ru <- media_proua %>% filter(lang == "ru")
  media_proru_ua <- media_proru  %>% filter(lang == "uk")
  media_proru_ru <- media_proru %>% filter(lang == "ru")
  
  stuff <- list(media_proua_ua,media_proua_ru,media_proru_ua,media_proru_ru)
  return(stuff)
}

to_datadict <- function(text, dict){
  #' turns list of texts and a dictionary into dataframe
  
  text <- tolower(text)
  text <- stringi::stri_replace_all_regex(text, "#", " ")
  dataset_corpus <- corpus(text)
  toks <- tokens(dataset_corpus, 
                 remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, 
                 remove_symbols = TRUE, split_hyphens = TRUE, verbose = TRUE) #%>%
  
  dataset_dict <- dfm(toks, dictionary = dict)
  dataset_dict_df <- quanteda::convert(dataset_dict, to='data.frame') 
  dataset_dict_df$doc_id <- NULL
  dataset_dict_df$total_tokens <- ntoken(toks)

  return(dataset_dict_df)
  
}

add_inout <- function(data){
  data$ingroup <- data$ingroup_identity + data$ingroup_politicians
  data$outgroup <- data$outgroup_identity + data$outgroup_politicians
  return(data)
}
add_interact <- function(data){
  data$in_negative <- data$ingroup * data$negative > 0
  data$out_negative <- data$outgroup * data$negative > 0
  data$in_positive <- data$ingroup * data$positive > 0
  data$out_positive <- data$outgroup * data$positive > 0
  return(data)
}

add_all <- function(data){
  data <- add_inout(data)
  data <- add_interact(data)
  return(data)
}

clean_mod <- function(modell,label){
  stuff <- broom::tidy(modell, exponentiate = TRUE) %>% 
    filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "is_fwdTRUE", term != "total_tokens",
           term !="Likes.at.Posting", term != "Followers.at.Posting", term != "is_retweetTRUE") %>% 
    mutate(model = label) 
  return(stuff)
}

#### Twitter ####
prep_tw_data <- function(data){
  
  data$has_media <- is.na(data$media_type) == FALSE
  data$has_URL <- is.na(data$urls_url) == FALSE
  
  data$retweet_count_log <- log(data$retweet_count + 1)
  data$favorite_count_log <- log(data$favorite_count + 1)
  
  return(data)
}

prep_tw_data_q <- function(dataset,dict){
  data <- to_datadict(dataset$text,dict)
  datasetcombined = cbind(dataset,data)
  
  datasetcombined$has_media <- is.na(datasetcombined$media_type) == FALSE
  datasetcombined$has_URL <- is.na(datasetcombined$urls_url) == FALSE
  
  datasetcombined$retweet_count_log <- log(datasetcombined$retweet_count + 1)
  datasetcombined$favorite_count_log <- log(datasetcombined$favorite_count + 1)
  datasetcombined$engagement_log <- log(datasetcombined$retweet_count + datasetcombined$favorite_count + 1)
  
  return(datasetcombined)
}

#### Facebook ####

prep_fb_data <- function(datasetcombined){
  #' facebook data preprocessing 
  
  datasetcombined$engagement <- datasetcombined$Shares +
                                       datasetcombined$Likes +
                                       datasetcombined$Comments + 
                                       datasetcombined$Love + 
                                       datasetcombined$Wow + 
                                       datasetcombined$Haha + 
                                       datasetcombined$Sad + 
                                       datasetcombined$Angry
  
  datasetcombined$has_URL <- ifelse(datasetcombined$Type == "Link", TRUE, FALSE)
  datasetcombined$has_media <- ifelse(datasetcombined$Type != "Link" & datasetcombined$Type != "Status", TRUE, FALSE)
  
  
  datasetcombined$shares_log <- log(datasetcombined$Shares + 1)
  datasetcombined$comments_log <- log(datasetcombined$Comments + 1)
  datasetcombined$likes_log <- log(datasetcombined$Likes + 1)
  datasetcombined$love_log <- log(datasetcombined$Love + 1)
  datasetcombined$care_log <- log(datasetcombined$Care + 1)
  datasetcombined$wow_log <- log(datasetcombined$Wow + 1)
  datasetcombined$haha_log <- log(datasetcombined$Haha + 1)
  datasetcombined$sad_log <- log(datasetcombined$Sad + 1)
  datasetcombined$angry_log <- log(datasetcombined$Angry + 1)
  
  datasetcombined$engagement_log <- log(datasetcombined$engagement + 1)
  
  return(datasetcombined)
  
}

prep_fb_data_q <- function(dataset, dict){
  #' facebook data preprocessing 
  
  dataset$all_text <- paste(dataset$Message, dataset$Link.Text, dataset$Image.Text)
  data <- to_datadict(dataset$all_text,dict)
  datasetcombined = cbind(dataset,data)
  
  datasetcombined$engagement <- datasetcombined$Shares +
    datasetcombined$Likes +
    datasetcombined$Comments + 
    datasetcombined$Love + 
    datasetcombined$Wow + 
    datasetcombined$Haha + 
    datasetcombined$Sad + 
    datasetcombined$Angry
  
  datasetcombined$has_URL <- ifelse(datasetcombined$Type == "Link", TRUE, FALSE)
  datasetcombined$has_media <- ifelse(datasetcombined$Type != "Link" & datasetcombined$Type != "Status", TRUE, FALSE)
  
  datasetcombined$shares_log <- log(datasetcombined$Shares + 1)
  datasetcombined$comments_log <- log(datasetcombined$Comments + 1)
  datasetcombined$likes_log <- log(datasetcombined$Likes + 1)
  datasetcombined$love_log <- log(datasetcombined$Love + 1)
  datasetcombined$care_log <- log(datasetcombined$Care + 1)
  datasetcombined$wow_log <- log(datasetcombined$Wow + 1)
  datasetcombined$haha_log <- log(datasetcombined$Haha + 1)
  datasetcombined$sad_log <- log(datasetcombined$Sad + 1)
  datasetcombined$angry_log <- log(datasetcombined$Angry + 1)
  
  datasetcombined$engagement_log <- log(datasetcombined$engagement + 1)
  
  return(datasetcombined)
  
}

plot_fb <- function(model,label){
  #lmsumm <- summ(model, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
  #names_var <- c("ua_identity","ru_identity","ua_politicians","ru_politicians", "positive", "negative")
  
  stuff <- broom::tidy(model, exponentiate = TRUE) %>% 
    filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "Likes.at.Posting", term !="Followers.at.Posting") 
  
  plot2 <- {dwplot(stuff, confint = .99, dot_args = list(size = 1), whisker_args = list(size = 0.9)) +
      geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
      theme_apa(legend.pos = "bottom") + 
      xlab("Change in odds of engagement") +
      theme(legend.position = "none") +
      scale_colour_hue(h = c(260, 0)) + 
      xlim(0.8, 1.6) +
      ggtitle(label)} # %>%
  #add_brackets(two_brackets)
  return(plot2)
}


#### Telegram ####
prep_tg_data_q <- function(dataset,dict){
  data <- to_datadict(dataset$message,dict)
  data <- cbind(dataset,data)
  data$has_media <- is.na(data$media._) == FALSE
  data$is_fwd <- is.na(data$fwd_from.from_id.channel_id) == FALSE
  
  data$views_log <- log(data$views + 1)
  data$forwards_log <- log(data$forwards + 1)
  data$engagement_log <- log(data$views + data$forwards + 1)
  
  return(data)
}

#### Plots ####

# identity and affect plots
identity_affect_plot <- function(clean_model, title){
  
  two_brackets <- list(
    c("Identity", "Ingroup", "Outgroup"),
    c("Emotion", "Negative", "Moral Emotional")
  )
  
  plot1 <- {dwplot(clean_model, confint = .95, dot_args = list(size = 1), whisker_args = list(size = 0.9)) %>%
      relabel_predictors(c(
        ingroup  = "Ingroup",
        outgroup = "Outgroup",
        negative = "Negative",
        positive = "Positive",
        moral_emotional = "Moral Emotional"
      )) +
      geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
      theme_apa() +
      theme(legend.position = "none") +
      theme(plot.margin = unit(c(0, 0, 0, 0), "null")) +
      xlab("Odds ratio") +
      xlim(0.7, 1.4) +
      scale_colour_manual(values=c("Facebook"="dodgerblue4", "Twitter"="deepskyblue", "Telegram"="turquoise"), 
                          labels=c("Facebook", "Twitter", "Telegram")) +
      ggtitle(title)}  %>%
    add_brackets(two_brackets)
  
  return(plot1)
}

identity_affect_plot_nolang <- function(clean_model, title){
  
  two_brackets <- list(
    c("Identity", "Ingroup", "Outgroup"),
    c("Emotion", "Negative", "Moral Emotional")
  )
  
  plot1 <- {dwplot(clean_model, confint = .95, dot_args = list(size=2, aes(shape = (p.value < .05))), whisker_args = list(size = 1.5), margins=TRUE) %>%
      relabel_predictors(c(
        ingroup  = "Ingroup",
        outgroup = "Outgroup",
        negative = "Negative",
        positive = "Positive",
        moral_emotional = "Moral Emotional"
      )) +
      geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
      scale_shape_manual(values=c(23,16)) +
      #geom_signif() +
      theme_apa() +
      theme(legend.position = "none") +
      theme(axis.text=element_text(size=12)) +
      theme(plot.margin = unit(c(0, 0, 0, 0), "null")) +
      xlab("Odds ratio") +
      xlim(0.6, 1.35) +
      scale_colour_manual(values=c("Facebook"="dodgerblue4", "Twitter"="deepskyblue", "Telegram"="turquoise"), 
                          labels=c("Facebook", "Twitter", "Telegram")) +
      ggtitle(title)}  %>%
    add_brackets(two_brackets, fontSize = 1)
  
  return(plot1)
}


identity_affect_plot_parties <- function(clean_model, title){
  
  two_brackets <- list(
    c("Identity", "Ingroup", "Russian"),
    c("Emotion", "Negative", "Moral Emotional")
    #c("Ingroup", "in_postitive", "in_negative"),
    #c("Outgroup", "out_postitive", "out_negative"),
  )
  
  plot1 <- {dwplot(clean_model, confint = .95, dot_args = list(size=2), whisker_args = list(size = 1.5)) %>% #aes(shape = (p.value < .05))
      relabel_predictors(c(
        ingroup  = "Ingroup",
        outgroup = "Outgroup",
        ua_identity = "Ukrainian",
        ru_identity = "Russian",
        negative = "Negative",
        positive = "Positive",
        moral_emotional = "Moral Emotional"
      )) +
      geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
      theme_apa() + 
      theme(legend.text=element_text(size=12)) + 
      theme(axis.text=element_text(size=12)) +
      xlab("Odds ratio") +
      theme(legend.position = "bottom") +
      scale_shape_manual(values=c(23,16)) +
      scale_colour_manual(values=c("Facebook"="dodgerblue4", "Twitter"="deepskyblue"),
                          labels=c("Facebook", "Twitter")) +
      #xlim(0.6, 1.6) +
      ggtitle(title)}  %>%
    add_brackets(two_brackets, fontSize = 1)
  
  return(plot1)
}


all_plot <- function(clean_model, title){

  plot1 <- {dwplot(clean_model, confint = .99, dot_args = list(size = 1), whisker_args = list(size = 0.9)) %>%
      relabel_predictors(c(
        moral_emotional  = "Moral Emotional",
        positive = "Positive",
        negative = "Negative"
      )) +
      geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
      theme_apa(legend.pos = "bottom") + 
      xlab("Change in odds of engagement") +
      #theme(legend.position = "none") +
      scale_colour_hue(h = c(260, 0)) + 
      #xlim(0.95, 1.4) +
      ggtitle(title)} 
  return(plot1)
}


reactions_plot <- function(clean_model,title){
  
  two_brackets <- list(
    c("Facebook", "share", "angry"),
    c("Twitter", "retweet", "favorite"),
    c("Telegram", "views", "forwards")
  )
  
  plural_plot <- {dwplot(clean_model, confint = .95, dot_args = list(size = 1.2), whisker_args = list(size = 0.7)) + 
    geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
    theme_apa() + 
    theme(legend.position = "none") +
    xlab("Odds ratio") +
    scale_colour_manual(values=c("ingroup"="dodgerblue4", "outgroup"="firebrick"), 
                          labels=c("Ingroup", "Outgroup")) + 
    xlim(.7,1.3) +
    ggtitle(title)}  %>%
    add_brackets(two_brackets)
  
  return(plural_plot)
}




#### Time Series ####
run_smoothing_regressions <- function(df, variable, predictors, start_num=18000, smoothing=28){
  chunk <- df[start_num <= df$date_num &  df$date_num <= start_num + smoothing,]
  chunk_r <- run_regression(chunk, variable, predictors)
  chunks <- clean_mod(chunk_r,start_num)
  for (i in (start_num+1):(max(df$date_num)-smoothing)){
    chunk <- df[i <= df$date_num &  df$date_num <= i+smoothing ,]
    chunk_r <- run_regression(chunk, variable, predictors)
    chunkmodel <- clean_mod(chunk_r,i)
    chunks <- rbind(chunks, chunkmodel)
    print(i)
    print(chunkmodel$estimate)
  }
  return(chunks)
} 

make_one_timeseries_plot <- function(chunks,  start_num, first_date, 
                                     term1="outgroup"){
  
  chunks_outgroup <- chunks %>% filter(term == term1)
  margin.error <- chunks_outgroup$statistic * chunks_outgroup$std.error
  lower.bound <- chunks_outgroup$estimate - margin.error
  upper.bound <- chunks_outgroup$estimate + margin.error
  
  data_out <- data.frame(
    xValue=(as.Date(first_date) + chunks_outgroup$model - start_num),
    yValue=chunks_outgroup$estimate,
    ci_low=lower.bound,  ci_high=upper.bound
  )
  
  plot <- ggplot(data_out, aes(x=xValue, y=yValue, ymin=ci_low, ymax=ci_high)) +
    geom_line( ) + 
    scale_x_date(date_breaks = "4 weeks", date_labels = "%Y %b") + 
    #theme(axis.text.x=element_text(angle=60, hjust=1)) +
    xlab("Time") +
    ylab("Odds Ratio") +
    theme_apa()
  
  return(plot)
  
}

make_many_timeseries_plot <- function(chunks,  start_num, first_date){
  data <- data.frame()
  for (term1 in unique(chunks$term)){ 
    
    if (term1 != "(Intercept)"){
      chunks_outgroup <- chunks %>% filter(term == term1)
      data_out <- data.frame(
        xValue=(as.Date(first_date) + chunks_outgroup$model - start_num),
        yValue=chunks_outgroup$estimate
      )
      data_out$Predictor <- term1
      data <- rbind(data,data_out)
    }
  }
  
  plot <- ggplot(data, aes(x=xValue, y=yValue,color=Predictor)) +
    geom_line( ) + 
    scale_x_date(date_breaks = "4 weeks", date_labels = "%Y %b") + 
    #theme(axis.text.x=element_text(angle=60, hjust=1)) +
    xlab("Time") +
    ylab("Odds Ratio") +
    theme_apa()
  
  return(plot)
}

make_two_timeseries_plot <- function(chunks, start_num, first_date, 
                                     term1="outgroup", term2="ingroup"){
  
  chunks_outgroup <- chunks %>% filter(term == term1)
  margin.error <- chunks_outgroup$statistic * chunks_outgroup$std.error
  lower.bound <- chunks_outgroup$estimate - margin.error
  upper.bound <- chunks_outgroup$estimate + margin.error
  
  chunks_ingroup <- chunks %>% filter(term == term2)
  margin.error <- chunks_ingroup$statistic * chunks_ingroup$std.error
  in_lower.bound <- chunks_ingroup$estimate - margin.error
  in_upper.bound <- chunks_ingroup$estimate + margin.error
  
  data_out <- data.frame(
    xValue=(as.Date(first_date) + chunks_outgroup$model - start_num),
    yValue=chunks_outgroup$estimate,
    ci_low=lower.bound,  ci_high=upper.bound
  )
  data_in <- data.frame(
    xValue=(as.Date(first_date) + chunks_ingroup$model - start_num),
    yValue=chunks_ingroup$estimate,
    ci_low=in_lower.bound,  ci_high=in_upper.bound
  )
  
  data_out$Predictor <- term1
  data_in$Predictor <-  term2
  
  data <- rbind(data_out,data_in)
  plot <- ggplot(data, aes(x=xValue, y=yValue, ymin=ci_low, ymax=ci_high, fill=Predictor, linetype=Predictor)) +
    geom_line( ) + 
    geom_ribbon(alpha=0.25) +
    scale_x_date(date_breaks = "4 weeks", date_labels = "%Y %b") + 
    theme(axis.text.x=element_text(angle=60, hjust=1)) +
    xlab("Time") +
    ylab("Odds Ratio") 
    #theme_apa()
  
  return(plot)
}


#### Parties ####
make_groups <- function(data){
  #' adds political groups and subgroups columns 
  
  data$out25 <- data$X3026 + data$X3027 + data$X3028 + data$X3029 + data$X3030
  data$out26 <- data$X3025 + data$X3027 + data$X3028 + data$X3029 + data$X3030
  data$out27 <- data$X3025 + data$X3026 + data$X3028 + data$X3029 + data$X3030  
  data$out28 <- data$X3025 + data$X3026 + data$X3027 + data$X3029 + data$X3030
  data$out29 <- data$X3025 + data$X3026 + data$X3027 + data$X3028 + data$X3030
  data$out30 <- data$X3025 + data$X3026 + data$X3027 + data$X3028 + data$X3029
    
  
  #datasetcombined$gov <- datasetcombined$sluganarodu + datasetcombined$GolosZmin
  #datasetcombined$antigov <- datasetcombined$OppositionPlatformForLife + datasetcombined$Batkivshchyna
  #+ datasetcombined$EuropeanSolidarity 
  
  return(data)
}


#### Other ####
three_brackets <- list(
  c("Identity", "Ingroup", "Outgroup"),
  c("Politicians", "Ingroups", "Outgroups"),
  c("Affect", "Positive", "Negative")
)

new_labels <- c(
  ingroup_identity = "Ingroup",
  outgroup_identity = "Outgroup",
  ingroup_politicians = "Ingroups",
  outgroup_politicians = "Outgroups",
  positive = "Positive",
  negative = "Negative"
)

