# Dictionary Helper Functions for analysis.R (Ukraine Social ID on Social Media)
# author: Yara Kyrychenko

make_ua_dict <- function(){
  #UA Only
  sent <- quanteda:::read_dict_liwc("dictionaries/sentiment/LIWC/Ukrainian_LIWC2015_Dictionary.dic")
  positive <- sent$posemo[[1]]
  negative <- sent$negemo[[1]]
  ua_moral_emotional <- scan("dictionaries/other/Ukraine-Moral-Emotional.txt", what='character', sep="\n", skipNul = TRUE)
  ua_ua_identity <- scan("dictionaries/ru-ua/ua/Ua-Ukrainian-Identity.txt", what='character', sep="\n", skipNul = TRUE)
  ua_ru_identity <- scan("dictionaries/ru-ua/ua/Ua-Russian-Identity.txt", what='character', sep="\n", skipNul = TRUE) #ru-ua/ua/Ua-Russian-Identity.txt
  ua_ua_politicians <- scan("dictionaries/ru-ua/ua/Ua-Ukrainian-Politicians.txt", what='character', sep="\n", skipNul = TRUE)
  ua_ru_politicians <- scan("dictionaries/ru-ua/ua/Ua-Russian-Politicians.txt", what='character', sep="\n", skipNul = TRUE)

  #Create list of dictionaries
  ua_dictionary = dictionary(list(
                                positive        = tolower(positive),
                                negative        = tolower(negative),
                                moral_emotional = tolower(ua_moral_emotional),
                                ua_identity     = tolower(ua_ua_identity),
                                ru_identity     = tolower(ua_ru_identity),
                                ua_politicians  = tolower(ua_ua_politicians[1:10]),
                                ru_politicians  = tolower(ua_ru_politicians[1:10])
                                ))

 return(ua_dictionary)
}

make_ru_dict <- function(){
  #RU Only
  sent <- quanteda:::read_dict_liwc("dictionaries/sentiment/LIWC/Russian_LIWC2007_Dictionary.dic")
  positive <- sent$Позитив[[1]]
  negative <- sent$Негатив[[1]]
  ru_moral_emotional <- scan("dictionaries/other/Russian-Moral-Emotional.txt", what='character', sep="\n", skipNul = TRUE)
  ru_ua_identity <- scan("dictionaries/ru-ua/ru/Ru-Ukrainian-Identity.txt", what='character', sep="\n", skipNul = TRUE)
  ru_ru_identity <- scan("dictionaries/ru-ua/ru/Ru-Russian-Identity.txt", what='character', sep="\n", skipNul = TRUE) #ru-ua/ru/Ru-Russian-Identity.txt
  ru_ua_politicians <- scan("dictionaries/ru-ua/ru/Ru-Ukrainian-Politicians.txt", what='character', sep="\n", skipNul = TRUE)
  ru_ru_politicians <- scan("dictionaries/ru-ua/ru/Ru-Russian-Politicians.txt", what='character', sep="\n", skipNul = TRUE)
  
  #Create list of ru dictionaries
  ru_dictionary = dictionary(list(
    positive           = tolower(positive),
    negative           = tolower(negative),
    moral_emotional = tolower(ru_moral_emotional),
    ua_identity     = tolower(ru_ua_identity),
    ru_identity     = tolower(ru_ru_identity),
    ua_politicians  = tolower(ru_ua_politicians[1:10]),
    ru_politicians  = tolower(ru_ru_politicians[1:10])
  ))
  return(ru_dictionary)
}

make_ua_party_dict <- function(){
  sent <- quanteda:::read_dict_liwc("dictionaries/sentiment/LIWC/Ukrainian_LIWC2015_Dictionary.dic")
  positive <- sent$posemo[[1]]
  negative <- sent$negemo[[1]]
  ua_moral_emotional <- scan("dictionaries/other/Ukraine-Moral-Emotional.txt", what='character', sep="\n", skipNul = TRUE)
  ua_ua_identity <- scan("dictionaries/ru-ua/ua/Ua-Ukrainian-Identity.txt", what='character', sep="\n", skipNul = TRUE)
  ua_ru_identity <- scan("dictionaries/ru-ua/ua/Ua-Russian-Identity.txt", what='character', sep="\n", skipNul = TRUE)
  sluganarodu <- scan("dictionaries/parties/factions/uk/3025.txt", what='character', sep="\n", skipNul = TRUE)
  OppositionPlatformForLife <- scan("dictionaries/parties/factions/uk/3026.txt", what='character', sep="\n", skipNul = TRUE)
  Batkivshchyna <- scan("dictionaries/parties/factions/uk/3027.txt", what='character', sep="\n", skipNul = TRUE)
  EuropeanSolidarity <- scan("dictionaries/parties/factions/uk/3028.txt", what='character', sep="\n", skipNul = TRUE)
  GolosZmin <- scan("dictionaries/parties/factions/uk/3029.txt", what='character', sep="\n", skipNul = TRUE)
  ForFutureOfficial <- scan("dictionaries/parties/factions/uk/3030.txt", what='character', sep="\n", skipNul = TRUE)
  
  ua_pol_dictionary = dictionary(list(positive        = tolower(positive),
                                      negative        = tolower(negative),
                                      moral_emotional = tolower(ua_moral_emotional),
                                      ua_identity     = tolower(ua_ua_identity),
                                      ru_identity     = tolower(ua_ru_identity),
                                      X3025           = tolower(sluganarodu),
                                      X3026           = tolower(OppositionPlatformForLife),
                                      X3027           = tolower(Batkivshchyna),
                                      X3028           = tolower(EuropeanSolidarity),
                                      X3029           = tolower(GolosZmin),
                                      X3030           = tolower(ForFutureOfficial)))
  return(ua_pol_dictionary)
}

make_ru_party_dict <- function(){
  sent <- quanteda:::read_dict_liwc("dictionaries/sentiment/LIWC/Russian_LIWC2007_Dictionary.dic")
  positive <- sent$Позитив[[1]]
  negative <- sent$Негатив[[1]]
  ru_moral_emotional <- scan("dictionaries/other/Russian-Moral-Emotional.txt", what='character', sep="\n", skipNul = TRUE)
  ru_ua_identity <- scan("dictionaries/ru-ua/ru/Ru-Ukrainian-Identity.txt", what='character', sep="\n", skipNul = TRUE)
  ru_ru_identity <- scan("dictionaries/ru-ua/ru/Ru-Russian-Identity.txt", what='character', sep="\n", skipNul = TRUE)
  sluganarodu <- scan("dictionaries/parties/factions/ru/ru3025.txt", what='character', sep="\n", skipNul = TRUE)
  OppositionPlatformForLife <- scan("dictionaries/parties/factions/ru/ru3026.txt", what='character', sep="\n", skipNul = TRUE)
  Batkivshchyna <- scan("dictionaries/parties/factions/ru/ru3027.txt", what='character', sep="\n", skipNul = TRUE)
  EuropeanSolidarity <- scan("dictionaries/parties/factions/ru/ru3028.txt", what='character', sep="\n", skipNul = TRUE)
  GolosZmin <- scan("dictionaries/parties/factions/ru/ru3029.txt", what='character', sep="\n", skipNul = TRUE)
  ForFutureOfficial <- scan("dictionaries/parties/factions/ru/ru3030.txt", what='character', sep="\n", skipNul = TRUE)
  
  ru_pol_dictionary = dictionary(list(positive        = tolower(positive),
                                      negative        = tolower(negative),
                                      moral_emotional = tolower(ru_moral_emotional),
                                      ua_identity     = tolower(ru_ua_identity),
                                      ru_identity     = tolower(ru_ru_identity),
                                      X3025           = tolower(sluganarodu),
                                      X3026           = tolower(OppositionPlatformForLife),
                                      X3027           = tolower(Batkivshchyna),
                                      X3028           = tolower(EuropeanSolidarity),
                                      X3029           = tolower(GolosZmin),
                                      X3030           = tolower(ForFutureOfficial)))
  
  return(ru_pol_dictionary)
}