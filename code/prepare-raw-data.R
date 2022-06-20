
# Helper Functions for Data Prep (Ukraine Social ID on Social Media)
# author: Yara Kyrychenko

#### Prepare FB files ####
# Load and stitch together media fb (up to 06/16)
umf1 <- read.csv("datasets-raw/media/Ukraine-Media-FB-2020-08-15--2021-08-15.csv")
umf2 <- read.csv("datasets-raw/media/Ukraine-Media-FB-2021-08-15--2021-08-25.csv")
umf3 <- read.csv("datasets-raw/media/Ukraine-Media-FB-2021-08-09--2021-09-24.csv") 
umf4 <- read.csv("datasets-raw/media/Ukraine-Media-FB-2021-09-23--2021-10-16.csv")
umf5 <- read.csv("datasets-raw/media/Ukraine-Media-FB-2021-09-23--2021-10-24.csv")
umf6 <- read.csv("datasets-raw/media/Ukraine-Media-FB-2021-10-22--2021-11-25.csv")

umf7 <- read.csv("datasets-raw/media/Ukraine-Media-FB-2021-11-23--2021-12-24.csv")
umf8 <- read.csv("datasets-raw/media/Ukraine-Media-FB-2021-12-22--2022-01-07.csv")
umf9 <- read.csv("datasets-raw/media/Ukraine-Media-FB-2022-01-01--2022-02-06.csv") 
umf10 <- read.csv("datasets-raw/media/Ukraine-Media-FB-2021-03-02--2022-03-02.csv")
umf11 <- read.csv("datasets-raw/media/Ukraine-Media-FB-2022-03-01--2022-04-02.csv")
umf12 <- read.csv("datasets-raw/media/Ukraine-Media-FB-2022-03-31--2022-05-01.csv")
umf13 <- read.csv("datasets-raw/media/Ukraine-Media-FB-2022-04-29--2022-05-31.csv")
umf14 <- read.csv("datasets-raw/media/Ukraine-Media-FB-2022-05-15--2022-06-16.csv")

all <- rbind(umf14,umf13,umf12,umf11,umf10,umf9,umf8,umf7,umf6,umf5,umf4,umf3,umf2,umf1)
all <- all[!duplicated(all$URL), ] 

write.csv(all, "datasets/media-fb-unique-2022-06-16.csv")
 
# Load and stitch together parties fb (up to 06/16)
umf1 <- read.csv("datasets-raw/parties/Ukraine-Political-Parties-FB-2021-08-09--2021-09-24.csv")
umf2 <- read.csv("datasets-raw/parties/Ukraine-Political-Parties-FB-2020-08-15--2021-08-15.csv")
umf3 <- read.csv("datasets-raw/parties/Ukraine-Political-Parties-FB-2021-03-02--2022-03-02.csv")
umf4 <- read.csv("datasets-raw/parties/Ukraine-Political-Parties-FB-2022-03-01--2022-06-16.csv")
all <- rbind(umf4,umf3,umf2,umf1)
all <- all[!duplicated(all$URL), ] 

write.csv(all, "datasets/parties-fb-unique-2022-06-16.csv")


# Load and stitch together Zelenskii fb (up to 08/24)
umf1 <- read.csv("datasets-raw/indep/zelenskii/Ukraine-Zelenskii-2021-07-16--2021-08-16.csv")
umf2 <- read.csv("datasets-raw/indep/zelenskii/Ukraine-Zelenskii-2021-08-16--2021-08-24.csv")
umf3 <- read.csv("datasets-raw/indep/zelenskii/Ukraine-Zelenskii-2021-08-23--2021-09-20.csv")
umf4 <- read.csv("datasets-raw/indep/zelenskii/Ukraine-Zelenskii-2021-09-18--2021-12-11.csv")
all <- rbind(umf4,umf3,umf2,umf1)
all <- all[!duplicated(all$URL), ] 

write.csv(all, "datasets/zelenskii-fb-unique-2021-12-11.csv")

# Load and stich together indep fb (up to 09/24)
umf1 <- read.csv("datasets-raw/indep/indep-fb/Ukraine-Independence-FB-2021-08-24--2021-09-01.csv")
umf2 <- read.csv("datasets-raw/indep/indep-fb/Ukraine-Independence-FV-2021-08-25--2021-09-01.csv")
umf3 <- read.csv("datasets-raw/indep/indep-fb/Ukraine-Independence-FB---2021-08-23--2021-08-25.csv")
umf4 <- read.csv("datasets-raw/indep/indep-fb/Ukraine-Independence-2021-08-20--2021-08-24.csv")
umf5 <- read.csv("datasets-raw/indep/indep-fb/Ukraine-Independence-FB-2021-08-13--2021-08-20.csv")

all <- rbind(umf1,umf2,umf3,umf4,umf5)
all <- all[!duplicated(all$URL), ] 

write.csv(all, "datasets/indep-fb-unique-09-01-21.csv")


#### Prepare TW files ####

#load and stitch together media TW (up to 06/18)
umf1 <- read.csv("datasets-raw/media/Ukraine-Media-TW-2021-08-15.csv")
umf2 <- read.csv("datasets-raw/media/Ukraine-Media-TW-2022-01-18.csv")
umf3 <- read.csv("datasets-raw/media/Ukraine-Media-TW-2022-06-18.csv")
all <- rbind(umf3,umf2,umf1)
all <- all[!duplicated(all$status_id), ] 

write.csv(all, "datasets/media-tw-unique-2022-06-18.csv")

#load and stitch together parties TW (up to 06/18)
umf1 <- read.csv("datasets-raw/parties/Ukraine-Political-Parties-TW-2021-08-15.csv")
umf2 <- read.csv("datasets-raw/parties/Ukraine-Political-Parties-TW-2022-01-18.csv")
umf3 <- read.csv("datasets-raw/parties/Ukraine-Political-Parties-TW-2022-06-18.csv")
all <- rbind(umf3,umf2,umf1)
all <- all[!duplicated(all$status_id), ] 

write.csv(all, "datasets/parties-tw-unique-2022-06-18.csv")

