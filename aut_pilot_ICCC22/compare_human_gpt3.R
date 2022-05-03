library(dplyr)

gpt3_aut_semdis <- read.csv("data_gpt3/SemDis-2022-04-22_gpt3_pilot.csv")
#gpt3_aut_meta <- read.csv("data_gpt3/gpt3_aut_pilot_response_meta.csv")

gpt3_aut <- gpt3_aut_semdis
gpt3_aut$human <- 0

human_tin_semdis <- read.csv("data_human/aut_tin_semdis_computed.csv")
human_book_semdis <- read.csv("data_human/aut_book_semdis_computed.csv")
human_fork_semdis <- read.csv("data_human/aut_fork_semdis_computed.csv")

human_aut <- human_book_semdis %>%
  add_row(human_fork_semdis) %>%
  add_row(human_tin_semdis) 
human_aut$human <- 1
human_aut$id <- as.factor(human_aut$id)

aut_semdis <- gpt3_aut %>%
  add_row(human_aut)

summary(aut_semdis$human)

t.test(SemDis_MEAN ~ human, aut_semdis)
plot(SemDis_MEAN ~ human, aut_semdis)
plot(aut_semdis$human, jitter(aut_semdis$SemDis_MEAN))

