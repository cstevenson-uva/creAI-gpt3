# script to merge gpt3 and human aut data
library(dplyr)

gpt3 <- read.csv("data_gpt3/04_to_score/220522_gpt3_aut_ratings.csv")
human <- read.csv("data_human/04_to_score/220522_human_aut_ratings.csv")

aut <- gpt3 %>%
  add_row(human)

write.csv(aut, "data_combined/01_to_score/aut_ratings.csv", row.names = FALSE)

aut_semdis <- aut
aut_semdis$id = row.names(aut_semdis)
aut_semdis <- aut_semdis %>%
  mutate(item = "chair",
         response = response_EN) %>%
  select(id, item, response, response_id, object, respondent_id, temperature)

write.csv(aut_semdis, "data_combined/01_to_score/aut_semdis_input.csv", 
          row.names = FALSE)
