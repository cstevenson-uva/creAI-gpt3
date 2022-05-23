# script to create human aut scoring files
library(dplyr)

human_book <- read.csv("data_human/02_EN_translation/aut-response_book_participants1-53.csv")
human_fork <- read.csv("data_human/02_EN_translation/aut-response_fork_participants1-53.csv")
human_tin <- read.csv("data_human/02_EN_translation/aut-response_tin_participants1-53.csv")

human_book$object <- "book"
human_fork$object <- "fork"
human_tin$object <- "tin can"

human <- human_book %>%
  add_row(human_fork) %>%
  add_row(human_tin) %>%
  mutate(research_id = "CESBP2016", 
         respondent_id = paste0("BP", 20160000 + participant_fk),
         response_id = 20160000 + AUT.response_id, temperature = NA, 
         response_EN = response_EN_corrected, originality_rating = NA, 
         utility_rating = NA, surprise_rating = NA) %>%
  select(research_id, respondent_id, response_id, temperature, object,
         response_EN, originality_rating, utility_rating, surprise_rating)

write.csv(human, "data_human/04_to_score/220522_human_aut_ratings.csv", 
          row.names = FALSE)
  
  
