# script to merge gpt3 and human aut data
library(dplyr)
library(stringr)
library(tidyr)

gpt3 <- read.csv("data_gpt3/04_to_score/220522_gpt3_aut_ratings.csv")
human_book <- read.csv("data_human/02_EN_translation/aut-response_book_participants1-53.csv")
human_fork <- read.csv("data_human/02_EN_translation/aut-response_fork_participants1-53.csv")
human_tin <- read.csv("data_human/02_EN_translation/aut-response_tin_participants1-53.csv")

human <- human_book %>%
  add_row(human_fork) %>%
  add_row(human_tin) %>%
  mutate(research_id = "CESBP2016", respondent_id = participant_fk,
         response_id = AUT.response_id, temperature = NA, 
         response_EN = response_EN_corrected, originality_rating = NA, 
         utility_rating = NA, surprise_rating = NA) %>%
  select(research_id, respondent_id, response_id, temperature, object,
         response_EN, originality_rating, utility_rating, surprise_rating)
  
  
