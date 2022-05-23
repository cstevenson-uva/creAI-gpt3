# gpt3 aut data cleaning script
library(dplyr)
library(stringr)
library(tidyr)

# read in gpt3 aut data
aut_1 <- read.csv("03_cleaned/220422_gpt3_aut_cleaned.csv")
aut_2 <- read.csv("03_cleaned/220520_gpt3_aut_cleaned.csv")

aut <- aut_1 %>%
  add_row(aut_2)

# create semdis input file to measure semantic distance
aut_semdis <- aut %>%
  rename(respondent_id = id, response = gpt3_response, item = aut_object) %>%
  rename(id = X) %>%
  select(id, item, response, respondent_id, gpt3_id, temperature)
write.csv(aut_semdis, "04_to_score/220522_gpt3_aut_score_semdis_input.csv", row.names = FALSE)

# create semdis input file to use as tool for scoring orig, util, surprise
# column names should be the same as those from human responses 
# so these can be merged later
aut_1a <- aut_1 %>%
  mutate(research_id = "GPT3202204", response_id = 2022040000 + X)
aut_2a <- aut_2 %>%
  mutate(research_id = "GPT3202205", response_id = 2022050000 + X)

aut_ratings <- aut_1a %>%
  add_row(aut_2a) %>%
  rename(respondent_id = id, object = aut_object, 
         response_EN = gpt3_response) %>%
  mutate(originality_rating = NA, utility_rating = NA, 
         surprise_rating = NA) %>%
  select(research_id, respondent_id, response_id, temperature, object,
         response_EN, originality_rating, utility_rating, surprise_rating)

write.csv(aut_ratings, "04_to_score/220522_gpt3_aut_ratings.csv", row.names = FALSE)

# create files for categorization
# update responses for aut_1, data has already been categorized
aut_cat1 <- aut_1 %>%
  mutate(id = gpt3_id, response = gpt3_response, 
         response_nofill = gpt3_response, item = aut_object, 
         item_nofill = aut_object) %>%
  select(id, response, response_nofill, item, item_nofill)

write.csv(aut_cat1, "04_to_score/220422_gpt3_aut_cat.csv", row.names = FALSE)

# will be appended to existing categorized file of aut_1
aut_cat2 <- aut_2 %>%
  mutate(id = gpt3_id, response = gpt3_response, 
         response_nofill = gpt3_response, item = aut_object, 
         item_nofill = aut_object) %>%
  select(id, response, response_nofill, item, item_nofill)

write.csv(aut_cat2, "04_to_score/220520_gpt3_aut_cat.csv", row.names = FALSE)
