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
write.csv(aut_semdis, "04_to_score/220522_gpt3_aut_score_semdis_input.csv")

# create semdis input file to use as tool for categorization
# column names should be the same as those from human responses 
# so these can be merged later
aut_cat <- aut

write.csv(aut_cat, "220522_gpt3_aut_cat_semdis_input.csv")

# create semdis input file to use as tool for scoring orig, util, surprise
# column names should be the same as those from human responses 
# so these can be merged later
aut_ratings <- aut

write.csv(aut_cat, "220522_gpt3_aut_ratings_semdis_input.csv")