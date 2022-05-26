### script to create file for raters and IRR 
library(dplyr)

aut_IS <- read.csv("data_combined/04_ratings/aut_ratings_Iris.csv")
aut_MB <- read.csv("data_combined/04_ratings/aut_ratings_MB.csv")
aut_gpt3_respondents <- read.csv("data_gpt3/04_to_score/220522_gpt3_aut_ratings.csv")
aut_human_respondents <- read.csv("data_human/04_to_score/220522_human_aut_ratings.csv")

# create mapping from response_id to respondent
aut_human_respondents <- aut_human_respondents %>%
  select(research_id, respondent_id, response_id, temperature)

response_ids <- aut_gpt3_respondents %>%
  select(research_id, respondent_id, response_id, temperature) %>%
  add_row(aut_human_respondents)

# create dataset with all scores and objects incl respondent_ids
# also for coding IRR
aut_merge_MB <- aut_MB %>%
  select(id, response_id, object, response, 
         originality_rater01, utility_rater01, surprise_rater01) %>%
  left_join(response_ids) 
aut_merge_MB$respondent_id <- as.factor(aut_merge_MB$respondent_id)

aut_merge_IS <- aut_IS %>%
  select(id, response_id, eucldist_object_response, eucldist_chair_response, 
         cosdist_object_response, cosdist_chair_response, object, response, 
         originality_rater02, utility_rater02, surprise_rater02) %>%
  left_join(response_ids)  
aut_merge_IS$respondent_id <- as.factor(aut_merge_IS$respondent_id)

aut_merge <- aut_merge_IS %>%
  full_join(aut_merge_MB) %>%
  mutate(rater01 = NA, rater02 = NA) %>% # in spreadsheet assign to rater
  select(id, response_id, eucldist_object_response, eucldist_chair_response, 
         cosdist_object_response, cosdist_chair_response, object, response, 
         originality_rater01, utility_rater01, surprise_rater01,
         originality_rater02, utility_rater02, surprise_rater02, rater01, rater02)

write.csv(aut_merge, "data_combined/01_to_score/aut_ratings_IRR.csv", row.names = FALSE)