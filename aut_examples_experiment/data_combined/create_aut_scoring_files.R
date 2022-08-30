# script to create files for categorizing and rating human and gpt3 aut data
library(dplyr)

aut <- read.csv("data_combined/02_semdis/aut_semdis_w_dists.csv", row.names = FALSE)

aut_ratings <- aut %>%
  mutate(eucldist_chair_response = eucl_item_resp, 
         cosdist_chair_response = cos_dist_item_resp,
         eucldist_object_response = eucl_objc_resp,
         cosdist_object_response = cos_dist_objc_resp,
         originality_rater01 = NA, utility_rater01 = NA, 
         surprise_rater01 = NA, originality_rater02 = NA, 
         utility_rater02 = NA, surprise_rater02 = NA) %>%
  select(id, response_id, eucldist_object_response, cosdist_object_response,
         eucldist_chair_response, cosdist_chair_response,
         object, response, originality_rater01, utility_rater01,
         surprise_rater01, originality_rater02, utility_rater02, 
         surprise_rater02)

write.csv(aut_ratings, "data_combined/01_to_score/aut_ratings.csv", row.names = FALSE)
