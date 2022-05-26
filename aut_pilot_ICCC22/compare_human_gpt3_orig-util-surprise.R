### compare human and gpt-3 performance on orig, util and surprise
library(dplyr)
library(lme4)
library(ggplot2)

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

# get data per object
## fork
aut_fork <- aut_MB %>%
  select(id, response_id, object, response, 
         originality_rater01, utility_rater01, surprise_rater01) %>%
  filter(object == "fork") %>%
  filter(originality_rater01 <= 5, utility_rater01 <= 5, surprise_rater01 <= 5) %>%
  filter(originality_rater01 > 0, utility_rater01 > 0, surprise_rater01 > 0) %>%
  left_join(response_ids) 
aut_fork$human = ifelse(aut_fork$response_id < 20180000, 1, 0)
aut_fork$human <- as.factor(aut_fork$human)
aut_fork$respondent_id <- as.factor(aut_fork$respondent_id)
write.csv(aut_fork, "data_combined/04_ratings/aut_fork_rated.csv")

## book
aut_book <- aut_IS %>%
  select(id, response_id, object, response, 
         originality_rater01, utility_rater01, surprise_rater01) %>%
  filter(object == "book")  %>%
  filter(originality_rater01 <= 5, utility_rater01 <= 5, surprise_rater01 <= 5) %>%
  filter(originality_rater01 > 0, utility_rater01 > 0, surprise_rater01 > 0) %>%
  left_join(response_ids) 
aut_book$human = ifelse(aut_book$response_id < 20180000, 1, 0)
aut_book$human <- as.factor(aut_book$human)
aut_book$respondent_id <- as.factor(aut_book$respondent_id)

# fork analyses
fork_orig_lmer <- lmer(originality_rater01 ~ human + (1|respondent_id), data = aut_fork)
summary(fork_orig_lmer)
t.test(originality_rater01 ~ human, aut_fork)
plot(originality_rater01 ~ human, aut_fork)
plot(aut_fork$human, jitter(aut_fork$originality_rater01))

fork_util_lmer <- lmer(utility_rater01 ~ human + (1|respondent_id), data = aut_fork)
summary(fork_util_lmer)
t.test(utility_rater01 ~ human, aut_fork)
plot(utility_rater01 ~ human, aut_fork)
plot(aut_fork$human, jitter(aut_fork$utility_rater01))

fork_surp_lmer <- lmer(surprise_rater01 ~ human + (1|respondent_id), data = aut_fork)
summary(fork_surp_lmer)
t.test(surprise_rater01 ~ human, aut_fork)
plot(surprise_rater01 ~ human, aut_fork)
plot(aut_fork$human, jitter(aut_fork$surprise_rater01))

# book analyses
book_orig_lmer <- lmer(originality_rater01 ~ human + (1|respondent_id), data = aut_book)
summary(book_orig_lmer)
t.test(originality_rater01 ~ human, aut_book)
plot(originality_rater01 ~ human, aut_book)
plot(aut_book$human, jitter(aut_book$originality_rater01))

book_util_lmer <- lmer(utility_rater01 ~ human + (1|respondent_id), data = aut_book)
summary(book_util_lmer)
t.test(utility_rater01 ~ human, aut_book)
plot(utility_rater01 ~ human, aut_book)
plot(aut_book$human, jitter(aut_book$utility_rater01))

book_surp_lmer <- lmer(surprise_rater01 ~ human + (1|respondent_id), data = aut_book)
summary(book_surp_lmer)
t.test(surprise_rater01 ~ human, aut_book)
plot(surprise_rater01 ~ human, aut_book)
plot(aut_book$human, jitter(aut_book$surprise_rater01))
