### compare human and gpt-3 on categories and flexibility
library(dplyr)
library(tidyr)
library(ggplot2)

aut_fork_cat_gpt3 <- read.csv("data_combined/03_categorized/GPT3_fork_cat_complete.csv")
aut_fork_cat_gpt3$human = 0
aut_fork_cat_human <- read.csv("data_combined/03_categorized/Human_fork_cat_complete.csv")
aut_fork_cat_human$human = 1

# add repsonse_ids to gpt3 data
aut_gpt3_respondents <- read.csv("data_gpt3/04_to_score/220522_gpt3_aut_ratings.csv")
aut_gpt3_respondents <- aut_gpt3_respondents %>%
  mutate(response = response_EN) %>%
  select(respondent_id, response_id, temperature, object, response) %>%
  filter(object == "fork") 

aut_fork_cat_gpt3 <- aut_fork_cat_gpt3 %>%
  left_join(aut_gpt3_respondents) %>%
  select(respondent_id, response_id, human, temperature, response, 
         category01, category02, category03, category04)
aut_fork_cat_gpt3$respondent_id = as.factor(aut_fork_cat_gpt3$respondent_id)
aut_fork_cat_gpt3$response_id = as.factor(aut_fork_cat_gpt3$response_id)
aut_fork_cat_gpt3$category01 = as.factor(aut_fork_cat_gpt3$category01)
aut_fork_cat_gpt3$category02 = as.factor(aut_fork_cat_gpt3$category02)
aut_fork_cat_gpt3$category03 = as.factor(aut_fork_cat_gpt3$category03)
aut_fork_cat_gpt3$category04 = as.factor(aut_fork_cat_gpt3$category04)
aut_fork_cat_gpt3$human = as.factor(aut_fork_cat_gpt3$human)
#write.csv(aut_fork_cat_gpt3, "data_combined/03_categorized/GPT3_fork_cat_complete_responseids.csv")

# rename columns to make gpt3 and human data comparable
aut_fork_cat_human <- aut_fork_cat_human %>%
  mutate(temperature = NA, category01 = category01_rater01, category02 = category02_rater01, 
         category03 = category03_rater01, category04 = category04_rater01) %>%
  select(respondent_id, response_id, human, temperature, response, 
         category01, category02, category03, category04)
aut_fork_cat_human$respondent_id = as.factor(aut_fork_cat_human$respondent_id)
aut_fork_cat_human$response_id = as.factor(aut_fork_cat_human$response_id)
aut_fork_cat_human$category01 = as.factor(aut_fork_cat_human$category01)
aut_fork_cat_human$category02 = as.factor(aut_fork_cat_human$category02)
aut_fork_cat_human$category03 = as.factor(aut_fork_cat_human$category03)
aut_fork_cat_human$category04 = as.factor(aut_fork_cat_human$category04)
aut_fork_cat_human$human = as.factor(aut_fork_cat_human$human)

# combine human and gpt3 data
aut_fork_cat <- aut_fork_cat_human %>%
  add_row(aut_fork_cat_gpt3)

# make summaries of switches and # categories to compare
aut_fork_cat_fluency <- aut_fork_cat %>%
  group_by(human, respondent_id) %>%
  mutate(fluency = length(unique(response)))

aut_fork_cat_categories <- aut_fork_cat %>%
  group_by(human, respondent_id) %>%
  mutate(num_categories = length(unique(category01))) %>%
  select(respondent_id, human, num_categories) %>%
  distinct()
write.csv(aut_fork_cat_categories, "data_combined/03_categorized/GPT3_fork_cat_num_categories.csv")

t.test(num_categories ~ human, aut_fork_cat_categories)
plot(num_categories ~ human, aut_fork_cat_categories)
plot(aut_fork_cat_categories$human, jitter(aut_fork_cat_categories$num_categories))
