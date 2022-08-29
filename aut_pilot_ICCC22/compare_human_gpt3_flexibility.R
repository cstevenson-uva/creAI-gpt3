### compare human and gpt-3 on categories and flexibility
library(dplyr)
library(tidyr)
library(ggplot2)

## FORK flexibility
# get categorized data
aut_fork_cat_gpt3 <- read.csv("data_combined/03_categorized/gpt3_fork_categorized.csv")
aut_fork_cat_gpt3$human = "GPT-3"
aut_fork_cat_human <- read.csv("data_combined/03_categorized/human_fork_categorized.csv")
aut_fork_cat_human$human = "human"

# repsonse_ids for gpt3 data
aut_gpt3_respondents <- read.csv("data_gpt3/04_to_score/220522_gpt3_aut_ratings.csv")
aut_gpt3_respondents <- aut_gpt3_respondents %>%
  mutate(response = response_EN) %>%
  select(respondent_id, response_id, temperature, object, response) %>%
  filter(object == "fork") 

aut_fork_cat_gpt3 <- aut_fork_cat_gpt3 %>%
  left_join(aut_gpt3_respondents) %>%
  select(respondent_id, response_id, human, temperature, response, 
         category01, category02, category03, category04) %>%
  mutate(respondent_id = as.factor(respondent_id),
         response_id = as.factor(response_id),
         category01 = as.factor(category01),
         category02 = as.factor(category02),
         category03 = as.factor(category03),
         category04 = as.factor(category04))

# rename columns to make gpt3 and human data comparable
aut_fork_cat_human <- aut_fork_cat_human %>%
  mutate(temperature = NA, category01 = category01_rater01, category02 = category02_rater01, 
         category03 = category03_rater01, category04 = category04_rater01) %>%
  select(respondent_id, response_id, human, temperature, response, 
         category01, category02, category03, category04) %>%
  mutate(respondent_id = as.factor(respondent_id),
         response_id = as.factor(response_id),
         category01 = as.factor(category01),
         category02 = as.factor(category02),
         category03 = as.factor(category03),
         category04 = as.factor(category04))

# combine human and gpt3 data
aut_fork_cat <- aut_fork_cat_human %>%
  add_row(aut_fork_cat_gpt3) 

aut_fork_cat_long <- aut_fork_cat %>%
  pivot_longer(cols = starts_with("category"),
               names_to = "category_src",
               values_to = "category",
               values_drop_na = TRUE)

aut_fork_cat_summary <- aut_fork_cat_long %>%
  group_by(respondent_id) %>%
  # count total num responses per respondent
  # count total num unique categories per respondent
  summarize(num_responses = length(unique(response)),
            num_categories = length(unique(category))) %>%
  mutate(flexibility = num_categories / num_responses,
         human = ifelse(grepl('cmpl', respondent_id), "GPT-3", "human"))

flexibility_aov_fit <- aov(flexibility ~ human + Error(respondent_id), data = aut_fork_cat_summary)
car::S(flexibility_aov_fit)

## TIN CAN FLEXIBILITY
aut_tin_cat <- read.csv("data_combined/03_categorized/tin_categorized.csv")
aut_gpt3_respondents <- read.csv("data_gpt3/04_to_score/220522_gpt3_aut_ratings.csv")
aut_human_respondents <- read.csv("data_human/04_to_score/220522_human_aut_ratings.csv")

# create mapping from response_id to respondent
aut_human_respondents <- aut_human_respondents %>%
  select(research_id, respondent_id, response_id, temperature)

response_ids <- aut_gpt3_respondents %>%
  select(research_id, respondent_id, response_id, temperature) %>%
  add_row(aut_human_respondents)

aut_tin_cat_summary <- aut_tin_cat %>% 
  # add respondent_ids
  left_join(response_ids) %>%
  # select essential data
  select(respondent_id, response_id, response, temperature,
         category01, category02, category03, category04) %>%
  # make sure variables are the right type
  mutate(respondent_id = as.factor(respondent_id),
         response_id = as.factor(response_id),
         category01 = as.factor(category01),
         category02 = as.factor(category02),
         category03 = as.factor(category03),
         category04 = as.factor(category04)) %>%
  pivot_longer(cols = starts_with("category"),
               names_to = "category_src",
               values_to = "category",
               values_drop_na = TRUE) %>%
  # now create summaries per respondent
  group_by(respondent_id) %>%
  # count total num responses per respondent
  # count total num unique categories per respondent
  summarize(num_responses = length(unique(response)),
            num_categories = length(unique(category)),
            temperature = mean(temperature)) %>%
  mutate(flexibility = num_categories / num_responses,
         human = ifelse(grepl('cmpl', respondent_id), "GPT-3", "human"))

gpt3_tin_cat <- aut_tin_cat_summary %>%
  filter(human == "GPT-3")
cor.test(gpt3_tin_cat$flexibility, gpt3_tin_cat$temperature)

flexibility_aov_fit <- aov(flexibility ~ human + Error(respondent_id), data = aut_tin_cat_summary)
car::S(flexibility_aov_fit)

ggplot(aut_tin_cat_summary, 
       aes(x=human, y=flexibility, color=human)) +
  geom_violin() +
  geom_boxplot(width = .1) +
  theme(legend.position = "none") + 
  labs(x="")
