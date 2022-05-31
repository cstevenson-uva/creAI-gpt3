### compare human and gpt-3 performance on orig, util and surprise
library(dplyr)
library(lme4)
library(ggplot2)

aut_IS <- read.csv2("data_combined/04_ratings/aut_ratings_IRR_Iris.csv")
aut_MB <- read.csv("data_combined/04_ratings/aut_ratings_IRR_MB.csv")
aut_gpt3_respondents <- read.csv("data_gpt3/04_to_score/220522_gpt3_aut_ratings.csv")
aut_human_respondents <- read.csv("data_human/04_to_score/220522_human_aut_ratings.csv")

# create mapping from response_id to respondent
aut_human_respondents <- aut_human_respondents %>%
  select(research_id, respondent_id, response_id, temperature)

response_ids <- aut_gpt3_respondents %>%
  select(research_id, respondent_id, response_id, temperature) %>%
  add_row(aut_human_respondents)

# create dataset with all scores and objects incl respondent_ids
aut_merge_MB <- aut_MB %>%
  select(id, response_id, object, response, 
         originality_rater01, utility_rater01, surprise_rater01) %>%
  left_join(response_ids) %>%
  mutate(respondent_id = as.factor(respondent_id))

aut_merge_IS <- aut_IS %>%
  select(id, response_id, object, response, 
         originality_rater02, utility_rater02, surprise_rater02) %>%
  left_join(response_ids) %>%
  mutate(respondent_id = as.factor(respondent_id))

aut_dat <- aut_merge_IS %>%
  # join raters' data
  full_join(aut_merge_MB) %>%
  # select required variables
  select(id, response_id, respondent_id, object, response, 
         originality_rater01, utility_rater01, surprise_rater01,
         originality_rater02, utility_rater02, surprise_rater02) %>%
  # create rating variables to do analyses with
  mutate(originality = ifelse(is.na(originality_rater01), originality_rater02, originality_rater01),
         utility = ifelse(is.na(utility_rater01), utility_rater02, utility_rater01),
         surprise = ifelse(is.na(surprise_rater01), surprise_rater02, surprise_rater01)) %>%
  # filter out all invalid and uninterpretable responses (coded 0 and 99 respectively)
  filter(originality <= 5 & originality > 0,
         utility <= 5 & utility > 0,
         surprise <= 5 & surprise > 0) %>%
  # create variable that says whether respondent was human or not
  mutate(human = ifelse(response_id < 20180000, TRUE, FALSE))

### data analysis and plots
## originality utility trade-off
aut_dat_human <- aut_dat %>%
  filter(human == TRUE)
cor.test(aut_dat_human$originality, aut_dat_human$utility)
# r orig-util human = -.556
aut_dat_gpt3 <- aut_dat %>%
  filter(human == FALSE)
cor.test(aut_dat_gpt3$originality, aut_dat_gpt3$utility)
# r orig-util gpt3 = -.608

aut_dat_respondent_means <- aut_dat %>%
  group_by(respondent_id) %>%
  summarize(human = ifelse(grepl('BP2016', respondent_id), "human", "GPT-3"),
            fluency = n(),
            originality_mean = mean(originality), utility_mean = mean(utility))
  
ggplot(data = aut_dat_respondent_means, 
       aes(x = originality_mean, y = utility_mean, color = human)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "mean originality of responses", y = "mean utility of responses", color = "")

## originality
# hierarchical model
originality_lmer_fit <- lmer(originality ~ human + object + (1|respondent_id), data = aut_dat)
summary(originality_lmer_fit)
car::S(originality_lmer_fit)
car::Anova(originality_lmer_fit)
# check with anova
originality_aov_fit <- aov(originality ~ human + object + Error(respondent_id), data = aut_dat)
car::S(originality_aov_fit)
# plot result
options(repr.plot.width=12, repr.plot.height=8)
aut_dat %>% group_by(object, human) %>% 
  summarise(originality_rating = mean(originality), se = sd(originality)/n()^0.5) %>% 
  ggplot(aes(object, originality_rating, fill = human)) + 
  geom_bar(stat = "identity", position = "dodge2") +
  geom_errorbar(aes(ymin = originality_rating-1.96*se, ymax = originality_rating+1.96*se), width=.5, position=position_dodge(.9)) +
  labs(x = "AUT object", y = "originality rating")

## utility
# hierarchical model
utility_lmer_fit <- lmer(utility ~ human + object + (1|respondent_id), data = aut_dat)
summary(utility_lmer_fit)
car::S(utility_lmer_fit)
car::Anova(utility_lmer_fit)
# check with anova
utility_aov_fit <- aov(utility ~ human + object + Error(respondent_id), data = aut_dat)
car::S(utility_aov_fit)
# plot result
aut_dat %>% group_by(object, human) %>% 
  summarise(utility_rating = mean(utility), se = sd(utility)/n()^0.5) %>% 
  ggplot(aes(object, utility_rating, fill = human)) + 
  geom_bar(stat = "identity", position = "dodge2") +
  geom_errorbar(aes(ymin = utility_rating-1.96*se, ymax = utility_rating+1.96*se), width=.5, position=position_dodge(.9))

## surprise
# hierarchical model
surprise_lmer_fit <- lmer(surprise ~ human + object + (1|respondent_id), data = aut_dat)
summary(surprise_lmer_fit)
car::S(surprise_lmer_fit)
car::Anova(surprise_lmer_fit)
# check with anova
surprise_aov_fit <- aov(surprise ~ human + object + Error(respondent_id), data = aut_dat)
car::S(surprise_aov_fit)
# plot result
aut_dat %>% group_by(object, human) %>% 
  summarise(surprise_rating = mean(surprise), se = sd(surprise)/n()^0.5) %>% 
  ggplot(aes(object, surprise_rating, fill = human)) + 
  geom_bar(stat = "identity", position = "dodge2") +
  geom_errorbar(aes(ymin = surprise_rating-1.96*se, ymax = surprise_rating+1.96*se), width=.5, position=position_dodge(.9))