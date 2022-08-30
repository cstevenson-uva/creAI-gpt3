### compare human and gpt-3 performance on cosine distance
library(dplyr)
library(lme4)
library(ggplot2)

aut_merged_file <- readline(prompt = "Enter merged human+gpt3 data filename incl path: ")
#aut_merged_semdis <- read.csv(aut_merged_file)
aut_merged_semdis <- aut

# select only relevant columns from autsemdis data
aut_semdis <- aut_merged_semdis %>%
  rename(semdis = cos_dist_objc_resp) %>%
  mutate(human = ifelse(human == 1, "human", "GPT-3")) %>%
  mutate(exp_cond_name = case_when(exp_condition == 1 ~ "None",
                                   exp_condition == 2 ~ "Uncreative",
                                   exp_condition == 3 ~ "Common uncreative",
                                   exp_condition == 4 ~ "Common creative",
                                   exp_condition == 5 ~ "Creative"))

semdis_human <- aut_semdis %>%
  filter(human == "human")

semdis_gpt3 <- aut_semdis %>%
  filter(human == "GPT-3")

### data analysis and plots
respondent_semdis_means <- aut_semdis %>%
  group_by(respondent_id) %>%
  summarize(fluency = n(),
            semdis_mean = mean(semdis))
  
## semdis human vs gpt-3
# hierarchical model comparing objects
semdis_lmer_fit <- lmer(semdis ~ human + object + (1|respondent_id), data = aut_semdis)
summary(semdis_lmer_fit)
car::S(semdis_lmer_fit)
car::Anova(semdis_lmer_fit)
# check with anova
semdis_aov_fit <- aov(semdis ~ human + object + Error(respondent_id), data = aut_semdis)
car::S(semdis_aov_fit)
# plot result
aut_semdis %>% group_by(object, human) %>% 
  summarise(semdis_score = mean(semdis), se = sd(semdis)/n()^0.5) %>% 
  ggplot(aes(object, semdis_score, fill = human)) + 
  geom_bar(stat = "identity", position = "dodge2") +
  geom_errorbar(aes(ymin = semdis_score-1.96*se, ymax = semdis_score+1.96*se), width=.5, position=position_dodge(.9)) +
  labs(x = "AUT object", y = "semantic distance", fill = "")

## semdis
# hierarchical model comparing experimental conditions
semdis_lmer_fit <- lmer(semdis ~ human + exp_condition + (1|respondent_id), data = aut_semdis)
semdis_lmer_fit2 <- lmer(semdis ~ human * exp_condition + (1|respondent_id), data = aut_semdis)
anova(semdis_lmer_fit, semdis_lmer_fit2)
summary(semdis_lmer_fit)
car::S(semdis_lmer_fit)
car::Anova(semdis_lmer_fit)
# check with anova
semdis_aov_fit <- aov(semdis ~ human + exp_condition + Error(respondent_id), data = aut_semdis)
car::S(semdis_aov_fit)
# plot result
aut_semdis %>% group_by(exp_condition, human) %>% 
  summarise(semdis_score = mean(semdis), se = sd(semdis)/n()^0.5) %>% 
  ggplot(aes(exp_condition, semdis_score, fill = human)) + 
  geom_bar(stat = "identity", position = "dodge2") +
  geom_errorbar(aes(ymin = semdis_score-1.96*se, ymax = semdis_score+1.96*se), width=.5, position=position_dodge(.9)) +
  labs(x = "instruction examples condition", y = "semantic distance", fill = "")

## semdis human vs gpt-3
# hierarchical model comparing objects
semdis_lmer_fit <- lmer(semdis ~ human + object + (1|respondent_id), data = aut_semdis)
summary(semdis_lmer_fit)
car::S(semdis_lmer_fit)
car::Anova(semdis_lmer_fit)
# check with anova
semdis_aov_fit <- aov(semdis ~ human + object + Error(respondent_id), data = aut_semdis)
car::S(semdis_aov_fit)
# plot result
aut_semdis %>% group_by(object, human) %>% 
  summarise(semdis_score = mean(semdis), se = sd(semdis)/n()^0.5) %>% 
  ggplot(aes(object, semdis_score, fill = human)) + 
  geom_bar(stat = "identity", position = "dodge2") +
  geom_errorbar(aes(ymin = semdis_score-1.96*se, ymax = semdis_score+1.96*se), width=.5, position=position_dodge(.9)) +
  labs(x = "AUT object", y = "semantic distance", fill = "")

## semdis
# hierarchical model comparing experimental conditions in humans
semdis_lmer_fit <- lmer(semdis ~ exp_condition + (1|respondent_id), data = semdis_human)
summary(semdis_lmer_fit)
car::S(semdis_lmer_fit)
car::Anova(semdis_lmer_fit)
# check with anova
semdis_aov_fit <- aov(semdis ~ exp_condition + Error(respondent_id), data = semdis_human)
car::S(semdis_aov_fit)
# plot result
semdis_human %>% group_by(exp_condition) %>% 
  summarise(semdis_score = mean(semdis), se = sd(semdis)/n()^0.5) %>% 
  ggplot(aes(exp_condition, semdis_score)) + 
  geom_bar(stat = "identity", position = "dodge2") +
  geom_errorbar(aes(ymin = semdis_score-1.96*se, ymax = semdis_score+1.96*se), width=.5, position=position_dodge(.9)) +
  labs(x = "instruction examples condition", y = "semantic distance", fill = "")

## semdis
# hierarchical model comparing experimental conditions in gpt-3
semdis_lmer_fit <- lmer(semdis ~ exp_condition + (1|respondent_id), data = semdis_gpt3)
summary(semdis_lmer_fit)
car::S(semdis_lmer_fit)
car::Anova(semdis_lmer_fit)
# check with anova
semdis_aov_fit <- aov(semdis ~ exp_condition + Error(respondent_id), data = semdis_gpt3)
car::S(semdis_aov_fit)
# plot result
semdis_gpt3 %>% group_by(exp_condition) %>% 
  summarise(semdis_score = mean(semdis), se = sd(semdis)/n()^0.5) %>% 
  ggplot(aes(exp_condition, semdis_score)) + 
  geom_bar(stat = "identity", position = "dodge2") +
  geom_errorbar(aes(ymin = semdis_score-1.96*se, ymax = semdis_score+1.96*se), width=.5, position=position_dodge(.9)) +
  labs(x = "instruction examples condition", y = "semantic distance", fill = "")
