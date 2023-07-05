### compare human and gpt-3 performance on serial order effect
library(dplyr)
library(lme4)
library(ggplot2)

aut_dat <- read.csv("data_combined/aut_human_vs_gpt3.csv")

# create response order variable
aut_dat <- aut_dat %>%
  arrange(respondent_id, id) %>%
  group_by(respondent_id, object) %>% 
  mutate(response_order = row_number())

### test serial order effect
## for all objects
# hierarchical model
model_serialorder_allobjs <- lmer(originality ~ 1 + human * response_order + object
                             + (1|respondent_id), data=aut_dat)
summary(model_serialorder_allobjs)
car::S(model_serialorder_allobjs)
car::Anova(model_serialorder_allobjs)
# check with anova
aov_serialorder_allobjs <- aov(originality ~ human * response_order + object 
                               + Error(respondent_id), data = aut_dat)
car::S(aov_serialorder_allobjs)
# plot result
#options(repr.plot.width=12, repr.plot.height=8)
ggplot(aut_dat, aes(x = response_order, y = originality, colour = human)) + 
  geom_smooth(method = "lm", se = TRUE)

## per object
# select book data
book_dat <- aut_dat %>%
  filter(object == "book")
# run hierarchical model
model_serialorder_book <- lmer(originality ~ 1 + human * response_order
                                  + (1|respondent_id), data=book_dat)
summary(model_serialorder_book)
# plot
ggplot(book_dat, aes(x = response_order, y = originality, colour = human)) + 
  geom_smooth(method = "lm", se = TRUE)

# select fork data
fork_dat <- aut_dat %>%
  filter(object == "fork")
# run hierarchical model
model_serialorder_fork <- lmer(originality ~ 1 + human * response_order
                               + (1|respondent_id), data=fork_dat)
summary(model_serialorder_fork)
# plot
ggplot(fork_dat, aes(x = response_order, y = originality, colour = human)) + 
  geom_smooth(method = "lm", se = TRUE)

# select tin can data
tincan_dat <- aut_dat %>%
  filter(object == "tin can")
# run hierarchical model
model_serialorder_tincan <- lmer(originality ~ 1 + human * response_order
                               + (1|respondent_id), data=tincan_dat)
summary(model_serialorder_tincan)
# plot
ggplot(tincan_dat, aes(x = response_order, y = originality, colour = human)) + 
  geom_smooth(method = "lm", se = TRUE)

