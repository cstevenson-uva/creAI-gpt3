# script to merge gpt3 and human aut data
# currently only for cos dist analysis
library(dplyr)
library(stringr)
library(tidyr)

# read in cleaned data
gpt3_formatted_file <- readline(prompt = "Enter cleaned gpt3 aut filename incl path: ")
gpt3_cleaned <- read.csv(gpt3_formatted_file)
human_formatted_file <- readline(prompt = "Enter cleaned human aut filename incl path: ")
human_cleaned <- read.csv(human_formatted_file)
human_exp_cond_file <- readline(prompt = "Enter filename incl path mapping human ids to experimental condition: ")
human_exp_cond <- read.csv(human_exp_cond_file)

# output filename
merged_data_file <- readline(prompt = "Enter filename incl path to write merged data to: ")

gpt3 <- gpt3_cleaned %>%
  mutate(human = 0, user_id = gpt3_id) %>%
  select(user_id, temperature, human, exp_condition, 
         respondent_id, response_id, object,
         response, cos_dist_objc_resp) %>%
  mutate(user_id = as.factor(user_id),
         respondent_id = as.factor(respondent_id),
         response_id = as.factor(response_id),
         object = as.factor(object),
         exp_condition = as.factor(exp_condition),
         temperature = as.factor(temperature),
         human = as.factor(human))

human <- human_cleaned %>%
  left_join(human_exp_cond, by = "user_id") %>% 
  mutate(human = 1, temperature = NA) %>%
  select(user_id, temperature, human, exp_condition, 
         respondent_id, response_id, object,
         response, cos_dist_objc_resp) %>%
  mutate(user_id = as.factor(user_id),
         respondent_id = as.factor(respondent_id),
         response_id = as.factor(response_id),
         object = as.factor(object),
         exp_condition = as.factor(exp_condition),
         temperature = as.factor(temperature),
         human = as.factor(human))
  
aut <- gpt3 %>%
  add_row(human)

write.csv(aut, merged_data_file, 
          row.names = FALSE)
