# gpt3 aut data cleaning script
library(dplyr)
library(stringr)
library(tidyr)

# read in cleaned data
aut_formatted_file <- readline(prompt = "Enter formatted gpt3 aut filename incl path: ")
aut <- read.csv(aut_formatted_file)

# filename to write output to
aut_clean_file <- readline(prompt = "Enter filename incl path to write cleaned data to: ")

aut_clean <- aut %>%
  # drop unnecessary columns
  select(id, gpt3_id, temperature, aut_object, gpt3_response)

# search & replace the strings with ""
str_replaces <- c(
  "A book can be used as a " = "",
  "A book can be used as a " = "",
  "A book can be used as a " = "",
  "A book can be used to " = "",
  "A book can be " = "",
  "A book " = "",
  "A book can " = "",
  "A book makes a " = "",
  "Turn a book into a " = "",
  "Books can be " = "",
  "You can use a book to " = "",
  "A fork can be used as a " = "",
  "A fork can be used to " = "",
  "A fork can be " = "",
  "A fork " = "",
  "A fork can " = "",
  "A fork makes a " = "",
  "Turn a fork into a " = "",
  "Forks can be " = "",
  "You can use a fork to " = "",
  "A tin can can be used as a " = "",
  "A tin can can be used to " = "",
  "A tin can can be " = "",
  "A tin can " = "",
  "A tin can can " = "",
  "A tin can makes a " = "",
  "Turn a tin can into a " = "",
  "Tin cans can be " = "",
  "You can use a book to " = "",
  "It could be used as " = "",
  "It could be used to " = "",
  "To make a " = "",
  "To use a " = "",
  "To make " = "",
  "To " = "",
  "A " = ""
)

aut_clean <- aut_clean %>%
  # replace all strings in list above
  mutate(gpt3_response = str_replace_all(gpt3_response, str_replaces)) %>%
  # remove any extra whitespace at start middle or end of string
  mutate(gpt3_response = str_squish(gpt3_response)) %>%
  # make empty strings NA
  mutate(gpt3_response = na_if(gpt3_response, "")) %>%
  # drop rows with NA in gpt_response, i.e. empty responses
  drop_na(gpt3_response)

write.csv(aut_clean, aut_clean_file)
