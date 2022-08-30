# gpt3 aut data cleaning script
library(dplyr)
library(stringr)
library(tidyr)

# read in cleaned data
aut_formatted_file <- readline(prompt = "Enter formatted gpt3 aut filename incl path: ")
aut <- read.csv(aut_formatted_file)

# filename to write output to
aut_clean_file <- readline(prompt = "Enter filename incl path to write cleaned data to: ")

# add research, run and responseids
aut <- aut %>%
  mutate(research_id = "GPT3202208", 
         respondent_id = 202208000 + id, 
         response_id = 2022080000 + row_number())

aut_clean1 <- aut %>%
  # drop unnecessary columns
  select(research_id, gpt3_id, temperature, respondent_id, response_id, 
         instr_cond, aut_object, gpt3_response) %>%
  rename(object = aut_object, 
         response = gpt3_response,
         exp_condition = instr_cond)

# search & replace the strings with ""
str_replaces <- c(
  "beused" = "be used",
  "Atin" = "A tin",
  "Abook" = "A book",
  "Afork" = "A fork",
  "thefork" = "the fork",
  "A book can be used as a " = "",
  "a book can be used as a " = "",
  "A book can be used to " = "",
  "a book can be used to " = "",
  "A book can be " = "",
  "a book can be " = "",
  "A book can " = "",
  "A book " = "",
  "a book can " = "",
  "a book " = "",
  "a book can be used to make a " = "",
  "books can be used as " = "",
  "A book makes a " = "",
  "Turn a book into a " = "",
  "Books can be " = "",
  "You can use a book to " = "",
  "Use the fork as a " = "",
  "Use the fork to " = "",
  "use the tines of a fork to " = "",
  "use the tines of the fork to " = "",
  "use the tines to " = "",
  "Use a fork as a " = "",
  "Use a fork as " = "",
  "Use a fork to " = "",
  "A fork can be used as a " = "",
  "A fork can be used to " = "",
  "A fork can be " = "",
  "A fork " = "",
  "A fork can " = "",
  "A fork makes a " = "",
  "Use forks as " = "",
  "Turn a fork into a " = "",
  "Forks can be " = "",
  "You can use a fork to " = "",
  "Turn an old fork into a " = "",
  "use a largefork as a " = "",
  "A tin can can be used as a " = "",
  "A tin can can be used to " = "",
  "A tin can can be " = "",
  "A tin can " = "",
  "A tin can can " = "",
  "A tin can makes a " = "",
  "Turn a tin can into a " = "",
  "Turn an old tin can into a " = "",
  "Turn a small tin can into a " = "",
  "Turn a tin can into an " = "",
  "Turn a tin can into " = "",
  "Tin cans can be " = "",
  "Use tin cans as " = "",
  "Use tin cans to " = "",
  "Use a tin can as a " = "",
  "Use a large tin can as a " = "",
  "One could use a tin can to make " = "",
  "You can use a book to " = "",
  "It could be used as " = "",
  "It could be used to " = "",
  "Turn into a " = "",
  "Turn it into a " = "",
  "turned into " = "",
  "To make a " = "",
  "To use a " = "",
  "To make " = "",
  "also be used in " = "",
  "also be used to " = "",
  "also function as " = "",
  "can also be used " = "",
  "can also function as " = "",
  "could be used as a " = "",
  "might be used as a " = "",
  "might also be used as " = "",
  "could be used as " = "",
  "could be used to " = "", 
  "could be used to make a " = "",
  "could also be used to " = "",
  "be used as a " = "",
  "be used to " = "",
  "Use it as a " = "",
  "Use it as " = "",
  "Use it to " = "",
  "Use it " = "",
  "use it as " = "",
  "use it to " = "",
  "use it " = "",
  "Use as a " =  "",
  "Use to " =  "",
  "use as a " =  "",
  "use to " =  "",
  "Used as a " = "",
  "Used as " = "", 
  "Used to " = "",  
  "used as a " = "",
  "used as " = "",
  "Using as " = "",
  "can serve as " = "",
  "Can be used as a " = "",
  "can act as a " = "",
  "can beused to make a " = "",
  "Can be " = "",
  "can be " = "",
  "could be " = "",
  "Create a " = "",
  "create a " = "",
  "Make a " = "",
  "make a " = "",
  "Make " = "",
  "make " = "",
  "To " = "",
  "A " = "",
  "to " = "",
  "a " = "",
  "As a " = "",
  "as a" = "",
  "As " = "",
  "as " = "",
  "Used " = "",
  "Use " = "",
  "used " = "",
  "use " = "",
  "an" = "",
  "An" = ""
)

aut_clean2 <- aut_clean1 %>%
  # replace all strings in list above
  mutate(response = str_replace_all(response, str_replaces)) %>%
  # remove any extra whitespace at start middle or end of string
  mutate(response = str_squish(response)) %>%
  # make empty strings NA
  mutate(response = na_if(response, "")) %>%
  # drop rows with NA in gpt_response, i.e. empty responses
  drop_na(response)

write.csv(aut_clean2, aut_clean_file)
