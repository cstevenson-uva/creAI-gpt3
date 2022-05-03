library(stringr)
library(dplyr)

## word and string counts for tin
human_tin_semdis <- read.csv("data_human/aut_tin_semdis_computed.csv")
human_tin_rater1 <- read.csv("data_human/CES_bp2016_tin_scored_rater01.csv")
human_tin_rater2 <- read.csv("data_human/CES_bp2016_tin_scored_rater02.csv")

tin <- human_tin_rater1 %>%
  left_join(human_tin_rater2) %>%
  mutate(id = floor(response_id)) %>%
  left_join(human_tin_semdis, by = "id") %>%
  mutate(str_count = str_count(response, '\\w+')) %>%
  add_count(respondent_id, name = "response_fluency")

# compute median response length to use for gpt-3
# remember to add fluency to total so that gpt-3 can number its responses
med_fluency <- median(tin$response_fluency)
med_response_length <- median(tin$str_count)
# gpt-3 length tin # 36
(med_fluency * med_response_length) + med_fluency
# gpt-3 runs approx number of respondents
length(unique(tin$respondent_id)) # 34

## word and string counts for fork
human_fork_semdis <- read.csv("data_human/aut_fork_semdis_computed.csv")
human_fork_rater1 <- read.csv("data_human/CES_bp2016_fork_scored_rater01.csv")
human_fork_rater2 <- read.csv("data_human/CES_bp2016_fork_scored_rater02.csv")

fork <- human_fork_rater1 %>%
  left_join(human_fork_rater2) %>%
  mutate(id = floor(response_id)) %>%
  left_join(human_fork_semdis, by = "id") %>%
  mutate(str_count = str_count(response, '\\w+')) %>%
  add_count(respondent_id, name = "response_fluency")

# compute median response length to use for gpt-3
# remember to add fluency to total so that gpt-3 can number its responses
med_fluency <- median(fork$response_fluency)
med_response_length <- median(fork$str_count)
# gpt-3 length fork # 24
(med_fluency * med_response_length) + med_fluency
# gpt-3 runs approx number of respondents
length(unique(fork$respondent_id)) # 35

## word and string counts for book
human_book_semdis <- read.csv("data_human/aut_book_semdis_computed.csv")
human_book_rater1 <- read.csv("data_human/CES_bp2016_book_scored_rater01.csv")
human_book_rater2 <- read.csv("data_human/CES_bp2016_book_scored_rater02.csv")

book <- human_book_rater1 %>%
  left_join(human_book_rater2) %>%
  mutate(id = floor(response_id)) %>%
  left_join(human_book_semdis, by = "id") %>%
  mutate(str_count = str_count(response, '\\w+')) %>%
  add_count(respondent_id, name = "response_fluency")

# compute median response length to use for gpt-3
# remember to add fluency to total so that gpt-3 can number its responses
med_fluency <- median(book$response_fluency)
med_response_length <- median(book$str_count)
# gpt-3 length book 40
(med_fluency * med_response_length) + med_fluency
# gpt-3 runs approx number of respondents
length(unique(book$respondent_id)) # 33
