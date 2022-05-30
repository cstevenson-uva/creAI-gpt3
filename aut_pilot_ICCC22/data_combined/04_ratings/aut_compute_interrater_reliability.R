# compute interrater reliability

# import libraries
library(dplyr)
library(tidyr)
library(irr)
library(psych)

# import data
irr_dir <- "~/Research/creAI-gpt3/aut_pilot_ICCC22/data_combined/04_ratings/"
rater1 <- read.csv("aut_ratings_IRR_MB.csv") 
rater2 <- read.csv2("aut_ratings_IRR_Iris.csv") 

# drop rater 2 data from rater 1 file and vice versa to create two datasets we can merge
# also replace 99 coding for uninterpretable responses to 0 coding to be consistent with 
# invalid coding, the invalid responses will be dropped from analyses

rater1 <- rater1 %>%
  select(id, response_id, object, response, 
         originality_rater01, utility_rater01, surprise_rater01) %>%
  mutate(originality_rater01 = replace(originality_rater01, originality_rater01 == 99, 0)) %>%
  mutate(utility_rater01 = replace(utility_rater01, utility_rater01 == 99, 0)) %>%
  mutate(surprise_rater01 = replace(surprise_rater01, surprise_rater01 == 99, 0))
rater2 <- rater2 %>%
  select(id, response_id, object, response, 
         originality_rater02, utility_rater02, surprise_rater02) %>%
  mutate(originality_rater02 = replace(originality_rater02, originality_rater02 == 99, 0)) %>%
  mutate(utility_rater02 = replace(utility_rater02, utility_rater02 == 99, 0)) %>%
  mutate(surprise_rater02 = replace(surprise_rater02, surprise_rater02 == 99, 0))
irr_dat <- rater1 %>%
  left_join(rater2) %>%
  drop_na(originality_rater01, utility_rater01, surprise_rater01,
          originality_rater02, utility_rater02, surprise_rater02) 
irr_dat_valid <- rater1 %>%
  left_join(rater2) %>%
  drop_na(originality_rater01, utility_rater01, surprise_rater01,
          originality_rater02, utility_rater02, surprise_rater02) %>%
  filter(!(originality_rater01 == 0 & utility_rater01 == 0 & surprise_rater01 == 0)) %>%
  filter(!(originality_rater02 == 0 & utility_rater02 == 0 & surprise_rater02 == 0))

# function to compute interrater reliability for aut
# for the AUT we assume subjects as random effects, we are interested in
# "consistency" rather than mean ratings (i.e., not "agreement") 
# the units of analysis are the "single" scores at response level
calc_icc_aut <- function(rater1, rater2) {
  dat <- data.frame(rater1, rater2)
  icc(dat, model="twoway", type="consistency", unit="single")
}

irr_orig <- calc_icc_aut(irr_dat$originality_rater01,
                         irr_dat$originality_rater02)
irr_util <- calc_icc_aut(irr_dat$utility_rater01,
                         irr_dat$utility_rater02)
irr_surp <- calc_icc_aut(irr_dat$surprise_rater01,
                         irr_dat$surprise_rater02)
irr_orig$value # fair
irr_util$value # good
irr_surp$value # good

irr_orig_valid <- calc_icc_aut(irr_dat_valid$originality_rater01,
                         irr_dat_valid$originality_rater02)
irr_util_valid <- calc_icc_aut(irr_dat_valid$utility_rater01,
                         irr_dat_valid$utility_rater02)
irr_surp_valid <- calc_icc_aut(irr_dat_valid$surprise_rater01,
                         irr_dat_valid$surprise_rater02)
irr_orig_valid$value # good
irr_util_valid$value # excellent
irr_surp_valid$value # excellent

### NOTE ICC interpretation
# from wikipedia: Cicchetti (1994) gives the following 
# guidelines to interpret ICC inter-rater agreement: 
# <0.40 is poor, 0.40-0.59 is fair, 0.60-0.74 is good, and
# 0.75-1.00 is excellent.
