library(irr)
# read data
gpt3_aut_engine_scores_IS <- read.csv("../engine_comparison/data/gpt3_aut_engine_comparison_scored_IS.csv")
gpt3_aut_engine_scores_CS <- read.csv("../engine_comparison/data/gpt3_aut_engine_comparison_scored_CS.csv")

calc_icc_aut <- function(rater1, rater2) {
  dat <- data.frame(rater1, rater2)
  icc(dat, model="twoway", type="consistency", unit="single")
}

irr1 <- calc_icc_aut(gpt3_aut_engine_scores_IS$snapshot_rater01, gpt3_aut_engine_scores_IS$snapshot_rater02)
irr1

irr2 <- calc_icc_aut(gpt3_aut_engine_scores_CS$snapshot_rater01, gpt3_aut_engine_scores_IS$snapshot_rater02)
irr2

# check and change data types
gpt3_aut_engine_scores <- gpt3_aut_engine_scores_IS
str(gpt3_aut_engine_scores)
gpt3_aut_engine_scores$engine <- as.factor(gpt3_aut_engine_scores$engine)
gpt3_aut_engine_scores$instr_nr <- as.factor(gpt3_aut_engine_scores$instr_nr)

# plot relationships independents with instructions followed before dichtomozing it
plot(gpt3_aut_engine_scores$temperature, gpt3_aut_engine_scores$instructions_followed)
plot(gpt3_aut_engine_scores$instr_nr, gpt3_aut_engine_scores$instructions_followed)
plot(gpt3_aut_engine_scores$engine, gpt3_aut_engine_scores$instructions_followed)
# temperature of .6 seems to follow instructions best
# instr_nr and engine do not affect instructions followed

gpt3_aut_engine_scores$instructions_followed[gpt3_aut_engine_scores$instructions_followed == .5] <- 0

# plot relationships between vars
plot(gpt3_aut_engine_scores$temperature, gpt3_aut_engine_scores$snapshot_rater02)
plot(gpt3_aut_engine_scores$instr_nr, gpt3_aut_engine_scores$snapshot_rater02)
plot(gpt3_aut_engine_scores$engine, gpt3_aut_engine_scores$snapshot_rater02)
# instr_2 seems to lead to better crea scores
# davinci 2 seems to lead to better crea scores

#snapshot_rater01 ~ engine + temperature + instr_nr
lm_engine_comp_crea <- lm(snapshot_rater01 ~ engine + instr_nr + temperature, 
                          gpt3_aut_engine_scores)
summary(lm_engine_comp_crea)
plot(lm_engine_comp_crea)

glm_engine_comp_instr <- glm(instructions_followed ~ engine + instr_nr + temperature, 
                             data = gpt3_aut_engine_scores, 
                             family = "binomial")
summary(glm_engine_comp_instr)
