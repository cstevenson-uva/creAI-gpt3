### optimization round 1: valid dv
gpt3_optimize <- read.csv("data/220416_gpt3_aut_instr_param_optimization.csv")

# check and change data types
str(gpt3_optimize)
gpt3_optimize$instr_nr <- as.factor(gpt3_optimize$instr_nr)

# plot relationships independents on whether/not valid response was produced
plot(jitter(gpt3_optimize$temperature), jitter(gpt3_optimize$valid)) # unclear
plot(gpt3_optimize$instr_nr, jitter(gpt3_optimize$valid)) # no clear difference
plot(jitter(gpt3_optimize$frequency_penalty), jitter(gpt3_optimize$valid)) # no clear difference
plot(jitter(gpt3_optimize$presence_penalty), jitter(gpt3_optimize$valid)) # 1 clearly best presence penalty
plot(jitter(gpt3_optimize$presence_penalty * gpt3_optimize$temperature), jitter(gpt3_optimize$valid))

# dichotomize valid
gpt3_optimize$valid[gpt3_optimize$valid == .5] <- 0

# analyze which settings lead to valid responses
t.test(temperature ~ valid, gpt3_optimize)       # no clear effect

glm_optimize_valid_1 <- glm(valid ~ instr_nr + temperature + frequency_penalty + presence_penalty, 
                            data = gpt3_optimize, 
                            family = "binomial")
summary(glm_optimize_valid_1)
# variance inflation factor
car::vif(glm_optimize_valid_1)
# outliers, influential cases, cook's distance
plot(glm_optimize_valid_1, which=5)

glm_optimize_valid_2 <- glm(valid ~ instr_nr * temperature + frequency_penalty + presence_penalty, 
                            data = gpt3_optimize, 
                            family = "binomial")
summary(glm_optimize_valid_2)
anova(glm_optimize_valid_1, glm_optimize_valid_2, test = "Chisq") # model 2 better

glm_optimize_valid_2b <- glm(valid ~ instr_nr * temperature + presence_penalty, 
                             data = gpt3_optimize, 
                             family = "binomial")
summary(glm_optimize_valid_2b)
anova(glm_optimize_valid_2, glm_optimize_valid_2b, test = "Chisq") # we can drop freq penalty, doesn't affect model fit

glm_optimize_valid_3 <- glm(valid ~ instr_nr * temperature + presence_penalty * temperature + frequency_penalty, 
                            data = gpt3_optimize, 
                            family = "binomial")
summary(glm_optimize_valid_3)
anova(glm_optimize_valid_2, glm_optimize_valid_3, test = "Chisq") # no improvement with 3, stick with model 2

## investigate what interaction instr x temperature means
# create a new dataset to investigate and plot interaction
# best model: valid ~ instr_nr * temperature + presence_penalty
new_data <- data.frame(
  instr_nr = rep(c("instr_1", "instr_2"), each = 18, times = 2),
  temperature = rep(4:9, each = 3, times = 4),
  presence_penalty = rep(c(1, 1.5, 2), each = 1, times = 24)
  )
new_data$temperature <- new_data$temperature * .1
new_data$valid <- predict(glm_optimize_valid_2b, newdata = new_data, type = "response")
plot(jitter(gpt3_optimize$temperature), jitter(gpt3_optimize$valid))
points(jitter(new_data$temperature[new_data$instr_nr == "instr_1"]), jitter(new_data$valid[new_data$instr_nr == "instr_1"]), pch=19, col="blue")
points(jitter(new_data$temperature[new_data$instr_nr == "instr_2"]), jitter(new_data$valid[new_data$instr_nr == "instr_2"]), pch=19, col="orange")
# conclusion: as temperature rises, instruction 2 leads to more invalid responses

# create a csv file for snapshot scoring of relevant parameter settings
gpt3_optimize_score <- gpt3_optimize[gpt3_optimize$presence_penalty == 1,]
write.csv(gpt3_optimize[gpt3_optimize$presence_penalty == 1,],
          "data/gpt3_aut_instr_param_optimization_presence1.csv")

### optimization round 2 snapshot dv
gpt3_optimize <- read.csv("data/gpt3_aut_instr_param_optimization_presence1_scored.csv")

# check and change data types
str(gpt3_optimize)
gpt3_optimize$instr_nr <- as.factor(gpt3_optimize$instr_nr)
gpt3_optimize$engine <- as.factor(gpt3_optimize$engine)
gpt3_optimize$aut_object <- as.factor(gpt3_optimize$aut_object)

# plot relationships between vars
plot(jitter(gpt3_optimize$temperature), jitter(gpt3_optimize$snapshot_rater01))
plot(jitter(gpt3_optimize$frequency_penalty), jitter(gpt3_optimize$snapshot_rater01))
plot(gpt3_optimize$instr_nr, gpt3_optimize$snapshot_rater01)
# instr_2 seems to lead to better crea scores

#snapshot_rater01 ~ temperature + instr_nr + frequency_penalty
lm_optimize_1 <- lm(snapshot_rater01 ~ instr_nr + temperature + frequency_penalty, 
                          gpt3_optimize)
summary(lm_optimize_1)
plot(lm_optimize_1)

#snapshot_rater01 ~ temperature + instr_nr 
lm_optimize_2 <- lm(snapshot_rater01 ~ instr_nr + temperature, 
                    gpt3_optimize)
summary(lm_optimize_2)
plot(lm_optimize_2)
anova(lm_optimize_1, lm_optimize_2) 
# no improvement in model fit from 1 to 2, choose model 2 because simpler

#snapshot_rater01 ~ temperature + instr_nr 
lm_optimize_3 <- lm(snapshot_rater01 ~ instr_nr * temperature, 
                    gpt3_optimize)
summary(lm_optimize_3)
plot(lm_optimize_3)
anova(lm_optimize_2, lm_optimize_3) 
# no improvement in model fit from 2 to 3, choose model 2 because simpler

# examine how temp & freq penalty influence performance
instr2_data <- gpt3_optimize[gpt3_optimize$instr_nr == "instr_2",]
plot(jitter(instr2_data$temperature), jitter(instr2_data$snapshot_rater01))
plot(jitter(instr2_data$frequency_penalty), jitter(instr2_data$snapshot_rater01))
# conclusions so far: 
# instruction 2 is better
# frequency_penalty has no reliable effect -> choose 1 based on plot

# investigate performance differences for different objects
plot(instr2_data$aut_object, jitter(instr2_data$snapshot_rater01))
plot(instr2_data$aut_object, jitter(instr2_data$valid))
# brick had the most invalid
# fork and tin perform the best on snapshot
