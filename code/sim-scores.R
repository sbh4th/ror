library(tidyverse)
library(faux)
library(modelsummary)
library(fixest)
library(tinytable)
library(truncnorm)
library(lme4)

# fake data to get structure correct
# 15 apps per committee, 24 members per committee. 
data_fake <- add_random(committee = 2, 
  application = 15, member = 24) |>
  
  # recode values for committee, application, and member
  add_between("committee", cmte = sprintf("%02d", 1:2)) |>
  add_between("application", app = 1:15) |>
  add_between("member", memno = sprintf("%02d", 1:24)) |>
  
  # create unique ID for each committee member
  mutate(cid = paste0(cmte, "_", memno)) |>
  
 
 
  # assign reviewers uniquely within each application
  group_by(cmte, app) %>%
  mutate(
    job = sample(c(rep("reviewer", 3), 
      rep("member", 21))),
  # add expertise for each member
    exp = sample(c(rep("high", 8), 
      rep("med", 12), rep("low", 4)))) %>%
  ungroup()

# Verify 3 reviewers for each application
data_fake |> 
  group_by(committee, application, job) |> 
  tally() |> 
  spread(job, n, fill = 0) |> 
  tt()

# Verify assignment for each member
data_fake |> 
  group_by(committee, memno, job) |> 
  tally() |> 
  spread(job, n, fill = 0) |> 
  tt()

# set seed for reproducibility
set.seed(4875)

# define parameters
cmte_n = 10    # number of committees
app_n = 15     # number of discussed applications
mem_n = 24      # number of committee members
b0 = 4.0        # intercept for average score
b1 = -2         # fixed effect of treated
b2 = -5         # fixed effect of time
b3 = -3         # fixed effect of treated x time (DD)
u0c_sd = 0.1    # random intercept SD for committee
u0m_sd = 0.3      # random intercept SD for members
u0a_sd = 0.4    # random intercept SD for applications
sigma_sd = 0.5   # error SD for overall scores
  
# set up data structure
data <- add_random(committee = cmte_n, 
  application = app_n, member = mem_n) |>
  
  # recode values for committee, application, and member
  add_between("committee", cmte = sprintf("%02d", 1:cmte_n)) |>
  add_between("application", app = 1:app_n) |>
  add_between("member", memno = sprintf("%02d", 1:mem_n)) |>
  
  # create unique ID for each committee member
  mutate(cid = paste0(cmte, "_", memno)) |>
 
  # assign reviewers uniquely within each application
  group_by(cmte, app) %>%
  mutate(
    job = sample(c(rep("reviewer", 3), rep("member", 21)))) %>%
  ungroup() |>
  
  # add random effects 
  add_ranef("cmte", u0c = u0c_sd) |>
  add_ranef("member", u0m = u0m_sd) |>
  add_ranef("application", u0a = u0a_sd) |>
  add_ranef(sigma = sigma_sd) |>
  
  # calculate outcome
  mutate(y = b0               # intercept
    # + (b1 * treated)          # fixed effect of treated
    # + (b2 * post)             # fixed effect of time
    # + (b3 * treated * post)   # fixed effect of treated x time
    + u0c + u0m + u0a + sigma,
    score = pmax(3.5, pmin(4.9, y)),
    ty = rtruncnorm(n(), 
      a = 3.5, b = 4.9, mean = b0 + u0c + u0m + u0a,
      sd = sigma))      # compound error term

# estimate on fake data to check parameters
mfake_re <- lmer(y ~ 1 + (1 | cmte) + (1 | memno) + 
  + (1 | app), data = data)
mfake_ret <- lmer(ty ~ 1 + (1 | cmte) + (1 | memno) + 
  + (1 | app), data = data)

mfake_fe <- lm(y ~ treated * post, data = data)
,
  clusters = village, se_type = "stata")

modelsummary(list("LMER" = mfake_re, "LM" = mfake_fe),
 vcov = c("iid", ~ village))

summary(mfake)




# Load required package
library(truncnorm)

# set seed for reproducibility
set.seed(4875)

# define parameters
cmte_n = 50      # number of committees
app_n = 15       # number of discussed applications
mem_n = 24       # number of committee members
b0 = 4.1         # intercept for average score
b1 = 0.3         # fixed effect of panelist vs. reviewer 
b2 = -0.2        # fixed effect of high expertise
b3 = 0.1         # fixed effect of low expertise
b4 = 0.2         # fixed effect of no expertise
u0c_sd = 0.1     # random intercept SD for committee
u0m_sd = 0.3     # random intercept SD for members
u0a_sd = 0.4     # random intercept SD for applications
sigma_sd = 0.5   # error SD for overall scores
score_min = 3.5  # lower bound for score
score_max = 4.9  # upper bound for score

# set up data structure
data2 <- add_random(committee = cmte_n, 
  application = app_n, member = mem_n) |>
  
  # recode values for committee, application, and member
  add_between("committee", 
    cmte = sprintf("%02d", 1:cmte_n)) |>
  add_between("application",
    app = 1:app_n) |>
  add_between("member", 
    memno = sprintf("%02d", 1:mem_n)) |>
  
  # create unique ID for each committee member
  mutate(cid = paste0(cmte, "_", memno)) |>
 
  # assign reviewers uniquely within each application
  group_by(cmte, app) |>
  mutate(
    job = sample(c(rep("reviewer", 3), 
      rep("panelist", 21))),
    # add expertise for each member
    exp = sample(c(rep("high", 6), 
      rep("med", 10), rep("low", 4),
      rep("none", 4)))) |>
  ungroup() |>
  
  # add indicators for reviewer, expertise
  mutate(
    panelist = if_else(job == "panelist", 1, 0),
    exp_high = if_else(exp == "high", 1, 0),
    exp_low = if_else(exp == "low", 1, 0),
    exp_none = if_else(exp == "none", 1, 0)
  ) |>
  
  # add random effects 
  add_ranef("cmte", u0c = u0c_sd) |>
  add_ranef("member", u0m = u0m_sd) |>
  add_ranef("application", u0a = u0a_sd) |>
  add_ranef(sigma = sigma_sd) |>

  # Compute score using a truncated normal distribution
  mutate(
    score = rtruncnorm(n(), a = score_min, b = score_max, 
      mean = b0 + u0c + u0m + u0a + 
        (b1 * panelist) + (b2 * exp_high) +
        (b3 * exp_low) + (b4 * exp_none), 
      sd = sigma_sd)
  ) |> 
  
  # drop intermediate variables
  select(-committee, -application, -member) #,
         #-u0c, -u0m, -u0a, -sigma)

# Check summary statistics
summary(data2$score)  # Should be between 3.5 and 4.9

# Preview first 10 rows
head(data2, 10)

