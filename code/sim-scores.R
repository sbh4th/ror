library(tidyverse)
library(faux)
library(modelsummary)
library(fixest)
library(tinytable)

data_fake <- add_random(committee = 2, 
  application = 15, member = 24) %>%
  # recode values
  add_between("committee", 
    cmte = sprintf("%02d", 1:2)) %>%
  add_between("application", app = 1:15) %>%
  add_between("member", 
    memno = sprintf("%02d", 1:24)) %>%
  # unique ID for each committee member
  mutate(cid = paste0(cmte, "_", memno)) %>%
  add_between("member", job = c("reviewer", "member"), 
              .prob = c(3, 21), .shuffle = TRUE) %>%
  add_between(c("committee", "application", "member"), rjob = c("reviewer", "member"), 
              .prob = c(3, 21), .shuffle = TRUE)
tt(data_fake)

df2 <- add_random(committee = 2, 
  application = 15) %>%
  add_within("application", score = 1:24) %>%
  add_between("score", job = c("rev", "mem"), 
    .prob = c(3, 21), .shuffle=TRUE)
%>%
  add_between(c("committee", "application", "member"), 
    job = c("rev", "mem"),
    .prob=c(3,21))
%>%

  
  add_between(c("committee", "application"), job = c("rev", "mem"),
    .prob=c(3,12)))

  add_("application", 
    job = c("reviewer", "member"),
    prob = c(3/15, 12/15), .shuffle = TRUE) 
%>%
tt(df2)

data <- add_random(subj = 10) %>%
  add_between("subj", 
              cond = c("control", "test"),
              age = c("young", "old"), 
              .prob = c(2, 3, 4, 1))

member = 3) %>%
  add_between("village", cond = c("control", "treated"),
    .prob = c(25, 25), .shuffle = TRUE) %>%
  add_recode("cond", "treated", control = 0, treated = 1) %>%
  add_between("wave", time = c("pre", "post1", "post2")) %>%
  mutate(post = ifelse(time == "pre", 0, 1))

# define parameters
cmte_n = 2    # number of committees
app_n = 15     # number of discussed applications
wave_n = 3      # number of committees
b0 = 120        # intercept
b1 = -2         # fixed effect of treated
b2 = -5         # fixed effect of time
b3 = -3         # fixed effect of treated x time (DD)
u0s_sd = 12     # random intercept SD for subjects
u0v_sd = 1      # random intercept SD for villages
sigma_sd = 12   # error SD
  
# set up data structure
data <- add_random(village = village_n, 
  subj = subj_n, wave = wave_n) %>%
  
  # add and recode categorical variables
  add_between("village", cond = c("control", "treated"),
    .prob = c(25, 25), .shuffle = TRUE) %>%
  add_recode("cond", "treated", control = 0, treated = 1) %>%
  add_between("wave", time = c("pre", "post1", "post2")) %>%
  mutate(post = ifelse(time == "pre", 0, 1)) %>%
  
  # add random effects 
  add_ranef("subj", u0s = u0s_sd) %>%
  add_ranef("village", u0v = u0v_sd) %>%
  add_ranef(sigma = sigma_sd) %>%
  
  # calculate outcome
  mutate(y = b0               # intercept
    + (b1 * treated)          # fixed effect of treated
    + (b2 * post)             # fixed effect of time
    + (b3 * treated * post)   # fixed effect of treated x time
    + u0s + u0v + sigma)      # compound error term

# estimate on fake data to check parameters
mfake_re <- lmer(y ~ treated * post + (1 | subj) 
  + (1 | village), data = data)

mfake_fe <- lm(y ~ treated * post, data = data)
,
  clusters = village, se_type = "stata")

modelsummary(list("LMER" = mfake_re, "LM" = mfake_fe),
 vcov = c("iid", ~ village))

summary(mfake)

# set up simulation function
sim <- function(
  # data structure
  village_n = 50, subj_n = 19, wave_n = 3,
  # fixed effects
  b0 = 0, b1 = 0, b2 = 0, b3 = 0,
  # random effects
  u0s_sd = 1, u0v_sd = 1, sigma_sd = 1,
  ... # helps the function work with pmap() below
                ) {

# set up data structure
data <- add_random(village = village_n, 
  subj = subj_n, wave = wave_n) %>%
  
  # add and recode categorical variables
  add_between("village", cond = c("control", "treated"),
    .prob = c(25, 25), .shuffle = TRUE) %>%
  add_recode("cond", "treated", control = 0, treated = 1) %>%
  add_between("wave", time = c("pre", "post1", "post2")) %>%
  mutate(post = ifelse(time == "pre", 0, 1)) %>%
  
  # add random effects 
  add_ranef("subj", u0s = u0s_sd) %>%
  add_ranef("village", u0v = u0v_sd) %>%
  add_ranef(sigma = sigma_sd) %>%
  
  # calculate outcome
  mutate(y = b0               # intercept
    + (b1 * treated)          # fixed effect of treated
    + (b2 * post)             # fixed effect of time
    + (b3 * treated * post)   # fixed effect of treated x time
    + u0s + u0v + sigma)      # compound error term

# estimate model
m <- fixest::feols(
  y ~ treated * post | village, data = data,
  notes = FALSE) # drop warnings about collinearity

tidy(m)
}

# check the function parameters for one simulation
# keeping basic data structure constant
# (50 villages, 19 subjects, 3 waves)
sim(b0 = 120, b1 = -2, b2 = -5, b3 = -3, 
  u0s_sd = 12, u0v_sd = 1, sigma_sd = 12)

# estimate over many simulated datasets
x <- crossing(
  rep = 1:100, # number of replicates
  # subj_n = 19, # range of subject N
  # village_n = 50, # fixed item N
  # wave_n = 3, # fixed wave N
  b0 = 120, # fixed intercept
  b1 = -2, # fixed treatment effect
  b2 = -5, # fixed time effect
  b3 = c(-1, -2.5, -5), # range of treatment effects
  u0s_sd = 12, # random intercept SD for subjects
  u0v_sd = 1, # random intercept SD for villages
  sigma_sd = 12, # error SD
) %>%
  mutate(analysis = pmap(., sim)) %>%
  unnest(analysis) %>%
  filter(term == "treated:post")


x %>% group_by(b3) %>%
  summarise(power = mean(p.value < .05), 
            .groups = "drop")