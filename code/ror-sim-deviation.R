#  program:  ror-sim-deviation.R
#  task:     extend the committee/application/member simulation with a
#            per-application consensus score and a two-part (any-deviation
#            + signed magnitude) process for how each member's final score
#            departs from consensus
#  input:    none (simulated from scratch)
#  output:   data/sim-deviation-data.csv
#  project:  RoR
#  author:   sam harper \ 2026-07-16
#
#  note:     writing/sim-data.qmd simulates a final `score` per member
#            directly (panelist/expertise effects act on the score level).
#            It has no `consensus` field, so it can't support the
#            deviation-from-consensus outcome discussed for the Bayesian
#            models below. This script is additive -- it doesn't touch
#            sim-data.qmd or data/sim-data.csv -- and produces a separate
#            data/sim-deviation-data.csv for that purpose.
#
#            All effect sizes/probabilities below are illustrative
#            placeholders, not estimates from real data. Revisit once we
#            know what CIHR can actually extract (see "Questions" section
#            of writing/sim-data.qmd).

##  0 Load needed packages ----
library(here)
library(tidyverse)
library(faux)
library(truncnorm)

# set seed for reproducibility
set.seed(4875)

##  1 Define parameters ----

cmte_n   = 50     # number of committees
app_n    = 15     # number of discussed applications per committee
mem_n    = 24     # number of committee members per committee

b0       = 4.1    # intercept for consensus score
u0c_sd   = 0.1    # random intercept SD for committee (consensus level)
u0a_sd   = 0.3    # random intercept SD for application (consensus level)

# probability of *any* deviation from consensus (logit scale) --
# this is the "hu"-equivalent part
a0       = -0.8   # baseline logit prob. of deviating
a1       =  0.3   # panelist vs. reviewer
a2       = -0.4   # high expertise
a3       =  0.2   # low expertise
a4       =  0.5   # no expertise

# signed magnitude of deviation, given deviation occurs, truncated to
# +/- 0.5 (CIHR's stated bound on final vs. consensus score)
dev_bias = 0      # placeholder: no systematic direction yet
dev_sd   = 0.15
dev_min  = -0.5
dev_max  =  0.5

score_min = 3.5   # lower bound for overall score (streamlining cutoff)
score_max = 4.9   # upper bound for overall score

# CIHR scores are entered to one decimal place only
round_tenth <- function(x) round(x * 10) / 10

##  2 Set up multilevel structure (mirrors writing/sim-data.qmd) ----

data <- add_random(committee = cmte_n,
  application = app_n, member = mem_n) |>

  add_between("committee", cmte = sprintf("%02d", 1:cmte_n)) |>
  add_between("application", app = 1:app_n) |>
  add_between("member", memno = sprintf("%02d", 1:mem_n)) |>

  mutate(cid = paste0(cmte, "_", memno)) |>

  # assign reviewers uniquely within each application
  group_by(cmte, app) |>
  mutate(
    job = sample(c(rep("reviewer", 3),
      rep("panelist", 21))),
    exp = sample(c(rep("high", 6),
      rep("med", 10), rep("low", 4),
      rep("none", 4)))) |>
  ungroup() |>

  mutate(
    panelist = if_else(job == "panelist", 1, 0),
    exp_high = if_else(exp == "high", 1, 0),
    exp_low  = if_else(exp == "low", 1, 0),
    exp_none = if_else(exp == "none", 1, 0)
  ) |>

  # random effects for the *consensus* score (committee + application only
  # -- no member-level noise, since consensus is agreed pre-discussion)
  add_ranef("cmte", u0c = u0c_sd) |>
  add_ranef("application", u0a = u0a_sd) |>

  mutate(
    consensus = round_tenth(pmax(score_min, pmin(score_max, b0 + u0c + u0a)))
  )

##  3 Two-part deviation from consensus ----
## CIHR scores (both consensus and individual final scores) can only be
## entered to one decimal place, so deviation -- and hence score -- must
## also land on a tenth. A raw truncnorm() draw is continuous, so we round
## it and then redraw any deviated == 1 case that rounds to exactly 0 (a
## "deviator" can't end up with a final score identical to consensus).

data <- data |>
  mutate(
    p_dev = plogis(a0 + (a1 * panelist) + (a2 * exp_high) +
      (a3 * exp_low) + (a4 * exp_none)),
    deviated = rbinom(n(), 1, p_dev),
    deviation = if_else(
      deviated == 1,
      round_tenth(rtruncnorm(n(), a = dev_min, b = dev_max,
        mean = dev_bias, sd = dev_sd)),
      0)
  )

# redraw+round any deviated == 1 rows whose rounded deviation collapsed to 0
zero_idx <- which(data$deviated == 1 & data$deviation == 0)
while (length(zero_idx) > 0) {
  data$deviation[zero_idx] <- round_tenth(
    rtruncnorm(length(zero_idx), a = dev_min, b = dev_max,
      mean = dev_bias, sd = dev_sd))
  zero_idx <- which(data$deviated == 1 & data$deviation == 0)
}

data <- data |>
  mutate(
    score = round_tenth(pmax(score_min, pmin(score_max, consensus + deviation)))
  ) |>
  select(-committee, -application, -member, -u0c, -u0a, -p_dev)

##  4 Sanity checks ----

stopifnot(
  "expect cmte_n x app_n x mem_n rows" =
    nrow(data) == cmte_n * app_n * mem_n,
  "expect exactly 3 reviewers per committee-application" =
    data |> filter(job == "reviewer") |>
      count(cmte, app) |> pull(n) |> unique() == 3,
  "expect exactly 24 members per committee-application" =
    data |> count(cmte, app) |> pull(n) |> unique() == mem_n,
  "deviated should be 0/1" =
    all(data$deviated %in% c(0, 1)),
  "deviation should be exactly 0 when deviated == 0" =
    all(data$deviation[data$deviated == 0] == 0),
  "score should stay within [score_min, score_max]" =
    all(data$score >= score_min & data$score <= score_max),
  "consensus should stay within [score_min, score_max]" =
    all(data$consensus >= score_min & data$consensus <= score_max),
  "consensus should be rounded to the nearest tenth" =
    all(abs(data$consensus * 10 - round(data$consensus * 10)) < 1e-8),
  "deviation should be rounded to the nearest tenth" =
    all(abs(data$deviation * 10 - round(data$deviation * 10)) < 1e-8),
  "score should be rounded to the nearest tenth" =
    all(abs(data$score * 10 - round(data$score * 10)) < 1e-8),
  "deviated == 1 rows should never have a zero deviation after rounding" =
    all(data$deviation[data$deviated == 1] != 0)
)

##  5 Write output ----

write_csv(data, here("data", "sim-deviation-data.csv"))
