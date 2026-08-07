#  program:  ror-sim-aim2.R
#  task:     extend the Aim 1 simulation (code/ror-sim-deviation.R) with
#            applicant-level gender/career stage, and bake in real,
#            detectable interaction effects for Aim 2 (does the impact of
#            reviewer engagement/expertise on scoring differ by applicant
#            characteristics?) to pressure-test against
#  input:    none (simulated from scratch; self-contained, does not
#            source ror-sim-deviation.R -- see note below)
#  output:   data/sim-aim2-data.csv
#  project:  RoR
#  author:   sam harper \ 2026-08-04
#
#  note:     per CIHR Funding Analytics' 2025-12-04 email (see
#            ror-research-log.qmd), applicant gender and career stage will
#            never be extractable, even as CIHR-generated dummy data --
#            they can only ever be touched by a CIHR analyst running our
#            code in-house. This simulation is therefore the only rehearsal
#            we get for that part of the pipeline, so the interaction
#            effects below are deliberately large enough to be cleanly
#            recoverable (see the empirical check at the bottom), NOT
#            estimates of real bias in CIHR review -- same illustrative-
#            placeholder status as every other parameter in this project's
#            simulations until real data says otherwise.
#
#            Self-contained rather than sourcing ror-sim-deviation.R,
#            matching this repo's existing convention of independently
#            runnable per-purpose scripts (see e.g. u2-sibs's
#            per-outcome analysis scripts) -- some duplication of the
#            committee/application/member setup is accepted deliberately.

##  0 Load needed packages ----
library(here)
library(tidyverse)
library(faux)
library(truncnorm)

# set seed for reproducibility
set.seed(9042)

##  1 Define parameters ----

cmte_n   = 50     # number of committees
app_n    = 15     # number of discussed applications per committee
mem_n    = 24     # number of committee members per committee

# candidate applications generated per committee before the streamlining
# filter below is applied -- see ror-sim-aim1.R for the rationale and the
# stopifnot() that fails loudly if this margin is ever too thin
app_n_candidates = app_n * 2

b0       = 4.1    # intercept for the application's true underlying quality
u0c_sd   = 0.1    # random intercept SD for committee (quality level)
u0a_sd   = 0.3    # random intercept SD for application (quality level)

# consensus is not simulated directly -- it's the mean of the 3 assigned
# reviewers' own initial scores around the true quality above.
# init_sd_lo/init_sd_hi: asymmetric (two-piece normal) reviewer noise --
# see ror-sim-aim1.R for the full rationale and calibration (2026-08-06)
init_sd_lo = 0.40   # SD below the true-quality center
init_sd_hi = 0.15   # SD above the true-quality center (tighter)

# probability of *any* deviation from consensus (logit scale) -- same
# Aim 1 main effects as ror-sim-deviation.R, plus applicant-level terms
a0       = -0.8   # baseline logit prob. of deviating
a1       =  0.3   # panelist vs. reviewer
a2       = -0.4   # high expertise
a3       =  0.2   # low expertise
a4       =  0.5   # no expertise

# applicant-level main effects (modest -- not the effects of interest)
g1       =  0.1   # applicant gender = female
c1       = -0.1   # applicant career stage = early

# Aim 2's actual target: does the reviewer-engagement / expertise effect
# on P(deviate) differ by applicant gender / career stage? Baked in large
# on purpose (see header note) so the empirical check below can confirm
# the two-part model machinery actually recovers a real interaction, not
# just a real main effect.
i_panelist_female    =  0.5  # job(panelist) x gender(female)
i_expnone_earlycareer =  0.6  # exp(none) x career_stage(early)

# signed magnitude of deviation, given deviation occurs -- deliberately
# left null for applicant-level terms, same logic as Aim 1's own
# empirical finding (ror-research-log.qmd, 2026-08-03): virtually all
# covariate signal lives in *whether* someone deviates, not in magnitude,
# because magnitude is symmetric around zero given deviation. Aim 2's
# signal goes entirely into p_dev below, consistent with that finding.
dev_bias = 0
dev_sd   = 0.15
dev_min  = -0.5
dev_max  =  0.5

# between-member SD in habitual leniency/harshness, same addition and
# same rationale as ror-sim-aim1.R (2026-08-05) -- some members
# consistently deviate a bit high or low across every application they
# review, population average stays at dev_bias (0). Independent
# parameter here since this script doesn't source Aim 1's.
u0m_bias_sd = 0.1

# scale_min/score_max are the true, hard bounds of an individual score;
# score_min is the streamlining/discussion-eligibility threshold on the
# rounded *mean* of the 3 initial scores, not a floor on any individual
# score -- see ror-sim-aim1.R for the full rationale
scale_min = 0     # true lower bound of the scoring scale
score_min = 3.5   # streamlining/discussion-eligibility threshold
score_max = 4.9   # true upper bound of the scoring scale (and therefore
                   # also the eligibility ceiling)

# CIHR scores are entered to one decimal place only
round_tenth <- function(x) round(x * 10) / 10

##  2 Set up multilevel structure (mirrors ror-sim-deviation.R) ----

data <- add_random(committee = cmte_n,
  application = app_n_candidates, member = mem_n) |>

  add_between("committee", cmte = sprintf("%02d", 1:cmte_n)) |>
  add_between("application", app = 1:app_n_candidates) |>
  add_between("member", memno = sprintf("%02d", 1:mem_n)) |>

  mutate(cid = paste0(cmte, "_", memno)) |>

  # random effects for the application's *true quality* (committee +
  # application only) -- see ror-sim-aim1.R; consensus is derived from
  # the reviewers' own initial scores next, not simulated directly.
  add_ranef("cmte", u0c = u0c_sd) |>
  add_ranef("application", u0a = u0a_sd) |>

  # assign reviewers uniquely within each application, and draw each of
  # the 3 reviewers' initial score as true quality + asymmetric
  # (split-normal) noise -- see ror-sim-aim1.R for the full rationale,
  # including why init_score is deliberately left unclamped here (the
  # redraw loop right after this block handles the scale's true bounds
  # properly instead of piling excess probability up at the edge)
  group_by(cmte, app) |>
  mutate(
    job = sample(c(rep("reviewer", 3),
      rep("panelist", 21))),
    exp = sample(c(rep("high", 6),
      rep("med", 10), rep("low", 4),
      rep("none", 4))),
    z_init = rnorm(n()),
    init_score = if_else(job == "reviewer",
      round_tenth(b0 + u0c + u0a +
        if_else(z_init < 0, z_init * init_sd_lo, z_init * init_sd_hi)),
      NA_real_),
    # applicant-level attributes: one draw per application, recycled
    # across all member-rows for that application (dplyr recycles a
    # length-1 mutate() result across the group)
    gender = sample(c("female", "male"), 1),
    career_stage = sample(c("early", "established"), 1)) |>
  ungroup() |>
  select(-z_init)

# redraw any reviewer's init_score that landed outside the scale's true
# bounds, rather than clamping it -- see ror-sim-aim1.R
out_idx <- which(!is.na(data$init_score) &
  (data$init_score < scale_min | data$init_score > score_max))
while (length(out_idx) > 0) {
  z <- rnorm(length(out_idx))
  noise <- if_else(z < 0, z * init_sd_lo, z * init_sd_hi)
  data$init_score[out_idx] <- round_tenth(
    b0 + data$u0c[out_idx] + data$u0a[out_idx] + noise)
  out_idx <- which(!is.na(data$init_score) &
    (data$init_score < scale_min | data$init_score > score_max))
}

# consensus is the mean of the 3 (now-finalized) initial reviewer scores
data <- data |>
  group_by(cmte, app) |>
  mutate(consensus = round_tenth(mean(init_score, na.rm = TRUE))) |>
  ungroup()

# streamlining filter -- see ror-sim-aim1.R for the full rationale: an
# application is discussed if the rounded mean of its 3 initial scores
# clears score_min, even if an individual reviewer's score didn't.
eligible <- data |>
  distinct(cmte, app, consensus) |>
  filter(consensus >= score_min & consensus <= score_max)

stopifnot(
  "app_n_candidates too small -- some committee has fewer than app_n eligible (consensus-in-range) candidates" =
    eligible |> count(cmte) |> pull(n) |> min() >= app_n
)

kept <- eligible |>
  group_by(cmte) |>
  slice_sample(n = app_n) |>
  ungroup() |>
  select(cmte, app)

data <- data |>
  semi_join(kept, by = c("cmte", "app")) |>

  mutate(
    panelist       = if_else(job == "panelist", 1, 0),
    exp_high       = if_else(exp == "high", 1, 0),
    exp_low        = if_else(exp == "low", 1, 0),
    exp_none       = if_else(exp == "none", 1, 0),
    female         = if_else(gender == "female", 1, 0),
    early_career   = if_else(career_stage == "early", 1, 0)
  ) |>

  # member-level leniency/harshness trait, one draw per unique cid --
  # see ror-sim-aim1.R for why this must be keyed to cid (add_ranef
  # handles an arbitrary existing column fine), not faux's own crossed
  # "member" factor
  add_ranef("cid", u0m_bias = u0m_bias_sd)

##  3 Two-part deviation from consensus ----
## Same discretization logic as ror-sim-deviation.R -- CIHR scores are
## entered to one decimal place, so deviation must land on a tenth, with
## rejection-sampling to avoid a "deviated == 1" row collapsing to zero.

data <- data |>
  mutate(
    p_dev = plogis(a0 + (a1 * panelist) + (a2 * exp_high) +
      (a3 * exp_low) + (a4 * exp_none) +
      (g1 * female) + (c1 * early_career) +
      (i_panelist_female * panelist * female) +
      (i_expnone_earlycareer * exp_none * early_career)),
    deviated = rbinom(n(), 1, p_dev),
    deviation = if_else(
      deviated == 1,
      round_tenth(rtruncnorm(n(), a = dev_min, b = dev_max,
        mean = dev_bias + u0m_bias, sd = dev_sd)),
      0)
  )

# redraw+round any deviated == 1 rows whose rounded deviation collapsed to 0
zero_idx <- which(data$deviated == 1 & data$deviation == 0)
while (length(zero_idx) > 0) {
  data$deviation[zero_idx] <- round_tenth(
    rtruncnorm(length(zero_idx), a = dev_min, b = dev_max,
      mean = dev_bias + data$u0m_bias[zero_idx], sd = dev_sd))
  zero_idx <- which(data$deviated == 1 & data$deviation == 0)
}

data <- data |>
  mutate(
    score = round_tenth(pmax(scale_min, pmin(score_max, consensus + deviation)))
  ) |>
  select(-committee, -application, -member, -u0c, -u0a, -u0m_bias, -p_dev)

##  4 Sanity checks ----

stopifnot(
  "expect cmte_n x app_n x mem_n rows" =
    nrow(data) == cmte_n * app_n * mem_n,
  "expect exactly 3 reviewers per committee-application" =
    data |> filter(job == "reviewer") |>
      count(cmte, app) |> pull(n) |> unique() == 3,
  "expect exactly 24 members per committee-application" =
    data |> count(cmte, app) |> pull(n) |> unique() == mem_n,
  "gender should be constant within each committee-application" =
    data |> group_by(cmte, app) |> summarise(n = n_distinct(gender)) |>
      pull(n) |> unique() == 1,
  "career_stage should be constant within each committee-application" =
    data |> group_by(cmte, app) |> summarise(n = n_distinct(career_stage)) |>
      pull(n) |> unique() == 1,
  "expect exactly 3 non-missing initial reviewer scores per application" =
    data |> filter(!is.na(init_score)) |>
      count(cmte, app) |> pull(n) |> unique() == 3,
  "consensus should never fall outside the range of the 3 reviewers' own initial scores" =
    data |> group_by(cmte, app) |>
      summarise(
        lo = min(init_score, na.rm = TRUE),
        hi = max(init_score, na.rm = TRUE),
        cons = first(consensus), .groups = "drop") |>
      summarise(ok = all(cons >= lo & cons <= hi)) |> pull(ok),
  "deviated should be 0/1" =
    all(data$deviated %in% c(0, 1)),
  "deviation should be exactly 0 when deviated == 0" =
    all(data$deviation[data$deviated == 0] == 0),
  "score should stay within [scale_min, score_max]" =
    all(data$score >= scale_min & data$score <= score_max),
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

##  5 Empirical check: are the baked-in interactions actually recoverable? ----
## Mirrors the Aim 1 check in ror-research-log.qmd (2026-08-03) -- confirms
## the simulator did what it was supposed to before any brms model gets
## built on top of it. Expect large, significant coefficients on the two
## interaction terms below; this is a check on the simulator, not a
## substantive result.

m_check <- glm(deviated ~ job * gender + exp * career_stage,
  data = data, family = binomial())
coefs <- summary(m_check)$coefficients
print(coefs[grepl(":", rownames(coefs)), ])

## Same check for the new member-level term (see ror-sim-aim1.R) --
## expect the cid-level SD near u0m_bias_sd (0.1), well below dev_sd
## (0.15, the residual/within-member noise)
suppressMessages(library(lme4))
m_member_check <- lmer(deviation ~ 1 + (1 | cid),
  data = data |> filter(deviated == 1))
print(VarCorr(m_member_check))

##  6 Write output ----

write_csv(data, here("data", "sim-aim2-data.csv"))
