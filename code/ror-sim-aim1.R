#  program:  ror-sim-aim1.R
#  task:     Aim 1 simulation. Extends the committee/application/member
#            simulation with a per-application consensus score and a
#            two-part (any-deviation + signed magnitude) process for how
#            each member's final score departs from consensus
#  input:    none (simulated from scratch)
#  output:   data/sim-data-aim1.csv
#  project:  RoR
#  author:   sam harper \ 2026-08-06

##  0 Load needed packages ----
library(here)
library(tidyverse)
library(faux)
library(truncnorm)

# set seed for reproducibility
set.seed(4138)

##  1 Define parameters ----

cmte_n   = 50     # number of committees
app_n    = 15     # number of discussed applications per committee
mem_n    = 24     # number of committee members per committee

# candidate applications generated per committee before the streamlining
# filter below is applied (~2x is a comfortable margin given the
# parameters here -- see the stopifnot() at the filter step, which fails
# loudly and specifically if this margin is ever too thin for whatever
# parameters end up being used)
app_n_candidates = app_n * 2

b0       = 4.1    # intercept for application's true underlying quality
u0c_sd   = 0.1    # random intercept SD for committee (quality level)
u0a_sd   = 0.3    # random intercept SD for application (quality level)

# consensus is not simulated directly -- it's the mean of the 3 assigned
# reviewers' own initial scores, each an independent noisy read of the
# application's true quality (b0 + u0c + u0a) before discussion. init_sd
# is how much reviewers initially disagree; a real negotiation is more
# textured than a plain average (e.g. a persuasive low scorer holding a
# "hard stop" a bit below where the other two land), but the mean is a
# reasonable simulation-level stand-in and guarantees, by construction,
# that consensus never falls outside the range of what the 3 reviewers
# actually said.
init_sd  = 0.4

# probability of *any* deviation from consensus (logit scale) --
# this is the "hurdle"-equivalent part
a0       = -0.8   # baseline logit prob. of deviating
a1       =  0.3   # panelist vs. reviewer
a2       = -0.4   # high expertise
a3       =  0.2   # low expertise
a4       =  0.5   # no expertise

# signed magnitude of deviation, given deviation occurs, truncated to
# +/- 0.5 (CIHR's stated bound on final vs. consensus score)
dev_bias = 0       # population-average bias: still none (see u0m_bias_sd
                   # below for between-member variation around this)
dev_sd   = 0.15
dev_min  = -0.5
dev_max  =  0.5

# between-member SD in habitual leniency/harshness -- some members
# consistently deviate a bit high, some a bit low, across every
# application they review, but the population average stays at
# dev_bias (0). Not a fixed direction for everyone (that would just be
# dev_bias != 0); this is heterogeneity *across* members. One draw per
# unique cid, not per raw member-slot -- see note where it's generated.
u0m_bias_sd = 0.1

# scale_min/score_max are the true, hard bounds of an individual score
# (0 "poor" to 4.9 "outstanding") -- they clamp individual init_score and
# final score values. score_min is a different kind of thing: it's the
# streamlining/discussion-eligibility threshold applied to the rounded
# *mean* of the 3 initial scores, not a floor on any individual score --
# individual reviewers can and do score below it (see the filter step in
# section 2). Both score_min and score_max double as the eligibility
# range for that filter, since 4.9 is the true ceiling either way.
scale_min = 0     # true lower bound of the scoring scale
score_min = 3.5   # streamlining/discussion-eligibility threshold (on the
                   # rounded mean of the 3 initial scores, not individual
                   # scores)
score_max = 4.9   # true upper bound of the scoring scale (and therefore
                   # also the eligibility ceiling)

# CIHR scores are entered to one decimal place only
round_tenth <- function(x) round(x * 10) / 10

##  2 Set up multilevel structure (mirrors writing/sim-data.qmd) ----

data <- add_random(committee = cmte_n,
  application = app_n_candidates, member = mem_n) |>

  add_between("committee", cmte = sprintf("%02d", 1:cmte_n)) |>
  add_between("application", app = 1:app_n_candidates) |>
  add_between("member", memno = sprintf("%02d", 1:mem_n)) |>

  mutate(cid = paste0(cmte, "_", memno)) |>

  # random effects for the application's *true quality* (committee +
  # application only) -- this is what the 3 assigned reviewers each
  # independently perceive, with their own noise, below. Not consensus
  # itself; consensus is derived from the reviewers' scores next.
  add_ranef("cmte", u0c = u0c_sd) |>
  add_ranef("application", u0a = u0a_sd) |>

  # assign reviewers uniquely within each application, have the 3
  # assign reviewers independently score it before discussion (noisy
  # reads of the true quality above), and set consensus to the mean of
  # those 3 initial scores -- guaranteed by construction to fall within
  # their range. rnorm(n(), ...) draws one value per row in the group
  # (24) even though only the 3 "reviewer" rows use one, via if_else --
  # simpler than hand-generating exactly 3, and each used draw is still
  # an independent N(0, init_sd) value. Individual init_score is clamped
  # only at the scale's true bounds (scale_min/score_max) -- a reviewer
  # is free to land below the score_min streamlining threshold; whether
  # the application clears that threshold is decided next, on the mean.
  group_by(cmte, app) |>
  mutate(
    job = sample(c(rep("reviewer", 3),
      rep("panelist", 21))),
    exp = sample(c(rep("high", 6),
      rep("med", 10), rep("low", 4),
      rep("none", 4))),
    init_score = if_else(job == "reviewer",
      round_tenth(pmax(scale_min, pmin(score_max,
        b0 + u0c + u0a + rnorm(n(), mean = 0, sd = init_sd)))),
      NA_real_),
    consensus = round_tenth(mean(init_score, na.rm = TRUE))
  ) |>
  ungroup()

# streamlining filter: an application is discussed if the rounded *mean*
# of its 3 initial scores clears score_min, even if one or more of the
# individual scores didn't -- e.g. [3.4, 4.1, 4.1] still discusses. Keep
# exactly app_n eligible candidates per committee; the stopifnot() below
# fails loudly (rather than slice_sample() erroring opaquely) if
# app_n_candidates wasn't a big enough pool for some committee.
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
    panelist = if_else(job == "panelist", 1, 0),
    exp_high = if_else(exp == "high", 1, 0),
    exp_low  = if_else(exp == "low", 1, 0),
    exp_none = if_else(exp == "none", 1, 0)
  ) |>

  # member-level leniency/harshness trait, one draw per unique cid (i.e.
  # per actual person, 50 x 24 = 1,200 of them). Deliberately add_ranef
  # on "cid", not faux's own "member" factor -- add_random() treats
  # member as fully crossed with committee (24 levels total, reused
  # identically across every committee), so add_ranef("member", ...)
  # would silently give the *same* draw to every committee's "member 07",
  # conflating 50 different people who happen to share a slot number.
  # cid (committee + slot) is the actual unique identity; add_ranef
  # handles an arbitrary existing column fine once pointed at it.
  add_ranef("cid", u0m_bias = u0m_bias_sd)

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

##  5 Empirical check: is member-level heterogeneity actually there? ----
## Same spirit as the Aim 1/2 job/exp/gender checks in the research log --
## confirms the simulator did what it was supposed to before any brms
## model gets built on top of it. lmer() decomposes the variance in
## deviation (among deviators) into a cid (member) component and
## residual; expect the cid-level SD to land near u0m_bias_sd (0.1),
## clearly nonzero, and well below dev_sd (0.15, the residual/within-
## member noise).

suppressMessages(library(lme4))
m_member_check <- lmer(deviation ~ 1 + (1 | cid),
  data = data |> filter(deviated == 1))
print(VarCorr(m_member_check))

##  6 Write output ----

write_csv(data, here("data", "sim-data-aim1.csv"))
