#  program:  ror-sim-streamlining-experiment.R
#  task:     EXPERIMENTAL. A more procedurally faithful streamlining DGP
#            for Aim 1, layered on top of the already-validated
#            ror-sim-aim1.R deviation-generating machinery (reused
#            unchanged for Stage 2 below). NOT part of the main Aim 1/2
#            pipeline -- this is a side-by-side check of whether the
#            added realism changes anything material for recoverability,
#            or whether the simpler rank/threshold-only approach in
#            ror-sim-aim1.R is a safe simplification.
#  input:    none (simulated from scratch)
#  output:   data/sim-streamlining-experiment.csv
#  project:  RoR
#  author:   sam harper \ 2026-08-11
#
#  note:     per Sam's account of the real CIHR procedure (2026-08-11,
#            drawing on his own committee/Scientific Officer experience):
#              1. each of the 3 assigned reviewers gives a score AND a
#                 separate categorical "top" (competitive) / "bottom"
#                 (not competitive) call -- correlated with their score,
#                 but not a deterministic function of it.
#              2. streamlining rule: an application is streamlined out
#                 (not discussed) iff >=1 reviewer called it "bottom"
#                 AND its mean-of-3 score ranks in the bottom 60% of
#                 *that committee's own* candidate pool (a relative/rank
#                 rule, not a fixed absolute score).
#            This exactly explains a pattern found in a hypothetical
#            dataset Sam constructed earlier the same day: two
#            applications with *identical* 3 reviewer scores had
#            different discussed/not-discussed outcomes, which a pure
#            score-threshold rule cannot produce but this mechanism can
#            (different top/bottom calls and/or different committee
#            rank context at the same raw score). A "bring back"
#            advocacy stage was tried on top of this and deliberately
#            dropped -- see the note where it used to live, in section 3
#            below, and ror-research-log.qmd.
#
#            All streamlining-specific parameters below (tb_center,
#            tb_slope, pool_per_cmte) are illustrative placeholders
#            invented to make the mechanism behave sensibly -- same
#            status as every other parameter in this project's
#            simulations, not estimates. streamline_rank_threshold =
#            0.60 is the one number here that's a stated real rule, not
#            a guess.

##  0 Load needed packages ----
library(here)
library(tidyverse)
library(faux)
library(truncnorm)

set.seed(20260811)

##  1 Define parameters ----

cmte_n = 50     # number of committees
mem_n  = 24     # number of committee members per committee

# EXPERIMENTAL vs. ror-sim-aim1.R: the candidate pool now stands in for
# an actual submission volume per committee, not "however many we need
# after a target-count filter" -- see writing/sim-data.qmd's own aside
# about up to ~50 applications for busier PH committees
pool_per_cmte = 40

b0         = 4.0    # intercept for application's true underlying quality
u0c_sd     = 0.1    # random intercept SD for committee (quality level)
u0a_sd     = 0.3    # random intercept SD for application (quality level)
init_sd_lo = 0.30   # reviewer-noise SD below the true-quality center
init_sd_hi = 0.15   # reviewer-noise SD above it (tighter) -- see
                     # ror-sim-aim1.R for the full split-normal rationale

# probability of *any* deviation from consensus (logit scale), and the
# signed magnitude given deviation -- identical to ror-sim-aim1.R
a0 = -0.8; a1 = 0.3; a2 = -0.4; a3 = 0.2; a4 = 0.5
dev_bias = 0; dev_sd = 0.15; dev_min = -0.5; dev_max = 0.5
u0m_bias_sd = 0.1

scale_min = 0     # true lower bound of the scoring scale
score_max = 4.9   # true upper bound of the scoring scale

round_tenth <- function(x) round(x * 10) / 10

# -- streamlining mechanism parameters (new to this script) --

# each reviewer's "top"/"bottom" call: p(bottom) = plogis(tb_slope *
# (tb_center - score)) -- tb_center is roughly where a reviewer is as
# likely to say "top" as "bottom"; tb_slope controls how tightly the
# call tracks their own score (higher = closer to deterministic)
tb_center = 3.9
tb_slope  = 6

# initial rule: streamlined out iff >=1 "bottom" call AND rank (of the
# mean of 3 scores, within this committee's own candidate pool) is at
# or below this percentile. This 0.60 is the one number here that's a
# stated real rule, not an invented placeholder.
streamline_rank_threshold = 0.60

##  2 Stage 1: candidate pool -- reviewer scores and top/bottom calls ----
## Mirrors ror-sim-aim1.R's committee/application/member setup and
## reviewer-score generation exactly (including the redraw-not-clamp
## handling of out-of-bounds scores); only the streamlining logic that
## follows in section 3 is new.

data <- add_random(committee = cmte_n,
  application = pool_per_cmte, member = mem_n) |>

  add_between("committee", cmte = sprintf("%02d", 1:cmte_n)) |>
  add_between("application", app = 1:pool_per_cmte) |>
  add_between("member", memno = sprintf("%02d", 1:mem_n)) |>

  mutate(cid = paste0(cmte, "_", memno)) |>

  add_ranef("cmte", u0c = u0c_sd) |>
  add_ranef("application", u0a = u0a_sd) |>

  group_by(cmte, app) |>
  mutate(
    job = sample(c(rep("reviewer", 3),
      rep("panelist", mem_n - 3))),
    exp = sample(c(rep("high", 6),
      rep("med", 10), rep("low", 4),
      rep("none", 4))),
    z_init = rnorm(n()),
    init_score = if_else(job == "reviewer",
      round_tenth(b0 + u0c + u0a +
        if_else(z_init < 0, z_init * init_sd_lo, z_init * init_sd_hi)),
      NA_real_)
  ) |>
  ungroup() |>
  select(-z_init)

# redraw any reviewer's init_score that landed outside the scale's true
# bounds, rather than clamping it -- identical mechanism to
# ror-sim-aim1.R
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

# each reviewer's separate top/bottom call, based on their now-finalized
# init_score -- correlated with score, not determined by it
data <- data |>
  mutate(
    p_bottom = plogis(tb_slope * (tb_center - init_score)),
    # rbinom() on the NA p_bottom values (panelist rows) warns even
    # though the result is discarded by the outer if_else() below --
    # substitute a dummy 0 there rather than let it warn every run
    top_bottom = if_else(job == "reviewer",
      if_else(rbinom(n(), 1, if_else(is.na(p_bottom), 0, p_bottom)) == 1,
        "bottom", "top"),
      NA_character_)
  ) |>
  select(-p_bottom)

##  3 Streamlining decision ----

# application-level summaries, broadcast back to every member-row
data <- data |>
  group_by(cmte, app) |>
  mutate(
    consensus = round_tenth(mean(init_score, na.rm = TRUE)),
    any_bottom = any(top_bottom == "bottom", na.rm = TRUE)
  ) |>
  ungroup()

# decide streamlining once per application (on distinct app-level rows,
# not the 24x-duplicated member-level rows -- rank has to be computed
# against other *applications*, not other rows), then join back
decision <- data |>
  distinct(cmte, app, consensus, any_bottom) |>
  group_by(cmte) |>
  mutate(rank_pct = percent_rank(consensus)) |>
  ungroup() |>
  mutate(
    discussed = !(any_bottom & (rank_pct <= streamline_rank_threshold))
  ) |>
  select(cmte, app, consensus, rank_pct, discussed)

stopifnot(
  "every committee should have at least 1 discussed application" =
    data |> left_join(decision |> select(-consensus), by = c("cmte", "app")) |>
      filter(discussed) |> distinct(cmte, app) |>
      count(cmte) |> pull(n) |> min() >= 1
)

data <- data |>
  left_join(decision |> select(-consensus), by = c("cmte", "app")) |>
  filter(discussed) |>

  mutate(
    panelist = if_else(job == "panelist", 1, 0),
    exp_high = if_else(exp == "high", 1, 0),
    exp_low  = if_else(exp == "low", 1, 0),
    exp_none = if_else(exp == "none", 1, 0)
  ) |>

  # member-level leniency/harshness trait -- identical mechanism to
  # ror-sim-aim1.R (add_ranef on cid, not faux's crossed "member" factor)
  add_ranef("cid", u0m_bias = u0m_bias_sd)

##  4 Two-part deviation from consensus ----
## Identical to ror-sim-aim1.R from here on -- this is the already-
## validated part of the pipeline, unchanged, just applied to whichever
## applications survived the (new) streamlining decision above.

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

##  5 Sanity checks ----

stopifnot(
  "row count should be a whole number of mem_n-sized application blocks" =
    nrow(data) %% mem_n == 0,
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
  "every retained application should actually be flagged discussed" =
    all(data$discussed),
  "deviated should be 0/1" =
    all(data$deviated %in% c(0, 1)),
  "deviation should be exactly 0 when deviated == 0" =
    all(data$deviation[data$deviated == 0] == 0),
  "score should stay within [scale_min, score_max]" =
    all(data$score >= scale_min & data$score <= score_max),
  "deviated == 1 rows should never have a zero deviation after rounding" =
    all(data$deviation[data$deviated == 1] != 0)
)

##  6 Empirical checks ----

n_candidates_total <- cmte_n * pool_per_cmte
n_discussed_total <- data |> distinct(cmte, app) |> nrow()

cat("=== streamlining mechanism ===\n")
cat("overall discussion rate:",
  round(n_discussed_total / n_candidates_total, 3), "\n")

per_cmte <- data |> distinct(cmte, app) |> count(cmte, name = "n_discussed")
cat("discussed applications per committee -- summary:\n")
print(summary(per_cmte$n_discussed))
cat("SD across committees:", round(sd(per_cmte$n_discussed), 2), "\n\n")

cat("=== can identical consensus scores still produce different outcomes? ===\n")
## uses `decision`, which covers the FULL candidate pool (both discussed
## and streamlined-out) -- `data` by this point only contains the
## discussed survivors, so checking against `data` alone could never
## find a not-discussed match
dupe_check <- decision |>
  group_by(consensus) |>
  filter(n_distinct(discussed) > 1) |>
  ungroup()
cat("distinct consensus values with both outcomes present:",
  n_distinct(dupe_check$consensus), "\n")
dupe_check |> arrange(consensus) |>
  select(cmte, app, consensus, rank_pct, discussed) |>
  slice_head(n = 8) |> print()
cat("\n")

cat("=== does job/exp on P(deviate) still recover cleanly despite the richer selection stage? ===\n")
m_check <- glm(deviated ~ job + exp, data = data, family = binomial())
print(summary(m_check)$coefficients)

cat("\n=== member-level heterogeneity still recoverable? ===\n")
suppressMessages(library(lme4))
m_member_check <- lmer(deviation ~ 1 + (1 | cid),
  data = data |> filter(deviated == 1))
print(VarCorr(m_member_check))

##  7 Write output ----

write_csv(data, here("data", "sim-streamlining-experiment.csv"))
