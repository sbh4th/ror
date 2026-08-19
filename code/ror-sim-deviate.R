#  program:  ror-sim-deviate.R
#  task:     generating a more faithful streamlining DGP
#            for Aim 1
#  input:    none (simulated from scratch)
#  output:   data/sim-deviate.csv
#  project:  RoR
#  author:   sam harper \ 2026-08-19
#
#  note:       1. each of the 3 assigned reviewers gives a score AND a
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
#            tb_slope) are illustrative placeholders invented to make
#            the mechanism behave sensibly -- same status as every other
#            parameter in this project's simulations, not estimates.
#            streamline_rank_threshold = 0.60 is a stated real rule, not
#            a guess.
#
#            Committee-level application-pool size (2026-08-14): was a
#            fixed pool_per_cmte = 40 for every committee; now drawn per
#            committee from a beta distribution on CIHR's own stated
#            [20, 80] range (cihr-irsc.gc.ca/e/51315.html), instead of a
#            real basis that didn't exist when 2026-08-06 explicitly
#            decided to leave committee size fixed. The beta's shape
#            parameters are illustrative -- chosen to roughly match the
#            range/right-skew implied by a one-off back-calculation from
#            real CIHR funded-application counts (see
#            code/ror-cihr-committee-size-check.R and
#            ror-research-log.qmd), not fit to it or drawn from it at
#            runtime -- deliberately not making this script depend on
#            that dataset for anything beyond a rough ballpark.

##  0 Load needed packages ----
library(here)
library(tidyverse)
library(faux)
library(truncnorm)

set.seed(20260819)

##  1 Define parameters ----

cmte_n = 50     # number of committees
mem_n  = 24     # number of committee members per committee

# Per-committee pool_size is drawn (below, section 2) from a beta
# distribution scaled to CIHR's own stated 20-80 range.

pool_min = 20        # lower bound on applications reviewed per committee
pool_max = 80        # upper bound
pool_shape1 = 2.5    # rbeta() shape -- illustrative, not fit; chosen to
pool_shape2 = 4      # roughly match the mean/SD/right-skew of the
                     # funded-count back-calculation (mean ~42, SD ~12)

b0         = 4.0    # intercept for (discussed) application's true quality
u0c_sd     = 0.1    # random intercept SD for committee (quality level)
u0a_sd     = 0.3    # random intercept SD for application (quality level)

# longer tail for lower-ranked applications. init_sd_lo raised from 0.30
# to 1.1 (2026-08-14): among DISCUSSED applications specifically, the
# any_bottom-AND-rank streamlining rule (section 3) means a reviewer's
# very low individual score can only survive to discussion if the
# other 2 reviewers' scores are high enough to keep the consensus above
# the committee's 60th-percentile rank -- an inherently rare
# combination. At the original 0.30, that combination essentially never
# occurred (0 discussed reviewer-scores below 3.4), which was too tight
# given real reviewer-score data Sam has access to, where a meaningful
# share of discussed applications carry within-application reviewer
# disagreement of a point or more, and individual scores as low as the
# low 2s. Tried a small-probability "discordant outlier" mixture first,
# but even at implausibly extreme settings (30% of reviews, SD 1.3) it
# barely moved the count -- the real lever is just the overall low-tail
# SD, which lengthens the tail with only a modest effect on the bulk of
# the distribution. Calibrated to roughly match that real reference
# distribution's share of discussed reviewer-scores below 3.5 (~8%) and
# below 3.0 (~1.5%); see ror-research-log.qmd.
init_sd_lo = 1.1
init_sd_hi = 0.15   # reviewer-noise SD above it (tighter)

# probability of deviation from consensus (logit scale), and the
# signed magnitude given deviation
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
dev_sd   =  0.15
dev_min  = -0.5
dev_max  =  0.5

# between-member SD in habitual leniency/harshness -- some members
# consistently deviate a bit high, some a bit low, across every
# application they review, but the population average stays at
# dev_bias (0). Not a fixed direction for everyone (that would just be
# dev_bias != 0); this is heterogeneity *across* members. One draw per
# unique cid, not per raw member-slot.
u0m_bias_sd = 0.1

scale_min = 0     # lower bound of the scoring scale
score_max = 4.9   # upper bound of the scoring scale

round_tenth <- function(x) round(x * 10) / 10

# -- streamlining mechanism parameters --

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
## handling of out-of-bounds scores); the streamlining logic that
## follows in section 3, and the variable per-committee pool_size below,
## are new.
##
## The grid is built manually (reframe + cross_join) rather than via
## add_random(application = ...), because add_random()'s "application"
## factor would be fully crossed with committee (same pool_per_cmte
## levels recycled identically in every committee) even when pool sizes
## vary by committee -- and add_ranef() on that raw factor would give
## identical u0a draws to same-labeled applications across different
## committees. This is the same conflation bug found and fixed in
## ror-sim-aim1.R/ror-sim-aim2.R on 2026-08-14 (there for "member";
## fixed here by never creating the crossed factor in the first place
## for "application"), via an explicit unique `aid` composite key.

cmte_pool <- tibble(
  cmte = sprintf("%02d", 1:cmte_n),
  pool_size = round(pool_min +
    (pool_max - pool_min) * rbeta(cmte_n, pool_shape1, pool_shape2))
)

cmte_app <- cmte_pool |>
  reframe(app = seq_len(pool_size), .by = c(cmte, pool_size)) |>
  select(cmte, app)

data <- cmte_app |>
  cross_join(tibble(memno = sprintf("%02d", 1:mem_n))) |>
  mutate(
    cid = paste0(cmte, "_", memno),
    aid = paste0(cmte, "_", app)
  ) |>

  add_ranef("cmte", u0c = u0c_sd) |>
  add_ranef("aid", u0a = u0a_sd) |>

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

  # member-level leniency/harshness trait
  # add_ranef on cid, not faux's crossed "member" factor
  add_ranef("cid", u0m_bias = u0m_bias_sd)

##  4 Two-part deviation from consensus ----
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

zero_idx <- which(data$deviated == 1 & data$deviation == 0)
while (length(zero_idx) > 0) {
  data$deviation[zero_idx] <- round_tenth(
    rtruncnorm(length(zero_idx), a = dev_min, b = dev_max,
      mean = dev_bias + data$u0m_bias[zero_idx], sd = dev_sd))
  zero_idx <- which(data$deviated == 1 & data$deviation == 0)
}

data <- data |>
  mutate(
    score = round_tenth(pmax(scale_min, 
      pmin(score_max, consensus + deviation)))
  ) |>
  select(-u0c, -u0a, -u0m_bias, -p_dev)

##  5 Checks ----

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

n_candidates_total <- sum(cmte_pool$pool_size)
n_discussed_total <- data |> distinct(cmte, app) |> nrow()

cat("=== per-committee pool size (drawn from beta on CIHR's stated [20,80] range) ===\n")
print(summary(cmte_pool$pool_size))
cat("SD across committees:", round(sd(cmte_pool$pool_size), 2), "\n\n")

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

write_csv(data, here("data", "sim-deviate.csv"))
