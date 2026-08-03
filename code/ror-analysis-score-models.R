#  program:  ror-analysis-score-models.R
#  task:     set up (but do not fit) a two-part Bayesian model for how
#            panel members' final scores deviate from the pre-discussion
#            consensus score
#  input:    data/sim-deviation-data.csv (code/ror-sim-deviation.R)
#  output:   fits/ror-deviate-m1, fits/ror-magnitude-m1 (when FIT_MODELS
#            is switched on)
#  project:  RoR
#  author:   sam harper \ 2026-07-16
#
#  note:     brms has no native family for "point mass at an interior
#            value (0) + continuous elsewhere" the way hurdle_poisson()
#            handles zero counts, since the deviation outcome here is
#            signed and bounded (+/- 0.5), not non-negative. So this
#            splits the u2-sibs hu/main pattern into two linked brm()
#            calls instead of one bf(y ~ ..., hu ~ ...) call:
#              m1: bernoulli() -- did this member deviate from consensus
#                  at all? (the "hu" part, just fit as its own model)
#              m2: cumulative() ordinal model on the signed deviation,
#                  among deviators only (the "magnitude" part). CIHR
#                  scores are only entered to one decimal place, so a
#                  deviator's score can only depart from consensus by
#                  one of 10 discrete steps (+/-0.1 ... +/-0.5) -- this
#                  is genuinely ordinal/discrete data, not a continuous
#                  quantity with occasional extreme values, so a
#                  cumulative() logit model (flexible, non-equidistant
#                  thresholds) is a better match than treating it as
#                  continuous (e.g. student()/gaussian()): it respects
#                  the +/-0.5 bound without truncation hacks, and it
#                  doesn't assume the 10 steps are equally likely.
#            Combine downstream the way u2s-analysis-priors.R combines
#            hu-part and main-part draws by hand:
#              E[deviation] = P(deviate) * E[deviation | deviate]
#            where E[deviation | deviate] from the ordinal model is the
#            probability-weighted sum over the 10 category values, not
#            a linear prediction -- see TODO block below.
#            See PROJECT.md for the fuller rationale.
#
#  status:   DRAFT. FIT_MODELS is FALSE below -- this script defines the
#            data prep and model calls but does not run brm() yet. This
#            is deliberate: priors and formulas below are first guesses,
#            not reviewed, and the compute + cmdstan setup should be
#            confirmed as available in CIHR's execution environment
#            before we invest time fitting anything (see PROJECT.md,
#            "Where we left off").

##  0 Load needed packages ----
library(here)
library(tidyverse)
library(tidybayes)
library(brms)
library(cmdstanr)
library(marginaleffects)
library(bayesplot)

# Use the cmdstanr backend for Stan
# You need to install the cmdstanr package first
# (https://mc-stan.org/cmdstanr/) and then run cmdstanr::install_cmdstan()
# to install cmdstan on your computer.
options(mc.cores = 4,
        brms.backend = "cmdstanr")

# fits/ must exist before any brm(file = ...) call, or brms errors before
# it even checks the cache (same footgun noted in u2-sibs's README)
dir.create(here("code", "fits"), showWarnings = FALSE, recursive = TRUE)

# Set to TRUE once priors/formulas below have been reviewed and cmdstan
# is confirmed to work in the target execution environment.
FIT_MODELS <- FALSE

## 1 Read in simulated dataset ----

d <- read_csv(here("data", "sim-deviation-data.csv"),
  show_col_types = FALSE)

# Sanity checks on the simulated analytic sample
stopifnot(
  "expect 18,000 rows (50 cmte x 15 app x 24 member)" =
    nrow(d) == 18000,
  "deviated should be 0/1" =
    all(d$deviated %in% c(0, 1)),
  "score should stay within [3.5, 4.9]" =
    all(d$score >= 3.5 & d$score <= 4.9)
)

d1 <- d |>
  mutate(
    job      = factor(job, levels = c("reviewer", "panelist")),
    exp      = factor(exp, levels = c("med", "high", "low", "none")),
    deviated = factor(deviated, levels = c(0, 1))
  )

# the 10 discrete steps a deviator's score can take relative to consensus
# (+/-0.1 ... +/-0.5, in tenths -- CIHR scores have one decimal place).
# Matched against sprintf()-formatted strings rather than the raw doubles
# to sidestep floating-point equality issues between how the simulator
# rounds values and how a plain numeric factor() call would compare them.
dev_levels <- sprintf("%.1f", setdiff((-5:5) / 10, 0))

## 2 Model 1: did this member deviate from consensus at all? ----
## (bernoulli "any deviation" model -- the hu-equivalent part)

#delete model if it exists
if (file.exists(here("code/fits/ror-deviate-m1.rds"))) {
  file.remove(here("code/fits/ror-deviate-m1.rds"))}

if (FIT_MODELS) {

  m1_deviate <-
    brm(data = d1,
        family = bernoulli(),
        deviated ~ 1 + job + exp + (1 | cmte) + (1 | cid) + (1 | app),
        prior = c(prior(normal(0, 1.5), class = Intercept),  # bar alpha
                  prior(normal(0, 0.5), class = b),           # betas
                  prior(exponential(1), class = sd)),         # sigma
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        sample_prior = "yes",
        seed = 4102,
        control = list(adapt_delta = 0.95),
        file = here("code/fits/ror-deviate-m1"))

}

## 3 Model 2: signed magnitude of deviation, among deviators ----
## ordinal cumulative() model over the 10 discrete +/-0.1 ... +/-0.5
## steps -- see header note above for why this replaced student().
## Thresholds default to "flexible" (not "equidistant"), so the model
## does not assume the 10 steps are equally likely -- important given
## how concentrated the simulated deviations are near +/-0.1 (see
## PROJECT.md / session log).

#delete model if it exists
if (file.exists(here("code/fits/ror-magnitude-m1.rds"))) {
  file.remove(here("code/fits/ror-magnitude-m1.rds"))}

d1_dev <- d1 |>
  filter(deviated == 1) |>
  mutate(deviation = factor(sprintf("%.1f", deviation),
    levels = dev_levels, ordered = TRUE))

if (FIT_MODELS) {

  m1_magnitude <-
    brm(data = d1_dev,
        family = cumulative(link = "logit", threshold = "flexible"),
        deviation ~ 1 + job + exp + (1 | cmte) + (1 | cid) + (1 | app),
        prior = c(prior(normal(0, 1.5), class = Intercept),  # thresholds
                  prior(normal(0, 0.5), class = b),           # betas
                  prior(exponential(1), class = sd)),         # group SDs
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        sample_prior = "yes",
        seed = 8253,
        control = list(adapt_delta = 0.95),
        file = here("code/fits/ror-magnitude-m1"))

}

## 4 TODO before fitting for real ----
# - Confirm cmdstan can actually compile/run in CIHR's execution
#   environment (flagged as the biggest practical risk -- see PROJECT.md)
# - Prior-predictive check both models on simulated data before ever
#   setting FIT_MODELS <- TRUE (mirror u2s-analysis-priors.R)
# - Decide re_formula = NULL vs NA for the headline marginaleffects
#   estimate (NULL = conditional on these committees, NA = population-
#   average across committees) -- see PROJECT.md discussion
# - m1_magnitude is now ordinal (cumulative()), so E[deviation | deviate]
#   is NOT a linear prediction -- it's the probability-weighted sum over
#   the 10 category values (posterior_epred(..., dpar or category probs)
#   x dev_levels, summed per draw). marginaleffects/tidybayes can do this
#   but it needs an explicit custom contrast, not the default continuous
#   marginal-effect output.
# - Write the combination step: E[deviation] = P(deviate) * E[deviation |
#   deviate], propagating full posterior uncertainty from both fits
#   (draws from m1_deviate and m1_magnitude, joined by posterior
#   iteration, not point estimates)
# - Confirm with CIHR that real data actually lands on the same +/-0.1
#   ... +/-0.5 grid (dev_levels above) -- this is currently an assumption
#   carried over from the simulator's parameters, not confirmed with the
#   Funding Analytics Team
# - Once real fields are confirmed with CIHR (see writing/sim-data.qmd,
#   "Questions"), add applicant gender/career-stage to both formulas for
#   Aim 2, and re-simulate data/sim-deviation-data.csv accordingly
# - job/exp reference levels above (reviewer, med) are arbitrary
#   placeholders -- revisit once we know which contrasts we actually
#   want to report
