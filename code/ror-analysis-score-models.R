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
#              m2: student_t() on the signed deviation, among deviators
#                  only (the "magnitude" part)
#            Combine downstream the way u2s-analysis-priors.R combines
#            hu-part and main-part draws by hand:
#              E[deviation] = P(deviate) * E[deviation | deviate]
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
        file = here("code/fits/ror-deviate-m1"))

}

## 3 Model 2: signed magnitude of deviation, among deviators ----
## bounded +/- 0.5; student_t() for some robustness to the tails while
## we don't yet have a truncated family wired up in brms

#delete model if it exists
if (file.exists(here("code/fits/ror-magnitude-m1.rds"))) {
  file.remove(here("code/fits/ror-magnitude-m1.rds"))}

d1_dev <- d1 |> filter(deviated == 1)

if (FIT_MODELS) {

  m1_magnitude <-
    brm(data = d1_dev,
        family = student_t(),
        deviation ~ 1 + job + exp + (1 | cmte) + (1 | cid) + (1 | app),
        prior = c(prior(normal(0, 0.2), class = Intercept),
                  prior(normal(0, 0.2), class = b),
                  prior(exponential(1), class = sd),
                  prior(exponential(1), class = sigma)),
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        sample_prior = "yes",
        seed = 8253,
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
# - Write the combination step: E[deviation] = P(deviate) * E[deviation |
#   deviate], propagating full posterior uncertainty from both fits
#   (draws from m1_deviate and m1_magnitude, joined by posterior
#   iteration, not point estimates)
# - Once real fields are confirmed with CIHR (see writing/sim-data.qmd,
#   "Questions"), add applicant gender/career-stage to both formulas for
#   Aim 2, and re-simulate data/sim-deviation-data.csv accordingly
# - job/exp reference levels above (reviewer, med) are arbitrary
#   placeholders -- revisit once we know which contrasts we actually
#   want to report
