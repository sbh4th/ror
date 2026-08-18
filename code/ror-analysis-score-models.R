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
#            data prep and model calls but does not run brm() yet).

##  0 Load needed packages ----
library(here)
library(tidyverse)
library(tidybayes)
library(brms)
library(cmdstanr)
library(marginaleffects)
library(bayesplot)
library(tinytable)
library(patchwork)

# Use the cmdstanr backend for Stan
# You need to install the cmdstanr package first
# (https://mc-stan.org/cmdstanr/) and then run cmdstanr::install_cmdstan()
# to install cmdstan on your computer.
options(mc.cores = 4,
        brms.backend = "cmdstanr")

# fits/ must exist before any brm(file = ...) call, or brms errors before
# it even checks the cache (same footgun noted in u2-sibs's README)
dir.create(here("code", "fits"), showWarnings = FALSE, recursive = TRUE)

## 1 Read in simulated dataset ----

d <- read_csv(here("data", "sim-data-aim1.csv"),
  show_col_types = FALSE)

# Sanity checks on the simulated analytic sample
stopifnot(
  "expect 18,000 rows (50 cmte x 15 app x 24 member)" =
    nrow(d) == 18000,
  "deviated should be 0/1" =
    all(d$deviated %in% c(0, 1)),
  "score should stay within [3.5, 4.9]" =
    all(d$consensus >= 3.5 & d$score <= 4.9)
)

d1 <- d |>
  mutate(
    job      = factor(job, levels = c("reviewer", "panelist")),
    exp      = factor(exp, levels = c("med", "high", "low", "none")),
    deviated = factor(deviated, levels = c(0, 1)),
    # data/sim-data-aim1.csv only saves the raw `app` label (recycled
    # across committees), not a unique per-committee-application
    # identifier -- same crossing issue diagnosed and fixed in the
    # simulation scripts on 2026-08-14 (see ror-research-log.qmd),
    # just discovered here too once the modeling-strategy table grew a
    # "Truth" column and it was obvious the Application random effect
    # wasn't recovering. `(1 | app)` below would pool together
    # unrelated applications from different committees that happen to
    # share a label; group on this composite instead.
    aid      = paste0(cmte, "_", app)
  )

# the 10 discrete steps a deviator's score can take relative to consensus
# (+/-0.1 ... +/-0.5, in tenths -- CIHR scores have one decimal place).
# Matched against sprintf()-formatted strings rather than the raw doubles
# to sidestep floating-point equality issues between how the simulator
# rounds values and how a plain numeric factor() call would compare them.
dev_levels <- sprintf("%.1f", setdiff((-5:5) / 10, 0))

## 2 Priors for Model 1

check_prior <- function(n = 4000, 
  sd_intercept = 1.5, sd_b = 0.5) {
  tibble(
    Intercept = rnorm(n, 0, sd_intercept),
    b         = rnorm(n, 0, sd_b)
  ) |>
    mutate(
      p1_s   = plogis(Intercept),
      p2_s   = plogis(Intercept + b),
      diff_s = p2_s - p1_s
    )
}

check_prior(sd_intercept = 1.5, sd_b = 0.5) |>
  ggplot(aes(diff_s)) + geom_density()

scenarios <- list(
  "SD = 0.5" = list(sd_intercept = 0.5, sd_b = 0.5),
  "SD = 1.0" = list(sd_intercept = 1.0, sd_b = 1.0),
  "SD = 1.5" = list(sd_intercept = 1.5, sd_b = 1.5)
)

pr_int <- map_dfr(scenarios, ~check_prior(
  sd_intercept = .x$sd_intercept, sd_b = .x$sd_b),
        .id = "scenario") |>
  ggplot(aes(p1_s, color = scenario)) + geom_density() +
  labs(x = "Probability of deviating (baseline)", y = NULL,
       title = "Prior for baseline P(deviate)") +
  theme_minimal()

pr_b <- map_dfr(scenarios, ~check_prior(
  sd_intercept = .x$sd_intercept, sd_b = .x$sd_b),
  .id = "scenario") |>
  ggplot(aes(diff_s, color = scenario)) + geom_density() +
  labs(x = "Difference in P(deviate)", y = NULL,
       title = "Prior for treatment effect") +
  theme_minimal()

pr_int / pr_b

## Overall looks like SD of 1.0 for the intercept and 
## 0.5 for the treatment effect seem reasonable

## 2 Model 1: did this member deviate from consensus at all? ----
## (bernoulli "any deviation" model -- the hu-equivalent part)

#delete model if it exists
if (file.exists(here("code/fits/ror-deviate-m1.rds"))) {
  file.remove(here("code/fits/ror-deviate-m1.rds"))}

  m1_deviate <-
    brm(data = d1,
        family = bernoulli(),
        deviated ~ 1 + job + exp + (1 | cmte) + (1 | cid) + (1 | aid),
        prior = c(prior(normal(0, 1.0), class = Intercept),   # bar alpha
                  prior(normal(0, 0.5), class = b),           # betas
                  prior(exponential(1), class = sd)),         # sigma
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        sample_prior = "yes",
        seed = 4102,
        control = list(adapt_delta = 0.95),
        file = here("code/fits/ror-deviate-m1"))
  
## Model 1 table

# named lookup: names = stripped term, values = display label.
# add to this as new terms show up (e.g. Aim 2 interactions).
term_labels <- c(
  "Intercept"   = "Intercept",
  "jobpanelist" = "Panelist vs. Reviewer",
  "exphigh"     = "High vs. Medium Expertise",
  "explow"      = "Low vs. Medium Expertise",
  "expnone"     = "None vs. Medium Expertise",
  "aid"         = "Application",
  "cid"         = "Committee Member",
  "cmte"        = "Committee"
)

# true generating values for each term, so the table can show recovery
# directly rather than asking the reader to hold the DGP parameters in
# their head. Fixed effects must be kept in sync by hand with the aim 1
# simulation script's a0-a4 (logit scale) -- there's no live link
# between the two scripts, since the simulation script only writes a
# CSV. The three random-effect SDs are all truly 0 here, NOT
# u0c_sd/u0a_sd/u0m_bias_sd (2026-08-14 correction) -- p_dev, which
# generates `deviated`, is plogis(a0 + a1*panelist + a2*exp_high +
# a3*exp_low + a4*exp_none) only. u0c/u0a never enter the deviation
# process at all (they only drive init_score/consensus); u0m_bias only
# enters the *magnitude* of deviation given deviation occurs (Part 2,
# m1_magnitude), not whether a member deviates (Part 1, m1_deviate,
# this table). See ror-research-log.qmd.
truth <- c(
  "Intercept"   = -0.8,   # a0
  "jobpanelist" =  0.3,   # a1
  "exphigh"     = -0.4,   # a2
  "explow"      =  0.2,   # a3
  "expnone"     =  0.5,   # a4
  "aid"         =  0,     # no application-level variation in p_dev
  "cid"         =  0,     # no member-level variation in p_dev (u0m_bias is magnitude-only)
  "cmte"        =  0      # no committee-level variation in p_dev
)

tab <- get_estimates(m1_deviate) |>
  select(term, estimate, mad, conf.low, conf.high) |>
  mutate(
    group = if_else(str_starts(term, "b_"),
      "Fixed effects", "Random effects (SD)"),
    term  = term |>
  str_remove("^b_") |>
  str_remove("^sd_") |>
  str_remove("__Intercept$")
  )

# fails loudly if a term shows up that term_labels/truth haven't been
# told about, rather than silently printing NA in the final table
stopifnot("term_labels is missing an entry for at least one term" =
  all(tab$term %in% names(term_labels)))
stopifnot("truth is missing an entry for at least one term" =
  all(tab$term %in% names(truth)))

tab <- tab |>
  mutate(
    truth = sprintf("%.3f", truth[term]),
    term  = term_labels[term],
    across(c(estimate, mad, conf.low, conf.high),
      ~sprintf("%.3f", .x))
  )

saveRDS(tab, here("output", "m1-deviate-table.rds"))

fixed_start  <- which(tab$group == "Fixed effects")[1]
random_start <- which(tab$group == "Random effects (SD)")[1]

tab |>
  select(term, truth, estimate, mad, conf.low, conf.high) |>
  setNames(c("Parameter", "Truth", "Estimate", "Error",
             "95% CrI Lower", "95% CrI Upper")) |>
  tt(caption = "Posterior estimates: m1_deviate") |>
  group_tt(i = list("Fixed effects (log odds)" = fixed_start,
                    "Random effects (SD)" = random_start)) |>
  style_tt(i = c(1,7), italic = TRUE) |>
  style_tt(i = 0, align = "l") |>
  style_tt(j = 1, align = "l")

## 3 Marginal effects

# How many deviate?
avg_predictions(m1_deviate, ndraws=200)

# Deviations by engagement
avg_predictions(m1_deviate, variables = "exp", ndraws = 200)
  
# Deviations by experience
avg_predictions(m1_deviate, variables = "job", ndraws = 200)


## 4 Model 2: signed magnitude of deviation, among deviators ----
## ordinal cumulative() model over the 10 discrete +/-0.1 ... +/-0.5
## steps. Thresholds default to "flexible" (not "equidistant"), 
## so the model does not assume the 10 steps are equally likely

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
        deviation ~ 1 + job + exp + (1 | cmte) + (1 | cid) + (1 | aid),
        prior = c(prior(normal(0, 1.5), class = Intercept),  # thresholds
                  prior(normal(0, 0.5), class = b),           # betas
                  prior(exponential(1), class = sd)),         # group SDs
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        sample_prior = "yes",
        seed = 8253,
        control = list(adapt_delta = 0.95),
        file = here("code/fits/ror-magnitude-m1"))
  
  
  avg_predictions(m1_deviate, variables = "exp", ndraws = 200)
  
  avg_predictions(m1_deviate, variables = "job", ndraws = 200)

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
