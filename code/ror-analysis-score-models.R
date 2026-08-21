#  program:  ror-analysis-score-models.R
#  task:     set up a two-part Bayesian model for how
#            panel members' final scores deviate from the pre-discussion
#            consensus score
#  input:    data/sim-deviate.csv
#  output:   fits/ror-deviate-m1, fits/ror-magnitude-m1
#  project:  RoR
#  author:   sam harper \ 2026-08-21
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
library(modelsummary)

# Use the cmdstanr backend for Stan
# You need to install the cmdstanr package first
# (https://mc-stan.org/cmdstanr/) and then run cmdstanr::install_cmdstan()
# to install cmdstan on your computer.
options(mc.cores = 4,
        brms.backend = "cmdstanr")

## 1 Read in simulated dataset ----

d <- read_csv(here("data", "sim-deviate.csv"),
  show_col_types = FALSE)

d1 <- d |>
  mutate(
    job      = factor(job, levels = c("reviewer", "panelist")),
    exp      = factor(exp, levels = c("high", "med", "low", "none")),
    deviated = factor(deviated, levels = c(0, 1)),
  )

# the 10 discrete steps a deviator's score can take relative to consensus
# (+/-0.1 ... +/-0.5, in tenths -- CIHR scores have one decimal place).
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

m1_dev_priors <- pr_int / pr_b

ggsave(here("output", "ror-priors-m1-deviate.png"), 
       plot = m1_dev_priors)

## Overall looks like SD of 1.0 for the intercept and 
## 0.5 for the treatment effect seem reasonable

## 2 Model 1: did this member deviate from consensus at all? ----

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
  "expmed"      = "Medium vs. High Expertise",
  "explow"      = "Low vs. High Expertise",
  "expnone"     = "None vs. High Expertise",
  "aid"         = "Application",
  "cid"         = "Committee Member",
  "cmte"        = "Committee"
)

# true generating values for each term, so the table can show recovery
truth <- c(
  "Intercept"   = -0.5,   # a0 (high expertise, reviewer baseline)
  "jobpanelist" =  0.3,   # a1
  "expmed"      = -0.3,   # a2 (medium vs. high)
  "explow"      = -0.5,   # a3 (low vs. high)
  "expnone"     = -0.8,   # a4 (none vs. high)
  "aid"         =  NA,     # no application-level variation in p_dev
  "cid"         =  NA,     # no member-level variation in p_dev
  "cmte"        =  NA      # no committee-level variation in p_dev
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

tab <- tab |>
  mutate(
    # unpadded values here for truth (e.g. "0.3", not "0.300")
    truth = if_else(is.na(truth[term]), "", as.character(truth[term])),
    term  = term_labels[term],
    across(c(estimate, mad, conf.low, conf.high),
      ~sprintf("%.3f", .x))
  )

saveRDS(tab, here("output", "m1-deviate-table.rds"))

fixed_start  <- which(tab$group == "Fixed effects")[1]
random_start <- which(tab$group == "Random effects (SD)")[1]

tab |>
  mutate(truth = if_else(group == "Random effects (SD)", "", truth)) |>
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

# Predicted P(deviate) -- overall, and by expertise/role -- as
# population-average predictions (marginaleffects' default re_formula
# behavior across the full observed dataset), not raw logit
# ndraws = 200 for speed; raise before reporting
# real posterior summaries.

exp_labels <- c(high = "High",
  med = "Medium", low = "Low", none = "Not enough")
job_labels <- c(reviewer = "Reviewer", 
  panelist = "Panelist")

p_overall <- avg_predictions(
  m1_deviate, ndraws = 200, re_formula = NULL) |>
  as.data.frame() |>
  mutate(group = "Overall", term = "All members")

p_exp <- avg_predictions(m1_deviate, 
  variables = "exp", ndraws = 200, re_formula = NULL) |>
  as.data.frame() |>
  mutate(group = "By self-rated expertise", term = exp_labels[exp])

p_job <- avg_predictions(m1_deviate, 
  variables = "job", ndraws = 200, re_formula = NULL) |>
  as.data.frame() |>
  mutate(group = "By role", term = job_labels[job])

pred_tab <- bind_rows(p_overall, p_exp, p_job) |>
  select(group, term, estimate, conf.low, conf.high) |>
  mutate(across(c(estimate, conf.low, conf.high), ~sprintf("%.3f", .x)))

# marginaleffects predictions objects carry a hidden "marginaleffects"
# attribute (the full model/draws context, ~140MB here) that survives
# select()/mutate()/as_tibble() -- none of those strip unrecognized
# attributes, so saveRDS() on what looks like a tiny 7-row table
# actually serializes that whole attribute too, ballooning the file to
# 100+MB. Rebuilding a fresh tibble from the bare column vectors (via
# $, which drops attributes) is what actually clears it.
pred_tab <- tibble(group = pred_tab$group, term = pred_tab$term,
  estimate = pred_tab$estimate, conf.low = pred_tab$conf.low,
  conf.high = pred_tab$conf.high)

saveRDS(pred_tab, here("output", "m1-deviate-me.rds"))

# group_tt() inserts a header row above each named start index -- so
# once inserted, every group's own start (and everything after it)
# shifts down by however many headers now precede it. group_starts
# must stay in ascending order for this offset to be correct.
group_starts <- which(!duplicated(pred_tab$group))
header_rows  <- group_starts + seq_along(group_starts) - 1

pred_tab |>
  select(term, estimate, conf.low, conf.high) |>
  setNames(c("Parameter", "P(deviate)", 
    "95% CI Lower", "95% CI Upper")) |>
  tt(caption = "Predicted probability of deviation from consensus") |>
  group_tt(i = setNames(as.list(group_starts), unique(pred_tab$group))) |>
  style_tt(i = header_rows, italic = TRUE) |>
  style_tt(i = 0, align = "l") |>
  style_tt(j = 1, align = "l")


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
