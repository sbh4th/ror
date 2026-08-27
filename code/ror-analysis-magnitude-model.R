#  program:  ror-analysis-magnitude-model.R
#  task:     Part 2 of the two-part deviation model -- the signed
#            magnitude of a member's deviation from consensus, among
#            members who deviated at all
#  input:    data/sim-deviate.csv
#  output:   code/fits/ror-magnitude-m2;
#            output/m2-magnitude-job-me.rds
#  project:  RoR
#  author:   sam harper \ 2026-08-26

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

d1_dev <- d1 |>
  filter(deviated == 1) |>
  mutate(deviation = factor(sprintf("%.1f", deviation),
    levels = dev_levels, ordered = TRUE))


## 2 Model: signed magnitude, with a job-specific dispersion term ----
## job now has a scale effect on magnitude too (dev_sd_reviewer = 0.15
## vs. dev_sd_panelist = 0.30 in ror-sim-deviate.R, 2026-08-25) --
## m1_magnitude above can't recover that (its threshold spacing is
## shared across job), so this adds `disc ~ 0 + job` on top of the same
## location formula.

if (file.exists(here("code/fits/ror-magnitude-m1.rds"))) {
  file.remove(here("code/fits/ror-magnitude-m1.rds"))}

m1_magnitude <- brm(
  bf(deviation ~ 1 + job + exp + (1 | cmte) + (1 | cid) + (1 | aid),
     disc ~ 0 + job),
  data = d1_dev,
  family = cumulative(link = "logit", threshold = "flexible"),
  prior = c(prior(normal(0, 1.5), class = Intercept),
            prior(normal(0, 0.5), class = b),
            prior(exponential(1), class = sd),
            prior(normal(0, 1), class = b, dpar = disc)),
  iter = 2000, warmup = 1000, chains = 4, cores = 4,
  seed = 8253,
  control = list(adapt_delta = 0.95),
  file = here("code/fits/ror-magnitude-m1"))

# job's dispersion effect on magnitude, from the disc-aware model --
p_magnitude_job <- avg_predictions(m1_magnitude, 
  variables = "job",
  re_formula = NULL, ndraws = 200) |>
  select(group, job, estimate, conf.low, conf.high)

d_magnitude_job <- avg_slopes(m1_magnitude, 
  variables = "job",
  re_formula = NULL, ndraws = 200) |>
  select(group, estimate, conf.low, conf.high) |>
  mutate(job = as.factor("difference"))

tm2 <- p_magnitude_job |> bind_rows(d_magnitude_job)

saveRDS(tm2, here("output", "m2-magnitude-job-me.rds"))

## 4 TODO before fitting for real ----
# - Prior-predictive check both magnitude models on simulated data
#   before ever fitting on real CIHR data (mirror u2s-analysis-priors.R)
# - m1_magnitude/m2_magnitude are ordinal (cumulative()), so
#   E[deviation | deviate] is NOT a linear prediction -- it's the
#   probability-weighted sum over the 10 category values
#   (posterior_epred(..., category probs) x dev_levels, summed per
#   draw). marginaleffects/tidybayes can do this but it needs an
#   explicit custom contrast, not the default continuous
#   marginal-effect output.
# - Write the combination step: E[deviation] = P(deviate) *
#   E[deviation | deviate], propagating full posterior uncertainty from
#   both fits (draws from ror-analysis-deviate-model.R's m1_deviate and
#   this script's m1_magnitude/m2_magnitude, joined by posterior
#   iteration, not point estimates)
# - Confirm with CIHR that real data actually lands on the same +/-0.1
#   ... +/-0.5 grid (dev_levels above) -- this is currently an assumption
#   carried over from the simulator's parameters, not confirmed with the
#   Funding Analytics Team
# - Confirm cmdstan can actually compile/run in CIHR's execution
#   environment (flagged as the biggest practical risk -- see PROJECT.md)


## 2 old model 1: signed magnitude of deviation, among deviators (location only) ----
## ordinal cumulative() model over the 10 discrete +/-0.1 ... +/-0.5
## steps. Thresholds default to "flexible" (not "equidistant"),
## so the model does not assume the 10 steps are equally likely

#delete model if it exists
if (file.exists(here("code/fits/ror-magnitude-m1.rds"))) {
  file.remove(here("code/fits/ror-magnitude-m1.rds"))}

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
      control = list(adapt_delta = 0.80),
      file = here("code/fits/ror-magnitude-m1"))

# exp has no simulated dispersion effect, so the location-only
# m1_magnitude is the right fit for it -- job's dispersion effect is
# handled by m2_magnitude below instead.
p_magnitude_exp <- avg_predictions(m1_magnitude, variables = "exp",
                                   re_formula = NULL, ndraws = 200) |>
  as.data.frame()

# rebuild as a bare tibble (see note above pred_tab's saveRDS in
# ror-analysis-deviate-model.R) so the ~140MB marginaleffects attribute
# doesn't get serialized along with it
p_magnitude_exp <- tibble(group = p_magnitude_exp$group,
                          exp = p_magnitude_exp$exp, estimate = p_magnitude_exp$estimate,
                          conf.low = p_magnitude_exp$conf.low,
                          conf.high = p_magnitude_exp$conf.high)

saveRDS(p_magnitude_exp, here("output", "m1-magnitude-exp-me.rds"))

