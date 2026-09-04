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

saveRDS(tm2, here("output", "m1-magnitude-job-me.rds"))

## table
dev_levels <- sprintf("%.1f", setdiff((-5:5) / 10, 0))
threshold_labels <- setNames(
  paste0("Threshold ", 1:9, " (", dev_levels[1:9], " | ", dev_levels[2:10], ")"),
  paste0("Intercept[", 1:9, "]"))

term_labels <- c(
  threshold_labels,
  "jobpanelist"       = "Panelist vs. Reviewer (location)",
  "expmed"            = "Medium vs. High Expertise",
  "explow"            = "Low vs. High Expertise",
  "expnone"           = "None vs. High Expertise",
  "aid"               = "Application",
  "cid"               = "Committee Member",
  "cmte"              = "Committee",
  "disc_jobreviewer"  = "Reviewer (dispersion)",
  "disc_jobpanelist"  = "Panelist (dispersion)"
)

# truth shown only where it's exactly 0 (scale-invariant); thresholds,
# disc coefficients, and cid's SD are left blank -- no direct mapping
# onto the fitted model's latent scale (see research log, 2026-08-27)
truth <- c(
  setNames(rep(NA_real_, 9), paste0("Intercept[", 1:9, "]")),
  "jobpanelist" = 0, "expmed" = 0, "explow" = 0, "expnone" = 0,
  "aid" = 0, "cid" = NA_real_, "cmte" = 0,
  "disc_jobreviewer" = NA_real_, "disc_jobpanelist" = NA_real_
)

tab <- get_estimates(m1_magnitude) |>
  select(term, estimate, mad, conf.low, conf.high) |>
  mutate(
    group = case_when(
      str_starts(term, "b_Intercept") ~ "Thresholds",
      str_starts(term, "b_disc_")     ~ "Fixed effects (dispersion)",
      str_starts(term, "b_")          ~ "Fixed effects (location)",
      str_starts(term, "sd_")         ~ "Random effects (SD)",
      TRUE ~ NA_character_),
    term = term |> str_remove("^b_") |> str_remove("^sd_") |> str_remove("__Intercept$")
  ) |>
  mutate(
    truth = if_else(is.na(truth[term]), "", as.character(truth[term])),
    term  = term_labels[term],
    across(c(estimate, mad, conf.low, conf.high), ~sprintf("%.3f", .x))
  )

saveRDS(tab, here("output", "m1-magnitude-table.rds"))



