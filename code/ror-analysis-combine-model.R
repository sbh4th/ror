#  program:  ror-analysis-combine-model.R
#  task:     Part 3 of the two-part deviation model -- combine
#            m1_deviate (P(deviate)) and m1_magnitude (E[deviation |
#            deviate=1]) into the overall estimand of interest,
#            E[deviation] = P(deviate) x E[deviation | deviate=1].
#            See ror-analysis-deviate-model.R and
#            ror-analysis-magnitude-model.R for Parts 1 and 2.
#  input:    code/fits/ror-deviate-m1.rds, code/fits/ror-magnitude-m1.rds,
#            data/sim-deviate.csv
#  output:   output/m1-combined-job-me.rds
#  project:  RoR
#  author:   sam harper \ 2026-09-04
#
#  note:     brms::posterior_epred.brmsfit() does not correctly forward
#            allow_new_levels/sample_new_levels through its internal
#            standata() path for m1_magnitude (fit on deviators only,
#            so most aid/cid levels in the full dataset are "new" to
#            it) -- confirmed brms 2.23.0 bug. Workaround: call
#            brms::prepare_predictions() directly (which does respect
#            these args) and then posterior_epred() on the resulting
#            brmsprep object, passing dpar/nlpar explicitly since
#            posterior_epred.brmsprep() has no defaults for them. The
#            magnitude model's newdata also needs a syntactically
#            valid placeholder value in the outcome column (matching
#            its training factor levels) since brms validates the
#            response column even for pure prediction.

##  0 Load needed packages ----
library(here)
library(tidyverse)
library(brms)
library(tinytable)

## 1 Load fitted models and data ----

m1_deviate   <- readRDS(here("code", "fits", "ror-deviate-m1.rds"))
m1_magnitude <- readRDS(here("code", "fits", "ror-magnitude-m1.rds"))

d <- read_csv(here("data", "sim-deviate.csv"), show_col_types = FALSE)

d1 <- d |>
  mutate(
    job      = factor(job, levels = c("reviewer", "panelist")),
    exp      = factor(exp, levels = c("high", "med", "low", "none")),
    deviated = factor(deviated, levels = c(0, 1))
  )

# the 10 discrete steps a deviator's score can take relative to
# consensus (+/-0.1 ... +/-0.5, in tenths -- CIHR scores have one
# decimal place); same levels m1_magnitude was trained on.
dev_levels_chr <- sprintf("%.1f", setdiff((-5:5) / 10, 0))
dev_levels_num <- as.numeric(dev_levels_chr)

# placeholder outcome value for the magnitude model's newdata -- the
# actual value is never used by posterior_epred(), but brms validates
# that the response column contains a level it was trained on.
d1_for_magnitude <- d1 |>
  mutate(deviation = factor("0.1", levels = dev_levels_chr, ordered = TRUE))

## 2 P(deviate) from m1_deviate, over the full dataset ----

NDRAWS <- 1000

p_dev <- posterior_epred(m1_deviate, newdata = d1,
  re_formula = NULL, ndraws = NDRAWS)

## 3 E[deviation | deviate=1] from m1_magnitude, over the full dataset
##   (allow_new_levels since most aid/cid here were never seen by
##   m1_magnitude, which was fit on deviators only) ----

prep <- brms::prepare_predictions(m1_magnitude, newdata = d1_for_magnitude,
  re_formula = NULL, ndraws = NDRAWS, allow_new_levels = TRUE,
  sample_new_levels = "gaussian")

p_cat <- posterior_epred(prep, dpar = NULL, nlpar = NULL)

E_dev_given_dev <- apply(p_cat, c(1, 2), function(p) sum(p * dev_levels_num))

## 4 Combine: E[d] = P(deviate) x E[deviation | deviate=1] ----

E_d <- p_dev * E_dev_given_dev

## 5 Population-average E[d] by job, and overall ----

job_vec <- d1$job
by_job  <- sapply(levels(job_vec), function(lv) rowMeans(E_d[, job_vec == lv, drop = FALSE]))

job_labels <- c(reviewer = "Reviewer", panelist = "Panelist")

tab_job <- tibble(
  group    = "By role",
  term     = job_labels[colnames(by_job)],
  estimate = colMeans(by_job),
  conf.low  = apply(by_job, 2, quantile, probs = 0.025),
  conf.high = apply(by_job, 2, quantile, probs = 0.975)
)

overall <- rowMeans(E_d)
tab_overall <- tibble(
  group    = "Overall",
  term     = "All members",
  estimate = mean(overall),
  conf.low  = quantile(overall, 0.025),
  conf.high = quantile(overall, 0.975)
)

tab_combined <- bind_rows(tab_overall, tab_job) |>
  mutate(across(c(estimate, conf.low, conf.high), ~sprintf("%.3f", .x)))

saveRDS(tab_combined, here("output", "m1-combined-job-me.rds"))

## table
group_starts <- which(!duplicated(tab_combined$group))
header_rows  <- group_starts + seq_along(group_starts) - 1

tab_combined |>
  select(term, estimate, conf.low, conf.high) |>
  setNames(c("Parameter", "E[deviation]", "95% CrI Lower", "95% CrI Upper")) |>
  tt(caption = "Combined estimate: E[deviation] = P(deviate) x E[deviation | deviate=1]") |>
  group_tt(i = setNames(as.list(group_starts), unique(tab_combined$group))) |>
  style_tt(i = header_rows, italic = TRUE) |>
  style_tt(i = 0, align = "l") |>
  style_tt(j = 1, align = "l")
