## ============================================================
## Three-part hurdle model for grant-score deviations
## Parts: (1) any deviation (bernoulli)
##        (2) direction | deviation (bernoulli)
##        (3) magnitude | deviation (ordinal, cumulative logit)
## Fit jointly in brms, sharing grant-level random effects.
## ============================================================

library(tidyverse)
library(marginaleffects)
library(brms)


options(mc.cores = 4,
        brms.backend = "cmdstanr")

set.seed(20260824)

## ------------------------------------------------------------
## 1. Simulate data
## ------------------------------------------------------------
## Design: G grants, each scored by K panel members.
## Two grant-level covariates:
##   spread       - SD of the 3 original reviewer scores (continuous,
##                  a proxy for how contested the grant is)
##   experienced  - proportion of experienced panel members present
##                  for that grant's discussion (0/1 grant-level flag
##                  for simplicity; make it continuous if you prefer)
##
## "Planted" effects (the ground truth we'll try to recover):
##   - higher spread  -> more likely to deviate at all (part 1)
##   - higher spread  -> more likely to deviate *upward* (part 2)
##   - higher spread  -> larger deviations when they occur (part 3)
##   - experienced panels deviate less overall, but when they do,
##     deviations are smaller (a "calibration" story)

G <- 60                      # number of grants
K <- 20                       # panel members per grant

grants <- tibble(
  grant_id    = 1:G,
  spread      = round(runif(G, 0.1, 1.0), 2),   # SD of original 3 reviews
  experienced = rbinom(G, 1, 0.5),               # experienced panel flag
  u_grant     = rnorm(G, 0, 0.4)                 # latent grant-level "contentiousness"
)

dat <- grants %>%
  slice(rep(1:n(), each = K)) %>%
  mutate(reviewer_id = rep(1:K, times = G))

## ---- Part 1: P(deviate) ----
lp_pi <- with(dat, -0.8 + 1.6*spread - 0.5*experienced + u_grant)
p_dev <- plogis(lp_pi)
dat$nonzero <- rbinom(nrow(dat), 1, p_dev)

## ---- Part 2: P(up | deviate) ----
lp_dir <- with(dat, 0.0 + 1.0*spread - 0.3*experienced + 0.5*u_grant)
p_up <- plogis(lp_dir)
dat$direction <- ifelse(dat$nonzero == 1,
                         rbinom(nrow(dat), 1, p_up),
                         NA_integer_)   # undefined when no deviation

## ---- Part 3: magnitude | deviate, ordinal 1..5 = |d| in {0.1,...,0.5} ----
## Cumulative-logit generative model: higher eta -> larger magnitudes
mag_levels <- c("0.1","0.2","0.3","0.4","0.5")
thresholds <- c(-1.0, 0.2, 1.2, 2.4)   # 4 cutpoints for 5 ordered categories

lp_mag <- with(dat, 0.9*spread - 0.6*experienced + 0.3*u_grant)

sim_ordinal <- function(eta, cuts) {
  probs_cum <- plogis(cuts - eta)          # P(Y <= k)
  probs_cum <- c(probs_cum, 1)
  probs <- diff(c(0, probs_cum))
  sample(seq_along(probs), size = 1, prob = probs)
}

dat$magnitude <- NA_integer_
idx <- which(dat$nonzero == 1)
dat$magnitude[idx] <- vapply(lp_mag[idx], sim_ordinal, integer(1), cuts = thresholds)
dat$magnitude <- factor(mag_levels[dat$magnitude], levels = mag_levels, ordered = TRUE)
dat$direction <- factor(dat$direction, levels = c(0,1), labels = c("down","up"))

## ---- Assemble observed signed deviation (for sanity checks / plots) ----
mag_num <- c("0.1"=0.1,"0.2"=0.2,"0.3"=0.3,"0.4"=0.4,"0.5"=0.5)
dat <- dat %>%
  mutate(
    mag_num = ifelse(nonzero==1, mag_num[as.character(magnitude)], 0),
    sign    = case_when(nonzero==0 ~ 0,
                         direction=="up" ~ 1,
                         direction=="down" ~ -1),
    deviation = sign * mag_num
  )

table(dat$deviation)

## ------------------------------------------------------------
## 2. Fit the joint 3-part brms model
## ------------------------------------------------------------
## Each sub-model gets its own formula. The direction and magnitude
## models only apply to rows where nonzero == 1; brms handles this
## with the `subset()` addition term, so partial-NA outcomes are fine.
## All three share a grant-level random intercept, which lets brms
## estimate any correlation in "contentiousness" across the three
## processes (set_rescor(FALSE) turns off residual correlation for
## the (non-existent, for these families) continuous residual part;
## the *random effects* correlation across responses is retained by
## default when you give them a common (1|grant_id) term via `|c|`).

bf_nonzero <- bf(
  nonzero ~ spread + experienced + (1|c|grant_id),
  family = bernoulli()
)

bf_direction <- bf(
  direction | subset(nonzero == 1) ~ spread + experienced + (1|c|grant_id),
  family = bernoulli()
)

bf_magnitude <- bf(
  magnitude | subset(nonzero == 1) ~ spread + experienced + (1|c|grant_id),
  family = cumulative("logit")
)

fit <- brm(
  mvbf(bf_nonzero, bf_direction, bf_magnitude, rescor = FALSE),
  data = dat,
  chains = 4, cores = 4, iter = 2000, seed = 1
)

summary(fit)

## ------------------------------------------------------------
## 3. Compose population-level estimands from the posterior
## ------------------------------------------------------------
## For a given covariate profile X, build:
##   pi(X)   = P(deviate)
##   q(X)    = P(up | deviate)
##   pbar(X) = expected |magnitude| | deviate  (from ordinal probs)
## Then:
##   E[|d| | X] = pi(X) * pbar(X)
##   E[d|X]     = pi(X) * (2*q(X)-1) * pbar(X)

mag_values <- c("0.1"=0.1, "0.2"=0.2, "0.3"=0.3, "0.4"=0.4, "0.5"=0.5)

## marginaleffects::predictions() is used for each *individual* response
## (this is the part it does cleanly and better than raw posterior_epred:
## tidy newdata handling, explicit rowid bookkeeping). It has no concept
## of combining across `resp`, so the pi * mbar / pi*(2q-1)*mbar algebra
## below is still bespoke -- marginaleffects can't discover that mapping
## on its own for a multivariate (non-native-hurdle) brms model.
##
## NOTE: column names returned by posterior_draws() (drawid/rowid/draw,
## plus `group` for multi-category families) have been stable across
## recent marginaleffects versions but are worth checking with
## `names(posterior_draws(predictions(fit, resp = "nonzero")))`
## against your installed version before trusting this blindly.

get_binary_draws <- function(fit, resp, newdata) {
  preds <- predictions(fit, newdata = newdata, resp = resp,
                       allow_new_levels = TRUE)
  pd <- posterior_draws(preds)   # long: one row per (rowid x drawid)
  pd %>%
    select(drawid, rowid, draw) %>%
    pivot_wider(names_from = rowid, values_from = draw) %>%
    arrange(as.numeric(drawid)) %>%
    select(-drawid) %>%
    as.matrix()                  # draws x nrow(newdata)
}

get_magnitude_mbar_draws <- function(fit, newdata, resp = "magnitude") {
  # For an ordinal family, predictions() returns one row per
  # (newdata row x category), identified by the `group` column.
  preds <- predictions(fit, newdata = newdata, resp = resp,
                       allow_new_levels = TRUE)
  pd <- posterior_draws(preds)
  pd %>%
    mutate(mag_val = mag_values[as.character(group)]) %>%
    group_by(drawid, rowid) %>%
    summarise(mbar = sum(draw * mag_val), .groups = "drop") %>%
    pivot_wider(names_from = rowid, values_from = mbar) %>%
    arrange(as.numeric(drawid)) %>%
    select(-drawid) %>%
    as.matrix()                  # draws x nrow(newdata)
}

compose_draws <- function(fit, newdata) {
  # newdata: one-row (or multi-row) data frame of covariate profiles
  # (grant_id can be a placeholder new level; allow_new_levels lets
  #  marginaleffects/brms marginalize over the random-effect distribution)
  pi_draws   <- get_binary_draws(fit, "nonzero", newdata)
  q_draws    <- get_binary_draws(fit, "direction", newdata)
  mbar_draws <- get_magnitude_mbar_draws(fit, newdata, "magnitude")
  
  list(
    pi   = pi_draws,
    q    = q_draws,
    mbar = mbar_draws,
    abs_dev = pi_draws * mbar_draws,
    signed_dev = pi_draws * (2*q_draws - 1) * mbar_draws
  )
}

## Example: contrast high- vs low-spread grants, holding experienced fixed
##
## NOTE: `nonzero` must be included here (set to 1) even though it's the
## *outcome* of the first submodel, because the direction/magnitude
## formulas use `subset(nonzero == 1)`. When marginaleffects/brms build
## predictions for those two responses, they evaluate that subset
## condition against `newdata` -- if the column is missing, every row
## gets filtered out before prediction happens, which is exactly the
## "All rows of 'data' were removed via 'subset'" error. Setting it to 1
## is also conceptually correct: the direction/magnitude submodels are
## about deviation *conditional on a deviation happening*, so we want
## predictions from the "assume nonzero" branch. It's harmless for the
## `nonzero` submodel itself, which ignores the extra column.
newdat <- tibble(
  spread      = c(0.2, 0.8),
  experienced = c(0, 0),
  nonzero     = c(1, 1),
  grant_id    = c(99999, 99998)     # unseen grant IDs -> new-level draws
)

comp <- compose_draws(fit, newdat)

## Posterior for E[|d|] at low vs high spread, and their difference
abs_low  <- comp$abs_dev[,1]
abs_high <- comp$abs_dev[,2]
contrast_abs <- abs_high - abs_low

cat("E[|d|] at spread=0.2:", round(mean(abs_low),3),
    " 95% CrI:", round(quantile(abs_low, c(.025,.975)),3), "\n")
cat("E[|d|] at spread=0.8:", round(mean(abs_high),3),
    " 95% CrI:", round(quantile(abs_high, c(.025,.975)),3), "\n")
cat("Contrast (high - low spread):", round(mean(contrast_abs),3),
    " 95% CrI:", round(quantile(contrast_abs, c(.025,.975)),3), "\n")

## Same for the signed estimand (will be smaller/attenuated toward 0
## unless direction is asymmetric)
signed_low  <- comp$signed_dev[,1]
signed_high <- comp$signed_dev[,2]
cat("\nE[d] (signed) at spread=0.2:", round(mean(signed_low),3), "\n")
cat("E[d] (signed) at spread=0.8:", round(mean(signed_high),3), "\n")

## ------------------------------------------------------------
## 4. Quick check against the "planted" truth
## ------------------------------------------------------------
## You simulated spread with strong positive effects on all three
## sub-processes, and experienced=0 in both profiles above, so the
## contrast_abs posterior should sit comfortably above 0. Try varying
## `experienced` in newdat as well to check that a *combination* of
## covariate contrasts composes correctly, and try shrinking G or K
## to see how much the CrIs widen at realistic panel sizes.

## ------------------------------------------------------------
## 5. Dose-response plot: E[|d|] and E[d] across a grid of spread
## ------------------------------------------------------------
## Build a grid crossing spread values with the two experienced levels,
## push it through compose_draws(), summarize each grid point's
## posterior (mean + 95% CrI), and plot with ggplot.

spread_grid <- seq(0.1, 1.0, by = 0.05)

grid_dat <- expand_grid(
  spread      = spread_grid,
  experienced = c(0, 1)
) %>%
  mutate(
    nonzero  = 1,                        # see note above compose_draws(fit, newdat)
    grant_id = row_number() + 100000      # unseen IDs, one per grid row
  )

grid_comp <- compose_draws(fit, grid_dat)

## grid_comp$abs_dev and $signed_dev are draws x nrow(grid_dat) matrices;
## summarize each column into a posterior mean + 95% CrI, then attach
## back to the covariate grid for plotting.
summarize_draws_matrix <- function(draw_mat, grid_dat, value_name) {
  tibble(
    spread      = grid_dat$spread,
    experienced = grid_dat$experienced,
    mean = apply(draw_mat, 2, mean),
    lwr  = apply(draw_mat, 2, quantile, probs = 0.025),
    upr  = apply(draw_mat, 2, quantile, probs = 0.975)
  ) %>%
    mutate(estimand = value_name)
}

plot_dat <- bind_rows(
  summarize_draws_matrix(grid_comp$abs_dev,    grid_dat, "E[|d|]  (expected absolute deviation)"),
  summarize_draws_matrix(grid_comp$signed_dev, grid_dat, "E[d]  (expected signed deviation)")
) %>%
  mutate(experienced = factor(experienced, levels = c(0,1),
                              labels = c("Less experienced panel", "Experienced panel")))

p <- ggplot(plot_dat, aes(x = spread, y = mean, color = experienced, fill = experienced)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ estimand, scales = "free_y") +
  geom_hline(data = filter(plot_dat, estimand == "E[d]  (expected signed deviation)"),
             aes(yintercept = 0), linetype = "dashed", color = "grey40", linewidth = 0.4) +
  labs(
    x = "Spread of original 3 reviewer scores (SD)",
    y = "Posterior mean deviation (points)",
    color = "Panel composition",
    fill  = "Panel composition",
    title = "Composed population-level deviation vs. review spread",
    subtitle = "Ribbons are 95% posterior credible intervals from the joint hurdle/direction/magnitude model"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p)

ggsave("deviation_doseresponse.png", p, width = 9, height = 5, dpi = 300)
