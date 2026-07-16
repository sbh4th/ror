# Project Overview: Reviewer Expertise & Engagement in Grant Peer Review

Tracking doc for ongoing work — updated as the project progresses.

## Where we left off (2026-07-16)

Decided to move the analysis to a Bayesian framework, matching the `brms`/`cmdstanr` setup used in `u2-sibs`. Drafted (not fit) two new scripts — see Repo contents below: `code/ror-sim-deviation.R` (simulates a consensus score + two-part deviation outcome) and `code/ror-analysis-score-models.R` (sets up the two linked `brm()` models, `FIT_MODELS <- FALSE`). Both run cleanly (verified via `Rscript`).

**Before setting `FIT_MODELS <- TRUE` and actually fitting anything**, still need to:
1. Confirm CIHR's execution environment can actually compile/run Stan (`cmdstanr` needs a C++ toolchain) — biggest practical risk to the whole remote-execution plan, worth raising with the Funding Analytics Team early.
2. Prior-predictive check both models on the simulated data (mirror `u2-sibs`'s `u2s-analysis-priors.R` pattern) before trusting the priors in `code/ror-analysis-score-models.R` — they're first-guess placeholders.
3. Decide `re_formula = NULL` vs. `NA` for the headline `marginaleffects` estimate (conditional on these committees vs. population-average across committees) — see the TODO block at the bottom of `code/ror-analysis-score-models.R`.
4. Write the draw-level combination step (`E[deviation] = P(deviate) × E[deviation | deviate]`) once both models are actually fit.

A good next-session prompt: *"Let's continue the RoR Bayesian modeling work — pick up from `code/ror-analysis-score-models.R`."*

## Grant basics

- **Title:** The Influence of Reviewer Expertise and Engagement on Peer Review of Grants
- **Funding opportunity:** Research on Research (RoR) Joint Initiative — SSHRC, CIHR, and Michael Smith Health Research BC
- **Application ID:** 1017-2025-00465
- **Status:** Funded. Notice of Decision Oct 7, 2025 — "Offered," ranked 7/68 within committee (First sextile), 15.16/18 weighted score. Notice of Award: **$192,017** total over the award period **Sept 1, 2025 – Aug 31, 2028** ($83,809 / $85,899 / $22,309 across FY25/26–27/28).
- **PI (Project Director):** Sam Harper, Professor, Epidemiology, Biostatistics & Occupational Health, McGill
- **Co-Applicant:** Arijit Nandi, Associate Professor, Equity, Ethics & Policy / Epi, Biostat & Occ Health, McGill
- **Administering org:** McGill University

SSHRC committee note to keep in mind for future framing/papers: *"The committee encourages the team to complement the project's use of empirical data and modelling by further consideration of the power dynamics, organizational behaviour, and institutional culture of peer review."*

## Research question & aims

Overall aim: investigate how the CIHR Project Grant peer review process affects **reliability and fairness** in scoring and funding success.

1. **Aim 1** — Evaluate how reviewer expertise (self-described: high/medium/low/not enough) and engagement (reviewer vs. non-reviewing panelist) affect how panel discussion changes scores (consensus → final score).
2. **Aim 2** — Assess whether these effects differ by applicant characteristics (gender, career stage).
3. **Aim 3** (exploratory) — Evaluate alternative funding-decision schemes (e.g., upweighting scores by engagement level, partial randomization within the "grey zone" of fundability) against the standard model.

## Data & methods

- Restricted, application-level data from **CIHR's Funding Analytics Team**, obtained by agreement (not public — goes beyond the CIHR Open Data Portal).
- Fields: funding result/amount, initial reviewer scores, consensus score, keywords/domain, re-submission status, # investigators, funding requested, PRC; panel-member-level self-described expertise and engagement (reviewer/panelist), gender, experience, career stage, past funding success, conflicts of interest; applicant gender & career stage.
- **Confidentiality workflow:** the team supplies analysis code to CIHR's Funding Analytics Team, who run it and return results — the team does not get direct access to confidential applicant/reviewer identifiers. Code has to run correctly *remotely*, without interactive debugging access to the real data — this is the main constraint shaping the modeling/simulation workflow below.
- **Primary outcome:** difference between consensus score and final panel-member score (`d_ijk`).
- **Statistical framework: Bayesian** (decision made 2026-07-16), following the `brms` + `cmdstanr` conventions established in the `u2-sibs` project (`/Users/samharper/git/u2-sibs`) — weakly-informative priors (`normal(0,1.5)` on `0 + Intercept`, `normal(0,0.5)` on betas, `exponential(1)` on group-level SDs), `sample_prior = "yes"` for prior-predictive checks, delete-then-refit caching via `file = here("code/fits/...")`, `marginaleffects` for contrasts.
- **Key modeling insight:** `d_ijk` is expected to have a spike at zero (members who simply adopt the consensus score) plus continuous variation among those who deviate — a two-part model (P(any deviation) × magnitude | deviation), analogous to `u2-sibs`'s `hurdle_poisson()` models but implemented as two linked `brm()` fits since brms has no native family for a signed, bounded, zero-inflated-at-an-interior-point outcome.
- Personnel: 1 postdoc (2 years, leads analysis/code/writing) + 1 part-time RA (lit review, OSF infrastructure, project website, coordination with CIHR).

## Repo contents

| Path | What it is |
|---|---|
| [writing/1017-2025-00465 - Harper, Sam - Final.pdf](<writing/1017-2025-00465 - Harper, Sam - Final.pdf>) | **The actual submitted/funded proposal** (29pp: application details, full Project Description w/ Aims, Background, Methods, Timeline, KMB, budget justification, bibliography, CVs for Harper & Nandi) |
| [writing/proposal.qmd](writing/proposal.qmd) | An early/skeleton draft of the proposal — much rougher and **superseded** by the final PDF above; has unfinished sentences and different framing (ECEC/daycare content that isn't in the funded version) |
| [writing/sim-data.qmd](writing/sim-data.qmd) | Working Quarto doc simulating the multilevel review-panel data structure (50 committees × 15 discussed apps × 24 members), with example `lmer` models and predicted-score contrasts. Generates `data/sim-data.csv`. Ends with open questions about what fields CIHR can actually provide. |
| [code/sim-scores.R](code/sim-scores.R) | Earlier, messier scratch version of the same simulation (has leftover/broken code fragments) — superseded by `sim-data.qmd` |
| [data/sim-data.csv](data/sim-data.csv) | Simulated dataset (18,000 rows) matching the parameters in `sim-data.qmd` |
| [reviews/](reviews/) | SSHRC administrative award package: Notice of Award, Notice of Decision (scores/ranking), Results Letter, Terms & Conditions. These are **award/decision documents**, not detailed peer-review comments — no reviewer-by-reviewer narrative feedback was included in this package. |
| [writing/ror.bib](writing/ror.bib) | Bibliography for the proposal |
| [code/ror-sim-deviation.R](code/ror-sim-deviation.R) | Extends the committee/application/member simulation with a per-application `consensus` score and a two-part (any-deviation × signed magnitude) process for how each member's final score departs from it. Writes `data/sim-deviation-data.csv`. Additive — doesn't touch `sim-data.qmd`/`sim-data.csv`. |
| [code/ror-analysis-score-models.R](code/ror-analysis-score-models.R) | **Draft, unfit.** Sets up the two-part Bayesian model (`m1_deviate`: bernoulli "did they deviate", `m1_magnitude`: `student_t()` magnitude among deviators) on the simulated deviation data, following `u2-sibs` conventions. `FIT_MODELS <- FALSE` gates the actual `brm()` calls off. |

## Open questions / next steps

From the "Questions" section of `sim-data.qmd`, still unresolved as of proposal submission:

- Which additional fields (initial reviewer scores, consensus score, keywords, re-submission status, reviewer/applicant gender & career stage) will CIHR actually be able to extract?
- Which analyses can be run as a one-off data extract vs. need to be run remotely by CIHR analysts using team-supplied code?
- How to budget the CIHR analyst time needed to run the remote analyses?

Practical next steps implied by the funded timeline (Year 1): finalize data-sharing/analysis-plan agreement with CIHR's Funding Analytics Team, hire the postdoc and RA, take the first Ottawa trip to nail down the analysis plan and confidential-field details.

## Log

- 2026-07-16: Repo reviewed for the first time this session (proposal, sim code, award/decision letters). Created this overview.
- 2026-07-16: Reviewed `u2-sibs` (`brms`/`cmdstanr` conventions) to inform the modeling approach. Decided on Bayesian framework + two-part (hurdle-style) model for the consensus→final score deviation. Drafted `code/ror-sim-deviation.R` and `code/ror-analysis-score-models.R` (unfit — see "Where we left off"). Installed the `faux` R package (was missing locally) to verify the simulation script runs.
