# Project Overview: Reviewer Expertise & Engagement in Grant Peer Review

Tracking doc for ongoing work — updated as the project progresses. This is a
lightweight, session-continuity note (where things stand, what to pick up
next), not a decision record. For the actual research log — dated entries
with rationale, code, and results for substantive decisions — see
[`code/ror-research-log.qmd`](code/ror-research-log.qmd).

## Where we left off (2026-08-05)

Simulations are split by aim rather than one shared script: `code/ror-sim-aim1.R` (renamed from `ror-sim-deviation.R`) and `code/ror-sim-aim2.R`, which adds applicant gender/career stage with real, recoverable interaction effects baked in. Aim 3's simulation hasn't been started — it needs an intervention/counterfactual design (partial randomization near a funding threshold, or similar) that isn't settled yet, not just an extension of Aims 1–2's structure.

Both Aim 1 and Aim 2 scripts now also include member-level heterogeneity in scoring leniency/harshness (`u0m_bias`, via `add_ranef("cid", ...)`, SD = 0.1, recovered at ~0.11–0.12 empirically) — this was missing until Sam caught it by plotting mean score by `memno` and noticing it was flat. Full story, including a real gotcha about `faux::add_ranef("member", ...)` silently reusing draws across committees, in `code/ror-research-log.qmd`.

**Bigger change, same day:** consensus is no longer simulated directly — it's now the mean of 3 independently-generated initial reviewer scores (`init_score`, new field, kept in the output — CIHR confirmed this is extractable), each a noisy read of the application's true underlying quality (`b0`/`u0c`/`u0a`, relabeled — they no longer mean "consensus level"). Guaranteed by construction (and checked via `stopifnot()`) to never fall outside the 3 reviewers' own range. This is a real structural fix, not cosmetic — required reordering the pipeline so committee/application random effects exist before reviewers are assigned.

CIHR's Funding Analytics Team confirmed the data-sharing workflow and field-by-field availability by email (2025-12-04, reviewed 2026-08-04) — full details and a follow-up-questions list in `code/ror-research-log.qmd`. Headline: applicant gender/career stage (Aim 2's whole basis) will never be extractable, even as CIHR-generated dummy data, so Aim 2's simulation is the only rehearsal that code gets.

Wrote `writing/ror-modeling-strategy.qmd` — a design/validation memo (not a full pre-analysis plan) meant for Arijit's review first, then a revision to send to CIHR. Renders cleanly to both HTML (folded code, self-contained) and typst/PDF. **Doubly out of date now** relative to the simulation scripts — written before both the member-level heterogeneity and the consensus-derivation change. Needs a refresh before sending to Arijit.

**Next steps:**
1. Refresh `writing/ror-modeling-strategy.qmd` for both changes above before sending to Arijit.
2. Send it to Arijit for feedback (open questions for him are listed in the doc itself).
3. Write an Aim-2-specific version of `code/ror-analysis-score-models.R` — the simulation has the `job:gender`/`exp:career_stage` interactions now, but no `brm()` formula includes them yet.
4. Send CIHR (Matt Hogel) the three follow-up questions logged in the research log and the modeling-strategy doc (synthetic dummy data for non-extractable fields? cohort start year given resubmission-status's 2023+ cutoff? how to define "reviewer experience"?).
5. Still open from before: confirm CIHR's environment can compile/run Stan; prior-predictive checks before ever setting `FIT_MODELS <- TRUE`; `re_formula = NULL` vs. `NA` decision; the `E[deviation] = P(deviate) × E[deviation | deviate]` combination step.

A good next-session prompt: *"Let's continue the RoR Bayesian modeling work — pick up from the Aim 2 model script."*

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
| [code/ror-sim-aim1.R](code/ror-sim-aim1.R) | **Aim 1 simulation** (renamed from `ror-sim-deviation.R`). Extends the committee/application/member simulation with a per-application `consensus` score, a two-part (any-deviation × signed magnitude) process for how each member's final score departs from it, and member-level leniency/harshness heterogeneity (`u0m_bias`, keyed to `cid`). Writes `data/sim-data-aim1.csv`. Additive — doesn't touch `sim-data.qmd`/`sim-data.csv`. |
| [code/ror-sim-aim2.R](code/ror-sim-aim2.R) | **Aim 2 simulation.** Builds on Aim 1's structure, adds application-level `gender`/`career_stage` with real, deliberately-recoverable `job:gender` and `exp:career_stage` interaction effects. Writes `data/sim-aim2-data.csv`. Self-contained (doesn't source Aim 1's script). |
| [code/ror-analysis-score-models.R](code/ror-analysis-score-models.R) | **Draft, unfit, Aim 1 only.** Sets up the two-part Bayesian model (`m1_deviate`: bernoulli "did they deviate", `m1_magnitude`: ordinal `cumulative()` magnitude among deviators — CIHR scores are one-decimal-place, so magnitude is a 10-category discrete outcome, not continuous) on the simulated deviation data, following `u2-sibs` conventions. `FIT_MODELS <- FALSE` gates the actual `brm()` calls off. No Aim 2 version exists yet. |
| [writing/ror-modeling-strategy.qmd](writing/ror-modeling-strategy.qmd) | Design/validation memo for Arijit → CIHR: the simulated data-generating process, the two-part model, empirical recovery checks (live-evaluated against the existing simulated CSVs), and open questions. Renders to `.html` (folded code, self-contained) and `.pdf` (typst). Supersedes the old `sim-data.qmd` note sent to CIHR. |

## Open questions / next steps

From the "Questions" section of `sim-data.qmd`, still unresolved as of proposal submission:

- Which additional fields (initial reviewer scores, consensus score, keywords, re-submission status, reviewer/applicant gender & career stage) will CIHR actually be able to extract?
- Which analyses can be run as a one-off data extract vs. need to be run remotely by CIHR analysts using team-supplied code?
- How to budget the CIHR analyst time needed to run the remote analyses?

Practical next steps implied by the funded timeline (Year 1): finalize data-sharing/analysis-plan agreement with CIHR's Funding Analytics Team, hire the postdoc and RA, take the first Ottawa trip to nail down the analysis plan and confidential-field details.

## Log

- 2026-07-16: Repo reviewed for the first time this session (proposal, sim code, award/decision letters). Created this overview.
- 2026-07-16: Reviewed `u2-sibs` (`brms`/`cmdstanr` conventions) to inform the modeling approach. Decided on Bayesian framework + two-part (hurdle-style) model for the consensus→final score deviation. Drafted `code/ror-sim-deviation.R` and `code/ror-analysis-score-models.R` (unfit — see "Where we left off"). Installed the `faux` R package (was missing locally) to verify the simulation script runs.
