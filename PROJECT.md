# Project Overview: Reviewer Expertise & Engagement in Grant Peer Review

Tracking doc for ongoing work — updated as the project progresses. This is a
lightweight, session-continuity note (where things stand, what to pick up
next), not a decision record. For the actual research log — dated entries
with rationale, code, and results for substantive decisions — see
[`code/ror-research-log.qmd`](code/ror-research-log.qmd).

## Where we left off (2026-08-19)

**`code/ror-sim-deviate.R` is now the canonical Aim 1 simulation** — the experimental streamlining mechanism (top/bottom calls + rank-based selection) is no longer a side comparison, it's the main DGP. `code/ror-analysis-score-models.R` now reads `data/sim-deviate.csv` accordingly. `code/ror-sim-aim1-old.R` is the old fixed-threshold version, superseded.

**Done: Appendix added to `writing/ror-modeling-strategy.qmd`** walking through the full simulation script. Uses a knitr `file` chunk option (`#| file: !expr here::here("code", "ror-sim-deviate.R")`, `eval: false`, `echo: true` — the explicit `echo: true` matters because the docx format block sets `execute: echo: false` document-wide) so the appendix always reflects whatever `ror-sim-deviate.R` currently says, with a framing paragraph + 7-point summary of the script's own numbered sections ahead of the full code. Verified rendered (html/typst/docx) — full script including the final `write_csv()` appears exactly once in each format's real body.

**Open, not urgent:** the `m1_deviate` Truth column's `aid`/`cid`/`cmte` rows are correctly `0` (see 2026-08-14 below for why), but Sam doesn't love how a bare "0.000" reads in the rendered table and was mid-edit on a better presentation when this session ended — revisit before treating that table as finished. Also minor/harmless: `ror-analysis-score-models.R`'s `d1` still reconstructs `aid = paste0(cmte, "_", app)` even though the CSV now carries `aid` directly (the simulation script no longer drops it) — could simplify to just use the CSV's column.

**Discussed and decided against (for now):** giving individual reviewers a persistent initial-scoring trait (some persistently harsh/lenient graders, distinct from the existing `u0m_bias` which only affects discussion-deviation, not initial scores). Real phenomenon per Sam's experience, but doesn't bear on Aim 1/2's actual estimand ($d_{ijk}$) — flagged as worth building in for **Aim 3** instead (funding-decision simulations depend on absolute score levels, where this could actually change a substantive conclusion), once Aim 3's simulation gets started.

**Also fixed this session: `writing/ror.bib` citekey regression (recurring).** A fresh Zotero export in `8be0df2` reintroduced the same Better BibTeX suffix-corruption pattern fixed once already — ~42 entries got spurious trailing letters from collisions across Sam's whole library, not this file. Re-applied the same protect-then-strip fix (one genuine collision, `governmentofcanada2016`, preserved). Also fixed an unrelated pre-existing bug found in the process: `@Jahagirdar:2017aa` in `cihr-ror-proposal.qmd` used a stray non-Zotero key that never actually resolved; renamed to `jahagirdar2017` in both the bib and the citation. Verified via full key-audit + `quarto render` (html for `cihr-ror-proposal.qmd`/`proposal.qmd`, html+typst for `ror-modeling-strategy.qmd`) — zero citation warnings. **This fix is local to the file and will likely be undone by the next Zotero export** — the durable fix is pinning citekeys on the Zotero side.

## Where we left off (2026-08-14)

**Bug fix across all three simulation scripts:** `ror-sim-aim1.R`, `ror-sim-aim2.R`, and `ror-sim-streamlining-experiment.R` all had `add_ranef()` called directly on `faux::add_random()`'s raw "application" factor, which is fully crossed with committee — meaning `u0a` draws were identical for same-labeled applications across different committees (same conflation bug class as the `member`/`cid` fix on 2026-08-05, just never caught for `application` until now). Fixed in all three by building an explicit unique `aid = paste0(cmte, "_", app)` key and pointing `add_ranef()` at that. Verified directly (consensus now differs by committee for the same `app` label); all existing `stopifnot()`/recovery checks still pass. Full details in the research log.

**Committee application-pool size now varies**, drawn per committee from a beta distribution scaled to CIHR's stated 20-80 range (shape chosen to roughly match a one-off back-calculation from real CIHR funded-application counts, `code/ror-cihr-committee-size-check.R` — not a runtime dependency, just informed the beta's shape). Reverses the 2026-08-06 decision to hold pool size fixed.

**Left tail of discussed-application initial scores lengthened.** `init_sd_lo` raised from 0.30 to 1.1 in `ror-sim-streamlining-experiment.R` — the streamlining rule's AND condition meant essentially no low individual reviewer score ever survived to discussion at the old value. Calibrated against a real reference distribution of individual reviewer scores (kept out of the log/memory per Sam's standing preference) — now matches reasonably well (median 4.2 vs. reference 4.1, ~7% vs. ~8.5% below 3.5).

**`exp` (self-rated expertise) category mix recalibrated** in all three simulation scripts, from 25/42/17/17% (high/med/low/none) to 8.3/20.8/29.2/41.7% — checked against real data showing "not enough"/"low" dominate and "high" is rare, the opposite of what was previously assumed. Source data kept out of the log per Sam's standing preference for real committee data.

**Found and fixed a mismatched "Truth" column in the `m1_deviate` modeling-strategy table.** Added a Truth column to the recovery table in `writing/ror-modeling-strategy.qmd` (`code/ror-analysis-score-models.R`) so simulated parameters and fitted estimates sit side by side. This surfaced a real bug along the way: `(1 | app)` in the brms formula grouped on `faux`'s raw crossed application label instead of a unique per-committee identifier — same conflation class as the `aid` fix above, just on the analysis side. Fixed by constructing `aid` in `d1` and using `(1 | aid)`. Comparing `app`- vs `aid`-based fits then surfaced a *second*, more fundamental issue: the Truth values for all three random effects (`cmte`/`cid`/`aid`) were wrong — they should be 0, not `u0c_sd`/`u0m_bias_sd`/`u0a_sd`, since `deviated`'s data-generating formula (`p_dev`) depends only on `job`/`exp`, with no random-effect term at all. Fixed; both the table and the modeling-strategy prose now correctly describe the small estimates as accurate recovery of a true value of 0, not underestimation. `code/fits/ror-deviate-m1.rds` (the `aid`-based fit) is current; `m1_magnitude` (Part 2) hasn't been fit yet — when it is, `cid` should show a genuine non-zero truth (`u0m_bias_sd = 0.1`); `cmte`/`aid` stay at 0 there too.

**File naming note:** `ror-sim-aim1.R` is now `code/ror-sim-aim1-old.R` (renamed outside this session) — it's still the script that produces `data/sim-data-aim1.csv` and is what the analysis script reads.

**Deferred, to pick up next:** whether/how to revise `dev_bias` given CIHR's own stated observation that discussion can *lower* the final rating relative to the preliminary average (an asymmetric claim, not currently modeled — `dev_bias = 0` everywhere). Sam wants to discuss this before any change is made.

## Where we left off (2026-08-11)

**Simulations are split by aim** rather than one shared script: `code/ror-sim-aim1.R` (renamed from `ror-sim-deviation.R`) and `code/ror-sim-aim2.R` (adds applicant gender/career stage with real, recoverable interaction effects). Both now include member-level heterogeneity (`u0m_bias`, via `add_ranef("cid", ...)`), a properly-derived consensus (mean of 3 independently-generated `init_score`s, not simulated directly), a real streamlining/eligibility selection step (not a per-score clamp), and an asymmetric (two-piece normal) `init_score` distribution calibrated so very high initial scores are rare, matching Sam's reviewing experience. Full blow-by-blow of each fix in `code/ror-research-log.qmd`. Aim 3's simulation still hasn't been started.

**New today: `code/ror-sim-streamlining-experiment.R`** — an explicitly EXPERIMENTAL side script (not part of the main pipeline) implementing a more procedurally faithful streamlining mechanism Sam described from his own CIHR committee/Scientific Officer experience: each reviewer gives a separate "top"/"bottom" call (correlated with, not determined by, their score), and an application is streamlined out iff ≥1 "bottom" call AND its mean score ranks in the bottom 60% of its own committee's candidate pool (relative rank, not `ror-sim-aim1.R`'s fixed absolute threshold). A "bring back" advocacy stage was tried and deliberately dropped (only ~6% of discussed applications, least-grounded piece of the mechanism, and the same-score/different-outcome pattern it was meant to explain already happens without it).

**Key finding so far:** despite the much richer selection process, Aim 1's core recovery checks (`job`/`exp` effects on `P(deviate)`, member-level `cid` variance) are unaffected — recover just as cleanly as in `ror-sim-aim1.R`. So the added realism doesn't change the bottom line for Aim 1's own question.

**Open question to pick up next session:** Sam plotted score-distribution histograms for `ror-sim-aim1.R` vs. the streamlining experiment side by side, and they look meaningfully different — the experiment's discussed-application scores are shifted right and missing the low tail (mean consensus 4.26 vs. 3.97; 87.5% of discussed applications are selected by rank alone). Checked this against the two hypothetical score/discussed-status datasets Sam constructed earlier the same day, and **the experiment's shape actually matches those real-world anchors better** — in both hypotheticals, no discussed application ever scored below ~4.0, while `ror-sim-aim1.R` routinely discusses applications down to 3.5. Not yet resolved: whether `ror-sim-aim1.R`'s simpler mechanism should itself be revisited given this, or whether to leave it as the intentionally-simple validated baseline with the experiment as a separate, more-realistic-but-still-experimental alternative. **This is the natural place to resume.**

**Resolved earlier this week:** `re_formula = NULL` (on the full dataset), not `NA`, is the settled convention for headline marginal-effects estimates — see research log for the Jensen's-inequality rationale. `code/ror-analysis-score-models.R` has its first real content beyond the draft skeleton (a marginal-effects section using `avg_predictions()`), still incremental/in progress.

`writing/ror-modeling-strategy.qmd` has been getting substantial direct attention from Sam (new descriptive figures, `patchwork`, prose revisions) — status of whether it's caught up with all the simulation changes above is unclear from this thread; check its current state rather than assuming.

**Next steps:**
1. Resolve the aim1-vs-experiment distribution-shape question above.
2. Write an Aim-2-specific version of `code/ror-analysis-score-models.R` (the simulation has `job:gender`/`exp:career_stage` interactions now, no `brm()` formula includes them yet).
3. Send CIHR (Matt Hogel) the follow-up questions logged in the research log and modeling-strategy doc.
4. Still open: confirm CIHR's environment can compile/run Stan; prior-predictive checks before ever setting `FIT_MODELS <- TRUE`; the `E[deviation] = P(deviate) × E[deviation | deviate]` combination step.

A good next-session prompt: *"Let's continue the RoR work — pick up the aim1-vs-streamlining-experiment distribution question."*

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
| [code/ror-sim-streamlining-experiment.R](code/ror-sim-streamlining-experiment.R) | **EXPERIMENTAL** side script (not part of the main pipeline): a more procedurally faithful streamlining mechanism (per-reviewer top/bottom calls, rank-based selection) layered on Aim 1's deviation machinery, with per-committee application-pool size drawn from real CIHR data (see below). Writes `data/sim-streamlining-experiment.csv`. |
| [code/ror-cihr-committee-size-check.R](code/ror-cihr-committee-size-check.R) | Reads CIHR's public "Investments" open data (manually downloaded `.xlsx` from open.canada.ca, gitignored) and back-calculates implied applications-per-committee from real funded counts. Saves `output/cihr-funded-per-committee.rds`, consumed by `ror-sim-streamlining-experiment.R`. |

## Open questions / next steps

From the "Questions" section of `sim-data.qmd`, still unresolved as of proposal submission:

- Which additional fields (initial reviewer scores, consensus score, keywords, re-submission status, reviewer/applicant gender & career stage) will CIHR actually be able to extract?
- Which analyses can be run as a one-off data extract vs. need to be run remotely by CIHR analysts using team-supplied code?
- How to budget the CIHR analyst time needed to run the remote analyses?

Practical next steps implied by the funded timeline (Year 1): finalize data-sharing/analysis-plan agreement with CIHR's Funding Analytics Team, hire the postdoc and RA, take the first Ottawa trip to nail down the analysis plan and confidential-field details.

## Log

- 2026-07-16: Repo reviewed for the first time this session (proposal, sim code, award/decision letters). Created this overview.
- 2026-07-16: Reviewed `u2-sibs` (`brms`/`cmdstanr` conventions) to inform the modeling approach. Decided on Bayesian framework + two-part (hurdle-style) model for the consensus→final score deviation. Drafted `code/ror-sim-deviation.R` and `code/ror-analysis-score-models.R` (unfit — see "Where we left off"). Installed the `faux` R package (was missing locally) to verify the simulation script runs.
