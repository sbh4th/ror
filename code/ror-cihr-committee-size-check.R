#  program:  ror-cihr-committee-size-check.R
#  task:     Use CIHR's public "Investments" open data (open.canada.ca) to
#            check our simulation's assumption of a fixed number of
#            applications per committee against real funded-application
#            counts, backed out via the approximate Project Grant funding
#            rate. Prompted by wondering whether pool_per_cmte (40, in
#            ror-sim-streamlining-experiment.R) or a fixed app_n (15, in
#            ror-sim-aim1.R) are realistic given real committees vary in
#            size (e.g. Population Health / Health Services split into
#            multiple subcommittees) -- see ror-research-log.qmd.
#  input:    data/cihr_investments_investissements_irsc_*.xlsx
#            (downloaded manually from open.canada.ca -- not committed to
#            git, see .gitignore)
#  output:   printed summary only for now; nothing written to disk
#  project:  RoR
#  author:   sam harper \ 2026-08-14
#
#  note:     CIHR's "Investments" file is FUNDED projects only (no record
#            of unfunded applications), one row per project per fiscal
#            year of *payment* -- multi-year grants appear in every
#            fiscal-year file that overlaps their payment term, tagged
#            with their original CompetitionCode/CompetitionFY, not the
#            file's own fiscal year. So a single recent file already
#            contains committee-level funded counts for several past
#            competition cycles (confirmed: the 2024-25 file has rows
#            tagged back to competition cycle 201516). Very old/small
#            cycle counts in a given file are incomplete remnants (a
#            long grant's last payment, not that competition's full
#            funded list) -- only cycles with counts in the same range as
#            the most recent ones should be treated as essentially
#            complete.
#
#            "Project Grant" (exact ProgramNameEN match) is the base
#            competition; "Project Grant - Priority Announcement: ..."
#            rows are a related but distinct program name. Checked: zero
#            overlap in FundingReferenceNumber between the two in the
#            2024-25 file, so filtering to the exact "Project Grant"
#            string does not double-count and does not need to also
#            exclude the Priority Announcement rows separately.

##  0 Load needed packages ----
library(here)
library(tidyverse)
library(readxl)

##  1 Parameters ----

# Which downloaded file(s) to read -- add more paths as more years get
# downloaded (see note above: one recent file already covers several
# past cycles, so this doesn't strictly need to grow, but pooling
# multiple files' worth of *fully-elapsed* cycles would give more of
# them at reliable completeness).
investment_files <- c(
  here("data", "cihr_investments_investissements_irsc_202425.xlsx")
)

# Which competition cycles to treat as essentially complete for this
# file -- chosen because their total funded count is in the same range
# as the most recent cycle, unlike older/tapering ones. Revisit this
# list by eye (see the diagnostic print in section 2) whenever a new
# file is added, rather than assuming it transfers unchanged.
complete_cycles <- c("202209PJT", "202303PJT", "202309PJT", "202403PJT")

# Approximate, not fixed, per Sam -- CIHR's own equalization documentation
# (ror-research-log.qmd, 2026-08-04 entry) is consistent with a rate in
# roughly this range. Sensitivity across a few plausible values rather
# than one point estimate.
funding_rates <- c(0.12, 0.15, 0.18)

##  2 Read + filter to Project Grant ----
## Only the Grants & Awards tab ("G&A_S&B") is needed -- the other 4
## tabs (Research Team, Partners, Research Categories, Institutions) are
## multiple-rows-per-project and irrelevant here, so reading only this
## sheet is most of the practical speedup for a 12MB file.

raw <- map_dfr(investment_files, ~read_excel(.x, sheet = "G&A_S&B"))

pg <- raw |>
  filter(ProgramNameEN_NomProgrammeAN == "Project Grant") |>
  select(
    FundingReferenceNumber_NumeroReferenceFinancement,
    CompetitionCode_CodeConcours,
    CommitteeNameEN_NomComiteAN
  ) |>
  distinct()

cat("=== Diagnostic: total funded count by competition cycle ===\n")
cat("(use this to sanity-check/update complete_cycles above -- cycles\n")
cat("with a much smaller total than their neighbours are incomplete\n")
cat("remnants, not that competition's real funded count)\n")
print(pg |> count(CompetitionCode_CodeConcours, name = "n_funded_total") |>
  arrange(CompetitionCode_CodeConcours))

##  3 Per-committee funded counts, pooled across the complete cycles ----

per_cycle_committee <- pg |>
  filter(CompetitionCode_CodeConcours %in% complete_cycles) |>
  count(CompetitionCode_CodeConcours, CommitteeNameEN_NomComiteAN,
    name = "n_funded")

cat("\n=== Funded applications per committee per cycle",
  "(pooled across", length(complete_cycles), "cycles) ===\n")
print(summary(per_cycle_committee$n_funded))
cat("SD:", round(sd(per_cycle_committee$n_funded), 2), "\n")
cat("n committee-cycle observations:", nrow(per_cycle_committee), "\n")

# saved as raw funded counts (not yet divided by an assumed funding
# rate) so any consumer -- e.g. ror-sim-streamlining-experiment.R -- can
# apply/vary the rate assumption itself rather than inheriting a rate
# baked in here
saveRDS(per_cycle_committee$n_funded,
  here("output", "cihr-funded-per-committee.rds"))

##  4 Back out implied total applications per committee ----
## funded / assumed funding rate = implied applicant pool for that
## committee-cycle. Sensitivity across funding_rates rather than a
## single assumed value.

cat("\n=== Implied total applications per committee, by assumed funding rate ===\n")
for (rate in funding_rates) {
  implied <- per_cycle_committee$n_funded / rate
  cat(sprintf(
    "funding rate %2.0f%%: median implied applications/committee = %.0f (IQR %.0f-%.0f, range %.0f-%.0f)\n",
    rate * 100, median(implied), quantile(implied, .25), quantile(implied, .75),
    min(implied), max(implied)))
}

cat("\nFor comparison, current simulation assumptions:\n")
cat("- ror-sim-aim1.R: fixed app_n = 15 (discussed applications per committee)\n")
cat("- ror-sim-streamlining-experiment.R: pool_per_cmte = 40 (candidate pool per committee)\n")
