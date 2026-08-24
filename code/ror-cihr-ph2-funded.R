#  program:  ror-cihr-ph2-funded.R
#  task:     pull funded Project Grant proposals for the Public, Community
#            & Population Health 2 (PH2) committee, by investigator and
#            competition cycle, from CIHR's public Investments open data
#  input:    data/cihr_investments_investissements_irsc_202122.xlsx
#  output:   printed table; output/cihr-ph2-funded-2021-22.csv
#  project:  RoR
#  author:   sam harper \ 2026-08-24

library(here)
library(tidyverse)
library(readxl)

path <- here("data", "cihr_investments_investissements_irsc_202122.xlsx")

# only the Grants & Awards tab is needed -- see
# code/ror-cihr-committee-size-check.R for why the other 4 tabs
# (multiple rows per project) aren't relevant here
raw <- read_excel(path, sheet = "G&A_S&B")

ph2 <- raw |>
  filter(
    ProgramNameEN_NomProgrammeAN == "Project Grant",
    CommitteeNameEN_NomComiteAN == "Public, Community & Population Health 2"
  ) |>
  transmute(
    investigator = paste(FirstName_Prenom, FamilyName_NomFamille),
    cycle        = CompetitionCode_CodeConcours,
    institution  = ResearchInstitutionNameEN_NomEtablissementRechercheAN,
    amount_awarded = TotalAmountAwarded_MontantTotalAccorde,
    title        = ApplicationTitle_TitreDemande
  ) |>
  distinct() |>
  arrange(cycle, investigator)

cat("=== PH2 (Population Health 2) funded Project Grants, 2021-22 file ===\n")
cat("n funded proposals:", nrow(ph2), "\n")
cat("competition cycles present:", paste(sort(unique(ph2$cycle)), collapse = ", "), "\n\n")

print(ph2 |> select(cycle, investigator, institution, amount_awarded), n = Inf)

dir.create(here("output"), showWarnings = FALSE, recursive = TRUE)
write_csv(ph2, here("output", "cihr-ph2-funded-2021-22.csv"))
cat("\nSaved to output/cihr-ph2-funded-2021-22.csv\n")
