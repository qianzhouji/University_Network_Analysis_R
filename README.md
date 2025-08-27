# University_Network_Analysis_R

Comprehensive R scripts for studying how university students' learning habits relate to mental health using mixed graphical models and network comparison techniques.

## Repository structure
- `MGM.R` – main analysis pipeline. Reads cleaned data, converts categorical variables, fits a mixed graphical model, visualizes the full network, performs condition-specific modeling, bootstrap resampling, edge-difference tests, community detection, Bridge Expected Influence (BEI) calculations, and network comparison tests (NCT). Outputs tables and plots in `xls`, `Rdata`, `plot` folders.
- `Supporting_Function.R` – collection of helper functions for plotting bootstrap edges, fitting models by moderator level, bootstrapping, comparing edges across conditions, community detection, BEI computation, and NCT helpers.
- `Data_Clean/` – scripts used to preprocess the raw survey data:
  - `Cleaned_data.R` – replaces missing school names, keeps rows containing "学院" or "大学".
  - `Clean2.R` – removes fictional or secondary school names.
  - `Sort3分.R` – classifies schools into three tiers for moderation analysis.
  - `UniqueUnique_Schools.R` – inspects unique schools and their frequencies.
- `TEST_Code/` – self‑contained examples demonstrating how to use the supporting functions:
  - `BEI_Compute_Test.R` – calculates BEI on a toy network.
  - `Edge_Diff_Test.R` – runs condition-specific MGM, bootstrap, and edge difference tests.
  - `NCT_Test.R` – performs pairwise Network Comparison Tests.
- `README_for_SupportingFunctions.md` – bilingual documentation for each helper function.

## Workflow
1. **Data cleaning** – run scripts in `Data_Clean` to prepare the survey data and generate classified school categories for the moderator variable.
2. **Main analysis** – edit file paths in `MGM.R` and execute it in R. The script:
   - splits data by school classification and constructs an MGM on the full sample;
   - bootstraps each condition to identify significant edges and compare edge weights between groups;
   - detects communities and computes BEI for the overall and condition‑specific networks;
   - conducts Network Comparison Tests to evaluate global strength, network structure and edgewise differences.
   Results are written to the `plot`, `xls`, and `Rdata` directories created by the script.
3. **Function testing/examples** – run scripts under `TEST_Code` to see small reproducible examples of BEI computation, edge difference testing, and NCT workflows.

## Requirements
R (≥4.0) with packages: `mgm`, `qgraph`, `ggplot2`, `showtext`, `dplyr`, `igraph`, `patchwork`, `NetworkComparisonTest`, `purrr`, `tidyr`, `glue`, `glasso`, `bruceR`, `lme4`, `emmeans`. Install missing packages via `install.packages()`.

## Documentation
For detailed descriptions of the helper functions, see [README_for_SupportingFunctions.md](README_for_SupportingFunctions.md).
