# Repository Guidelines

## How to Use This Guide

- Start here for project-level norms. KoTaP is an academic multivariate financial analysis project over Korean listed firms.
- Treat `R/Kotap analisis 9 de mayo.R` as the current methodological reference for the analysis objective unless a later reviewed script replaces it.
- The objective is to understand the behavior of the constructed target variable `target_ROA`, defined as next-year ROA: `target_ROA(t) = ROA(t + 1)` for the same firm.
- When guidance conflicts, course methodology constraints and temporal causality rules override convenience.

## Available Skills

Use these skills for detailed patterns on-demand:

### Generic Skills

| Skill | Description | URL |
|-------|-------------|-----|
| `sdd-explore` | Explore ideas, code, data flow, and methodological risks before changing files | `/home/sebastian/.config/opencode/skills/sdd-explore/SKILL.md` |
| `sdd-propose` | Create an SDD proposal from an exploration | `/home/sebastian/.config/opencode/skills/sdd-propose/SKILL.md` |
| `sdd-spec` | Write requirements and scenarios for a proposed change | `/home/sebastian/.config/opencode/skills/sdd-spec/SKILL.md` |
| `sdd-design` | Define technical design and analysis approach | `/home/sebastian/.config/opencode/skills/sdd-design/SKILL.md` |
| `sdd-tasks` | Break the change into implementation tasks | `/home/sebastian/.config/opencode/skills/sdd-tasks/SKILL.md` |
| `sdd-apply` | Implement planned SDD tasks | `/home/sebastian/.config/opencode/skills/sdd-apply/SKILL.md` |
| `sdd-verify` | Verify implementation against specs and project constraints | `/home/sebastian/.config/opencode/skills/sdd-verify/SKILL.md` |
| `cognitive-doc-design` | Write guides and docs that are easy to scan and review | `/home/sebastian/.config/opencode/skills/cognitive-doc-design/SKILL.md` |

### Auto-invoke Skills

When performing these actions, ALWAYS invoke the corresponding skill FIRST:

| Action | Skill |
|--------|-------|
| Explore a methodological question or existing R pipeline | `sdd-explore` |
| Plan a new analysis objective or refactor | `sdd-propose` |
| Define requirements for PCA, K-Means, OLS, or temporal split behavior | `sdd-spec` |
| Design a reusable analysis flow or Shiny/script alignment | `sdd-design` |
| Break implementation into safe steps | `sdd-tasks` |
| Modify R scripts after planning | `sdd-apply` |
| Check that outputs match the intended methodology | `sdd-verify` |
| Write README, AGENTS, methodology notes, or presentation-facing docs | `cognitive-doc-design` |

---

## Project Overview

KoTaP studies firms listed in KOSPI and KOSDAQ using classical multivariate analysis. The main goal is to explain and interpret the behavior of `target_ROA`, a forward-looking profitability variable built from the panel structure.

| Component | Location | Purpose |
|-----------|----------|---------|
| Main R analysis | `R/Kotap analisis 9 de mayo.R` | Current methodological reference: target construction, temporal split, EDA, PCA, K-Means, OLS |
| Dataset preparation | `R/Dataset_Nuevo.R` | Dataset construction and exploratory variants |
| Slides dataset | `R/Dataset_Nuevo_Diapos.R` | Reduced/support version for presentation |
| Shiny dashboard | `R/shiny/app.R` | Interactive exploration of the analysis flow |
| Figures and results | `output/` | Generated charts, CSVs, summaries, and model outputs |

---

## Dataset Source

Use the Zenodo record as the canonical source reference for KoTaP metadata, but verify citation details before final submission.

| Field | Detail |
|-------|--------|
| Source URL | https://zenodo.org/records/17149808 |
| Title | KoTaP: A Panel Dataset for Corporate Tax Avoidance, Performance, and Governance in Korea (2011–2024) |
| Record page DOI | `10.5281/zenodo.17149808` |
| Published/version | September 18, 2025, v2 |
| Authors/creators | Hyungjong Na, Hyungjoon Kim, WonHo Song, Sejin Myung, Seungyong Han, Donghyeon Jo |
| Files | `KoTaP_Dataset.csv`, `README.md`, `Supplementary.zip` |
| README facts | 12,653 firm-year observations; 1,754 non-financial firms; CSV UTF-8; 65 variables; 2011–2024 |
| Categories | Tax avoidance, profitability, stability, growth, governance |

Dataset gotchas:

- The panel is unbalanced: no missing values does not mean every firm has every year.
- Outliers are retained by the source; apply project-specific outlier handling after creating `target_ROA`.
- Supplementary machine-learning materials are context only and stay out of the core syllabus analysis.
- The DOI/license details are inconsistent between the Zenodo page and README; verify them before the final citation.

---

## Methodological Constraints

The main R analysis must stay inside the classical multivariate methods expected by the course:

- Descriptive statistics: mean, median, standard deviation, coefficient of variation.
- Shape diagnostics: skewness and kurtosis.
- Covariance and Spearman correlation.
- Eigenvalues, eigenvectors, and PCA.
- K-Means clustering.
- Multiple linear regression OLS with diagnostics: VIF, Breusch-Pagan, Durbin-Watson, Anderson-Darling.

Do not introduce advanced machine learning models into the main analysis unless explicitly marked as an external extension and kept out of the core methodology.

---

## Target Variable and Temporal Logic

`target_ROA` is not the same-year ROA. It is constructed as:

```r
target_ROA = lead(ROA, n = 1)
```

grouped by firm. Therefore, each row uses financial characteristics from year `t` to explain ROA from year `t + 1`.

### Required order

1. Load and type the complete panel.
2. Create `target_ROA` on the full firm-year panel.
3. Remove outliers from predictors using IQR × 3.
4. Split temporally.

Do not remove outliers before creating the target. If a year is removed first, the lead may jump from `t` to `t + 2`, breaking the meaning of “next-year ROA”.

### Why temporal analysis must not mix all years as if they were one cross-section

The dataset is a panel: the same firm can appear in multiple years. If all years are mixed without temporal control, the analysis treats repeated observations from the same company as independent and can leak future information into interpretation or validation.

For prediction and validation, keep time ordered:

| Set | Rows | Interpretation |
|-----|------|----------------|
| Train | `year <= 2022` and valid `target_ROA` | Learn historical relationships |
| Test | `year == 2023` and valid `target_ROA` | Predict/validate ROA 2024 |
| Future | `year == 2024` | Predict ROA 2025, no ground truth yet |

For an explanatory PCA snapshot, use a single year when the question is “how are firms positioned in a specific economic moment?”. PCA is sensitive to scale, variance, and covariance. Pooling many years can mix structural differences between firms with macroeconomic time effects, so components may describe “period effects” instead of the financial profile of companies.

---

## R Development

```r
# Main analysis
source("R/Kotap analisis 9 de mayo.R")

# Shiny dashboard
shiny::runApp("R/shiny/app.R")
```

Before running, update local paths inside the script:

```r
DATA_PATH  <- "path/to/KoTaP_Dataset.csv"
OUTPUT_DIR <- "path/to/output_kotap_v14"
```

Important: the script contains `readline()` pauses. In non-interactive execution, remove or bypass them deliberately.

---

## PCA Guidance

For PCA, use numeric explanatory variables only. Exclude identifiers, raw accounting totals when ratios already summarize them, and the target variable.

Recommended starting point:

```r
vars_pca <- c(
  "SIZE", "LEV", "CUR", "GRW", "CFO", "PPE", "AGE", "INVREC", "MB", "TQ",
  "GETR", "CETR", "GETR3", "CETR3", "GETR5", "CETR5",
  "TSTA", "TSDA", "A_GETR", "A_CETR", "A_GETR3", "A_CETR3", "A_GETR5", "A_CETR5",
  "forn", "own", "KOSPI", "big4", "LOSS"
)
```

Then filter collinearity before PCA. In the current script, the manual post-correlation removal is:

```r
VARS_REMOVIDAS <- c("GETR3", "GETR5", "A_GETR3", "A_GETR5")
```

Keep `ROE` out of the main candidate set because it shares net income structure with ROA and can introduce endogeneity or leakage-like interpretation.

---

## Commit & Pull Request Guidelines

Follow conventional-commit style:

```text
<type>[scope]: <description>
```

Allowed types:

- `docs`
- `fix`
- `feat`
- `refactor`
- `chore`
- `test`

Before asking for review:

1. Explain whether the change affects methodology or only presentation.
2. Preserve the temporal construction of `target_ROA`.
3. Confirm that the main analysis still uses only syllabus-approved methods.
4. Avoid committing generated large outputs unless they are required for delivery.
