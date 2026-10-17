# PhenotypeReport

An R package for generating epidemiological phenotype reports from [Strategus](https://github.com/OHDSI/Strategus) results folders.

## Overview

`PhenotypeReport` provides modular functions for reading, summarizing, and visualizing Strategus pipeline output. Each domain follows a consistent two-function pattern:

- `get*()` — reads CSVs from the results folder into a plain data frame
- `table*()` — renders a formatted [`gt`](https://gt.rstudio.com/) table for reports
- `plot*()` — produces a [`ggplot2`](https://ggplot2.tidyverse.org/) visualization

Results can be scoped to a subset of cohorts via a settings object, making it straightforward to compare multiple phenotype definitions side by side.

## Installation

```r
# Install from GitHub
remotes::install_github("ohdsi/PhenotypeReport")
```

## Quick Start

```r
library(PhenotypeReport)

# Point to your Strategus results folder
settings <- createSettings(
  resultsFolder = "path/to/strategus/results",
  cohortIds     = c(1L, 2L, 3L)   # omit to include all cohorts
)

# Database metadata
dbInfo <- getDatabaseInformation(settings)
tableDatabaseInformation(dbInfo)

# Cohort counts
counts <- getCohortCounts(settings)
tableCohortCounts(counts)
plotCohortCounts(counts)

# Cohort incidence
ci <- getCohortIncidence(settings)
plotCohortIncidence(ci, stratifyBy = c("year", "gender"))
```

## Simulating a Results Folder

No Strategus run yet? Generate synthetic data for exploration and testing:

```r
simData <- simulateStrategusResults(nDatabases = 3, nCohorts = 4, seed = 42)
writeStrategusResults(simData, outputFolder = "path/to/output")
```

The simulated folder mirrors the real Strategus output structure:

```
output/
  results/
    DatabaseMetaData/
      database_meta_data.csv
    CohortGeneratorModule/
      cg_cohort_definition.csv
      cg_cohort_count.csv
    CohortIncidenceModule/
      ci_incidence_summary.csv
```

## Modules

### Database Information

```r
dbInfo <- getDatabaseInformation(settings)
tableDatabaseInformation(dbInfo)
```

### Cohort Counts

```r
counts <- getCohortCounts(settings)
tableCohortCounts(counts)   # wide table: cohorts as rows, databases as column groups
plotCohortCounts(counts)
```

### Cohort Incidence

`stratifyBy` accepts a character vector of one, two, or three of `"age"`, `"gender"`, `"year"`:

| `stratifyBy` | Plot type |
|---|---|
| `"overall"` | Grouped bar chart |
| `"age"` | Line chart by age group |
| `"gender"` | Grouped bar chart by gender |
| `"year"` | Line chart over time |
| `c("year", "gender")` | `facet_grid(cohort ~ database)`, lines by gender |
| `c("year", "age")` | `facet_grid(cohort ~ database)`, lines by age group |
| `c("age", "gender")` | `facet_grid(cohort ~ database)`, lines by gender across age groups |
| `c("age", "gender", "year")` | One plot per database: `facet_grid(cohort ~ age group)`, lines by sex |

```r
ci <- getCohortIncidence(settings)

# Single strata
plotCohortIncidence(ci, stratifyBy = "year")

# Two strata
plotCohortIncidence(ci, stratifyBy = c("year", "gender"))

# Three strata — returns a named list of ggplot objects, one per database
plots <- plotCohortIncidence(ci, stratifyBy = c("age", "gender", "year"))
plots[["synpuf"]]
```

## Code Style

This package follows the [OHDSI HADES code style](https://ohdsi.github.io/Hades/codeStyle.html).

## License

Apache License 2.0
