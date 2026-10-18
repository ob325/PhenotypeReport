# Copyright 2025 Observational Health Data Sciences and Informatics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Simulate a Strategus Results Folder
#'
#' Creates a list of simulated data frames that mimic the CSV output produced
#' by a Strategus pipeline run, covering the DatabaseMetaData,
#' CohortGeneratorModule, and CohortDiagnosticsModule modules.
#'
#' @param nDatabases  Number of databases to simulate. Must be between 1 and 12.
#' @param nCohorts    Number of cohorts to simulate. Must be between 1 and 10.
#' @param seed        Integer random seed for reproducibility.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{databaseMetaData}{Data frame matching \code{database_meta_data.csv}.}
#'     \item{cohortDefinition}{Data frame matching \code{cg_cohort_definition.csv}.}
#'     \item{cohortCount}{Data frame matching \code{cg_cohort_count.csv}.}
#'     \item{cohortIncidence}{Data frame matching \code{cd_incidence_rate.csv}.}
#'   }
#'
#' @examples
#' simData <- simulateStrategusResults(nDatabases = 2, nCohorts = 3)
#' str(simData)
#'
#' @export
simulateStrategusResults <- function(nDatabases = 2,
                                     nCohorts = 3,
                                     seed = 42) {
  checkmate::assertInt(nDatabases, lower = 1, upper = 12)
  checkmate::assertInt(nCohorts, lower = 1, upper = 10)
  checkmate::assertInt(seed)

  set.seed(seed)

  dbPool <- data.frame(
    name = c(
      "Synpuf", "Ambnet", "MDCD", "CCAE", "MDCR",
      "OptumDOD", "CPRD GOLD", "JMDC", "CUIMC",
      "VA CDW", "Premier", "IQVIA"
    ),
    abbreviation = c(
      "SynPUF", "AMBNET", "MDCD", "CCAE", "MDCR",
      "OPTDOD", "CPRD", "JMDC", "CUIMC",
      "VACDW", "PREMIER", "IQVIA"
    ),
    holder = c(
      "CMS", "Inovalon", "IBM MarketScan", "IBM MarketScan", "IBM MarketScan",
      "Optum", "CPRD", "JMDC", "Columbia University",
      "US Dept of Veterans Affairs", "Premier Healthcare", "IQVIA"
    ),
    stringsAsFactors = FALSE
  )

  idx <- sample(nrow(dbPool), nDatabases, replace = FALSE)
  selectedDbs <- dbPool[idx, ]
  databaseIds <- gsub("[^a-z0-9]", "_", tolower(selectedDbs$name))

  databaseMetaData <- data.frame(
    cdm_source_name      = selectedDbs$name,
    cdm_source_abbreviation = selectedDbs$abbreviation,
    cdm_holder           = selectedDbs$holder,
    source_release_date  = as.Date("2023-01-01") - sample(0:365, nDatabases, replace = TRUE),
    cdm_release_date     = as.Date("2023-06-01") - sample(0:180, nDatabases, replace = TRUE),
    cdm_version          = "5.4",
    vocabulary_version   = sample(
      c("v20230601", "v20231001", "v20240101"),
      nDatabases,
      replace = TRUE
    ),
    database_id          = databaseIds,
    max_obs_period_end_date = as.Date("2022-12-31") - sample(0:90, nDatabases, replace = TRUE),
    stringsAsFactors = FALSE
  )

  cohortPool <- c(
    "T2DM Broad", "T2DM Narrow", "HF Incident",
    "AF Prevalent", "COPD Sensitive", "COPD Specific",
    "HTN Any", "CAD Broad", "Stroke First", "PE Any"
  )

  selectedCohorts <- sample(cohortPool, nCohorts, replace = FALSE)

  cohortDefinition <- data.frame(
    cohort_definition_id = seq_len(nCohorts),
    cohort_name          = selectedCohorts,
    description          = paste("Simulated cohort:", selectedCohorts),
    stringsAsFactors = FALSE
  )

  cohortCount <- expand.grid(
    database_id = databaseIds,
    cohort_id   = seq_len(nCohorts),
    stringsAsFactors = FALSE
  )
  cohortCount$cohort_subjects <- sample(500L:50000L, nrow(cohortCount), replace = TRUE)
  cohortCount$cohort_entries  <- round(
    cohortCount$cohort_subjects * stats::runif(nrow(cohortCount), 1.0, 1.5)
  )

  ageGroups <- c("18-34", "35-44", "45-54", "55-64", "65-74", "75+")
  years     <- 2010L:2026L
  nYears    <- length(years)

  # Multipliers relative to overall IR
  ageMults    <- c("18-34" = 0.25, "35-44" = 0.55, "45-54" = 1.00,
                   "55-64" = 1.70, "65-74" = 2.80, "75+"   = 3.80)
  genderMults <- c(Male = 1.18, Female = 0.85)

  # One baseline IR and person-years per cohort x database
  ciCombos <- expand.grid(
    database_id = databaseIds,
    cohort_id   = seq_len(nCohorts),
    stringsAsFactors = FALSE
  )
  nCombos <- nrow(ciCombos)

  baseIR  <- stats::runif(nCombos, min = 3, max = 18)   # per 100 person-years
  basePY  <- round(exp(stats::rnorm(nCombos, log(8000), 0.5)))  # person-years

  # Smooth year multipliers: random walk with slight drift per combo
  yearMults <- matrix(NA_real_, nrow = nCombos, ncol = nYears)
  for (i in seq_len(nCombos)) {
    steps          <- stats::rnorm(nYears - 1, mean = 0.01, sd = 0.05)
    yearMults[i, ] <- exp(cumsum(c(0, steps)))
  }

  # NA in age_group, gender, or calendar_year means "not stratified" (overall),
  # matching the convention used in real CohortDiagnostics output.
  buildRows <- function(ageVec, genderVec, yearVec) {
    grid <- expand.grid(
      combo_idx     = seq_len(nCombos),
      age_group     = ageVec,
      gender        = genderVec,
      calendar_year = yearVec,
      stringsAsFactors = FALSE
    )

    aM <- ifelse(is.na(grid$age_group), 1.0, ageMults[grid$age_group])
    gM <- ifelse(is.na(grid$gender),    1.0, genderMults[grid$gender])
    yM <- ifelse(
      is.na(grid$calendar_year), 1.0,
      yearMults[cbind(grid$combo_idx, match(grid$calendar_year, years))]
    )

    ir <- round(baseIR[grid$combo_idx] * aM * gM * yM, 2)
    py <- round(basePY[grid$combo_idx] * aM *
                  ifelse(is.na(grid$calendar_year), 1.0, 1 / nYears))
    cc <- pmax(1L, round(ir / 100 * py))

    cbind(
      ciCombos[grid$combo_idx, ],
      grid[, c("age_group", "gender", "calendar_year")],
      data.frame(
        cohort_count   = cc,
        person_years   = py,
        incidence_rate = ir,
        stringsAsFactors = FALSE
      )
    )
  }

  cohortIncidence <- rbind(
    buildRows(NA_character_,  NA_character_,                     NA_integer_),
    buildRows(ageGroups,      NA_character_,                     NA_integer_),
    buildRows(NA_character_,  c("Male", "Female"),               NA_integer_),
    buildRows(ageGroups,      c("Male", "Female"),               NA_integer_),
    buildRows(NA_character_,  c(NA_character_, "Male", "Female"), years),
    buildRows(ageGroups,      NA_character_,                     years),
    buildRows(ageGroups,      c("Male", "Female"),               years)
  )
  row.names(cohortIncidence) <- NULL

  list(
    databaseMetaData = databaseMetaData,
    cohortDefinition = cohortDefinition,
    cohortCount      = cohortCount,
    cohortIncidence  = cohortIncidence
  )
}

#' Write Simulated Strategus Results to Disk
#'
#' Writes the output of \code{\link{simulateStrategusResults}} to disk,
#' creating the folder structure expected by downstream PhenotypeReport
#' functions.
#'
#' @param simulatedResults A list returned by \code{\link{simulateStrategusResults}}.
#' @param outputFolder     Path to the root folder where module sub-folders
#'   will be created. The folder is created if it does not exist.
#'
#' @return Invisibly returns \code{outputFolder}.
#'
#' @examples
#' \dontrun{
#' simData <- simulateStrategusResults(nDatabases = 2, nCohorts = 3)
#' writeStrategusResults(simData, outputFolder = tempfile("strategus_sim"))
#' }
#'
#' @export
writeStrategusResults <- function(simulatedResults, outputFolder) {
  checkmate::assertList(simulatedResults)
  checkmate::assertNames(
    names(simulatedResults),
    must.include = c("databaseMetaData", "cohortDefinition", "cohortCount", "cohortIncidence")
  )
  checkmate::assertString(outputFolder)

  resultsFolder <- file.path(outputFolder, "results")
  databaseIds   <- simulatedResults$databaseMetaData$database_id

  for (dbId in databaseIds) {
    dbFolder                  <- file.path(resultsFolder, dbId)
    databaseMetaDataFolder    <- file.path(dbFolder, "DatabaseMetaData")
    cohortGeneratorFolder     <- file.path(dbFolder, "CohortGeneratorModule")
    cohortDiagnosticsFolder   <- file.path(dbFolder, "CohortDiagnosticsModule")

    dir.create(databaseMetaDataFolder,  recursive = TRUE, showWarnings = FALSE)
    dir.create(cohortGeneratorFolder,   recursive = TRUE, showWarnings = FALSE)
    dir.create(cohortDiagnosticsFolder, recursive = TRUE, showWarnings = FALSE)

    utils::write.csv(
      simulatedResults$databaseMetaData[simulatedResults$databaseMetaData$database_id == dbId, ],
      file.path(databaseMetaDataFolder, "database_meta_data.csv"),
      row.names = FALSE
    )

    utils::write.csv(
      simulatedResults$cohortDefinition,
      file.path(cohortGeneratorFolder, "cg_cohort_definition.csv"),
      row.names = FALSE
    )

    utils::write.csv(
      simulatedResults$cohortCount[simulatedResults$cohortCount$database_id == dbId, ],
      file.path(cohortGeneratorFolder, "cg_cohort_count.csv"),
      row.names = FALSE
    )

    utils::write.csv(
      simulatedResults$cohortIncidence[simulatedResults$cohortIncidence$database_id == dbId, ],
      file.path(cohortDiagnosticsFolder, "cd_incidence_rate.csv"),
      row.names = FALSE
    )
  }

  invisible(outputFolder)
}
