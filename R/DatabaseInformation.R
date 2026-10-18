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

#' Get Database Information
#'
#' Reads database metadata from a Strategus results folder.
#'
#' @param settings A \code{PhenotypeReportSettings} object created by
#'   \code{\link{createSettings}}.
#'
#' @return A data frame with one row per database containing database metadata.
#'
#' @export
getDatabaseInformation <- function(settings) {
  checkmate::assertClass(settings, "PhenotypeReportSettings")

  dbFolders <- list.dirs(settings$resultsFolder, recursive = FALSE, full.names = TRUE)
  checkmate::assert(length(dbFolders) > 0,
                    .var.name = "settings$resultsFolder",
                    "No database sub-folders found in results folder.")

  data <- do.call(rbind, lapply(dbFolders, function(dbFolder) {
    csvPath <- file.path(dbFolder, "DatabaseMetaData", "database_meta_data.csv")
    if (!file.exists(csvPath)) return(NULL)
    utils::read.csv(csvPath, stringsAsFactors = FALSE)
  }))

  data$source_release_date     <- as.Date(data$source_release_date)
  data$cdm_release_date        <- as.Date(data$cdm_release_date)
  data$max_obs_period_end_date <- as.Date(data$max_obs_period_end_date)

  data
}

#' Create a Database Information Table
#'
#' Formats database metadata as a \code{gt} table suitable for inclusion in a
#' Quarto or R Markdown report.
#'
#' @param databaseInformation A data frame returned by
#'   \code{\link{getDatabaseInformation}}.
#'
#' @return A \code{gt_tbl} object.
#'
#' @export
tableDatabaseInformation <- function(databaseInformation) {
  checkmate::assertDataFrame(databaseInformation)
  checkmate::assertNames(
    names(databaseInformation),
    must.include = c(
      "cdm_source_name", "cdm_version", "vocabulary_version",
      "source_release_date", "cdm_release_date", "max_obs_period_end_date"
    )
  )

  display <- databaseInformation[, c(
    "cdm_source_name",
    "cdm_version",
    "vocabulary_version",
    "source_release_date",
    "cdm_release_date",
    "max_obs_period_end_date"
  )]

  gt::gt(display) |>
    gt::cols_label(
      cdm_source_name          = "Database",
      cdm_version              = "CDM Version",
      vocabulary_version       = "Vocabulary Version",
      source_release_date      = "Source Release Date",
      cdm_release_date         = "CDM Release Date",
      max_obs_period_end_date  = "Max Observation Date"
    ) |>
    gt::tab_header(title = "Database Information") |>
    gt::opt_stylize(style = 1) |>
    gt::cols_align(align = "left")
}
