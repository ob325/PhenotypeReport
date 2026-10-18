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

#' Get Cohort Counts
#'
#' Reads cohort counts and cohort definitions from a Strategus results folder
#' and returns a combined data frame.
#'
#' @param settings A \code{PhenotypeReportSettings} object created by
#'   \code{\link{createSettings}}.
#'
#' @return A data frame with one row per database-cohort combination containing
#'   columns \code{database_id}, \code{cohort_id}, \code{cohort_name},
#'   \code{cohort_subjects}, and \code{cohort_entries}.
#'
#' @export
getCohortCounts <- function(settings) {
  checkmate::assertClass(settings, "PhenotypeReportSettings")

  dbFolders <- list.dirs(settings$resultsFolder, recursive = FALSE, full.names = TRUE)

  counts <- do.call(rbind, lapply(dbFolders, function(dbFolder) {
    csvPath <- file.path(dbFolder, "CohortGeneratorModule", "cg_cohort_count.csv")
    if (!file.exists(csvPath)) return(NULL)
    utils::read.csv(csvPath, stringsAsFactors = FALSE)
  }))

  # Cohort definitions are identical across databases; read from the first available
  defPath <- Filter(file.exists,
                    file.path(dbFolders, "CohortGeneratorModule", "cg_cohort_definition.csv"))[[1]]
  definitions <- utils::read.csv(defPath, stringsAsFactors = FALSE)

  data <- merge(
    counts,
    definitions[, c("cohort_definition_id", "cohort_name")],
    by.x = "cohort_id",
    by.y = "cohort_definition_id",
    all.x = TRUE
  )

  if (!is.null(settings$cohortIds)) {
    data <- data[data$cohort_id %in% settings$cohortIds, ]
  }

  data[, c("database_id", "cohort_id", "cohort_name", "cohort_subjects", "cohort_entries")]
}

#' Plot Cohort Counts
#'
#' Creates a grouped bar chart of cohort subject counts by database.
#'
#' @param cohortCounts A data frame returned by \code{\link{getCohortCounts}}.
#'
#' @return A \code{ggplot} object.
#'
#' @export
plotCohortCounts <- function(cohortCounts) {
  checkmate::assertDataFrame(cohortCounts)
  checkmate::assertNames(
    names(cohortCounts),
    must.include = c("database_id", "cohort_name", "cohort_subjects")
  )

  ggplot2::ggplot(
    cohortCounts,
    ggplot2::aes(
      x    = .data$cohort_name,
      y    = .data$cohort_subjects,
      fill = .data$database_id
    )
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(
      title = "Cohort Counts by Database",
      x     = NULL,
      y     = "Persons",
      fill  = "Database"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))
}

#' Create a Cohort Counts Table
#'
#' Formats cohort counts as a wide \code{gt} table with one row per database
#' and one column group per cohort, showing subjects and entries.
#'
#' @param cohortCounts A data frame returned by \code{\link{getCohortCounts}}.
#'
#' @return A \code{gt_tbl} object.
#'
#' @export
tableCohortCounts <- function(cohortCounts) {
  checkmate::assertDataFrame(cohortCounts)
  checkmate::assertNames(
    names(cohortCounts),
    must.include = c("database_id", "cohort_name", "cohort_subjects", "cohort_entries")
  )

  databaseIds <- unique(cohortCounts$database_id)

  wide <- stats::reshape(
    cohortCounts[, c("cohort_name", "database_id", "cohort_subjects", "cohort_entries")],
    idvar     = "cohort_name",
    timevar   = "database_id",
    direction = "wide"
  )

  subjectCols <- paste0("cohort_subjects.", databaseIds)
  entryCols   <- paste0("cohort_entries.", databaseIds)
  colOrder    <- c("cohort_name", as.vector(rbind(subjectCols, entryCols)))
  wide        <- wide[, colOrder]

  subjectLabels <- stats::setNames(
    as.list(rep("Persons", length(databaseIds))),
    subjectCols
  )
  entryLabels <- stats::setNames(
    as.list(rep("Events", length(databaseIds))),
    entryCols
  )

  tbl <- gt::gt(wide) |>
    gt::cols_label(.list = c(list(cohort_name = "Cohort"), subjectLabels, entryLabels)) |>
    gt::tab_header(title = "Cohort Counts")

  for (db in databaseIds) {
    tbl <- gt::tab_spanner(
      tbl,
      label   = db,
      columns = c(
        paste0("cohort_subjects.", db),
        paste0("cohort_entries.", db)
      )
    )
  }

  tbl |>
    gt::fmt_integer(columns = -"cohort_name") |>
    gt::opt_stylize(style = 1) |>
    gt::cols_align(align = "left", columns = "cohort_name") |>
    gt::cols_align(align = "right", columns = -"cohort_name")
}
