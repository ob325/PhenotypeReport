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

.filterIndexEvents <- function(data, standard) {
  if (standard) {
    data[!is.na(data$standard_concept) & data$standard_concept == "S", ]
  } else {
    data[is.na(data$standard_concept) | data$standard_concept != "S", ]
  }
}

#' Get Index Event Breakdown
#'
#' Reads index event breakdown data from each database's
#' \code{CohortDiagnosticsModule} folder, merges with the concept reference
#' table, and returns a combined data frame.
#'
#' @param settings A \code{PhenotypeReportSettings} object created by
#'   \code{\link{createSettings}}.
#'
#' @return A data frame with one row per database-cohort-concept combination
#'   containing columns \code{database_id}, \code{cohort_id},
#'   \code{cohort_name}, \code{concept_id}, \code{concept_name},
#'   \code{domain_id}, \code{vocabulary_id}, \code{concept_class_id},
#'   \code{standard_concept}, \code{concept_code}, \code{domain_table},
#'   \code{domain_field}, \code{concept_count}, \code{subject_count}, and
#'   \code{pct} (percentage of total concept count for that cohort-database).
#'
#' @export
getIndexEventBreakdown <- function(settings) {
  checkmate::assertClass(settings, "PhenotypeReportSettings")

  dbFolders <- list.dirs(settings$resultsFolder, recursive = FALSE, full.names = TRUE)

  breakdown <- do.call(rbind, lapply(dbFolders, function(dbFolder) {
    csvPath <- file.path(dbFolder, "CohortDiagnosticsModule", "cd_index_event_breakdown.csv")
    if (!file.exists(csvPath)) return(NULL)
    utils::read.csv(csvPath, stringsAsFactors = FALSE)
  }))

  if (is.null(breakdown) || nrow(breakdown) == 0) {
    stop("No cd_index_event_breakdown.csv files found in results folder.")
  }

  conceptPaths <- Filter(file.exists,
                         file.path(dbFolders, "CohortDiagnosticsModule", "cd_concept.csv"))
  if (length(conceptPaths) == 0) {
    stop("No cd_concept.csv found in any database folder.")
  }
  concepts <- utils::read.csv(conceptPaths[[1]], stringsAsFactors = FALSE)

  data <- merge(
    breakdown,
    concepts[, c("concept_id", "concept_name", "domain_id", "vocabulary_id",
                 "concept_class_id", "standard_concept", "concept_code")],
    by     = "concept_id",
    all.x  = TRUE
  )

  defPath <- Filter(file.exists,
                    file.path(dbFolders, "CohortGeneratorModule", "cg_cohort_definition.csv"))[[1]]
  definitions <- utils::read.csv(defPath, stringsAsFactors = FALSE)
  data <- merge(
    data,
    definitions[, c("cohort_definition_id", "cohort_name")],
    by.x  = "cohort_id",
    by.y  = "cohort_definition_id",
    all.x = TRUE
  )

  abbrevs <- .readDatabaseAbbreviations(dbFolders)
  if (!is.null(abbrevs)) {
    data <- merge(data, abbrevs, by = "database_id", all.x = TRUE)
    data$database_id <- ifelse(!is.na(data$cdm_source_abbreviation),
                               data$cdm_source_abbreviation, data$database_id)
    data$cdm_source_abbreviation <- NULL
  }

  if (!is.null(settings$cohortIds)) {
    data <- data[data$cohort_id %in% settings$cohortIds, ]
  }

  # pct = each concept's share of all concept counts within its cohort+database
  totals <- stats::aggregate(
    concept_count ~ cohort_id + database_id,
    data = data,
    FUN  = sum
  )
  names(totals)[3] <- "total_count"
  data <- merge(data, totals, by = c("cohort_id", "database_id"))
  data$pct <- round(data$concept_count / data$total_count * 100, 1)
  data$total_count <- NULL

  data[order(data$cohort_name, data$database_id, -data$concept_count),
       c("database_id", "cohort_id", "cohort_name",
         "concept_id", "concept_name", "domain_id", "vocabulary_id",
         "concept_class_id", "standard_concept", "concept_code",
         "domain_table", "domain_field",
         "concept_count", "subject_count", "pct")]
}

#' Plot Index Event Breakdown
#'
#' Creates a horizontal bar chart of the most frequent index event concepts,
#' faceted by cohort and coloured by database.
#'
#' @param indexEvents A data frame returned by \code{\link{getIndexEventBreakdown}}.
#' @param standard    If \code{TRUE} (default), plots standard concepts
#'   (\code{standard_concept = "S"}); \code{FALSE} plots non-standard concepts.
#' @param topN        Maximum number of concepts to display, ranked by total
#'   concept count summed across all databases and cohorts. Default \code{20}.
#'
#' @return A \code{ggplot} object.
#'
#' @export
plotIndexEventBreakdown <- function(indexEvents, standard = TRUE, topN = 20L) {
  checkmate::assertDataFrame(indexEvents)
  checkmate::assertFlag(standard)
  checkmate::assertInt(topN, lower = 1L)

  d <- .filterIndexEvents(indexEvents, standard)
  if (nrow(d) == 0) {
    stop("No ", if (standard) "standard" else "non-standard",
         " concepts found in indexEvents.")
  }

  # Select top N concepts by total concept_count across all cohorts and databases
  globalTotals <- stats::aggregate(
    concept_count ~ concept_id + concept_name,
    data = d,
    FUN  = sum
  )
  globalTotals <- globalTotals[order(-globalTotals$concept_count), ]
  topIds <- globalTotals$concept_id[seq_len(min(topN, nrow(globalTotals)))]
  d <- d[d$concept_id %in% topIds, ]

  # Attach global total for ordering
  d <- merge(d,
             globalTotals[, c("concept_id", "concept_count")],
             by     = "concept_id",
             suffixes = c("", "_global"))

  ggplot2::ggplot(d, ggplot2::aes(
    x    = .data$concept_count,
    y    = stats::reorder(.data$concept_name, .data$concept_count_global),
    fill = .data$database_id
  )) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::facet_wrap(~cohort_name, scales = "free_x") +
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::labs(
      title = paste("Top", topN,
                    if (standard) "Standard" else "Non-Standard",
                    "Index Events"),
      x    = "Concept Count",
      y    = NULL,
      fill = "Database"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8))
}

#' Create an Index Event Breakdown Table
#'
#' Formats index event breakdown data as a wide \code{gt} table grouped by
#' cohort, with one percentage column per database showing each concept's
#' contribution to total index events within that database.
#'
#' @param indexEvents A data frame returned by \code{\link{getIndexEventBreakdown}}.
#' @param standard    If \code{TRUE} (default), shows standard concepts
#'   (\code{standard_concept = "S"}); \code{FALSE} shows non-standard concepts.
#'
#' @return A \code{gt_tbl} object.
#'
#' @export
tableIndexEventBreakdown <- function(indexEvents, standard = TRUE) {
  checkmate::assertDataFrame(indexEvents)
  checkmate::assertFlag(standard)

  d <- .filterIndexEvents(indexEvents, standard)
  if (nrow(d) == 0) {
    stop("No ", if (standard) "standard" else "non-standard",
         " concepts found in indexEvents.")
  }

  # Sort rows within each cohort by mean pct across databases (highest first)
  meanPct <- stats::aggregate(pct ~ cohort_name + concept_id, data = d, FUN = mean)
  names(meanPct)[3] <- "mean_pct"

  wide <- tidyr::pivot_wider(
    d[, c("cohort_name", "concept_id", "concept_name", "concept_code",
          "domain_field", "vocabulary_id", "database_id", "pct")],
    names_from  = "database_id",
    values_from = "pct"
  )
  wide <- merge(wide, meanPct, by = c("cohort_name", "concept_id"))
  wide <- wide[order(wide$cohort_name, -wide$mean_pct), ]
  wide$mean_pct <- NULL

  dbCols <- setdiff(
    names(wide),
    c("cohort_name", "concept_id", "concept_name", "concept_code",
      "domain_field", "vocabulary_id")
  )

  gt::gt(wide, groupname_col = "cohort_name") |>
    gt::tab_spanner("% of Index Events", columns = dplyr::all_of(dbCols)) |>
    gt::cols_label(
      concept_id    = "Concept ID",
      concept_name  = "Concept Name",
      concept_code  = "Code",
      domain_field  = "Domain Field",
      vocabulary_id = "Vocabulary"
    ) |>
    gt::tab_header(
      title = paste(if (standard) "Standard" else "Non-Standard", "Index Events")
    ) |>
    gt::fmt_number(columns = dplyr::all_of(dbCols), decimals = 1) |>
    gt::opt_stylize(style = 1) |>
    gt::cols_align("left",  columns = c("concept_name", "concept_code",
                                        "domain_field", "vocabulary_id")) |>
    gt::cols_align("right", columns = c("concept_id", dplyr::all_of(dbCols)))
}
