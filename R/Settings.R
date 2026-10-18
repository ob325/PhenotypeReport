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

#' Create Phenotype Report Settings
#'
#' @param resultsFolder Path to the Strategus results folder (the folder
#'   containing sub-folders such as \code{DatabaseMetaData} and
#'   \code{CohortGeneratorModule}).
#' @param cohortIds     Integer vector of cohort definition IDs to include in
#'   the report. If \code{NULL} (default), all cohorts found in the results are
#'   included.
#' @param minAge        Lower bound of the youngest 10-year age group to include
#'   (e.g. \code{10} for the \code{"10-19"} group). Must be a multiple of 10.
#'   Defaults to \code{10}.
#' @param maxAge        Upper bound of the oldest 10-year age group to include
#'   (e.g. \code{99} for the \code{"90-99"} group). Must satisfy
#'   \code{(maxAge + 1) \%\% 10 == 0}. Defaults to \code{99}.
#'
#' @return An object of class \code{PhenotypeReportSettings}.
#'
#' @examples
#' settings <- createSettings(resultsFolder = "path/to/results")
#' settings <- createSettings(resultsFolder = "path/to/results", cohortIds = c(1L, 2L))
#' settings <- createSettings(resultsFolder = "path/to/results", minAge = 20L, maxAge = 79L)
#'
#' @export
createSettings <- function(resultsFolder,
                           cohortIds = NULL,
                           minAge    = 10L,
                           maxAge    = 99L) {
  checkmate::assertDirectoryExists(resultsFolder)
  checkmate::assertIntegerish(cohortIds, null.ok = TRUE, any.missing = FALSE)
  checkmate::assertInt(minAge, lower = 0L)
  checkmate::assertInt(maxAge, lower = 9L)

  if (minAge %% 10L != 0L) {
    stop("minAge must be the lower bound of a 10-year age group (a multiple of 10, e.g. 0, 10, 20, ..., 90).")
  }
  if ((maxAge + 1L) %% 10L != 0L) {
    stop("maxAge must be the upper bound of a 10-year age group (e.g. 9, 19, 29, ..., 99).")
  }
  if (minAge >= maxAge) {
    stop("minAge (", minAge, ") must be less than maxAge (", maxAge, ").")
  }

  settings <- list(
    resultsFolder = resultsFolder,
    cohortIds     = if (is.null(cohortIds)) NULL else as.integer(cohortIds),
    minAge        = as.integer(minAge),
    maxAge        = as.integer(maxAge)
  )

  class(settings) <- "PhenotypeReportSettings"
  settings
}

#' @export
print.PhenotypeReportSettings <- function(x, ...) {
  cat("PhenotypeReport Settings\n")
  cat("  Results folder:", x$resultsFolder, "\n")
  if (is.null(x$cohortIds)) {
    cat("  Cohorts:        all\n")
  } else {
    cat("  Cohorts:       ", paste(x$cohortIds, collapse = ", "), "\n")
  }
  cat("  Age range:     ", x$minAge, "-", x$maxAge, "\n")
  invisible(x)
}
