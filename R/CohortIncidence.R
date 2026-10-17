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

.ageOrder <- c("18-34", "35-44", "45-54", "55-64", "65-74", "75+")

# Filter incidence data to the rows matching a stratifyBy vector.
# Returns a list: data + metadata used by table/plot helpers.
.filterIncidence <- function(cohortIncidence, strata) {
  strata <- sort(strata)   # canonical order

  if (identical(strata, character(0)) || identical(strata, "overall")) {
    list(
      data       = cohortIncidence[is.na(cohortIncidence$year) &
                                     cohortIncidence$age_group == "Overall" &
                                     cohortIncidence$gender    == "Overall", ],
      colorVar   = NULL,
      xVar       = NULL,
      facetType  = "overall"
    )
  } else if (identical(strata, "age")) {
    d <- cohortIncidence[is.na(cohortIncidence$year) &
                           cohortIncidence$age_group != "Overall" &
                           cohortIncidence$gender    == "Overall", ]
    d$age_group <- factor(d$age_group, levels = .ageOrder)
    list(data = d, colorVar = "database_id", xVar = "age_group", facetType = "cohort")

  } else if (identical(strata, "gender")) {
    list(
      data      = cohortIncidence[is.na(cohortIncidence$year) &
                                    cohortIncidence$age_group == "Overall" &
                                    cohortIncidence$gender    != "Overall", ],
      colorVar  = "database_id",
      xVar      = "gender",
      facetType = "cohort"
    )
  } else if (identical(strata, "year")) {
    list(
      data      = cohortIncidence[!is.na(cohortIncidence$year) &
                                    cohortIncidence$age_group == "Overall" &
                                    cohortIncidence$gender    == "Overall", ],
      colorVar  = "database_id",
      xVar      = "year",
      facetType = "cohort"
    )
  } else if (identical(strata, c("gender", "year"))) {
    d <- cohortIncidence[!is.na(cohortIncidence$year) &
                           cohortIncidence$age_group == "Overall", ]
    d$gender <- factor(d$gender, levels = c("Overall", "Male", "Female"))
    list(data = d, colorVar = "gender", xVar = "year", facetType = "grid")

  } else if (identical(strata, c("age", "year"))) {
    d <- cohortIncidence[!is.na(cohortIncidence$year) &
                           cohortIncidence$gender == "Overall" &
                           cohortIncidence$age_group != "Overall", ]
    d$age_group <- factor(d$age_group, levels = .ageOrder)
    list(data = d, colorVar = "age_group", xVar = "year", facetType = "grid")

  } else if (identical(strata, c("age", "gender"))) {
    d <- cohortIncidence[is.na(cohortIncidence$year) &
                           cohortIncidence$age_group != "Overall" &
                           cohortIncidence$gender    != "Overall", ]
    d$age_group <- factor(d$age_group, levels = .ageOrder)
    d$gender    <- factor(d$gender, levels = c("Male", "Female"))
    list(data = d, colorVar = "gender", xVar = "age_group", facetType = "grid")

  } else if (identical(strata, c("age", "gender", "year"))) {
    d <- cohortIncidence[!is.na(cohortIncidence$year) &
                           cohortIncidence$age_group != "Overall" &
                           cohortIncidence$gender    != "Overall", ]
    d$age_group <- factor(d$age_group, levels = .ageOrder)
    d$gender    <- factor(d$gender, levels = c("Male", "Female"))
    list(data = d, colorVar = "gender", xVar = "year", facetType = "age_year_gender")

  } else {
    stop("Unsupported stratifyBy combination: ", paste(strata, collapse = " + "),
         ". Use one or two of: \"age\", \"gender\", \"year\".")
  }
}

#' Get Cohort Incidence
#'
#' Reads incidence results from a Strategus CohortIncidenceModule results
#' folder and returns a combined data frame with cohort names attached.
#'
#' @param settings A \code{PhenotypeReportSettings} object created by
#'   \code{\link{createSettings}}.
#'
#' @return A data frame with columns \code{database_id}, \code{cohort_id},
#'   \code{cohort_name}, \code{age_group}, \code{gender}, \code{year},
#'   \code{persons_at_risk}, \code{person_days}, \code{outcomes},
#'   \code{incidence_proportion_p100p}, and \code{incidence_rate_p100py}.
#'
#' @export
getCohortIncidence <- function(settings) {
  checkmate::assertClass(settings, "PhenotypeReportSettings")

  summaryPath    <- file.path(settings$resultsFolder, "CohortIncidenceModule", "ci_incidence_summary.csv")
  definitionPath <- file.path(settings$resultsFolder, "CohortGeneratorModule", "cg_cohort_definition.csv")
  checkmate::assertFileExists(summaryPath)
  checkmate::assertFileExists(definitionPath)

  data        <- utils::read.csv(summaryPath,    stringsAsFactors = FALSE, na.strings = "NA")
  definitions <- utils::read.csv(definitionPath, stringsAsFactors = FALSE)

  data$year <- suppressWarnings(as.integer(data$year))

  data <- merge(
    data,
    definitions[, c("cohort_definition_id", "cohort_name")],
    by.x  = "cohort_id",
    by.y  = "cohort_definition_id",
    all.x = TRUE
  )

  if (!is.null(settings$cohortIds)) {
    data <- data[data$cohort_id %in% settings$cohortIds, ]
  }

  data[, c(
    "database_id", "cohort_id", "cohort_name",
    "age_group", "gender", "year",
    "persons_at_risk", "person_days", "outcomes",
    "incidence_proportion_p100p", "incidence_rate_p100py"
  )]
}

#' Create a Cohort Incidence Table
#'
#' Formats incidence results as a \code{gt} table. \code{stratifyBy} accepts a
#' character vector containing one of \code{"age"}, \code{"gender"}, or
#' \code{"year"}, or \code{"overall"} for unstratified results.
#'
#' @param cohortIncidence A data frame returned by \code{\link{getCohortIncidence}}.
#' @param stratifyBy      \code{"overall"} (default), or a character vector
#'   containing one of \code{"age"}, \code{"gender"}, or \code{"year"}.
#'   Multi-strata combinations are not supported for tables.
#'
#' @return A \code{gt_tbl} object.
#'
#' @export
tableCohortIncidence <- function(cohortIncidence, stratifyBy = "overall") {
  checkmate::assertDataFrame(cohortIncidence)
  checkmate::assertCharacter(stratifyBy, min.len = 1)
  if (length(stratifyBy) > 1) {
    stop("tableCohortIncidence() supports a single stratifyBy value. Use plotCohortIncidence() for multi-strata.")
  }
  checkmate::assertChoice(stratifyBy, c("overall", "age", "gender", "year"))

  filtered  <- .filterIncidence(cohortIncidence, stratifyBy)
  data      <- filtered$data
  strataCol <- filtered$xVar

  strataLabels <- c(age_group = "Age Group", gender = "Gender", year = "Year")

  if (is.null(strataCol)) {
    display <- data[, c("cohort_name", "database_id", "persons_at_risk",
                        "outcomes", "incidence_proportion_p100p", "incidence_rate_p100py")]
    title   <- "Cohort Incidence"
  } else {
    display <- data[order(data$cohort_name, data$database_id), ]
    display <- display[, c("cohort_name", "database_id", strataCol, "persons_at_risk",
                           "outcomes", "incidence_proportion_p100p", "incidence_rate_p100py")]
    title   <- paste("Cohort Incidence by", strataLabels[[strataCol]])
  }

  leftCols <- c("cohort_name", "database_id", strataCol)

  labelList <- list(
    cohort_name                = "Cohort",
    database_id                = "Database",
    age_group                  = "Age Group",
    gender                     = "Gender",
    year                       = "Year",
    persons_at_risk            = "Persons at Risk",
    outcomes                   = "Outcomes",
    incidence_proportion_p100p = "IP per 100p",
    incidence_rate_p100py      = "IR per 100py"
  )

  gt::gt(display) |>
    gt::cols_label(.list = labelList[names(labelList) %in% names(display)]) |>
    gt::tab_header(title = title) |>
    gt::fmt_integer(columns = c("persons_at_risk", "outcomes")) |>
    gt::fmt_number(columns = c("incidence_proportion_p100p", "incidence_rate_p100py"),
                   decimals = 2) |>
    gt::opt_stylize(style = 1) |>
    gt::cols_align(align = "left",  columns = dplyr::any_of(leftCols)) |>
    gt::cols_align(align = "right", columns = -dplyr::any_of(leftCols))
}

#' Plot Cohort Incidence
#'
#' Creates a chart of incidence rates for a given stratification.
#' \code{stratifyBy} accepts a character vector of one or two variables from
#' \code{"age"}, \code{"gender"}, and \code{"year"}.
#'
#' For single strata: facets by cohort, color by database.
#' For two strata: \code{facet_grid(cohort ~ database)}, color by the
#' non-year strata (or by database when both strata are non-year).
#'
#' @param cohortIncidence A data frame returned by \code{\link{getCohortIncidence}}.
#' @param stratifyBy      \code{"overall"} (default), or a character vector
#'   containing one or two of \code{"age"}, \code{"gender"}, \code{"year"}.
#'
#' @return A \code{ggplot} object.
#'
#' @export
plotCohortIncidence <- function(cohortIncidence, stratifyBy = "overall") {
  checkmate::assertDataFrame(cohortIncidence)
  checkmate::assertCharacter(stratifyBy, min.len = 1)
  checkmate::assertSubset(stratifyBy, c("overall", "age", "gender", "year"))

  filtered  <- .filterIncidence(cohortIncidence, sort(stratifyBy))
  data      <- filtered$data
  colorVar  <- filtered$colorVar
  xVar      <- filtered$xVar
  facetType <- filtered$facetType

  colorLabels <- c(database_id = "Database", gender = "Gender", age_group = "Age Group")
  xLabels     <- c(age_group = "Age Group", gender = "Gender", year = "Year")

  # Age x gender x year: one ggplot per database, rows = cohort, cols = age group
  if (facetType == "age_year_gender") {
    databases <- unique(data$database_id)
    plots <- lapply(stats::setNames(databases, databases), function(db) {
      d <- data[data$database_id == db, ]
      ggplot2::ggplot(d, ggplot2::aes(
        x     = .data$year,
        y     = .data$incidence_rate_p100py,
        color = .data$gender,
        group = .data$gender
      )) +
        ggplot2::geom_line() +
        ggplot2::facet_grid(
          rows = ggplot2::vars(.data$cohort_name),
          cols = ggplot2::vars(.data$age_group)
        ) +
        ggplot2::scale_x_continuous(breaks = seq(2010, 2026, by = 8)) +
        ggplot2::scale_color_manual(
          values = c(Male = "#2166ac", Female = "#d6604d")
        ) +
        ggplot2::labs(
          title = paste("Incidence Rate by Age Group and Sex Over Time —", db),
          x     = "Year",
          y     = "IR per 100 person-years",
          color = "Sex"
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1),
          strip.text.y = ggplot2::element_text(size = 7)
        )
    })
    return(plots)
  }

  baseTheme <- list(
    ggplot2::theme_bw(),
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))
  )

  # Overall: grouped bar, cohorts on x, databases as fill
  if (facetType == "overall") {
    return(
      ggplot2::ggplot(data, ggplot2::aes(
        x    = .data$cohort_name,
        y    = .data$incidence_rate_p100py,
        fill = .data$database_id
      )) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::labs(title = "Incidence Rate by Cohort and Database",
                      x = NULL, y = "IR per 100 person-years", fill = "Database") +
        baseTheme
    )
  }

  # Gender: bar chart (discrete x, no line needed)
  if (xVar == "gender") {
    p <- ggplot2::ggplot(data, ggplot2::aes(
      x    = .data$gender,
      y    = .data$incidence_rate_p100py,
      fill = .data[[colorVar]]
    )) +
      ggplot2::geom_col(position = "dodge")
  } else {
    # Age or year: line chart
    p <- ggplot2::ggplot(data, ggplot2::aes(
      x     = .data[[xVar]],
      y     = .data$incidence_rate_p100py,
      color = .data[[colorVar]],
      group = .data[[colorVar]]
    )) +
      ggplot2::geom_line()
  }

  # Faceting
  if (facetType == "cohort") {
    p <- p + ggplot2::facet_wrap(~cohort_name)
  } else {
    p <- p + ggplot2::facet_grid(
      rows = ggplot2::vars(.data$cohort_name),
      cols = ggplot2::vars(.data$database_id)
    )
  }

  # Year x-axis formatting
  if (xVar == "year") {
    p <- p + ggplot2::scale_x_continuous(breaks = seq(2010, 2026, by = 4))
  }

  # Gender color scale
  if (colorVar == "gender") {
    p <- p + ggplot2::scale_color_manual(
      values = c(Overall = "black", Male = "#2166ac", Female = "#d6604d")
    )
  }

  aesLabel <- if (xVar == "gender") "fill" else "color"

  p +
    ggplot2::labs(
      title         = paste("Incidence Rate by", paste(sort(stratifyBy), collapse = " and ")),
      x             = xLabels[[xVar]],
      y             = "IR per 100 person-years",
      !!aesLabel   := colorLabels[[colorVar]]
    ) +
    baseTheme
}
