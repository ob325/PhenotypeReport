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

.ageOrder    <- c("18-34", "35-44", "45-54", "55-64", "65-74", "75+")
.genderScale <- ggplot2::scale_color_manual(
  values = c(Overall = "black", Male = "#2166ac", Female = "#d6604d")
)
.bwTheme <- list(
  ggplot2::theme_bw(),
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))
)
.yearAxis <- ggplot2::scale_x_continuous(breaks = seq(2010, 2026, by = 4))

# Filter incidence data to rows matching a stratifyBy vector.
# Returns a list: data + metadata used by tableCohortIncidence.
.filterIncidence <- function(cohortIncidence, strata) {
  strata <- sort(strata)

  if (identical(strata, character(0)) || identical(strata, "overall")) {
    list(
      data      = cohortIncidence[is.na(cohortIncidence$year) &
                                    cohortIncidence$age_group == "Overall" &
                                    cohortIncidence$gender    == "Overall", ],
      colorVar  = NULL,
      xVar      = NULL,
      facetType = "overall"
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
                           cohortIncidence$gender    == "Overall" &
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
         ". Supported values: \"age\", \"gender\", \"year\" (one, two, or all three).")
  }
}

# ── Private plot functions ────────────────────────────────────────────────────

.plotIncidenceOverall <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(
    x    = .data$cohort_name,
    y    = .data$incidence_rate_p100py,
    fill = .data$database_id
  )) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(title = "Incidence Rate by Cohort and Database",
                  x = NULL, y = "IR per 100 person-years", fill = "Database") +
    .bwTheme
}

.plotIncidenceAge <- function(data) {
  d <- data
  d$age_group <- factor(d$age_group, levels = .ageOrder)
  ggplot2::ggplot(d, ggplot2::aes(
    x     = .data$age_group,
    y     = .data$incidence_rate_p100py,
    color = .data$database_id,
    group = .data$database_id
  )) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~cohort_name) +
    ggplot2::labs(title = "Incidence Rate by Age Group",
                  x = "Age Group", y = "IR per 100 person-years", color = "Database") +
    .bwTheme
}

.plotIncidenceGender <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(
    x    = .data$gender,
    y    = .data$incidence_rate_p100py,
    fill = .data$database_id
  )) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::facet_wrap(~cohort_name) +
    ggplot2::labs(title = "Incidence Rate by Gender",
                  x = NULL, y = "IR per 100 person-years", fill = "Database") +
    .bwTheme
}

.plotIncidenceYear <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(
    x     = .data$year,
    y     = .data$incidence_rate_p100py,
    color = .data$database_id,
    group = .data$database_id
  )) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~cohort_name) +
    .yearAxis +
    ggplot2::labs(title = "Incidence Rate Over Time",
                  x = "Year", y = "IR per 100 person-years", color = "Database") +
    .bwTheme
}

.plotIncidenceYearGender <- function(data) {
  d <- data
  d$gender <- factor(d$gender, levels = c("Overall", "Male", "Female"))
  ggplot2::ggplot(d, ggplot2::aes(
    x     = .data$year,
    y     = .data$incidence_rate_p100py,
    color = .data$gender,
    group = .data$gender
  )) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$cohort_name),
                        cols = ggplot2::vars(.data$database_id)) +
    .yearAxis +
    .genderScale +
    ggplot2::labs(title = "Incidence Rate Over Time by Gender",
                  x = "Year", y = "IR per 100 person-years", color = "Gender") +
    .bwTheme
}

.plotIncidenceYearAge <- function(data) {
  d <- data
  d$age_group <- factor(d$age_group, levels = .ageOrder)
  ggplot2::ggplot(d, ggplot2::aes(
    x     = .data$year,
    y     = .data$incidence_rate_p100py,
    color = .data$age_group,
    group = .data$age_group
  )) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$cohort_name),
                        cols = ggplot2::vars(.data$database_id)) +
    .yearAxis +
    ggplot2::labs(title = "Incidence Rate Over Time by Age Group",
                  x = "Year", y = "IR per 100 person-years", color = "Age Group") +
    .bwTheme
}

.plotIncidenceAgeGender <- function(data) {
  d <- data
  d$age_group <- factor(d$age_group, levels = .ageOrder)
  d$gender    <- factor(d$gender, levels = c("Male", "Female"))
  ggplot2::ggplot(d, ggplot2::aes(
    x     = .data$age_group,
    y     = .data$incidence_rate_p100py,
    color = .data$gender,
    group = .data$gender
  )) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$cohort_name),
                        cols = ggplot2::vars(.data$database_id)) +
    .genderScale +
    ggplot2::labs(title = "Incidence Rate by Age Group and Gender",
                  x = "Age Group", y = "IR per 100 person-years", color = "Gender") +
    .bwTheme
}

.plotIncidenceAgeGenderYear <- function(data) {
  d <- data
  d$age_group <- factor(d$age_group, levels = .ageOrder)
  d$gender    <- factor(d$gender, levels = c("Male", "Female"))
  databases   <- unique(d$database_id)

  lapply(stats::setNames(databases, databases), function(db) {
    ggplot2::ggplot(d[d$database_id == db, ], ggplot2::aes(
      x     = .data$year,
      y     = .data$incidence_rate_p100py,
      color = .data$gender,
      group = .data$gender
    )) +
      ggplot2::geom_line() +
      ggplot2::facet_grid(rows = ggplot2::vars(.data$cohort_name),
                          cols = ggplot2::vars(.data$age_group)) +
      ggplot2::scale_x_continuous(breaks = seq(2010, 2026, by = 8)) +
      .genderScale +
      ggplot2::labs(
        title = paste("Incidence Rate by Age Group and Sex Over Time —", db),
        x     = "Year",
        y     = "IR per 100 person-years",
        color = "Sex"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1),
                     strip.text.y = ggplot2::element_text(size = 7))
  })
}

# ── Dispatch table ────────────────────────────────────────────────────────────

.incidencePlotDispatch <- list(
  overall         = .plotIncidenceOverall,
  age             = .plotIncidenceAge,
  gender          = .plotIncidenceGender,
  year            = .plotIncidenceYear,
  gender_year     = .plotIncidenceYearGender,
  age_year        = .plotIncidenceYearAge,
  age_gender      = .plotIncidenceAgeGender,
  age_gender_year = .plotIncidenceAgeGenderYear
)

# ── Private table functions ───────────────────────────────────────────────────

# Shared gt formatting applied to all incidence tables.
.fmtIncidenceTable <- function(tbl, irCols, leftCols) {
  tbl |>
    gt::fmt_number(columns = dplyr::any_of(irCols), decimals = 2) |>
    gt::opt_stylize(style = 1) |>
    gt::cols_align("left",  columns = dplyr::any_of(leftCols)) |>
    gt::cols_align("right", columns = -dplyr::any_of(leftCols))
}

.tableCiOverall <- function(data) {
  d <- data[is.na(data$year) & data$age_group == "Overall" & data$gender == "Overall",
            c("cohort_name", "database_id", "persons_at_risk", "outcomes",
              "incidence_proportion_p100p", "incidence_rate_p100py")]

  gt::gt(d) |>
    gt::cols_label(cohort_name = "Cohort", database_id = "Database",
                   persons_at_risk = "Persons at Risk", outcomes = "Outcomes",
                   incidence_proportion_p100p = "IP per 100p",
                   incidence_rate_p100py = "IR per 100py") |>
    gt::tab_header(title = "Cohort Incidence") |>
    gt::fmt_integer(columns = c("persons_at_risk", "outcomes")) |>
    .fmtIncidenceTable(
      irCols   = c("incidence_proportion_p100p", "incidence_rate_p100py"),
      leftCols = c("cohort_name", "database_id")
    )
}

.tableCiAge <- function(data) {
  ageLevels <- c("Overall", .ageOrder)
  d <- data[is.na(data$year) & data$gender == "Overall" & data$age_group %in% ageLevels,
            c("cohort_name", "database_id", "age_group", "incidence_rate_p100py")]

  wide <- tidyr::pivot_wider(d, names_from = "age_group", values_from = "incidence_rate_p100py")
  wide <- wide[order(wide$cohort_name, wide$database_id), c("cohort_name", "database_id", ageLevels)]

  gt::gt(wide) |>
    gt::tab_spanner("Age Group", columns = ageLevels) |>
    gt::cols_label(cohort_name = "Cohort", database_id = "Database") |>
    gt::tab_header(title = "IR per 100 Person-Years by Age Group") |>
    .fmtIncidenceTable(irCols = ageLevels, leftCols = c("cohort_name", "database_id"))
}

.tableCiGender <- function(data) {
  genderLevels <- c("Overall", "Male", "Female")
  d <- data[is.na(data$year) & data$age_group == "Overall" & data$gender %in% genderLevels,
            c("cohort_name", "database_id", "gender", "incidence_rate_p100py")]

  wide <- tidyr::pivot_wider(d, names_from = "gender", values_from = "incidence_rate_p100py")
  wide <- wide[order(wide$cohort_name, wide$database_id), c("cohort_name", "database_id", genderLevels)]

  gt::gt(wide) |>
    gt::tab_spanner("Gender", columns = genderLevels) |>
    gt::cols_label(cohort_name = "Cohort", database_id = "Database") |>
    gt::tab_header(title = "IR per 100 Person-Years by Gender") |>
    .fmtIncidenceTable(irCols = genderLevels, leftCols = c("cohort_name", "database_id"))
}

.tableCiYear <- function(data) {
  d <- data[data$age_group == "Overall" & data$gender == "Overall",
            c("cohort_name", "database_id", "year", "incidence_rate_p100py", "incidence_proportion_p100p")]
  d$year_label <- ifelse(is.na(d$year), "Overall", as.character(d$year))
  d <- d[order(d$cohort_name, d$database_id, d$year, na.last = FALSE),
         c("cohort_name", "database_id", "year_label", "incidence_rate_p100py", "incidence_proportion_p100p")]

  gt::gt(d) |>
    gt::cols_label(cohort_name = "Cohort", database_id = "Database", year_label = "Year",
                   incidence_rate_p100py = "IR per 100py",
                   incidence_proportion_p100p = "IP per 100p") |>
    gt::tab_header(title = "Cohort Incidence Over Time") |>
    .fmtIncidenceTable(
      irCols   = c("incidence_rate_p100py", "incidence_proportion_p100p"),
      leftCols = c("cohort_name", "database_id", "year_label")
    )
}

.tableCiAgeGender <- function(data) {
  ageLevels    <- c("Overall", .ageOrder)
  genderLevels <- c("Overall", "Male", "Female")

  d <- data[is.na(data$year) & data$age_group %in% ageLevels & data$gender %in% genderLevels,
            c("cohort_name", "database_id", "age_group", "gender", "incidence_rate_p100py")]
  d$col_key <- paste0(d$age_group, "__", d$gender)

  wide    <- tidyr::pivot_wider(d[, c("cohort_name", "database_id", "col_key", "incidence_rate_p100py")],
                                names_from = "col_key", values_from = "incidence_rate_p100py")
  allKeys <- as.vector(outer(ageLevels, genderLevels, paste, sep = "__"))
  wide    <- wide[order(wide$cohort_name, wide$database_id), c("cohort_name", "database_id", allKeys)]

  gt::gt(wide) |>
    gt::tab_spanner_delim(delim = "__", columns = dplyr::all_of(allKeys)) |>
    gt::cols_label(cohort_name = "Cohort", database_id = "Database") |>
    gt::tab_header(title = "IR per 100 Person-Years by Age Group and Gender") |>
    .fmtIncidenceTable(irCols = allKeys, leftCols = c("cohort_name", "database_id"))
}

.tableCiYearGender <- function(data) {
  genderLevels <- c("Overall", "Male", "Female")

  d <- data[data$age_group == "Overall" & data$gender %in% genderLevels,
            c("cohort_name", "database_id", "year", "gender", "incidence_rate_p100py")]
  d$year_label <- ifelse(is.na(d$year), "Overall", as.character(d$year))
  d <- d[order(d$cohort_name, d$database_id, d$year, na.last = FALSE),
         c("cohort_name", "database_id", "year_label", "gender", "incidence_rate_p100py")]

  wide <- tidyr::pivot_wider(d, names_from = "gender", values_from = "incidence_rate_p100py")
  wide <- wide[, c("cohort_name", "database_id", "year_label", genderLevels)]

  gt::gt(wide) |>
    gt::tab_spanner("Gender", columns = genderLevels) |>
    gt::cols_label(cohort_name = "Cohort", database_id = "Database", year_label = "Year") |>
    gt::tab_header(title = "IR per 100 Person-Years by Year and Gender") |>
    .fmtIncidenceTable(irCols = genderLevels, leftCols = c("cohort_name", "database_id", "year_label"))
}

.tableCiYearAge <- function(data) {
  ageLevels <- c("Overall", .ageOrder)

  d <- data[data$gender == "Overall" & data$age_group %in% ageLevels,
            c("cohort_name", "database_id", "year", "age_group", "incidence_rate_p100py")]
  d$year_label <- ifelse(is.na(d$year), "Overall", as.character(d$year))
  d <- d[order(d$cohort_name, d$database_id, d$year, na.last = FALSE),
         c("cohort_name", "database_id", "year_label", "age_group", "incidence_rate_p100py")]

  wide <- tidyr::pivot_wider(d, names_from = "age_group", values_from = "incidence_rate_p100py")
  wide <- wide[, c("cohort_name", "database_id", "year_label", ageLevels)]

  gt::gt(wide) |>
    gt::tab_spanner("Age Group", columns = ageLevels) |>
    gt::cols_label(cohort_name = "Cohort", database_id = "Database", year_label = "Year") |>
    gt::tab_header(title = "IR per 100 Person-Years by Year and Age Group") |>
    .fmtIncidenceTable(irCols = ageLevels, leftCols = c("cohort_name", "database_id", "year_label"))
}

.tableCiAgeGenderYear <- function(data) {
  ageLevels    <- c("Overall", .ageOrder)
  genderLevels <- c("Overall", "Male", "Female")

  d <- data[data$age_group %in% ageLevels & data$gender %in% genderLevels,
            c("cohort_name", "database_id", "year", "age_group", "gender", "incidence_rate_p100py")]
  d$year_label <- ifelse(is.na(d$year), "Overall", as.character(d$year))
  d$col_key    <- paste0(d$age_group, "__", d$gender)
  d <- d[order(d$cohort_name, d$database_id, d$year, na.last = FALSE),
         c("cohort_name", "database_id", "year_label", "col_key", "incidence_rate_p100py")]

  wide    <- tidyr::pivot_wider(d, names_from = "col_key", values_from = "incidence_rate_p100py")
  allKeys <- as.vector(outer(ageLevels, genderLevels, paste, sep = "__"))
  wide    <- wide[, c("cohort_name", "database_id", "year_label", allKeys)]

  gt::gt(wide) |>
    gt::tab_spanner_delim(delim = "__", columns = dplyr::all_of(allKeys)) |>
    gt::cols_label(cohort_name = "Cohort", database_id = "Database", year_label = "Year") |>
    gt::tab_header(title = "IR per 100 Person-Years by Year, Age Group, and Gender") |>
    .fmtIncidenceTable(irCols = allKeys, leftCols = c("cohort_name", "database_id", "year_label"))
}

# ── Table dispatch ────────────────────────────────────────────────────────────

.incidenceTableDispatch <- list(
  overall         = .tableCiOverall,
  age             = .tableCiAge,
  gender          = .tableCiGender,
  year            = .tableCiYear,
  age_gender      = .tableCiAgeGender,
  gender_year     = .tableCiYearGender,
  age_year        = .tableCiYearAge,
  age_gender_year = .tableCiAgeGenderYear
)

# ── Exported functions ────────────────────────────────────────────────────────

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
#' Formats incidence results as a wide \code{gt} table with column spanners for
#' each strata dimension. An \code{"Overall"} level is always included for each
#' requested strata. For combinations involving \code{"year"}, year appears as
#' rows with \code{"Overall"} at the top; other strata become column spanners.
#' For \code{c("age", "gender")}, nested spanners are produced via
#' \code{tab_spanner_delim}.
#'
#' @param cohortIncidence A data frame returned by \code{\link{getCohortIncidence}}.
#' @param stratifyBy      \code{"overall"} (default), or a character vector
#'   containing one, two, or three of \code{"age"}, \code{"gender"},
#'   \code{"year"}.
#'
#' @return A \code{gt_tbl} object.
#'
#' @export
tableCohortIncidence <- function(cohortIncidence, stratifyBy = "overall") {
  checkmate::assertDataFrame(cohortIncidence)
  checkmate::assertCharacter(stratifyBy, min.len = 1)
  checkmate::assertSubset(stratifyBy, c("overall", "age", "gender", "year"))

  key    <- paste(sort(stratifyBy), collapse = "_")
  tableFn <- .incidenceTableDispatch[[key]]

  if (is.null(tableFn)) {
    stop("Unsupported stratifyBy combination: ", paste(sort(stratifyBy), collapse = " + "),
         ". Supported values: \"age\", \"gender\", \"year\" (one, two, or all three).")
  }

  tableFn(cohortIncidence)
}

#' Plot Cohort Incidence
#'
#' Creates a chart of incidence rates for a given stratification.
#' \code{stratifyBy} accepts \code{"overall"} or a character vector of one,
#' two, or three variables from \code{"age"}, \code{"gender"}, \code{"year"}.
#'
#' For \code{c("age", "gender", "year")}, returns a named list of
#' \code{ggplot} objects (one per database); all other combinations return a
#' single \code{ggplot} object.
#'
#' @param cohortIncidence A data frame returned by \code{\link{getCohortIncidence}}.
#' @param stratifyBy      \code{"overall"} (default), or a character vector
#'   containing one, two, or three of \code{"age"}, \code{"gender"},
#'   \code{"year"}.
#'
#' @return A \code{ggplot} object, or a named list of \code{ggplot} objects
#'   when \code{stratifyBy = c("age", "gender", "year")}.
#'
#' @export
plotCohortIncidence <- function(cohortIncidence, stratifyBy = "overall") {
  checkmate::assertDataFrame(cohortIncidence)
  checkmate::assertCharacter(stratifyBy, min.len = 1)
  checkmate::assertSubset(stratifyBy, c("overall", "age", "gender", "year"))

  key      <- paste(sort(stratifyBy), collapse = "_")
  plotFn   <- .incidencePlotDispatch[[key]]

  if (is.null(plotFn)) {
    stop("Unsupported stratifyBy combination: ", paste(sort(stratifyBy), collapse = " + "),
         ". Supported values: \"age\", \"gender\", \"year\" (one, two, or all three).")
  }

  filtered <- .filterIncidence(cohortIncidence, sort(stratifyBy))
  plotFn(filtered$data)
}
