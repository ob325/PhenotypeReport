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

# Sort age-group strings by their leading number so any label format works.
.sortAgeGroups <- function(ageVec) {
  obs    <- sort(unique(ageVec[!is.na(ageVec)]))
  starts <- suppressWarnings(as.integer(sub("^([0-9]+).*", "\\1", obs)))
  obs[order(starts, na.last = TRUE)]
}

.genderScale <- ggplot2::scale_color_manual(
  values = c(Overall = "black", Male = "#2166ac", Female = "#d6604d")
)
.bwTheme <- list(
  ggplot2::theme_bw(),
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))
)

# Filter incidence data to rows matching a stratifyBy vector.
# Returns a list: data + metadata used by tableCohortIncidence.
.filterIncidence <- function(cohortIncidence, strata) {
  strata <- sort(strata)

  if (identical(strata, character(0)) || identical(strata, "overall")) {
    list(
      data      = cohortIncidence[is.na(cohortIncidence$year) &
                                    is.na(cohortIncidence$age_group) &
                                    is.na(cohortIncidence$gender), ],
      colorVar  = NULL,
      xVar      = NULL,
      facetType = "overall"
    )
  } else if (identical(strata, "age")) {
    d <- cohortIncidence[is.na(cohortIncidence$year) &
                           !is.na(cohortIncidence$age_group) &
                           is.na(cohortIncidence$gender), ]
    d$age_group <- factor(d$age_group, levels = .sortAgeGroups(d$age_group))
    list(data = d, colorVar = "database_id", xVar = "age_group", facetType = "cohort")

  } else if (identical(strata, "gender")) {
    list(
      data      = cohortIncidence[is.na(cohortIncidence$year) &
                                    is.na(cohortIncidence$age_group) &
                                    !is.na(cohortIncidence$gender), ],
      colorVar  = "database_id",
      xVar      = "gender",
      facetType = "cohort"
    )
  } else if (identical(strata, "year")) {
    list(
      data      = cohortIncidence[!is.na(cohortIncidence$year) &
                                    is.na(cohortIncidence$age_group) &
                                    is.na(cohortIncidence$gender), ],
      colorVar  = "database_id",
      xVar      = "year",
      facetType = "cohort"
    )
  } else if (identical(strata, c("gender", "year"))) {
    d <- cohortIncidence[!is.na(cohortIncidence$year) &
                           is.na(cohortIncidence$age_group), ]
    d$gender <- ifelse(is.na(d$gender), "Overall", d$gender)
    d$gender <- factor(d$gender, levels = c("Overall", "Male", "Female"))
    list(data = d, colorVar = "gender", xVar = "year", facetType = "grid")

  } else if (identical(strata, c("age", "year"))) {
    d <- cohortIncidence[!is.na(cohortIncidence$year) &
                           is.na(cohortIncidence$gender) &
                           !is.na(cohortIncidence$age_group), ]
    d$age_group <- factor(d$age_group, levels = .sortAgeGroups(d$age_group))
    list(data = d, colorVar = "age_group", xVar = "year", facetType = "grid")

  } else if (identical(strata, c("age", "gender"))) {
    d <- cohortIncidence[is.na(cohortIncidence$year) &
                           !is.na(cohortIncidence$age_group) &
                           !is.na(cohortIncidence$gender), ]
    d$age_group <- factor(d$age_group, levels = .sortAgeGroups(d$age_group))
    d$gender    <- factor(d$gender, levels = c("Male", "Female"))
    list(data = d, colorVar = "gender", xVar = "age_group", facetType = "grid")

  } else if (identical(strata, c("age", "gender", "year"))) {
    d <- cohortIncidence[!is.na(cohortIncidence$year) &
                           !is.na(cohortIncidence$age_group) &
                           !is.na(cohortIncidence$gender), ]
    d$age_group <- factor(d$age_group, levels = .sortAgeGroups(d$age_group))
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
                  x = NULL, y = "IR per 1,000 person-years", fill = "Database") +
    .bwTheme
}

.plotIncidenceAge <- function(data) {
  d <- data
  ggplot2::ggplot(d, ggplot2::aes(
    x     = .data$age_group,
    y     = .data$incidence_rate_p100py,
    color = .data$database_id,
    group = .data$database_id
  )) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~cohort_name) +
    ggplot2::labs(title = "Incidence Rate by Age Group",
                  x = "Age Group", y = "IR per 1,000 person-years", color = "Database") +
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
                  x = NULL, y = "IR per 1,000 person-years", fill = "Database") +
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
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    ggplot2::labs(title = "Incidence Rate Over Time",
                  x = "Year", y = "IR per 1,000 person-years", color = "Database") +
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
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    .genderScale +
    ggplot2::labs(title = "Incidence Rate Over Time by Gender",
                  x = "Year", y = "IR per 1,000 person-years", color = "Gender") +
    .bwTheme
}

.plotIncidenceYearAge <- function(data) {
  d <- data
  ggplot2::ggplot(d, ggplot2::aes(
    x     = .data$year,
    y     = .data$incidence_rate_p100py,
    color = .data$age_group,
    group = .data$age_group
  )) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$cohort_name),
                        cols = ggplot2::vars(.data$database_id)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    ggplot2::labs(title = "Incidence Rate Over Time by Age Group",
                  x = "Year", y = "IR per 1,000 person-years", color = "Age Group") +
    .bwTheme
}

.plotIncidenceAgeGender <- function(data) {
  d <- data
  d$gender <- factor(d$gender, levels = c("Male", "Female"))
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
                  x = "Age Group", y = "IR per 1,000 person-years", color = "Gender") +
    .bwTheme
}

.plotIncidenceAgeGenderYear <- function(data) {
  d <- data
  d$gender <- factor(d$gender, levels = c("Male", "Female"))
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
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
      .genderScale +
      ggplot2::labs(
        title = paste("Incidence Rate by Age Group and Sex Over Time —", db),
        x     = "Year",
        y     = "IR per 1,000 person-years",
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
  d <- data[is.na(data$year) & is.na(data$age_group) & is.na(data$gender),
            c("cohort_name", "database_id", "cohort_count", "person_years",
              "incidence_rate_p100py")]

  gt::gt(d) |>
    gt::cols_label(cohort_name = "Cohort", database_id = "Database",
                   cohort_count = "Cohort Count", person_years = "Person-Years",
                   incidence_rate_p100py = "IR per 1,000py") |>
    gt::tab_header(title = "Cohort Incidence") |>
    gt::fmt_integer(columns = c("cohort_count", "person_years")) |>
    .fmtIncidenceTable(
      irCols   = "incidence_rate_p100py",
      leftCols = c("cohort_name", "database_id")
    )
}

.tableCiAge <- function(data) {
  d <- data[is.na(data$year) & is.na(data$gender),
            c("cohort_name", "database_id", "age_group", "incidence_rate_p100py")]
  d$age_group <- ifelse(is.na(d$age_group), "Overall", d$age_group)
  ageLevels   <- c("Overall", .sortAgeGroups(d$age_group[d$age_group != "Overall"]))

  wide        <- tidyr::pivot_wider(d, names_from = "age_group", values_from = "incidence_rate_p100py")
  presentCols <- intersect(ageLevels, names(wide))
  wide        <- wide[order(wide$cohort_name, wide$database_id), c("cohort_name", "database_id", presentCols)]

  gt::gt(wide) |>
    gt::tab_spanner("Age Group", columns = presentCols) |>
    gt::cols_label(cohort_name = "Cohort", database_id = "Database") |>
    gt::tab_header(title = "IR per 1,000 Person-Years by Age Group") |>
    .fmtIncidenceTable(irCols = presentCols, leftCols = c("cohort_name", "database_id"))
}

.tableCiGender <- function(data) {
  genderLevels <- c("Overall", "Male", "Female")
  d <- data[is.na(data$year) & is.na(data$age_group),
            c("cohort_name", "database_id", "gender", "incidence_rate_p100py")]
  d$gender <- ifelse(is.na(d$gender), "Overall", d$gender)

  wide        <- tidyr::pivot_wider(d, names_from = "gender", values_from = "incidence_rate_p100py")
  presentCols <- intersect(genderLevels, names(wide))
  wide        <- wide[order(wide$cohort_name, wide$database_id), c("cohort_name", "database_id", presentCols)]

  gt::gt(wide) |>
    gt::tab_spanner("Gender", columns = presentCols) |>
    gt::cols_label(cohort_name = "Cohort", database_id = "Database") |>
    gt::tab_header(title = "IR per 1,000 Person-Years by Gender") |>
    .fmtIncidenceTable(irCols = presentCols, leftCols = c("cohort_name", "database_id"))
}

.tableCiYear <- function(data) {
  d <- data[is.na(data$age_group) & is.na(data$gender),
            c("cohort_name", "database_id", "year", "incidence_rate_p100py")]
  d$year_label <- ifelse(is.na(d$year), "Overall", as.character(d$year))
  d <- d[order(d$cohort_name, d$database_id, d$year, na.last = FALSE),
         c("cohort_name", "database_id", "year_label", "incidence_rate_p100py")]

  gt::gt(d) |>
    gt::cols_label(cohort_name = "Cohort", database_id = "Database", year_label = "Year",
                   incidence_rate_p100py = "IR per 1,000py") |>
    gt::tab_header(title = "Cohort Incidence Over Time") |>
    .fmtIncidenceTable(
      irCols   = "incidence_rate_p100py",
      leftCols = c("cohort_name", "database_id", "year_label")
    )
}

.tableCiAgeGender <- function(data) {
  genderLevels <- c("Overall", "Male", "Female")

  d <- data[is.na(data$year),
            c("cohort_name", "database_id", "age_group", "gender", "incidence_rate_p100py")]
  d$age_group <- ifelse(is.na(d$age_group), "Overall", d$age_group)
  d$gender    <- ifelse(is.na(d$gender),    "Overall", d$gender)
  ageLevels   <- c("Overall", .sortAgeGroups(d$age_group[d$age_group != "Overall"]))
  d <- d[d$age_group %in% ageLevels & d$gender %in% genderLevels, ]
  d$col_key <- paste0(d$age_group, "__", d$gender)

  wide        <- tidyr::pivot_wider(d[, c("cohort_name", "database_id", "col_key", "incidence_rate_p100py")],
                                    names_from = "col_key", values_from = "incidence_rate_p100py")
  allKeys     <- as.vector(outer(ageLevels, genderLevels, paste, sep = "__"))
  presentKeys <- intersect(allKeys, names(wide))
  wide        <- wide[order(wide$cohort_name, wide$database_id), c("cohort_name", "database_id", presentKeys)]

  gt::gt(wide) |>
    gt::tab_spanner_delim(delim = "__", columns = dplyr::all_of(presentKeys)) |>
    gt::cols_label(cohort_name = "Cohort", database_id = "Database") |>
    gt::tab_header(title = "IR per 1,000 Person-Years by Age Group and Gender") |>
    .fmtIncidenceTable(irCols = presentKeys, leftCols = c("cohort_name", "database_id"))
}

.tableCiYearGender <- function(data) {
  genderLevels <- c("Overall", "Male", "Female")

  d <- data[is.na(data$age_group),
            c("cohort_name", "database_id", "year", "gender", "incidence_rate_p100py")]
  d$year_label <- ifelse(is.na(d$year), "Overall", as.character(d$year))
  d$gender     <- ifelse(is.na(d$gender), "Overall", d$gender)
  d <- d[d$gender %in% genderLevels, ]
  d <- d[order(d$cohort_name, d$database_id, d$year, na.last = FALSE),
         c("cohort_name", "database_id", "year_label", "gender", "incidence_rate_p100py")]

  wide        <- tidyr::pivot_wider(d, names_from = "gender", values_from = "incidence_rate_p100py")
  presentCols <- intersect(genderLevels, names(wide))
  wide        <- wide[, c("cohort_name", "database_id", "year_label", presentCols)]

  gt::gt(wide) |>
    gt::tab_spanner("Gender", columns = presentCols) |>
    gt::cols_label(cohort_name = "Cohort", database_id = "Database", year_label = "Year") |>
    gt::tab_header(title = "IR per 1,000 Person-Years by Year and Gender") |>
    .fmtIncidenceTable(irCols = presentCols, leftCols = c("cohort_name", "database_id", "year_label"))
}

.tableCiYearAge <- function(data) {
  d <- data[is.na(data$gender),
            c("cohort_name", "database_id", "year", "age_group", "incidence_rate_p100py")]
  d$year_label <- ifelse(is.na(d$year), "Overall", as.character(d$year))
  d$age_group  <- ifelse(is.na(d$age_group), "Overall", d$age_group)
  ageLevels    <- c("Overall", .sortAgeGroups(d$age_group[d$age_group != "Overall"]))
  d <- d[d$age_group %in% ageLevels, ]
  d <- d[order(d$cohort_name, d$database_id, d$year, na.last = FALSE),
         c("cohort_name", "database_id", "year_label", "age_group", "incidence_rate_p100py")]

  wide        <- tidyr::pivot_wider(d, names_from = "age_group", values_from = "incidence_rate_p100py")
  presentCols <- intersect(ageLevels, names(wide))
  wide        <- wide[, c("cohort_name", "database_id", "year_label", presentCols)]

  gt::gt(wide) |>
    gt::tab_spanner("Age Group", columns = presentCols) |>
    gt::cols_label(cohort_name = "Cohort", database_id = "Database", year_label = "Year") |>
    gt::tab_header(title = "IR per 1,000 Person-Years by Year and Age Group") |>
    .fmtIncidenceTable(irCols = presentCols, leftCols = c("cohort_name", "database_id", "year_label"))
}

.tableCiAgeGenderYear <- function(data) {
  genderLevels <- c("Overall", "Male", "Female")

  d <- data[, c("cohort_name", "database_id", "year", "age_group", "gender", "incidence_rate_p100py")]
  d$year_label <- ifelse(is.na(d$year), "Overall", as.character(d$year))
  d$age_group  <- ifelse(is.na(d$age_group), "Overall", d$age_group)
  d$gender     <- ifelse(is.na(d$gender),    "Overall", d$gender)
  ageLevels    <- c("Overall", .sortAgeGroups(d$age_group[d$age_group != "Overall"]))
  d <- d[d$age_group %in% ageLevels & d$gender %in% genderLevels, ]
  d$col_key    <- paste0(d$age_group, "__", d$gender)
  d <- d[order(d$cohort_name, d$database_id, d$year, na.last = FALSE),
         c("cohort_name", "database_id", "year_label", "col_key", "incidence_rate_p100py")]

  wide        <- tidyr::pivot_wider(d, names_from = "col_key", values_from = "incidence_rate_p100py")
  allKeys     <- as.vector(outer(ageLevels, genderLevels, paste, sep = "__"))
  presentKeys <- intersect(allKeys, names(wide))
  wide        <- wide[, c("cohort_name", "database_id", "year_label", presentKeys)]

  gt::gt(wide) |>
    gt::tab_spanner_delim(delim = "__", columns = dplyr::all_of(presentKeys)) |>
    gt::cols_label(cohort_name = "Cohort", database_id = "Database", year_label = "Year") |>
    gt::tab_header(title = "IR per 1,000 Person-Years by Year, Age Group, and Gender") |>
    .fmtIncidenceTable(irCols = presentKeys, leftCols = c("cohort_name", "database_id", "year_label"))
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
#' Reads incidence results from the CohortDiagnosticsModule
#' \code{cd_incidence_rate.csv} file in each database sub-folder and returns a
#' combined data frame with cohort names attached.
#'
#' @param settings A \code{PhenotypeReportSettings} object created by
#'   \code{\link{createSettings}}.
#'
#' @return A data frame with columns \code{database_id}, \code{cohort_id},
#'   \code{cohort_name}, \code{age_group}, \code{gender}, \code{year},
#'   \code{cohort_count}, \code{person_years}, and
#'   \code{incidence_rate_p100py}.
#'
#' @export
getCohortIncidence <- function(settings) {
  checkmate::assertClass(settings, "PhenotypeReportSettings")

  dbFolders <- list.dirs(settings$resultsFolder, recursive = FALSE, full.names = TRUE)
  message("Found ", length(dbFolders), " database folder(s): ",
          paste(basename(dbFolders), collapse = ", "))

  data <- do.call(rbind, lapply(dbFolders, function(dbFolder) {
    csvPath <- file.path(dbFolder, "CohortDiagnosticsModule", "cd_incidence_rate.csv")
    if (!file.exists(csvPath)) {
      message("  [SKIP] No cd_incidence_rate.csv in: ", basename(dbFolder))
      return(NULL)
    }
    d <- utils::read.csv(csvPath, stringsAsFactors = FALSE)
    message("  [READ] ", basename(dbFolder), ": ", nrow(d), " rows")
    d
  }))
  message("Combined incidence data: ", nrow(data), " rows")

  # Rename calendar_year -> year; NA means unstratified (overall).
  # gender and age_group are also NA for unstratified rows.
  data$year <- suppressWarnings(as.integer(data$calendar_year))
  data$calendar_year <- NULL
  message("Unique years (NA = overall): ",
          paste(c("NA", sort(na.omit(unique(data$year)))), collapse = ", "))
  message("Gender values (NA = overall): ",
          paste(sort(na.omit(unique(data$gender))), collapse = ", "))
  message("Age group values (NA = overall): ",
          paste(sort(na.omit(unique(data$age_group))), collapse = ", "))

  names(data)[names(data) == "incidence_rate"] <- "incidence_rate_p100py"

  defPath <- Filter(file.exists,
                    file.path(dbFolders, "CohortGeneratorModule", "cg_cohort_definition.csv"))[[1]]
  message("Reading cohort definitions from: ", defPath)
  definitions <- utils::read.csv(defPath, stringsAsFactors = FALSE)
  message("Loaded ", nrow(definitions), " cohort definition(s)")

  data <- merge(
    data,
    definitions[, c("cohort_definition_id", "cohort_name")],
    by.x  = "cohort_id",
    by.y  = "cohort_definition_id",
    all.x = TRUE
  )
  message("After cohort name merge: ", nrow(data), " rows")

  abbrevs <- .readDatabaseAbbreviations(dbFolders)
  if (!is.null(abbrevs)) {
    message("Merging database abbreviations: ",
            paste(abbrevs$cdm_source_abbreviation, collapse = ", "))
    data <- merge(data, abbrevs, by = "database_id", all.x = TRUE)
    data$database_id <- ifelse(
      !is.na(data$cdm_source_abbreviation),
      data$cdm_source_abbreviation,
      data$database_id
    )
    data$cdm_source_abbreviation <- NULL
  } else {
    message("No database abbreviations found; using folder names as database_id")
  }

  if (!is.null(settings$cohortIds)) {
    nBefore <- nrow(data)
    data <- data[data$cohort_id %in% settings$cohortIds, ]
    message("Filtered to cohortIds ", paste(settings$cohortIds, collapse = ", "),
            ": ", nBefore, " -> ", nrow(data), " rows")
  }

  data$cohort_count          <- pmax(0, data$cohort_count)
  data$incidence_rate_p100py <- pmax(0, data$incidence_rate_p100py)

  ageLower <- suppressWarnings(
    as.integer(sub("^([0-9]+).*", "\\1", data$age_group))
  )
  nBefore <- nrow(data)
  data <- data[is.na(data$age_group) |
                 (ageLower >= settings$minAge & ageLower <= settings$maxAge), ]
  message("Age range filter [", settings$minAge, ", ", settings$maxAge, "]: ",
          nBefore, " -> ", nrow(data), " rows")

  nBefore <- nrow(data)
  data <- data[is.na(data$year) |
                 (data$year >= settings$minYear & data$year <= settings$maxYear), ]
  message("Year range filter [", settings$minYear, ", ", settings$maxYear, "]: ",
          nBefore, " -> ", nrow(data), " rows")

  message("Returning ", nrow(data), " rows for ",
          length(unique(data$database_id)), " database(s) and ",
          length(unique(data$cohort_id)), " cohort(s)")

  data[, c(
    "database_id", "cohort_id", "cohort_name",
    "age_group", "gender", "year",
    "cohort_count", "person_years", "incidence_rate_p100py"
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
