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

# Read database_id -> cdm_source_abbreviation mapping from each database folder.
.readDatabaseAbbreviations <- function(dbFolders) {
  rows <- lapply(dbFolders, function(dbFolder) {
    csvPath <- file.path(dbFolder, "DatabaseMetaData", "database_meta_data.csv")
    if (!file.exists(csvPath)) return(NULL)
    d <- utils::read.csv(csvPath, stringsAsFactors = FALSE)
    d[, c("database_id", "cdm_source_abbreviation")]
  })
  do.call(rbind, rows)
}
