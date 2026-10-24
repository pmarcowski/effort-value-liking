# Build the shared Qualtrics post-survey union consumed by R/prepare_data.R.
#
# Run from the repository root: Rscript R/build_qualtrics.R
# Reads every export in experiments/qualtrics/source/ and writes their
# privacy-minimized union to experiments/qualtrics/post_survey_combined.csv.
# Re-run whenever an export is added; prepare_data.R refuses to run against a
# combined file older than its sources.
#
# The union keeps every response, unfinished ones included, so the file stays a
# complete record of what the surveys collected. Which responses are analyzable
# is decided by the readers in prepare_data.R, not here.
#
# Exports may carry different schemas — the studies span three revisions of the
# survey — so columns are matched by name and absent ones filled with NA. Adding
# an export that overlaps an existing one is safe: identical rows collapse, and
# a ResponseId whose values disagree between exports stops the run.

library(tidyverse)

stopifnot(file.exists('effort-value-liking.Rproj'))

source_dir <- 'experiments/qualtrics/source'
output_path <- 'experiments/qualtrics/post_survey_combined.csv'

QUALTRICS_HEADER_ROWS <- 2     # metadata rows below the header in raw exports

# The column contract. prepare_data.R states the same list independently, so
# a drift between writer and reader halts a run instead of passing silently.
# nasa_tlx_8 is the older survey's pleasantness item; its item 7, a second
# frustration question, is not used.
combined_cols <- c(
  'study', 'id', 'ResponseId', 'RecordedDate', 'Finished', 'Progress',
  'experiment_name', 'experiment_id',
  'gender', 'age', 'belief_liking', 'belief_value',
  paste0('tlx_', c('mental', 'physical', 'temporal', 'performance', 'effort',
                   'frustration', 'pleasure'), '_1'),
  paste0('nasa_tlx_', c(1:6, 8)),
  'like_cognitive', 'like_physical', 'like_which_effort',
  paste0('ncs_', 1:6),
  'instrumental_task', 'necessary_effort', 'worthwhile_effort',
  'meaning_task', 'meaning_effort', 'actions_purpose'
)

# Read one raw export into the contract: drop the metadata rows and the junk id
# rows (empty, literal header repeats, expInfo templates), then label each
# response with its experiment_name. Every study in this project passes its name
# to the survey, so a response without one belongs to none of them and stops the
# build until it is removed from the export.
read_export <- function(path) {
  data <- data.table::fread(path, colClasses = 'character',
                            na.strings = c('', 'NA')) %>%
    as_tibble() %>%
    slice(-seq_len(QUALTRICS_HEADER_ROWS)) %>%
    filter(!is.na(id), id != 'id', id != 'undefined',
           !str_detect(id, fixed('expInfo')))

  data[setdiff(combined_cols, names(data))] <- NA_character_

  unnamed <- data$ResponseId[is.na(data$experiment_name)]
  if (length(unnamed) > 0) {
    stop(basename(path), ': ', length(unnamed), ' response(s) without an ',
         'experiment_name belong to no study of this project; remove them from ',
         'the export: ', paste(unnamed, collapse = ', '))
  }

  data %>%
    mutate(study = experiment_name) %>%
    select(all_of(combined_cols))
}

exports <- fs::dir_ls(source_dir, type = 'file', regexp = '[.]csv$')
if (length(exports) == 0) {
  stop('No Qualtrics exports found in ', source_dir)
}

message('Reading exports:')
responses <- map(exports, read_export)
walk2(exports, responses, function(path, data) {
  message(sprintf('  %-46s %5d responses', basename(path), nrow(data)))
})

combined <- bind_rows(responses) %>% distinct()

conflicting <- combined$ResponseId[duplicated(combined$ResponseId)]
if (length(conflicting) > 0) {
  stop('Exports disagree on the contents of response(s): ',
       paste(unique(conflicting), collapse = ', '))
}
if (anyNA(combined$id) || anyNA(combined$ResponseId)) {
  stop('A response is missing its participant or response key')
}

write_csv(combined, output_path, na = '')

message(sprintf('\nWrote %s (%d responses)', output_path, nrow(combined)))
combined %>%
  count(study, sort = TRUE) %>%
  pwalk(function(study, n) message(sprintf('  %-42s %5d', study, n)))
