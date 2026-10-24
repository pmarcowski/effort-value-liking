# Prepare analysis-ready datasets from raw experiment data.
#
# Run from the repository root: Rscript R/prepare_data.R
# Each study reads from experiments/<experiment>/data/raw/ and writes prepared
# CSVs to experiments/<experiment>/data/; the shared Qualtrics post-survey is
# the union R/build_qualtrics.R writes to experiments/qualtrics/. A participant
# enters a prepared file only with matched task data and a fully finished
# survey response. Blocks are ordered as in the analysis
# notebooks (main, then supplementary). Session-file dedup
# keys on the timestamp inside each Pavlovia file name, so the prepared CSVs
# reproduce byte for byte from the same raw inputs on any machine.

library(tidyverse)

stopifnot(file.exists('effort-value-liking.Rproj'))

# Constants ----

# Qualtrics post-survey NASA-TLX item columns (0-20 per item); _aff adds the
# custom Pleasure item kept out of the composite score
tlx_cols <- c('tlx_mental_1', 'tlx_physical_1', 'tlx_temporal_1',
              'tlx_performance_1', 'tlx_effort_1', 'tlx_frustration_1')
tlx_cols_aff <- c(tlx_cols, 'tlx_pleasure_1')

# Post-experiment NASA-TLX components on the 0-100 metric of the original
# instrument, in NASA order. Each block converts its own instrument to it, and
# Pleasure exists only where a survey asked it.
tlx_component_cols <- c('tlx_mental', 'tlx_physical', 'tlx_temporal', 'tlx_performance',
                        'tlx_effort', 'tlx_frustration', 'tlx_pleasure')

# The older survey (observed effort and the clicking validation sample) asked the
# six NASA items as nasa_tlx_1..6 and pleasantness as nasa_tlx_8 (its item 7, a
# second frustration question, is not used), all on one slider from 1 to 20 that
# snapped to 11 positions, recorded as these codes. Its performance item asked how
# successful the performance was, the reverse of the NASA-TLX's perfect-to-failure
# direction, onto which it is turned.
older_tlx_items <- c(tlx_mental = 'nasa_tlx_1', tlx_physical = 'nasa_tlx_2',
                     tlx_temporal = 'nasa_tlx_3', tlx_performance = 'nasa_tlx_4',
                     tlx_effort = 'nasa_tlx_5', tlx_frustration = 'nasa_tlx_6',
                     tlx_pleasure = 'nasa_tlx_8')
older_tlx_codes <- c(1, 3, 5, 7, 9, 11, 12, 14, 16, 18, 20)
older_tlx_reversed <- 'tlx_performance'

qualtrics_combined_cols <- c(
  'study', 'id', 'ResponseId', 'RecordedDate', 'Finished', 'Progress',
  'experiment_name', 'experiment_id',
  'gender', 'age', 'belief_liking', 'belief_value',
  tlx_cols_aff, paste0('nasa_tlx_', c(1:6, 8)),
  'like_cognitive', 'like_physical', 'like_which_effort',
  paste0('ncs_', 1:6),
  'instrumental_task', 'necessary_effort', 'worthwhile_effort',
  'meaning_task', 'meaning_effort', 'actions_purpose'
)

# Likert recodes for the cognitive/wait post-survey effort-preference items
cognitive_enjoy_levels <- c(
  'Very unenjoyable' = 1, 'Somewhat unenjoyable' = 2,
  'Neither unenjoyable nor enjoyable' = 3,
  'Somewhat enjoyable' = 4, 'Very enjoyable' = 5
)
cognitive_ncs_levels <- c(
  'Extremely uncharacteristic of me' = 1, 'Somewhat uncharacteristic of me' = 2,
  'Neither uncharacteristic nor characteristic of me' = 3,
  'Somewhat characteristic of me' = 4, 'Extremely characteristic of me' = 5
)
ncs_reverse_cols <- c('ncs_3', 'ncs_4')

# Wait post-survey opportunity-cost item recodes (opportunity_cost is scored on
# extent_levels below)
wait_opportunity_activity_levels <- c(
  'I waited without doing anything else' = 1,
  'I sometimes did other things while waiting' = 2,
  'I usually did other things while waiting' = 3,
  'I did other things during almost all of the waiting periods' = 4
)

# Instrumentality block of the post-surveys: how instrumental, necessary,
# worthwhile, and meaningful the effort felt, each coded so higher means more
# of the label. The wait study asked the same six items about its waiting.
extent_levels <- c(
  'Not at all' = 1, 'Slightly' = 2, 'Moderately' = 3,
  'Very much' = 4, 'Completely' = 5
)
necessity_levels <- c(
  'Very unnecessary' = 1, 'Somewhat unnecessary' = 2,
  'Neither unnecessary nor necessary' = 3,
  'Somewhat necessary' = 4, 'Very necessary' = 5
)
worthwhile_levels <- c(
  'Very worthless' = 1, 'Somewhat worthless' = 2,
  'Neither worthless nor worthwhile' = 3,
  'Somewhat worthwhile' = 4, 'Very worthwhile' = 5
)
meaning_levels <- c(
  'Very meaningless' = 1, 'Somewhat meaningless' = 2,
  'Neither meaningless nor meaningful' = 3,
  'Somewhat meaningful' = 4, 'Very meaningful' = 5
)
purpose_levels <- c(
  'Very purposeless' = 1, 'Somewhat purposeless' = 2,
  'Neither purposeless nor purposeful' = 3,
  'Somewhat purposeful' = 4, 'Very purposeful' = 5
)
instrumentality_scales <- list(
  instrumental_task = extent_levels,
  necessary_effort  = necessity_levels,
  worthwhile_effort = worthwhile_levels,
  meaning_task      = meaning_levels,
  meaning_effort    = meaning_levels,
  actions_purpose   = purpose_levels
)
instrumentality_cols <- names(instrumentality_scales)

# Design lengths and exclusion thresholds
TRIALS_PER_SESSION <- 18       # complete-session threshold and per-id trial cap
TASKLOAD_ROWS_CAP <- 126       # 18 trials x 7 task-load components
TIME_OUTLIER_SD <- 2.5         # one-sided upper cutoff on summed effort time
QUALTRICS_HEADER_ROWS <- 2     # metadata rows below the header in raw exports

# Helpers ----

# Read a set of Pavlovia CSVs (a whole directory, or an explicit file list)
# into one long frame tagged with its source file. all_character avoids type
# coercion on sparse columns (the newer jsPsych exports); na_strings values are
# normalized to NA after reading.
read_pavlovia_dir <- function(dir = NULL, files = NULL, all_character = FALSE, na_strings = '') {
  if (is.null(files)) files <- fs::dir_ls(dir, type = 'file', regexp = '\\.csv$')
  stopifnot(length(files) > 0)
  reader <- if (all_character) {
    function(f) data.table::fread(f, colClasses = 'character')
  } else {
    data.table::fread
  }
  data <- tibble(file_name = files) %>%
    mutate(file_content = map(file_name, reader)) %>%
    unnest(cols = c(file_content))
  for (s in na_strings) {
    data <- mutate(data, across(where(is.character), ~na_if(.x, s)))
  }
  data
}

# Session start time parsed out of a Pavlovia file name, which ends
# _YYYY-MM-DD_HHhMM.SS.mmm; the fractional part runs one to three digits.
# Read as UTC so the answer never depends on the reading machine's zone — in a
# local zone a stamp falling inside a daylight-saving gap names a time that
# does not exist there and silently becomes NA. NA still comes back for a name
# carrying no stamp at all, and for exports written with hour 24.
session_timestamp <- function(files) {
  stamps <- sub('.*_(\\d{4}-\\d{2}-\\d{2}_\\d{2}h\\d{2}\\.\\d{2}\\.\\d{1,3}).*', '\\1', files)
  stamps[stamps == files] <- NA_character_
  as.POSIXct(gsub('[h.]', ':', stamps), format = '%Y-%m-%d_%H:%M:%S', tz = 'UTC')
}

# Resolve repeat runs when the participant id is embedded in the filename:
# keep each id's earliest file. Asserts the id pattern matched every file —
# on PARTICIPANT_SESSION-named dirs (no id in the filename) this pattern
# would silently collapse everything to one file, so failure must be loud.
resolve_sessions_by_filename <- function(files, id_pattern) {
  ids <- sub(id_pattern, '\\1', files)
  stopifnot(!any(ids == files))
  timestamps <- session_timestamp(files)
  # An unreadable stamp only matters where it would decide an ordering. A
  # participant with a single session is never ordered against anything, and the
  # oldest exports carry names no format parses (hour 24, seconds with no
  # fractional part). Demanding a stamp from those would block the resolution
  # the repeat participants actually need.
  ambiguous <- is.na(timestamps) & ids %in% ids[duplicated(ids)]
  if (any(ambiguous)) {
    stop('Unreadable session timestamp for repeat participant(s): ',
         paste(unique(ids[ambiguous]), collapse = ', '))
  }
  ordered_indices <- order(ids, timestamps)
  files[ordered_indices[!duplicated(ids[ordered_indices])]]
}

# Resolve repeat runs when sessions must be told apart from file content (the
# id column inside each file): keep, per id, the earliest complete session
# (>= min_trials trial summaries). Selecting a whole session — rather than a
# row-level distinct(id, trial) — keeps each participant's trials and memory
# check from the same run and is robust to partial restarts. A participant
# with trial summaries but no complete session stops the run, as does an
# unparseable file name: arrange() would otherwise sort NA last and quietly
# hand a tie to a later session.
resolve_sessions_by_content <- function(data, min_trials = TRIALS_PER_SESSION) {
  sessions <- data %>%
    filter(trial_id == 'trial_summary', !is.na(id)) %>%
    count(id, file_name, name = 'n_trials') %>%
    mutate(timestamp = session_timestamp(file_name))
  stopifnot(!anyNA(sessions$timestamp))
  n_without_complete <- sessions %>%
    group_by(id) %>%
    summarise(has_complete = any(n_trials >= min_trials), .groups = 'drop') %>%
    filter(!has_complete) %>%
    nrow()
  if (n_without_complete > 0) {
    stop('Session resolution found ', n_without_complete,
         ' participant(s) without a complete session')
  }
  sessions <- sessions %>%
    arrange(id, desc(n_trials >= min_trials), timestamp, desc(n_trials)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()
  semi_join(data, sessions, by = c('id', 'file_name'))
}

# Guarded numeric id coercion: identical to as.numeric() except that silently
# NA-ing out a non-numeric id (e.g. a hex Prolific PID reaching a SONA-id
# pipeline) stops the run instead of dropping the participant downstream.
as_numeric_id <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  introduced <- is.na(out) & !is.na(x)
  if (any(introduced)) {
    stop('Non-numeric participant id(s): ',
         paste(unique(x[introduced]), collapse = ', '))
  }
  out
}

# Keep only responses the participant carried to the end. A survey abandoned
# part way can still hold every item an analysis reads, so the rule is stated
# here and applied by both readers rather than left to each study block, where
# it would hold by luck. The comparison tolerates the logical and character
# forms the two export schemas produce.
finished_responses <- function(data, source_label) {
  if (!all(c('Finished', 'Progress') %in% names(data))) {
    stop('Qualtrics export carries no completion status: ', source_label)
  }
  finished <- tolower(as.character(data$Finished)) == 'true' &
    suppressWarnings(as.numeric(data$Progress)) == 100
  if (anyNA(finished)) {
    stop('Qualtrics response with unreadable completion status: ', source_label)
  }
  if (any(!finished)) {
    message(sprintf('  %s: %d unfinished response(s) dropped',
                    source_label, sum(!finished)))
  }
  filter(data, finished)
}

# Read a raw Qualtrics export: drop the metadata rows under the header, the
# junk id rows (empty, literal header repeats, expInfo templates), and the
# unfinished responses
read_qualtrics <- function(path) {
  data.table::fread(path) %>%
    slice(-seq_len(QUALTRICS_HEADER_ROWS)) %>%
    filter(id != '' & id != 'id' & id != 'undefined' & !str_detect(id, 'expInfo')) %>%
    finished_responses(basename(path))
}

# Read the privacy-minimized, metadata-free union of the shared Qualtrics
# exports. ResponseId identifies an exported response; study + id identifies
# the participant record used by an experiment block. The union on disk keeps
# every response collected; the unfinished ones are dropped here, where the
# analysis rule belongs.
read_combined_qualtrics <- function(path) {
  data <- data.table::fread(
    path,
    colClasses = 'character',
    na.strings = c('', 'NA')
  ) %>%
    as_tibble()

  if (!identical(names(data), qualtrics_combined_cols)) {
    stop(path, ' does not match the column contract (missing: ',
         toString(setdiff(qualtrics_combined_cols, names(data))), '; unexpected: ',
         toString(setdiff(names(data), qualtrics_combined_cols)),
         ') - rebuild it with Rscript R/build_qualtrics.R, or reconcile the ',
         'contract lists of the two scripts')
  }
  if (anyNA(data$study) || anyNA(data$id) || anyNA(data$ResponseId) ||
      anyDuplicated(data$ResponseId)) {
    stop('The combined Qualtrics file has missing or duplicate response keys')
  }

  finished_responses(data, basename(path))
}

# Post-survey belief items -> More / Less / No Difference
recode_beliefs <- function(data) {
  data %>%
    mutate(
      belief_liking = case_when(
        str_detect(belief_liking, 'like things more') ~ 'More',
        str_detect(belief_liking, 'like things less') ~ 'Less',
        str_detect(belief_liking, 'no difference in how I like') ~ 'No Difference'
      ),
      belief_value = case_when(
        str_detect(belief_value, 'higher financial value') ~ 'More',
        str_detect(belief_value, 'lower financial value') ~ 'Less',
        str_detect(belief_value, 'no difference in how I assign') ~ 'No Difference'
      )
    )
}

# Instrumentality items -> 1-5; a skipped item (NA or blank) stays NA, and a
# response label the scales do not define stops the run rather than becoming NA
score_instrumentality <- function(data) {
  for (item in instrumentality_cols) {
    responses <- na_if(as.character(data[[item]]), '')
    scored <- unname(instrumentality_scales[[item]][responses])
    unscorable <- unique(responses[is.na(scored) & !is.na(responses)])
    if (length(unscorable) > 0) {
      stop(item, ': unscorable response label(s): ', paste(unscorable, collapse = ', '))
    }
    data[[item]] <- scored
  }
  data
}

# Post-survey task load: the seven 0-20 slider items -> the components on the
# 0-100 NASA-TLX metric
score_tlx_components <- function(data) {
  mutate(data, across(all_of(tlx_cols_aff), ~ 5 * as.numeric(.x),
                      .names = "{str_remove(.col, '_1$')}"))
}

# Older-survey task load: each slider code -> its position (0-10) times 10, the
# component on the 0-100 NASA-TLX metric, with the reversed item turned; a code
# off the slider's grid stops the run
score_older_tlx_components <- function(data) {
  for (component in names(older_tlx_items)) {
    codes <- as.numeric(data[[older_tlx_items[[component]]]])
    position <- match(codes, older_tlx_codes) - 1
    off_grid <- unique(codes[is.na(position) & !is.na(codes)])
    if (length(off_grid) > 0) {
      stop(older_tlx_items[[component]], ': code(s) off the slider grid: ', toString(off_grid))
    }
    data[[component]] <- if (component %in% older_tlx_reversed) 100 - 10 * position else 10 * position
  }
  data
}

# Recognition-memory frame from memory_check rows, with log-linear-corrected
# d' (the exclusion criterion is d' <= 0, applied by the calling block).
# keep_cols carries extra between-subjects design columns through, so d' can
# be summarized by a factor other than rating condition.
memory_frame <- function(data, id_numeric = TRUE, keep_cols = character()) {
  out <- data %>%
    filter(trial_id == 'memory_check') %>%
    mutate(
      condition = assigned_condition,
      mem_hits = as.integer(mem_hits),
      mem_false_alarms = as.integer(mem_false_alarms),
      mem_correct_rejections = as.integer(mem_correct_rejections),
      mem_misses = as.integer(mem_misses),
      mem_rt = as.numeric(mem_rt)
    )
  if (id_numeric) out <- mutate(out, id = as_numeric_id(id))
  out %>%
    select(id, condition, all_of(keep_cols), mem_hits, mem_false_alarms,
           mem_correct_rejections, mem_misses, mem_rt) %>%
    mutate(
      hit_rate = mem_hits / (mem_hits + mem_misses),
      fa_rate = mem_false_alarms / (mem_false_alarms + mem_correct_rejections),
      hit_rate_adj = (mem_hits + 0.5) / (mem_hits + mem_misses + 1),
      fa_rate_adj = (mem_false_alarms + 0.5) / (mem_false_alarms + mem_correct_rejections + 1),
      dprime = qnorm(hit_rate_adj) - qnorm(fa_rate_adj)
    ) %>%
    distinct(id, .keep_all = TRUE)
}

# Ids whose summed effort time exceeds mean + TIME_OUTLIER_SD x SD within each
# effort task (one-sided upper: slow outliers only, the total-task-time rule)
time_outlier_ids <- function(data, already_excluded) {
  data %>%
    filter(!id %in% already_excluded) %>%
    group_by(effort_task, id) %>%
    summarise(total_effort_time = sum(effort_trial_duration, na.rm = TRUE), .groups = 'drop') %>%
    group_by(effort_task) %>%
    filter(total_effort_time > mean(total_effort_time) + TIME_OUTLIER_SD * sd(total_effort_time)) %>%
    ungroup() %>%
    pull(id)
}

# Write one prepared CSV and report the block's participant count
write_prepared <- function(data, experiment, file, id_col = 'id') {
  path <- file.path('experiments', experiment, 'data', file)
  stopifnot(nrow(data) > 0)
  write_csv(data, path)
  message(sprintf('  %-58s %4d ids, %6d rows', path, n_distinct(data[[id_col]]), nrow(data)))
  invisible(data)
}

# Shared Qualtrics Post-Surveys ----

# Privacy-minimized union of the shared survey exports, filtered per study by
# each calling block. Study-qualified ids prevent collisions between surveys.
qualtrics_combined_path <- 'experiments/qualtrics/post_survey_combined.csv'
qualtrics_source_dir <- 'experiments/qualtrics/source'

# The union is a build product, and preparing data from one that predates its
# own sources yields stale analyses with nothing to show for it. Refuse instead.
if (!file.exists(qualtrics_combined_path)) {
  stop(qualtrics_combined_path, ' is missing - run Rscript R/build_qualtrics.R')
}
if (dir.exists(qualtrics_source_dir)) {
  qualtrics_sources <- fs::dir_ls(qualtrics_source_dir, type = 'file')
  if (length(qualtrics_sources) > 0 &&
      max(file.mtime(qualtrics_sources)) > file.mtime(qualtrics_combined_path)) {
    stop('A Qualtrics export is newer than ', qualtrics_combined_path,
         ' - run Rscript R/build_qualtrics.R')
  }
}

shared_post_survey <- read_combined_qualtrics(qualtrics_combined_path)

post_survey <-
  shared_post_survey %>%
  filter(study %in% c('ratingclicks_within', 'buysell_within2')) %>%
  mutate(
    id = as_numeric_id(id),
    age = as.numeric(age),
    across(all_of(tlx_cols_aff), as.numeric),
    nasa_tlx = rowSums(across(all_of(tlx_cols)), na.rm = TRUE)
  ) %>%
  recode_beliefs() %>%
  score_instrumentality() %>%
  score_tlx_components() %>%
  group_by(study, id) %>%
  arrange(RecordedDate, ResponseId, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  drop_na(id)

# Older-schema post-survey serving the clicking validation and observed-effort
# studies; its nasa_tlx sums the six NASA components on the 0-100 metric
# (0-600), performance reversed
online_post_survey <-
  shared_post_survey %>%
  filter(study %in% c('effortobserve', 'effortratingclickstaskload')) %>%
  score_older_tlx_components() %>%
  mutate(
    nasa_tlx = rowSums(across(all_of(head(tlx_component_cols, 6)))),
    id = as_numeric_id(id)
  ) %>%
  group_by(study, id) %>%
  arrange(RecordedDate, ResponseId, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(study, experiment_name, id, gender, age, nasa_tlx, all_of(tlx_component_cols))

# Main Experiments ----

## Stepping (Experiment 1) ----
#
# Lab stepping study with between-subjects rating condition (Liking / Wanting /
# Value); demographics, NASA-TLX, performance, and post-experiment items join
# onto the main trial frame. Post-experiment rows are limited to participants
# present in the demographics file.

core1_demographics_data <- read_csv('experiments/stepping/data/raw/exp1-demographics.csv',
                                    show_col_types = FALSE)

core1_nasa_tlx_raw_data <-
  read_csv('experiments/stepping/data/raw/exp1-TLX.csv', show_col_types = FALSE) %>%
  select(subjID, scale_type, raw_rating, tally) %>%
  mutate(adjusted_rating = raw_rating * tally)

core1_nasa_tlx_frustration_scores <-
  core1_nasa_tlx_raw_data %>%
  filter(scale_type %in% c('Frustration')) %>%
  select(-scale_type, -tally) %>%
  rename(
    'frustration_raw' = 'raw_rating',
    'frustration_adjusted' = 'adjusted_rating'
  )

core1_nasa_tlx_total_scores <-
  core1_nasa_tlx_raw_data %>%
  group_by(subjID) %>%
  summarise(
    raw_overall = sum(raw_rating),
    adjusted_overall = sum(adjusted_rating) / 15
  ) %>%
  ungroup()

# The six raw component ratings, already on the 0-100 NASA-TLX metric
core1_nasa_tlx_components <-
  core1_nasa_tlx_raw_data %>%
  mutate(component = recode(
    scale_type, Mental_Demand = 'tlx_mental', Physical_Demand = 'tlx_physical',
    Temporal_Demand = 'tlx_temporal', Performance = 'tlx_performance',
    Effort = 'tlx_effort', Frustration = 'tlx_frustration'
  )) %>%
  select(subjID, component, raw_rating) %>%
  pivot_wider(names_from = component, values_from = raw_rating)
stopifnot(setequal(setdiff(names(core1_nasa_tlx_components), 'subjID'),
                   head(tlx_component_cols, 6)))

core1_performance_data <- read_csv('experiments/stepping/data/raw/exp1-perf.csv',
                                   show_col_types = FALSE)

core1_post_experiment_data <- read_csv('experiments/stepping/data/raw/exp1-post.csv',
                                       show_col_types = FALSE) %>%
  filter(subjID %in% core1_demographics_data$subjID) %>%
  select(-fitness)

core1_main_data <-
  read_csv('experiments/stepping/data/raw/exp1-main.csv', show_col_types = FALSE) %>%
  left_join(core1_demographics_data, by = 'subjID') %>%
  left_join(core1_nasa_tlx_frustration_scores, by = 'subjID') %>%
  left_join(core1_nasa_tlx_total_scores, by = 'subjID') %>%
  left_join(core1_performance_data, by = c('subjID', 'trial')) %>%
  left_join(core1_post_experiment_data, by = 'subjID') %>%
  left_join(core1_nasa_tlx_components, by = 'subjID') %>%
  mutate(
    across(c(sex, condition, effort_level), str_to_sentence),
    across(c(rel_liking, rel_value, rel_wanting),
           ~recode(.x, `2` = 'Less', `3` = 'Neither', `1` = 'More'))
  )

message(sprintf('stepping: %d participants', n_distinct(core1_main_data$subjID)))
write_prepared(core1_main_data, 'stepping', 'main.csv', id_col = 'subjID')

## Observed Effort (Experiment 2) ----
#
# Online study where participants watch effort being performed (video) and rate
# the outcome on liking/value scales. Repeat runs resolve to each id's earliest
# session before anything else: rating condition is assigned per session, so
# concatenating a participant's runs can place one person in both arms of this
# between-subjects design. Rows with any missing field (metadata rows) drop, a
# post-survey match is required, and each participant is capped at the 18-trial
# design length.

online_observe_files <- resolve_sessions_by_filename(
  fs::dir_ls('experiments/observed/data/raw', type = 'file', regexp = '\\.csv$'),
  id_pattern = '.*_observe_(\\d+)_.*'
)

online_observe_data <-
  read_pavlovia_dir(files = online_observe_files) %>%
  mutate(
    across(c(condition, effort_level, scale_type), str_to_sentence)
  ) %>%
  group_by(id) %>%
  fill(effort_steps, .direction = 'up') %>%
  mutate(
    effort_ans = case_when(
      scale_steps == 30 ~ 270,
      scale_steps == 270 ~ 30,
      TRUE ~ as.numeric(scale_steps)
    ),
    steps_correct = if_else(effort_steps == effort_ans, 1, 0)
  ) %>%
  ungroup() %>%
  select(
    experiment_name, experiment_id, id, condition, trial,
    effort_level, effort_steps, effort_ans, steps_correct,
    scale_type, rating = scale_outcome, rt = scale_outcome_rt
  ) %>%
  drop_na() %>%
  left_join(
    online_post_survey %>%
      filter(study == 'effortobserve') %>%
      select(id, gender, age, nasa_tlx, all_of(tlx_component_cols)),
    by = 'id'
  ) %>%
  drop_na(gender, age, nasa_tlx) %>%
  group_by(id) %>%
  arrange(trial, .by_group = TRUE) %>%
  slice_head(n = TRIALS_PER_SESSION) %>%
  ungroup()

message(sprintf('observed: %d participants after NA/post-survey filtering',
                n_distinct(online_observe_data$id)))
write_prepared(online_observe_data, 'observed', 'main.csv')

## Clicking Within Subjects (Experiment 3) ----
#
# Within-subjects clicking study (Liking and Value rated by everyone in two
# blocks) plus an in-task task-load block that does not enter the prepared
# frame. Exclusions, as preregistered and as in the cognitive study: failed
# in-task attention check, no complete post-survey, or summed clicking time
# over the 18 main trials > mean + 2.5 SD. A trial without a rating is dropped.

online_ratingclicks_within_files <- resolve_sessions_by_filename(
  fs::dir_ls('experiments/clicking_within/data/raw', type = 'file', regexp = '\\.csv$'),
  id_pattern = '.*ratingclicks_within_(\\d+)_.*'
)

online_ratingclicks_within_all <-
  read_pavlovia_dir(files = online_ratingclicks_within_files) %>%
  drop_na(trial) %>%
  mutate(
    rating = case_when(
      !is.na(rating) ~ rating,
      !is.na(load_rating) ~ load_rating
    ),
    rt = case_when(
      !is.na(rt) ~ rt,
      !is.na(load_rt) ~ load_rt
    ),
    condition = case_when(
      !is.na(rating_type) ~ rating_type,
      !is.na(load_type) ~ load_type
    ),
    condition = str_to_sentence(condition),
    effort_level = str_to_sentence(effort)
  ) %>%
  group_by(id) %>%
  mutate(attention_check = as.integer(isTRUE(any(attention_check == 1)))) %>%
  ungroup() %>%
  select(
    experiment_name, id, attention_check, block,
    trial, condition, effort_level, effort_time, rating, rt
  )

online_ratingclicks_within_exclude_attention <- online_ratingclicks_within_all %>%
  group_by(id) %>%
  summarise(failed = !any(attention_check == 1), .groups = 'drop') %>%
  filter(failed) %>%
  pull(id)

online_ratingclicks_within_post_all <- post_survey %>%
  filter(study == 'ratingclicks_within') %>%
  select(id, gender, age, belief_liking, belief_value, all_of(instrumentality_cols),
         all_of(tlx_component_cols)) %>%
  drop_na(id, gender, age, belief_liking, belief_value)

online_ratingclicks_within_exclude_no_post <- setdiff(
  online_ratingclicks_within_all$id,
  online_ratingclicks_within_post_all$id
)

online_ratingclicks_within_exclude_time <- time_outlier_ids(
  online_ratingclicks_within_all %>%
    filter(block == 'main') %>%
    transmute(id, effort_task = 'clicks', effort_trial_duration = effort_time),
  already_excluded = c(online_ratingclicks_within_exclude_attention,
                       online_ratingclicks_within_exclude_no_post)
)

online_ratingclicks_within_exclude <- unique(c(
  online_ratingclicks_within_exclude_attention,
  online_ratingclicks_within_exclude_no_post,
  online_ratingclicks_within_exclude_time
))

message(sprintf(
  'clicking_within: %d collected; excluded %d (attention %d, no post-survey %d, effort time %d)',
  n_distinct(online_ratingclicks_within_all$id),
  length(online_ratingclicks_within_exclude),
  length(online_ratingclicks_within_exclude_attention),
  length(online_ratingclicks_within_exclude_no_post),
  length(online_ratingclicks_within_exclude_time)
))

online_ratingclicks_within_filtered <- online_ratingclicks_within_all %>%
  filter(!id %in% online_ratingclicks_within_exclude)

online_ratingclicks_within_post <- online_ratingclicks_within_post_all %>%
  filter(!id %in% online_ratingclicks_within_exclude)

message(sprintf(
  'clicking_within: %d main trials without a rating dropped',
  sum(online_ratingclicks_within_filtered$block == 'main' &
        is.na(online_ratingclicks_within_filtered$rating))
))

online_ratingclicks_within_main <-
  online_ratingclicks_within_filtered %>%
  filter(block == 'main', !is.na(rating)) %>%
  select(
    experiment_name, id, trial, condition, effort_level, effort_time, rating, rt
  ) %>%
  left_join(
    select(online_ratingclicks_within_post, id, gender, age, belief_liking, belief_value),
    by = 'id'
  )

stopifnot(!anyDuplicated(online_ratingclicks_within_main[, c('id', 'trial', 'condition')]))
write_prepared(online_ratingclicks_within_main, 'clicking_within', 'main.csv')
write_prepared(online_ratingclicks_within_post, 'clicking_within', 'post.csv')

## Effort-Contingent Reveal (Experiment 4) ----
#
# Physical clicking task in which the object is scrambled into a tile grid and
# resolves toward the true object under the participant's own presses or stays
# scrambled until the endpoint. Effort is 30/100/270 presses within subjects,
# and rating condition (Liking vs Value) varies between subjects. Progressive
# sessions were recruited through Prolific and endpoint sessions through SONA;
# raw ids stay character until schedule-specific pseudonymization. Both
# schedules retain the earliest complete task exposure. As preregistered for
# both schedules, and as in the sibling studies, the analyzed sample excludes
# participants without a matching completed post-survey, with chance recognition
# memory (d' <= 0), or whose summed walk time exceeds the schedule mean + 2.5 SD;
# repeats and viewport width remain diagnostics.

online_reveal_post_survey_raw <-
  shared_post_survey %>%
  filter(study == 'ratingclicks2_physical_scrambled_reveal') %>%
  group_by(study, id) %>%
  arrange(RecordedDate, ResponseId, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

reveal_survey_required <- c(
  tlx_cols_aff, instrumentality_cols, 'like_physical',
  'like_cognitive', paste0('ncs_', 1:6), 'belief_liking', 'belief_value'
)
if (!all(c('id', 'experiment_name', 'experiment_id', 'Finished', 'Progress',
           reveal_survey_required) %in%
         names(online_reveal_post_survey_raw))) {
  stop('scrambled_reveal: the post-survey export is missing required columns')
}
valid_reveal_survey_route <- with(
  online_reveal_post_survey_raw,
  experiment_name == 'ratingclicks2_physical_scrambled_reveal' &
    ((is.na(experiment_id) | experiment_id == '') | experiment_id == '3132')
)
if (anyNA(valid_reveal_survey_route) || any(!valid_reveal_survey_route)) {
  stop('scrambled_reveal: a post-survey row has an unexpected study route')
}
if (any(!complete.cases(select(
  online_reveal_post_survey_raw, all_of(reveal_survey_required)
)))) {
  stop('scrambled_reveal: a linked post-survey is missing a required response')
}

online_reveal_post_survey <-
  online_reveal_post_survey_raw %>%
  mutate(
    age = as.numeric(age),
    across(all_of(tlx_cols_aff), as.numeric),
    nasa_tlx = rowSums(across(all_of(tlx_cols)), na.rm = TRUE)
  ) %>%
  score_tlx_components() %>%
  score_instrumentality() %>%
  recode_beliefs() %>%
  mutate(
    like_cognitive = unname(cognitive_enjoy_levels[like_cognitive]),
    like_physical  = unname(cognitive_enjoy_levels[like_physical]),
    across(matches('^ncs_[0-9]+$'), ~ unname(cognitive_ncs_levels[.x])),
    across(all_of(ncs_reverse_cols), ~ 6 - .x),
    ncs_mean = rowMeans(across(matches('^ncs_[0-9]+$')), na.rm = TRUE)
  ) %>%
  select(id, gender, age, nasa_tlx, all_of(tlx_cols_aff),
         belief_liking, belief_value,
         like_cognitive, like_physical, ncs_mean,
         all_of(instrumentality_cols), all_of(tlx_component_cols))

reveal_survey_scored <- c(
  'age', 'nasa_tlx', tlx_cols_aff, 'belief_liking', 'belief_value',
  'like_cognitive', 'like_physical', 'ncs_mean', instrumentality_cols
)
if (anyNA(select(online_reveal_post_survey, all_of(reveal_survey_scored)))) {
  stop('scrambled_reveal: a post-survey response could not be scored')
}

online_reveal_raw <-
  read_pavlovia_dir('experiments/reveal/data/raw',
                    all_character = TRUE, na_strings = c('', 'null'))

online_reveal_sessions <-
  online_reveal_raw %>%
  filter(trial_id == 'trial_summary', !is.na(id)) %>%
  count(id, file_name, name = 'n_trials')
if (nrow(online_reveal_sessions) == 0) {
  stop('scrambled_reveal: no task sessions contain trial summaries')
}

online_reveal_raw_routes <-
  online_reveal_raw %>%
  filter(trial_id == 'trial_summary', !is.na(id)) %>%
  distinct(id, reveal_assignment, recruitment, experiment_id)
if (any(count(online_reveal_raw_routes, id)$n != 1)) {
  stop('scrambled_reveal: a participant id appears under multiple task routes')
}

online_reveal_repeat_status <-
  online_reveal_sessions %>%
  group_by(id) %>%
  summarise(
    n_complete_sessions = sum(n_trials >= TRIALS_PER_SESSION),
    repeat_complete = n_complete_sessions > 1,
    .groups = 'drop'
  )

online_reveal_all <-
  resolve_sessions_by_content(online_reveal_raw) %>%
  left_join(online_reveal_repeat_status, by = 'id')

online_reveal_data <-
  online_reveal_all %>%
  filter(trial_id == 'trial_summary') %>%
  mutate(
    experiment_name       = 'ratingclicks2_physical_scrambled_reveal',
    trial                 = as.integer(trial),
    effort_task           = as.character(effort_task),
    effort_level          = str_to_sentence(effort_level),
    condition             = assigned_condition,
    reveal_assignment     = as.character(reveal_assignment),
    reveal_source         = as.character(reveal_source),
    recruitment           = as.character(recruitment),
    experiment_id         = as.character(experiment_id),
    rating_design         = as.character(rating_design),
    active_stimuli_pool   = as.character(active_stimuli_pool),
    n_trials              = as.character(n_trials),
    viewport_width        = as.integer(viewport_width),
    debug                 = as.integer(debug),
    img                   = as.character(img),
    color_variant         = as.integer(color_variant),
    steps_required        = as.integer(steps_required),
    presses_rejected      = as.integer(presses_rejected),
    walk_focus_lost       = as.integer(walk_focus_lost),
    walk_blur_ms          = as.numeric(walk_blur_ms),
    stimulus_load_error   = as.integer(stimulus_load_error),
    walk_error            = as.character(walk_error),
    effort_trial_duration = as.numeric(effort_trial_duration),
    liking_rating_num     = as.numeric(liking_rating),
    value_rating_num      = as.numeric(value_rating),
    rating                = coalesce(liking_rating_num, value_rating_num),
    rt                    = coalesce(as.numeric(liking_rt), as.numeric(value_rt))
  )

if (anyDuplicated(online_reveal_data[, c('id', 'trial')])) {
  stop('scrambled_reveal: duplicate trial numbers remain after session resolution')
}

online_reveal_participant_checks <-
  online_reveal_data %>%
  group_by(id) %>%
  summarise(
    n_trials = n(),
    n_trial_numbers = n_distinct(trial),
    first_trial = min(trial),
    last_trial = max(trial),
    n_low = sum(effort_level == 'Low'),
    n_medium = sum(effort_level == 'Medium'),
    n_high = sum(effort_level == 'High'),
    n_images = n_distinct(img),
    n_conditions = n_distinct(condition),
    n_reveal_assignments = n_distinct(reveal_assignment),
    n_reveal_sources = n_distinct(reveal_source),
    n_viewports = n_distinct(viewport_width),
    .groups = 'drop'
  )
if (any(
  online_reveal_participant_checks$n_trials != TRIALS_PER_SESSION |
  online_reveal_participant_checks$n_trial_numbers != TRIALS_PER_SESSION |
  online_reveal_participant_checks$first_trial != 1 |
  online_reveal_participant_checks$last_trial != TRIALS_PER_SESSION |
  online_reveal_participant_checks$n_low != 6 |
  online_reveal_participant_checks$n_medium != 6 |
  online_reveal_participant_checks$n_high != 6 |
  online_reveal_participant_checks$n_images != TRIALS_PER_SESSION |
  online_reveal_participant_checks$n_conditions != 1 |
  online_reveal_participant_checks$n_reveal_assignments != 1 |
  online_reveal_participant_checks$n_reveal_sources != 1 |
  online_reveal_participant_checks$n_viewports != 1
)) {
  stop('scrambled_reveal: one or more selected sessions violate the 18-trial design')
}

expected_reveal_steps <- c(Low = 30L, Medium = 100L, High = 270L)
expected_steps <- unname(expected_reveal_steps[online_reveal_data$effort_level])
if (anyNA(expected_steps) || any(online_reveal_data$steps_required != expected_steps)) {
  stop('scrambled_reveal: effort labels and required press counts do not match')
}
valid_reveal_route <- with(
  online_reveal_data,
  (reveal_assignment == 'progressive' & recruitment == 'prolific' &
     (is.na(experiment_id) | experiment_id == '')) |
    (reveal_assignment == 'endpoint' & recruitment == 'sona' &
       experiment_id == '3132')
)
if (anyNA(online_reveal_data$viewport_width) ||
    any(!online_reveal_data$condition %in% c('Liking', 'Value')) ||
    any(!online_reveal_data$reveal_assignment %in% c('progressive', 'endpoint')) ||
    any(online_reveal_data$reveal_source != 'forced') ||
    any(online_reveal_data$effort_task != 'walk') ||
    any(online_reveal_data$rating_design != 'between') ||
    any(online_reveal_data$active_stimuli_pool != 'high_value') ||
    any(online_reveal_data$n_trials != as.character(TRIALS_PER_SESSION)) ||
    any(online_reveal_data$debug != 0) ||
    anyNA(valid_reveal_route) || any(!valid_reveal_route)) {
  stop('scrambled_reveal: selected sessions contain an unexpected task configuration')
}

online_reveal_survey_routes <-
  online_reveal_post_survey_raw %>%
  transmute(
    id,
    survey_reveal_assignment = if_else(
      experiment_id == '3132', 'endpoint', 'progressive',
      missing = 'progressive'
    )
  )
matched_reveal_routes <-
  online_reveal_data %>%
  distinct(id, reveal_assignment) %>%
  inner_join(online_reveal_survey_routes, by = 'id')
if (any(
  matched_reveal_routes$reveal_assignment !=
    matched_reveal_routes$survey_reveal_assignment
)) {
  stop('scrambled_reveal: a task id matches a post-survey from another route')
}
if (anyNA(online_reveal_data$stimulus_load_error) ||
    any(!online_reveal_data$stimulus_load_error %in% 0:1) ||
    anyNA(online_reveal_data$effort_trial_duration) ||
    any(online_reveal_data$effort_trial_duration <= 0)) {
  stop('scrambled_reveal: a walk lacks a valid completion duration or error flag')
}

wrong_rating_column <-
  (online_reveal_data$condition == 'Liking' &
     (is.na(online_reveal_data$liking_rating_num) |
        !is.na(online_reveal_data$value_rating_num))) |
  (online_reveal_data$condition == 'Value' &
     (is.na(online_reveal_data$value_rating_num) |
        !is.na(online_reveal_data$liking_rating_num)))
online_reveal_ratable <- with(
  online_reveal_data,
  stimulus_load_error == 0 & is.na(walk_error)
)
if (any(wrong_rating_column[online_reveal_ratable]) ||
    anyNA(online_reveal_data$rating[online_reveal_ratable]) ||
    any(online_reveal_data$rating[online_reveal_ratable] < 1 |
          online_reveal_data$rating[online_reveal_ratable] > 5) ||
    anyNA(online_reveal_data$rt[online_reveal_ratable]) ||
    any(online_reveal_data$rt[online_reveal_ratable] <= 0) ||
    anyNA(online_reveal_data$presses_rejected) ||
    any(online_reveal_data$presses_rejected < 0)) {
  stop('scrambled_reveal: a trial has an invalid rating or response record')
}

online_reveal_memory_checks <-
  online_reveal_all %>%
  filter(trial_id == 'memory_check', !is.na(id)) %>%
  count(id, name = 'n_memory_rows')
if (nrow(online_reveal_memory_checks) != n_distinct(online_reveal_data$id) ||
    any(online_reveal_memory_checks$n_memory_rows != 1)) {
  stop('scrambled_reveal: every selected session must contain one memory check')
}

# A trial whose stimulus fails to decode still runs to completion — the task
# flags the row and carries on rather than stranding the participant — so its
# walk time is real while the rating it produced is not interpretable. Such
# trials, and any whose walk threw, are held out of the rating frame further
# down but left in the frame the summed-walk-time cutoff reads, so their
# seconds still count toward the participant who actually walked them.
online_reveal_incomplete <-
  online_reveal_data %>%
  filter(stimulus_load_error == 1 | !is.na(walk_error))

message(sprintf('reveal: %d unratable walk trials held out',
                nrow(online_reveal_incomplete)))

online_reveal_memory <- memory_frame(
  online_reveal_all,
  id_numeric = FALSE,
  keep_cols = c('reveal_assignment', 'repeat_complete', 'viewport_width')
) %>%
  mutate(
    repeat_complete = as.logical(repeat_complete),
    viewport_width = as.integer(viewport_width)
  )

# Pseudonym map for the prepared files. Progressive codes retain their stable
# r-prefix; endpoint codes use a separate e-prefix so either schedule can grow
# without renumbering the other.
reveal_id_lookup <- local({
  ids_by_schedule <- online_reveal_data %>%
    distinct(id, reveal_assignment)
  progressive_ids <- ids_by_schedule %>%
    filter(reveal_assignment == 'progressive') %>%
    pull(id) %>%
    sort()
  endpoint_ids <- ids_by_schedule %>%
    filter(reveal_assignment == 'endpoint') %>%
    pull(id) %>%
    sort()
  bind_rows(
    tibble(
      reveal_assignment = 'progressive',
      id = progressive_ids,
      prepared_id = sprintf('r%03d', seq_along(progressive_ids))
    ),
    tibble(
      reveal_assignment = 'endpoint',
      id = endpoint_ids,
      prepared_id = sprintf('e%03d', seq_along(endpoint_ids))
    )
  )
})

pseudonymize_reveal_ids <- function(data) {
  out <- data %>%
    left_join(
      reveal_id_lookup,
      by = c('reveal_assignment', 'id'),
      relationship = 'many-to-one'
    )
  if (anyNA(out$prepared_id)) {
    stop('scrambled_reveal: a prepared participant id could not be assigned')
  }
  out %>%
    mutate(id = prepared_id) %>%
    select(-prepared_id)
}

online_reveal_exclude_no_post <- setdiff(
  unique(online_reveal_data$id),
  online_reveal_post_survey$id
)

# The summed-time cutoff is computed within reveal schedule. Pooling the two
# schedules would change the progressive cutoff because their completion-time
# scales differ.
online_reveal_exclude_time_linked <- c(
  time_outlier_ids(
    filter(online_reveal_data, reveal_assignment == 'progressive'),
    already_excluded = intersect(
      online_reveal_exclude_no_post,
      online_reveal_data$id[online_reveal_data$reveal_assignment == 'progressive']
    )
  ),
  time_outlier_ids(
    filter(online_reveal_data, reveal_assignment == 'endpoint'),
    already_excluded = intersect(
      online_reveal_exclude_no_post,
      online_reveal_data$id[online_reveal_data$reveal_assignment == 'endpoint']
    )
  )
)
online_reveal_exclude_time_all <- c(
  time_outlier_ids(
    filter(online_reveal_data, reveal_assignment == 'progressive'),
    already_excluded = character()
  ),
  time_outlier_ids(
    filter(online_reveal_data, reveal_assignment == 'endpoint'),
    already_excluded = character()
  )
)

online_reveal_eligibility <-
  online_reveal_data %>%
  distinct(id, condition, reveal_assignment, reveal_source,
           repeat_complete, viewport_width) %>%
  left_join(select(online_reveal_memory, id, dprime), by = 'id') %>%
  mutate(
    # Unfinished responses never reach the survey frame, so this flags a
    # participant whose survey was both matched and completed.
    has_post_survey = id %in% online_reveal_post_survey$id,
    memory_pass = !is.na(dprime) & dprime > 0,
    time_outlier_linked = id %in% online_reveal_exclude_time_linked,
    time_outlier_all = id %in% online_reveal_exclude_time_all,
    primary_included = has_post_survey & memory_pass & !time_outlier_linked
  )
if (anyDuplicated(online_reveal_eligibility$id) ||
    anyNA(online_reveal_eligibility$primary_included)) {
  stop('scrambled_reveal: participant eligibility could not be resolved uniquely')
}

online_reveal_flow <-
  online_reveal_eligibility %>%
  group_by(reveal_assignment) %>%
  summarise(
    collected = n(),
    matched_survey = sum(has_post_survey),
    memory_fail = sum(!memory_pass),
    time_outlier = sum(time_outlier_linked),
    repeat_complete = sum(repeat_complete),
    narrow_viewport = sum(viewport_width < 940),
    primary = sum(primary_included),
    .groups = 'drop'
  )
if (!setequal(online_reveal_flow$reveal_assignment, c('progressive', 'endpoint'))) {
  stop('scrambled_reveal: both reveal schedules must be present')
}
for (schedule in c('progressive', 'endpoint')) {
  flow <- filter(online_reveal_flow, reveal_assignment == schedule)
  message(sprintf(
    paste0(
      'reveal %s: %d collected; %d survey-matched; %d primary; ',
      'excluded on d\' <= 0 %d, walk time %d; diagnostics: repeats %d, viewport < 940 px %d'
    ),
    schedule, flow$collected, flow$matched_survey, flow$primary,
    flow$memory_fail, flow$time_outlier, flow$repeat_complete,
    flow$narrow_viewport
  ))
}

online_reveal_complete <-
  online_reveal_data %>%
  anti_join(online_reveal_incomplete, by = c('id', 'trial')) %>%
  select(
    experiment_name, id, trial, condition, reveal_assignment, reveal_source,
    effort_task, effort_level, steps_required, repeat_complete,
    viewport_width,
    img, color_variant, rating, rt,
    presses_rejected, walk_focus_lost, walk_blur_ms, effort_trial_duration
  ) %>%
  left_join(
    select(online_reveal_post_survey, id, gender, age, nasa_tlx,
           belief_liking, belief_value, like_cognitive, like_physical, ncs_mean,
           all_of(instrumentality_cols), all_of(tlx_component_cols)),
    by = 'id'
  ) %>%
  left_join(
    select(
      online_reveal_eligibility, id, has_post_survey, memory_pass,
      time_outlier_linked, time_outlier_all, primary_included
    ),
    by = 'id'
  )

online_reveal_main <-
  online_reveal_complete %>%
  filter(primary_included)

online_reveal_memory <-
  online_reveal_memory %>%
  left_join(
    select(
      online_reveal_eligibility, id, has_post_survey, memory_pass,
      time_outlier_linked, time_outlier_all, primary_included
    ),
    by = 'id'
  ) %>%
  filter(primary_included)

online_reveal_complete <- pseudonymize_reveal_ids(online_reveal_complete)
online_reveal_main <- pseudonymize_reveal_ids(online_reveal_main)
online_reveal_memory <- pseudonymize_reveal_ids(online_reveal_memory)
stopifnot(!anyNA(online_reveal_complete$id), !anyNA(online_reveal_main$id),
          !anyNA(online_reveal_memory$id))

stopifnot(!anyDuplicated(online_reveal_complete[, c('id', 'trial')]))
stopifnot(!anyDuplicated(online_reveal_main[, c('id', 'trial')]))
write_prepared(online_reveal_main, 'reveal', 'main.csv')
write_prepared(online_reveal_complete, 'reveal', 'main_complete.csv')
write_prepared(online_reveal_memory, 'reveal', 'memory.csv')

## Cognitive Effort (Experiment 5) ----
#
# Cognitive-effort study with two between-subjects task arms (cogletters vs
# subtractn) and between-subjects rating condition (Liking vs Value). Mixed-task
# raw output: after common-column coercion and exclusions the frame splits on
# effort_task into two prepared CSVs, one per task.
# Exclusions: no post-survey, chance recognition memory (d' <= 0), or summed
# effort time > mean + 2.5 SD within task (the preregistered total-time rule).

online_ratingclicks2_cognitive_post_survey <-
  shared_post_survey %>%
  filter(study == 'ratingclicks2_cognitive') %>%
  mutate(
    id = as_numeric_id(id),
    age = as.numeric(age),
    across(all_of(tlx_cols_aff), as.numeric),
    nasa_tlx = rowSums(across(all_of(tlx_cols)), na.rm = TRUE)
  ) %>%
  recode_beliefs() %>%
  score_instrumentality() %>%
  score_tlx_components() %>%
  mutate(
    like_cognitive = unname(cognitive_enjoy_levels[like_cognitive]),
    like_physical  = unname(cognitive_enjoy_levels[like_physical]),
    like_which_effort = case_when(
      str_detect(like_which_effort, 'cognitive') ~ 'Cognitive',
      str_detect(like_which_effort, 'physical')  ~ 'Physical'
    ),
    across(matches('^ncs_[0-9]+$'), ~ unname(cognitive_ncs_levels[.x])),
    across(all_of(ncs_reverse_cols), ~ 6 - .x),
    ncs_mean = rowMeans(across(matches('^ncs_[0-9]+$')), na.rm = TRUE)
  ) %>%
  group_by(study, id) %>%
  arrange(RecordedDate, ResponseId, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  drop_na(id) %>%
  select(id, gender, age, nasa_tlx, all_of(tlx_cols_aff),
         belief_liking, belief_value,
         like_cognitive, like_physical, like_which_effort, ncs_mean,
         all_of(instrumentality_cols), all_of(tlx_component_cols))

online_ratingclicks2_cognitive_all <-
  read_pavlovia_dir('experiments/cognitive/data/raw',
                    all_character = TRUE, na_strings = c('', 'null')) %>%
  resolve_sessions_by_content()

online_ratingclicks2_cognitive_data <-
  online_ratingclicks2_cognitive_all %>%
  filter(trial_id == 'trial_summary') %>%
  mutate(
    experiment_name       = 'ratingclicks2_cognitive',
    id                    = as_numeric_id(id),
    trial                 = as.integer(trial),
    effort_task           = as.character(effort_task),
    effort_level          = str_to_sentence(effort_level),
    magnitude             = as.integer(magnitude),
    condition             = assigned_condition,
    img                   = as.character(img),
    color_variant         = as.integer(color_variant),
    operations_completed  = as.integer(operations_completed),
    operations_total      = as.integer(operations_total),
    effort_completed      = as.integer(effort_completed),
    effort_trial_duration = as.numeric(effort_trial_duration),
    rating                = coalesce(as.numeric(liking_rating), as.numeric(value_rating)),
    rt                    = coalesce(as.numeric(liking_rt), as.numeric(value_rt))
  ) %>%
  distinct(id, trial, .keep_all = TRUE)

if (anyNA(online_ratingclicks2_cognitive_data$effort_trial_duration)) {
  stop('cognitive: a trial lacks a numeric effort duration')
}

online_ratingclicks2_cognitive_memory <-
  memory_frame(online_ratingclicks2_cognitive_all, id_numeric = TRUE)

online_ratingclicks2_cognitive_exclude_no_post <- setdiff(
  unique(online_ratingclicks2_cognitive_data$id),
  online_ratingclicks2_cognitive_post_survey$id
)
online_ratingclicks2_cognitive_exclude_memory <- online_ratingclicks2_cognitive_memory %>%
  filter(dprime <= 0) %>%
  pull(id)
online_ratingclicks2_cognitive_exclude_time <- time_outlier_ids(
  online_ratingclicks2_cognitive_data,
  already_excluded = online_ratingclicks2_cognitive_exclude_no_post
)
online_ratingclicks2_cognitive_exclude <- unique(c(
  online_ratingclicks2_cognitive_exclude_no_post,
  online_ratingclicks2_cognitive_exclude_memory,
  online_ratingclicks2_cognitive_exclude_time
))

message(sprintf(
  'cognitive: %d collected; excluded %d (no post-survey %d, d\' <= 0 %d, effort time %d)',
  n_distinct(online_ratingclicks2_cognitive_data$id),
  length(online_ratingclicks2_cognitive_exclude),
  length(online_ratingclicks2_cognitive_exclude_no_post),
  length(online_ratingclicks2_cognitive_exclude_memory),
  length(online_ratingclicks2_cognitive_exclude_time)
))

online_ratingclicks2_cognitive_memory <- online_ratingclicks2_cognitive_memory %>%
  filter(!id %in% online_ratingclicks2_cognitive_exclude)

online_ratingclicks2_cognitive_cogletters_main <-
  online_ratingclicks2_cognitive_data %>%
  filter(effort_task == 'cogletters', !id %in% online_ratingclicks2_cognitive_exclude) %>%
  mutate(
    effort_source     = as.character(effort_source),
    effort_matches    = as.integer(effort_matches),
    effort_mismatches = as.integer(effort_mismatches)
  ) %>%
  select(
    experiment_name, id, trial, condition, effort_task, effort_level, magnitude,
    img, color_variant, rating, rt,
    operations_completed, operations_total, effort_completed, effort_trial_duration,
    effort_source, effort_matches, effort_mismatches
  ) %>%
  left_join(
    select(online_ratingclicks2_cognitive_post_survey, id, gender, age, nasa_tlx,
           belief_liking, belief_value,
           like_cognitive, like_physical, like_which_effort, ncs_mean,
           all_of(instrumentality_cols), all_of(tlx_component_cols)),
    by = 'id'
  )

online_ratingclicks2_cognitive_subtractn_main <-
  online_ratingclicks2_cognitive_data %>%
  filter(effort_task == 'subtractn', !id %in% online_ratingclicks2_cognitive_exclude) %>%
  mutate(
    effort_start_number          = as.integer(effort_start_number),
    effort_first_attempt_correct = as.integer(effort_first_attempt_correct),
    effort_total_attempts        = as.integer(effort_total_attempts),
    effort_incorrect_attempts    = as.integer(effort_incorrect_attempts)
  ) %>%
  select(
    experiment_name, id, trial, condition, effort_task, effort_level, magnitude,
    img, color_variant, rating, rt,
    operations_completed, operations_total, effort_completed, effort_trial_duration,
    effort_start_number, effort_first_attempt_correct, effort_total_attempts,
    effort_incorrect_attempts
  ) %>%
  left_join(
    select(online_ratingclicks2_cognitive_post_survey, id, gender, age, nasa_tlx,
           belief_liking, belief_value,
           like_cognitive, like_physical, like_which_effort, ncs_mean,
           all_of(instrumentality_cols), all_of(tlx_component_cols)),
    by = 'id'
  )

stopifnot(
  !anyDuplicated(online_ratingclicks2_cognitive_cogletters_main[, c('id', 'trial')]),
  !anyDuplicated(online_ratingclicks2_cognitive_subtractn_main[, c('id', 'trial')])
)
write_prepared(online_ratingclicks2_cognitive_cogletters_main, 'cognitive', 'cogletters.csv')
write_prepared(online_ratingclicks2_cognitive_subtractn_main, 'cognitive', 'subtractn.csv')
write_prepared(online_ratingclicks2_cognitive_memory, 'cognitive', 'memory.csv')

## Prices Between Subjects (Experiment 6A) ----
#
# Lab WTP/WTA study (Buy vs Sell assigned between subjects). Main ratings,
# stepping performance (per-participant xlsx), and NASA-TLX (per-participant
# export trees) join by inner join: only participants present in all three
# sources enter the prepared frame.

supp_buysell_main <-
  tibble(
    file_name = fs::dir_ls(
      'experiments/prices_between/data/raw/',
      type = 'file', regexp = '\\d{1}_data.csv'
    )
  ) %>%
  mutate(file_content = map(file_name, data.table::fread)) %>%
  unnest(cols = c(file_content)) %>%
  mutate(
    rating = as.numeric(str_extract(scaleRating, '(?<=\\$)\\d+(?:\\.\\d+)?')),
    across(c(condition, effort), str_to_sentence),
    run = stringr::str_extract(basename(file_name), '^[0-9]+'),
    experiment_name = if_else(is.na(run), 'buysell_between', paste0('buysell_between', run))
  ) %>%
  select(experiment_name, id, condition, trial, effort_level = effort, stim, rating, scaleRT, time) %>%
  drop_na(id, trial, effort_level, rating)

supp_buysell_performance <-
  tibble(
    file_name = fs::dir_ls(
      'experiments/prices_between/data/raw/perf/',
      type = 'file', regexp = '.xlsx'
    )
  ) %>%
  mutate(file_content = map(file_name, ~readxl::read_excel(file.path(.)))) %>%
  unnest(cols = c(file_content)) %>%
  mutate(id = as.numeric(str_extract(file_name, '(?<=perf/)\\d+(?=.xlsx)'))) %>%
  group_by(id) %>%
  mutate(
    effortLevel = str_to_sentence(effortLevel),
    performed = actual - first(`initial count:`),
    effort_steps = performed - lag(performed, default = first(performed))
  ) %>%
  ungroup() %>%
  filter(practiceMain == 'main') %>%
  filter(id %in% unique(supp_buysell_main$id)) %>%
  left_join(unique(supp_buysell_main[, c('id', 'condition')]), by = 'id') %>%
  select(id, condition, trial = trialNo, effort_level = effortLevel, effort_steps)

supp_buysell_tlx_data <-
  tibble(
    file_name = fs::dir_ls(
      'experiments/prices_between/data/raw/TLX/',
      type = 'file', recurse = TRUE,
      regexp = '.csv'
    )
  ) %>%
  mutate(file_content = map(file_name, ~read_csv(file.path(.), col_names = TRUE, skip = 3,
                                                 n_max = 6, show_col_types = FALSE))) %>%
  unnest(cols = c(file_content)) %>%
  select(file_name, component = `Scale name`, raw_rating = `Raw Rating`, Tally) %>%
  mutate(
    id = as.numeric(str_extract(file_name, '(?<=TLX/)\\d+(?=/Task)')),
    weight = as.numeric(Tally) / 15,
    adjusted_rating = as.numeric(raw_rating) * weight,
    adjusted_rating = if_else(adjusted_rating == Inf, 0, adjusted_rating)
  ) %>%
  filter(id %in% unique(supp_buysell_main$id)) %>%
  left_join(unique(supp_buysell_main[, c('id', 'condition')]), by = 'id') %>%
  select(id, condition, component, raw_rating, adjusted_rating)

supp_buysell_tlx <-
  supp_buysell_tlx_data %>%
  group_by(id, condition) %>%
  summarise(nasa_tlx = sum(raw_rating), .groups = 'drop')

# The six raw component ratings, already on the 0-100 NASA-TLX metric
supp_buysell_tlx_components <-
  supp_buysell_tlx_data %>%
  mutate(component = recode(
    component, `Mental Demand` = 'tlx_mental', `Physical Demand` = 'tlx_physical',
    `Temporal Demand` = 'tlx_temporal', Performance = 'tlx_performance',
    Effort = 'tlx_effort', Frustration = 'tlx_frustration'
  ), raw_rating = as.numeric(raw_rating)) %>%
  select(id, condition, component, raw_rating) %>%
  pivot_wider(names_from = component, values_from = raw_rating)
stopifnot(setequal(setdiff(names(supp_buysell_tlx_components), c('id', 'condition')),
                   head(tlx_component_cols, 6)))

supp_buysell_data <-
  supp_buysell_main %>%
  inner_join(supp_buysell_performance, by = c('id', 'condition', 'trial', 'effort_level')) %>%
  inner_join(supp_buysell_tlx, by = c('id', 'condition')) %>%
  inner_join(supp_buysell_tlx_components, by = c('id', 'condition')) %>%
  select(experiment_name, id, condition, trial, effort_level, effort_time = time,
         rating, rt = scaleRT, nasa_tlx, all_of(head(tlx_component_cols, 6)))

message(sprintf('prices_between: %d participants with main+performance+TLX',
                n_distinct(supp_buysell_data$id)))
write_prepared(supp_buysell_data, 'prices_between', 'main.csv')

## Prices Within Subjects (Experiment 6B) ----
#
# Within-subjects buying/selling price study. Exclusions, as preregistered and
# as in the clicking and cognitive studies: failed attention check (the
# in-task check, or a role-order answer inconsistent with the participant's
# actual order), no complete post-survey (the buysell_within2 entry in the
# shared survey), or summed clicking time over the 18 main trials
# > mean + 2.5 SD. A trial without a rating is dropped.

online_buysell_within_files <- resolve_sessions_by_filename(
  fs::dir_ls('experiments/prices_within/data/raw', type = 'file', regexp = '\\.csv$'),
  id_pattern = '.*buysell_within_(\\d+)_.*'
)

online_buysell_within_raw <-
  read_pavlovia_dir(files = online_buysell_within_files) %>%
  mutate(
    condition = str_to_sentence(condition),
    effort_level = str_to_sentence(effort),
    component = str_to_sentence(component),
    experiment_name = 'buysell_within',
    attention_check2 = as.integer(attention_check2)
  ) %>%
  group_by(id) %>%
  mutate(
    attention_check1 = as.integer(any(attention_check == 1, na.rm = TRUE))
  ) %>%
  fill(attention_check2, .direction = 'updown') %>%
  ungroup()

# Buyer/Seller order from the trial sequence
order_by_id <- online_buysell_within_raw %>%
  filter(block == 'main', condition %in% c('Buyer', 'Seller')) %>%
  group_by(id, condition) %>%
  summarise(first_trial = min(trial, na.rm = TRUE), .groups = 'drop') %>%
  arrange(id, first_trial) %>%
  group_by(id) %>%
  summarise(order = paste(condition, collapse = ''), .groups = 'drop')

attn_by_id <- online_buysell_within_raw %>%
  group_by(id) %>%
  summarise(
    attention_check1 = max(attention_check1, na.rm = TRUE),
    attention_check2 = first(attention_check2[!is.na(attention_check2)], default = NA_integer_),
    .groups = 'drop'
  )

online_buysell_within_all <- online_buysell_within_raw %>%
  drop_na(rating) %>%
  left_join(order_by_id, by = 'id') %>%
  select(
    experiment_name, id, attention_check1, attention_check2,
    block, trial, order, condition, effort_level, effort_time, component, rating, rt
  )

online_buysell_within_exclude_attention <- attn_by_id %>%
  left_join(order_by_id, by = 'id') %>%
  mutate(
    expected = case_when(
      order == 'SellerBuyer' ~ 1L,
      order == 'BuyerSeller' ~ 2L,
      TRUE ~ NA_integer_
    ),
    should_exclude = attention_check1 == 0L | (!is.na(attention_check2) & attention_check2 != expected)
  ) %>%
  filter(should_exclude) %>%
  pull(id)

online_buysell_within_post_all <- post_survey %>%
  filter(study == 'buysell_within2') %>%
  select(id, gender, age, belief_liking, belief_value, all_of(instrumentality_cols),
         all_of(tlx_component_cols)) %>%
  drop_na(id, gender, age, belief_liking, belief_value)

online_buysell_within_exclude_no_post <- setdiff(
  online_buysell_within_all$id,
  online_buysell_within_post_all$id
)

online_buysell_within_exclude_time <- time_outlier_ids(
  online_buysell_within_raw %>%
    filter(block == 'main', !is.na(trial)) %>%
    transmute(id, effort_task = 'clicks', effort_trial_duration = effort_time),
  already_excluded = c(online_buysell_within_exclude_attention,
                       online_buysell_within_exclude_no_post)
)

online_buysell_within_exclude <- unique(c(
  online_buysell_within_exclude_attention,
  online_buysell_within_exclude_no_post,
  online_buysell_within_exclude_time
))

message(sprintf(
  'prices_within: %d collected; excluded %d (attention %d, no post-survey %d, effort time %d)',
  n_distinct(online_buysell_within_all$id),
  length(online_buysell_within_exclude),
  length(online_buysell_within_exclude_attention),
  length(online_buysell_within_exclude_no_post),
  length(online_buysell_within_exclude_time)
))

online_buysell_within_filtered <- online_buysell_within_all %>%
  filter(!id %in% online_buysell_within_exclude)

online_buysell_within_post <- online_buysell_within_post_all %>%
  filter(!id %in% online_buysell_within_exclude)

online_buysell_within_main <-
  online_buysell_within_filtered %>%
  filter(block == 'main') %>%
  left_join(
    select(online_buysell_within_post, id, gender, age, belief_liking, belief_value,
           all_of(instrumentality_cols), all_of(tlx_component_cols)),
    by = 'id'
  ) %>%
  select(
    experiment_name, id, gender, age,
    trial, condition, effort_level, effort_time, rating, rt,
    belief_liking, belief_value, all_of(instrumentality_cols), all_of(tlx_component_cols)
  )

write_prepared(online_buysell_within_main, 'prices_within', 'main.csv')

# Supplementary Studies ----

## Task-Load Validation, Stepping (Experiment S1) ----
#
# Lab stepping-task validation: task-load component ratings across effort
# levels (demographics and performance join by participant/trial).

supp_task_demo <- read_csv('experiments/taskload_stepping/data/raw/task-demo.csv',
                           show_col_types = FALSE) %>%
  group_by(subjID) %>%
  slice(1) %>%
  ungroup()

supp_task_performance <- read_csv('experiments/taskload_stepping/data/raw/task-perf.csv',
                                  show_col_types = FALSE)

supplementary_task_data <-
  read_csv('experiments/taskload_stepping/data/raw/task-main.csv', show_col_types = FALSE) %>%
  left_join(supp_task_demo, by = 'subjID') %>%
  left_join(supp_task_performance, by = c('subjID', 'trial')) %>%
  mutate(
    sex = str_to_sentence(sex),
    effort_level = str_to_sentence(effort_level),
    effort_steps = effort_trials,
    component = recode(scale_type, Pleasant = 'Pleasure'),
    component = str_remove(component, '_Demand'),
    comp_type = case_when(
      component %in% c('Frustration', 'Pleasure') ~ 'Affective',
      TRUE ~ 'Demand'
    )
  ) %>%
  select(-scale_text, -scale_type, -steps_req, -effort_trials,
         rating = scale_rating, rt = scale_RT)

message(sprintf('taskload_stepping: %d participants', n_distinct(supplementary_task_data$subjID)))
write_prepared(supplementary_task_data, 'taskload_stepping', 'main.csv', id_col = 'subjID')

## Task-Load Validation, Clicking (Experiment S2) ----
#
# Online clicking-task validation: task-load component ratings across effort
# levels. Repeat runs resolve to each id's earliest session first — the 126-row
# cap fills from a concatenated stream otherwise, which yields a full-looking
# participant holding half the design twice. Rows with any missing field drop,
# a post-survey match is required, and each participant is capped at the
# 126-row design length (18 trials x 7 components).

online_taskloadclicks_files <- resolve_sessions_by_filename(
  fs::dir_ls('experiments/taskload_clicking/data/raw', type = 'file',
             regexp = '\\.csv$'),
  id_pattern = '.*_taskloadclicks_(\\d+)_.*'
)

online_taskloadclicks_data <-
  read_pavlovia_dir(files = online_taskloadclicks_files) %>%
  mutate(
    across(c(effort_level, load_type), str_to_sentence)
  ) %>%
  group_by(id) %>%
  fill(trial, effort_level, .direction = 'down') %>%
  fill(effort_time, .direction = 'up') %>%
  ungroup() %>%
  select(
    experiment_name, experiment_id, id,
    trial, effort_level, effort_time,
    component = load_type, rt = load_rt, rating = load_rating
  ) %>%
  drop_na() %>%
  left_join(
    online_post_survey %>%
      filter(study == 'effortratingclickstaskload') %>%
      select(id, gender, age, nasa_tlx),
    by = 'id'
  ) %>%
  drop_na() %>%
  group_by(id) %>%
  arrange(trial, .by_group = TRUE) %>%
  slice_head(n = TASKLOAD_ROWS_CAP) %>%
  ungroup()

message(sprintf('taskload_clicking: %d participants',
                n_distinct(online_taskloadclicks_data$id)))
write_prepared(online_taskloadclicks_data, 'taskload_clicking', 'main.csv')

## Price-Liking Order (Experiment S3) ----
#
# Qualtrics survey study: within-participant price and liking judgments of
# ideographs under counterbalanced question order (price-first vs liking-first).
# Responses unstack from the order-embedding export columns into one row per
# participant x item pair with Liking and Price columns. Two price items carry
# export codes off the 1-5 scale: Q170 codes its options 6-10, and Q167 codes
# its top option ($500) as 6; both map back onto 1-5.

supplementary_price_liking_raw_data <-
  read_csv('experiments/price_liking_order/data/raw/supp1.csv', show_col_types = FALSE) %>%
  mutate(
    Q170 = Q170 - 5,
    Q167 = if_else(Q167 == 6, 5, Q167),
    priceFirst = `DO-BL-PriceFirst`,
    likeFirst = `DO-BL-DefaultQuestionBlock`
  )
stopifnot(all(as.matrix(select(supplementary_price_liking_raw_data, Q161:Q236, V1.1:Q95))
              %in% c(1:5, NA)))

supplementary_price_order_data <- supplementary_price_liking_raw_data %>%
  select(ID, priceFirst) %>%
  separate(priceFirst, paste0('a', as.character(1:22)), sep = '\\|') %>%
  select(-a1, -a2) %>%
  pivot_longer(a3:a22, names_to = 'order', values_to = 'ideograph') %>%
  separate(order, into = c('drop', 'order1'), 1, convert = TRUE) %>%
  select(-drop)

supplementary_liking_order_data <- supplementary_price_liking_raw_data %>%
  select(ID, likeFirst) %>%
  separate(likeFirst, paste0('a', as.character(1:22)), sep = '\\|') %>%
  select(-a1, -a2) %>%
  pivot_longer(a3:a22, names_to = 'order', values_to = 'ideograph') %>%
  separate(order, into = c('drop', 'order1'), 1, convert = TRUE) %>%
  select(-drop)

supplementary_price_responses <- supplementary_price_liking_raw_data %>%
  select(ID, Q161:Q236) %>%
  pivot_longer(Q161:Q236, names_to = 'ideograph', values_to = 'resp') %>%
  inner_join(supplementary_price_order_data, by = c('ID', 'ideograph')) %>%
  arrange(ID, order1) %>%
  mutate(
    liking = (order1 + 1) %% 2,
    qg = (order1 + 1) %/% 2,
    cond = 'price_first'
  )

supplementary_liking_responses <- supplementary_price_liking_raw_data %>%
  select(ID, V1.1:Q95) %>%
  pivot_longer(V1.1:Q95, names_to = 'ideograph', values_to = 'resp') %>%
  inner_join(supplementary_liking_order_data, by = c('ID', 'ideograph')) %>%
  arrange(ID, order1) %>%
  mutate(
    liking = order1 %% 2,
    qg = (order1 + 1) %/% 2,
    cond = 'liking_first'
  )

supplementary_price_liking_data <-
  bind_rows(
    supplementary_liking_responses %>%
      select(-ideograph, -order1) %>%
      pivot_wider(names_from = liking, values_from = resp),
    supplementary_price_responses %>%
      select(-ideograph, -order1) %>%
      pivot_wider(names_from = liking, values_from = resp)
  ) %>%
  mutate(
    Liking = `1`,
    Price = `0`,
    cond = recode(cond, liking_first = 'Liking First', price_first = 'Price First')
  ) %>%
  select(-c(`0`, `1`), id = ID)

message(sprintf('price_liking_order: %d participants',
                n_distinct(supplementary_price_liking_data$id)))
write_prepared(supplementary_price_liking_data, 'price_liking_order', 'main.csv')

## Price Magnitude (Experiment S4) ----
#
# Qualtrics survey study: liking ratings of items paired with explicit price
# information at small vs large price magnitudes. The export unstacks into one
# row per participant x item with the shown price value and the liking rating.

supplementary_price_magnitude_data <-
  read_csv('experiments/price_magnitude/data/raw/supp2.csv', col_types = cols(.default = 'c')) %>%
  slice(-1) %>%
  # One row is one session and one participant; the self-entered SubID repeats
  # across sessions, so the session index is the participant id
  mutate(id = row_number()) %>%
  select(id, magnitude = Values, val1:val5, matches('[VL]\\d\\.')) %>%
  mutate(across(val1:val5, ~str_extract(.x, '\\d+'))) %>%
  pivot_longer(
    cols = matches('[VL]\\d\\.'),
    names_to = 'type',
    values_to = 'liking'
  ) %>%
  mutate(
    value = case_when(
      str_extract(type, '(?<=[VL])\\d{1}') == 1 ~ val1,
      str_extract(type, '(?<=[VL])\\d{1}') == 2 ~ val2,
      str_extract(type, '(?<=[VL])\\d{1}') == 3 ~ val3,
      str_extract(type, '(?<=[VL])\\d{1}') == 4 ~ val4,
      str_extract(type, '(?<=[VL])\\d{1}') == 5 ~ val5
    ),
    value = str_extract(value, '^\\d{1}')
  ) %>%
  drop_na() %>%
  select(-c(type, val1:val5))

message(sprintf('price_magnitude: %d participants',
                n_distinct(supplementary_price_magnitude_data$id)))
write_prepared(supplementary_price_magnitude_data, 'price_magnitude', 'main.csv')

## Foraging Experience (Experiment S5) ----
#
# Foraging value-judgment study with prior-experience factor; hand-coded
# strategy labels join by participant. Only post-foraging trials are kept
# (firstsec == 0). The raw files key participants by MTurk worker id, which
# stays in raw/: each participant is coded by the rank of their worker id, and
# the hand-made strategy-category coding takes the same codes.

core2_raw <- read_csv('experiments/foraging/data/raw/exp2-data.csv', show_col_types = FALSE)
core2_ids <- levels(as.factor(core2_raw$uniqueid))

core2_data <-
  core2_raw %>%
  left_join(select(
    read_csv('experiments/foraging/data/raw/exp2-coded.csv', show_col_types = FALSE),
    uniqueid, DirExp, strat2, DirStrat
  ),
  by = 'uniqueid'
  ) %>%
  select(
    where(~ !all(is.na(.))),
    any_of(c('experiment_name', 'experiment_id'))
  ) %>%
  mutate(
    type = ifelse(Rating == 'Liking_Rating', 'Liking', 'Value'),
    experience = ifelse(Exp == 'Experience', 'Experience', 'No Experience'),
    difmost = element == 5 | element == 6,
    effort_level = factor(diff, levels = c(.9, .5, .2), labels = c('Low', 'Medium', 'High')),
    subjID = as.factor(match(uniqueid, core2_ids))
  ) %>%
  select(subjID, type, experience, current_trial, firstsec, effort_level, numpressess, rt, value) %>%
  filter(firstsec == 0)

message(sprintf('foraging: %d participants', n_distinct(core2_data$subjID)))
write_prepared(core2_data, 'foraging', 'main.csv', id_col = 'subjID')

# Strategy categories, hand-coded per participant from the free-text strategy
# answers; read as text so the coding is carried through verbatim
core2_strategies <-
  read_csv('experiments/foraging/data/raw/coded_categories.csv',
           col_types = cols(.default = col_character()), na = character())

unmatched_strategies <- setdiff(core2_strategies$uniqueid, core2_ids)
if (length(unmatched_strategies) > 0) {
  stop('foraging: ', length(unmatched_strategies), ' coded strategy response(s) ',
       'match no participant in exp2-data.csv')
}

core2_strategies %>%
  mutate(subjID = match(uniqueid, core2_ids), .before = 1) %>%
  select(-uniqueid) %>%
  write_prepared('foraging', 'coded_categories.csv', id_col = 'subjID')

## Waiting (Experiment S6) ----
#
# Passive waiting variant of the cognitive line: the cognitive effort
# task is replaced by a forced wait (10/20/40 s = Low/Medium/High, within
# subjects). Object preview and rating condition (Liking vs Value) are crossed
# between subjects. Recruitment is Prolific, so participant ids are 24-char hex
# strings kept as character throughout. Exclusions are no post-survey and chance
# recognition memory. The cognitive block's summed-time cutoff is deliberately
# absent: the wait is imposed by the program, so its duration is 420 s for
# everyone up to timer overshoot and carries no signal about whether anyone was
# on task — a mean + 2.5 SD rule there separates browsers, not participants.
# Disengagement is instead read from the focus-loss logging the notebook
# analyzes.

online_wait_post_survey <-
  read_qualtrics('experiments/wait/data/raw/post_survey_wait.csv') %>%
  mutate(
    age = as.numeric(age),
    across(all_of(tlx_cols_aff), as.numeric),
    nasa_tlx = rowSums(across(all_of(tlx_cols)), na.rm = TRUE)
  ) %>%
  recode_beliefs() %>%
  mutate(
    like_cognitive = unname(cognitive_enjoy_levels[like_cognitive]),
    like_physical  = unname(cognitive_enjoy_levels[like_physical]),
    across(matches('^ncs_[0-9]+$'), ~ unname(cognitive_ncs_levels[.x])),
    across(all_of(ncs_reverse_cols), ~ 6 - .x),
    ncs_mean = rowMeans(across(matches('^ncs_[0-9]+$')), na.rm = TRUE),
    opportunity_cost     = unname(extent_levels[opportunity_cost]),
    opportunity_activity = unname(wait_opportunity_activity_levels[opportunity_general]),
    opportunity_trials   = as.numeric(opportunity_trials_1),
    opportunity_accomp   = as.numeric(opportunity_accomp_1)
  ) %>%
  score_instrumentality() %>%
  score_tlx_components() %>%
  group_by(experiment_id, id) %>%
  arrange(RecordedDate, ResponseId, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  drop_na(id) %>%
  select(id, gender, age, nasa_tlx, all_of(tlx_cols_aff),
         belief_liking, belief_value,
         like_cognitive, like_physical, ncs_mean,
         opportunity_cost, opportunity_activity, opportunity_trials,
         opportunity_accomp, all_of(instrumentality_cols), all_of(tlx_component_cols))

online_wait_all <-
  read_pavlovia_dir('experiments/wait/data/raw',
                    all_character = TRUE, na_strings = c('', 'null')) %>%
  resolve_sessions_by_content()

online_wait_data <-
  online_wait_all %>%
  filter(trial_id == 'trial_summary') %>%
  mutate(
    experiment_name       = 'ratingclicks2_cognitive_nopreview_wait',
    trial                 = as.integer(trial),
    effort_task           = as.character(effort_task),
    effort_level          = str_to_sentence(effort_level),
    condition             = assigned_condition,
    preview_assignment    = as.character(preview_assignment),
    img                   = as.character(img),
    color_variant         = as.integer(color_variant),
    wait_seconds          = as.numeric(wait_seconds),
    wait_focus_lost       = as.integer(wait_focus_lost),
    wait_blur_ms          = as.numeric(wait_blur_ms),
    effort_trial_duration = as.numeric(effort_trial_duration),
    rating                = coalesce(as.numeric(liking_rating), as.numeric(value_rating)),
    rt                    = coalesce(as.numeric(liking_rt), as.numeric(value_rt))
  ) %>%
  distinct(id, trial, .keep_all = TRUE)

online_wait_memory <- memory_frame(online_wait_all, id_numeric = FALSE)

# Pseudonym map for the public prepared files: stable sequential codes over
# the sorted set of all observed session ids (raw files keep the Prolific hex
# ids; built pre-exclusion so codes never shift if exclusions change)
wait_id_lookup <- local({
  ids <- sort(union(unique(online_wait_data$id), unique(online_wait_memory$id)))
  setNames(sprintf('w%03d', seq_along(ids)), ids)
})

online_wait_exclude_no_post <- setdiff(
  unique(online_wait_data$id),
  online_wait_post_survey$id
)
online_wait_exclude_memory <- online_wait_memory %>%
  filter(dprime <= 0) %>%
  pull(id)
online_wait_exclude <- unique(c(
  online_wait_exclude_no_post,
  online_wait_exclude_memory
))

message(sprintf(
  'wait: %d collected; excluded %d (no post-survey %d, d\' <= 0 %d)',
  n_distinct(online_wait_data$id),
  length(online_wait_exclude),
  length(online_wait_exclude_no_post),
  length(online_wait_exclude_memory)
))

online_wait_memory <- online_wait_memory %>%
  filter(!id %in% online_wait_exclude)

online_wait_main <-
  online_wait_data %>%
  filter(!id %in% online_wait_exclude) %>%
  select(
    experiment_name, id, trial, condition, preview_assignment,
    effort_task, effort_level, wait_seconds,
    img, color_variant, rating, rt,
    wait_focus_lost, wait_blur_ms, effort_trial_duration
  ) %>%
  left_join(
    select(online_wait_post_survey, id, gender, age, nasa_tlx,
           belief_liking, belief_value, like_cognitive, like_physical, ncs_mean,
           opportunity_cost, opportunity_activity, opportunity_trials,
           opportunity_accomp, all_of(instrumentality_cols), all_of(tlx_component_cols)),
    by = 'id'
  )

online_wait_main <- mutate(online_wait_main, id = unname(wait_id_lookup[id]))
online_wait_memory <- mutate(online_wait_memory, id = unname(wait_id_lookup[id]))
stopifnot(!anyNA(online_wait_main$id), !anyNA(online_wait_memory$id))

stopifnot(!anyDuplicated(online_wait_main[, c('id', 'trial')]))
write_prepared(online_wait_main, 'wait', 'main.csv')
write_prepared(online_wait_memory, 'wait', 'memory.csv')
