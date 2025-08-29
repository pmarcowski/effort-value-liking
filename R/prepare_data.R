# Analysis for Effort and Liking Study: Data Preparation
# Prepare analysis-ready datasets from raw inputs; outputs saved to data/prepared

library(tidyverse)

# Define NASA-TLX Components

## Base NASA-TLX components
nasa_tlx_base_components <- c('Mental', 'Physical', 'Temporal',
                              'Performance', 'Effort', 'Frustration')

## Custom NASA-TLX components
nasa_tlx_custom_components <- c('Meaning', 'Pleasure', 'Efficient', 'Depend')

# Core Experiments ----

## Core 1 ----

# Load and preprocess core 1 experiment data
core1_demographics_data <- read_csv('data/raw/core/exp1-demographics.csv')

# NASA-TLX data for core 1
core1_nasa_tlx_raw_data <-
  read_csv('data/raw/core/exp1-TLX.csv') %>%
  dplyr::select(subjID, scale_type, raw_rating, tally) %>%
  mutate(adjusted_rating = raw_rating * tally)

# Task frustration score
core1_nasa_tlx_frustration_scores <-
  core1_nasa_tlx_raw_data %>%
  filter(scale_type %in% c('Frustration')) %>%
  select(-scale_type, -tally) %>%
  rename(
    'frustration_raw' = 'raw_rating',
    'frustration_adjusted' = 'adjusted_rating'
  )

# Overall TLX score
core1_nasa_tlx_total_scores <-
  core1_nasa_tlx_raw_data %>%
  group_by(subjID) %>%
  summarise(
    raw_overall = sum(raw_rating),
    adjusted_overall = sum(adjusted_rating) / 15
  ) %>%
  ungroup()

# Performance data
core1_performance_data <- read_csv('data/raw/core/exp1-perf.csv')

# Post-experiment data
core1_post_experiment_data <- read_csv('data/raw/core/exp1-post.csv') %>%
  filter(subjID %in% core1_demographics_data$subjID) %>%
  select(-fitness)

# Main experiment data
core1_main_data <-
  read_csv('data/raw/core/exp1-main.csv') %>%
  left_join(core1_demographics_data) %>%
  left_join(core1_nasa_tlx_frustration_scores) %>%
  left_join(core1_nasa_tlx_total_scores) %>%
  left_join(core1_performance_data) %>%
  left_join(core1_post_experiment_data) %>%
  mutate(
    across(c(sex, condition, effort_level), str_to_sentence),
    across(c(rel_liking, rel_value, rel_wanting), recode, `2` = 'Less', `3` = 'Neither', `1` = 'More')
  )

## Core 2 ----

# Load and preprocess core 2 experiment data
core2_data <-
  read_csv('data/raw/core/exp2-data.csv') %>%
  left_join(select(
    read_csv('data/raw/core/exp2-coded.csv'),
    uniqueid, DirExp, strat2, DirStrat
  ),
  by = 'uniqueid'
  ) %>%
  select(
    where(~ !all(is.na(.))),                  
    any_of(c("experiment_name","experiment_id"))
  ) %>%
  mutate(
    type = ifelse(Rating == 'Liking_Rating', 'Liking', 'Value'),
    experience = ifelse(Exp == 'Experience', 'Experience', 'No Experience'),
    difmost = element == 5 | element == 6,
    effort_level = factor(diff, levels = c(.9, .5, .2), labels = c('Low', 'Medium', 'High')),
    subjID = as.factor(as.numeric(as.factor(uniqueid)))
  ) %>%
  select(subjID, type, experience, current_trial, firstsec, effort_level, numpressess, rt, value) %>%
  filter(firstsec == 0)

# Online Experiments ----

## Effort Survey Post 1 ----

# Load Qualtrics post-survey, compute TLX scores, and keep first response per id
online_post_survey <-
  data.table::fread('data/raw/qualtrics/effort-liking-post.csv') %>%
  slice(3:n()) %>%
  filter(!id %in% 'id' & !str_detect(id, 'expInfo')) %>%
  mutate(
    across(contains('nasa_tlx_'), as.numeric),
    across(contains('nasa_tlx_'), ~floor(scales::rescale(.x, c(0, 100)))),
    nasa_tlx = rowSums(across(all_of(paste0('nasa_tlx_', 1:6)))),
    nasa_tlx_aff = rowSums(across(contains('nasa_tlx_'))),
    id = as.numeric(id)
  ) %>%
  select(experiment_name, id, gender, age, nasa_tlx, nasa_tlx_aff) %>%
  group_by(experiment_name, id) %>%
  slice(1) %>%
  ungroup()

## Rating Clicks Data ----

online_ratingclicks_files <- fs::dir_ls('data/raw/online/ratingclicks', type = 'file', regexp = '.csv')

# Combine rating clicks files, harmonize rating/rt, and join post-survey
online_ratingclicks_data <-
  tibble(file_name = online_ratingclicks_files) %>%
  mutate(file_content = map(file_name, data.table::fread)) %>%
  unnest(cols = c(file_content)) %>%
  mutate(across(where(is.character), ~na_if(.x, ''))) %>%
  group_by(id) %>%
  mutate(
    rt = coalesce(liking_rt, value_rt),
    rating = coalesce(liking_rating, value_rating),
    scale_first = scale_order,
    across(c(effort_level, scale_type), str_to_sentence)
  ) %>%
  fill(trial, effort_level, effort_time, .direction = 'down') %>%
  ungroup() %>%
  select(experiment_name, id, trial, effort_level, effort_time, scale_first, scale_type, rt, rating) %>%
  drop_na() %>%
  left_join(select(online_post_survey, id, gender, age, nasa_tlx), by = "id") %>%
  drop_na()

## Taskload ----

online_taskloadclicks_files <- fs::dir_ls('data/raw/online/taskloadclicks', type = 'file', regexp = '.csv')

# Combine taskload clicks files, reconstruct trial metadata, and join post-survey
online_taskloadclicks_data <-
  tibble(file_name = online_taskloadclicks_files) %>%
  mutate(file_content = map(file_name, data.table::fread)) %>%
  unnest(cols = c(file_content)) %>%
  mutate(
    across(where(is.character), ~na_if(.x, '')),
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
  left_join(select(online_post_survey, id, gender, age, nasa_tlx), by = "id") %>%
  drop_na() %>%
  group_by(id) %>%
  arrange(trial, .by_group = TRUE) %>%
  slice_head(n = 126) %>%
  ungroup()

## Observe ----

online_observe_files <- fs::dir_ls('data/raw/online/observe', type = 'file', regexp = '.csv')

# Combine observe files, derive answer/accuracy, limit to design length, and join post-survey
online_observe_data <-
  tibble(file_name = online_observe_files) %>%
  mutate(file_content = map(file_name, data.table::fread)) %>%
  unnest(cols = c(file_content)) %>%
  mutate(
    across(where(is.character), ~na_if(.x, '')),
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
  left_join(select(online_post_survey, id, gender, age, nasa_tlx), by = "id") %>%
  drop_na() %>%
  group_by(id) %>%
  arrange(trial, .by_group = TRUE) %>%
  slice_head(n = 18) %>%
  ungroup()

## Participate/Observe ----

# Combine lab perform and online observe datasets with aligned columns
supplementary_participate_observe_data <-
  bind_rows(
    cbind(perspective = 'Perform', select(core1_main_data, id = subjID, rt = ratingRT, gender = sex, nasa_tlx = raw_overall, effort_steps = effort_trials, everything())),
    cbind(perspective = 'Observe', mutate(online_observe_data, age = as.numeric(age)))
  ) %>%
  select(
    -experiment_id,
    -steps_correct,
    -starts_with("frustration_"),
    -starts_with("rel_"),
    -effort_time
  )

## Effort Survey Post 2 ----

online_effort_post_survey2 <-
  data.table::fread('data/raw/qualtrics/effort-liking-post2.csv') %>%
  select(
    experiment_name, id, gender, age,
    belief_liking, belief_value
  ) %>%
  slice(-c(1, 2)) %>%
  mutate(
    id = as.numeric(id),
    age = as.numeric(age),
    belief_liking = case_when(
      str_detect(belief_liking, "High effort makes me like things more") ~ 'More',
      str_detect(belief_liking, "High effort makes me like things less") ~ 'Less',
      str_detect(belief_liking, "Effort makes no difference in how I like things") ~ 'No Difference'
    ),
    belief_value = case_when(
      str_detect(belief_value, "High effort makes me assign higher financial value to things") ~ 'More',
      str_detect(belief_value, "High effort makes me assign lower financial value to things") ~ 'Less',
      str_detect(belief_value, "Effort makes no difference in how I assign financial value") ~ 'No Difference'
    )) %>%
  drop_na()

## Rating Clicks Within ----

online_ratingclicks_within_files <- fs::dir_ls('data/raw/online/ratingclicks_within', type = 'file', regexp = '.csv')

# Retain first run only for duplicate ids
ids <- sub('.*/([0-9]+)_.*', '\\1', online_ratingclicks_within_files)
timestamps <- sub('.*_(\\d{4}-\\d{2}-\\d{2}_\\d{2}h\\d{2}\\.\\d{2}\\.\\d{3}).*', '\\1', online_ratingclicks_within_files)
timestamps <- as.POSIXct(gsub('[h.]', ':', timestamps), format = '%Y-%m-%d_%H:%M:%S')
ordered_indices <- order(ids, timestamps)
unique_indices <- ordered_indices[!duplicated(ids[ordered_indices])]
online_ratingclicks_within_files <- online_ratingclicks_within_files[unique_indices]

# Read, clean, and align within-subject rating clicks files across runs
online_ratingclicks_within_all <-
  tibble(file_name = online_ratingclicks_within_files) %>%
  mutate(file_content = map(file_name, data.table::fread)) %>%
  unnest(cols = c(file_content)) %>%
  mutate(
    across(where(is.character), ~na_if(.x, ''))
  ) %>%
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
  mutate(
    attention_check = case_when(
      any(attention_check == 1) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  ungroup() %>%
  select(
    experiment_name, id, attention_check, block,
    trial, condition, effort_level, rating, rt
  )

# Collect exclusion ids for attention failures
online_ratingclicks_within_exclude_attention <- names(which(tapply(online_ratingclicks_within_all$attention_check,
                                          online_ratingclicks_within_all$id,
                                          function(x) !any(x == 1))))

online_ratingclicks_within_exclude_missing <- names(which(tapply(online_ratingclicks_within_all$rating,
                                         online_ratingclicks_within_all$id,
                                         function(x) any(is.na(x)))))

online_ratingclicks_within_exclude_no_post <- setdiff(online_ratingclicks_within_all$id, online_effort_post_survey2$id)

online_ratingclicks_within_exclude <- c(online_ratingclicks_within_exclude_attention, online_ratingclicks_within_exclude_missing, online_ratingclicks_within_exclude_no_post)
length(unique((online_ratingclicks_within_exclude)))

online_ratingclicks_within_filtered <- online_ratingclicks_within_all[!online_ratingclicks_within_all$id %in% online_ratingclicks_within_exclude, ]
online_ratingclicks_within_post <- online_effort_post_survey2[!online_effort_post_survey2$id %in% online_ratingclicks_within_exclude, ]

online_ratingclicks_within_taskload <-
  online_ratingclicks_within_filtered %>%
  filter(block == 'taskload') %>%
  select(
    experiment_name, id,
    effort_level, component = condition,
    rating, rt
  )

n_distinct(online_ratingclicks_within_taskload$id)

# Derive within-subject rating trend covariates
online_ratingclicks_within_covariates <- online_ratingclicks_within_taskload %>%
  select(id, effort_level, component, rating) %>%
  filter(component == 'Effort') %>%
  pivot_wider(names_from = effort_level, values_from = rating) %>%
  group_by(id) %>%
  summarize(
    profile = case_when(
      (Low <= Medium & Medium <= High) & (Low < High) ~ 'Increasing',
      (Low >= Medium & Medium >= High) & (Low > High) ~ 'Decreasing',
      TRUE ~ 'Mixed'
    ),
    score_diff = (Medium - Low) + (High - Medium),
    score_weighted = (0.5 * (Medium - Low) + 0.5 * (High - Medium)),
    score_slope = if (all(c("Low","Medium","High") %in% names(cur_data()))) {
      coef(lm(c(Low, Medium, High) ~ c(1, 2, 3)))[2]
    } else NA_real_
  )

count(online_ratingclicks_within_covariates, profile)

# Subset main experiment data
# Assemble main block and join beliefs and covariates for analysis
online_ratingclicks_within_main <-
  online_ratingclicks_within_filtered %>%
  filter(block == 'main') %>%
  select(
    experiment_name, id, trial, condition, effort_level, rating, rt
  ) %>%
  left_join(
    select(online_ratingclicks_within_post, id, gender, age, belief_liking, belief_value),
    by = "id"
  ) %>%
  left_join(online_ratingclicks_within_covariates, by = "id")

online_ratingclicks_within_taskload <- left_join(
  online_ratingclicks_within_taskload, online_ratingclicks_within_covariates, by = "id"
)

## Effort Survey Post 3 ----

online_effort_post_survey3 <-
  data.table::fread('data/raw/qualtrics/effort-liking-post3.csv') %>%
  select(
    experiment_name, id, gender, age,
    belief_liking, belief_value
  ) %>%
  slice(-c(1, 2)) %>%
  mutate(
    id = as.numeric(id),
    gender = case_when(
      gender == 1 ~ 'Female',
      gender == 2 ~ 'Male',
      gender == 3 ~ 'Other',
      TRUE ~ NA_character_
    ),
    age = as.numeric(age),
    across(matches('belief_'), ~ case_when(
      .x == 1 ~ 'More',
      .x == 2 ~ 'Less',
      .x == 3 ~ 'No Difference'
    ))
  ) %>%
  drop_na()

## Buy/Sell Between ----

# Assemble Buy/Sell (between) main table from raw files
supp_buysell_main <-
  tibble(
    file_name = fs::dir_ls(
      'data/raw/supplementary/buysell_between/',
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
      'data/raw/supplementary/buysell/perf/',
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
  left_join(unique(supp_buysell_main[, c('id', 'condition')])) %>%
  select(id, condition, trial = trialNo, effort_level = effortLevel, effort_steps)

supp_buysell_tlx_id <-
  tibble(
    file_name = fs::dir_ls(
      'data/raw/supplementary/buysell/tlx/',
      type = 'file', recurse = TRUE, regexp = '.csv'
    )
  ) %>%
  mutate(file_content = map(file_name, ~read_csv(file.path(.), col_names = FALSE))) %>%
  unnest(cols = c(file_content)) %>%
  group_by(file_name) %>%
  summarize(id = as.numeric(first(X2))) %>%
  ungroup() %>%
  filter(id %in% unique(supp_buysell_main$id)) %>%
  left_join(unique(supp_buysell_main[, c('id', 'condition')])) %>%
  select(id, condition)

supp_buysell_tlx_data <-
  tibble(
    file_name = fs::dir_ls(
      'data/raw/supplementary/buysell/tlx/',
      type = 'file', recurse = TRUE,
      regexp = '.csv'
    )
  ) %>%
  mutate(file_content = map(file_name, ~read_csv(file.path(.), col_names = TRUE, skip = 3, n_max = 6))) %>%
  unnest(cols = c(file_content)) %>%
  select(file_name, component = `Scale name`, raw_rating = `Raw Rating`, Tally) %>%
  mutate(
    id = as.numeric(str_extract(file_name, '(?<=tlx/)\\d+(?=/Task)')),
    weight = as.numeric(Tally) / 15,
    adjusted_rating = as.numeric(raw_rating) * weight,
    adjusted_rating = if_else(adjusted_rating == Inf, 0, adjusted_rating)
  ) %>%
  filter(id %in% unique(supp_buysell_main$id)) %>%
  left_join(unique(supp_buysell_main[, c('id', 'condition')])) %>%
  select(id, condition, component, raw_rating, adjusted_rating)

supp_buysell_tlx <-
  supp_buysell_tlx_data %>%
  group_by(id, condition) %>%
  summarise(nasa_tlx = sum(raw_rating)) %>%
  ungroup()

# Join main, performance, and TLX into Buy/Sell (between) analysis table
supp_buysell_data <-
  supp_buysell_main %>%
  inner_join(supp_buysell_performance) %>%
  inner_join(supp_buysell_tlx) %>%
  select(experiment_name, id, condition, trial, effort_level, effort_time = time, rating, rt = scaleRT, nasa_tlx)

## Buy/Sell Within ----

online_buysell_within_files <- fs::dir_ls(
  'data/raw/online/buysell_within',
  type = 'file', regexp = '.csv'
)

# Retain first run only for duplicate ids
ids <- sub('.*/([0-9]+)_.*', '\\1', online_buysell_within_files)
timestamps <- sub('.*_(\\d{4}-\\d{2}-\\d{2}_\\d{2}h\\d{2}\\.\\d{2}\\.\\d{3}).*', '\\1', online_buysell_within_files)
timestamps <- as.POSIXct(gsub('[h.]', ':', timestamps), format = '%Y-%m-%d_%H:%M:%S')
ordered_indices <- order(ids, timestamps)
unique_indices <- ordered_indices[!duplicated(ids[ordered_indices])]
online_buysell_within_files <- online_buysell_within_files[unique_indices]

# Read and combine individual data files
online_buysell_within_raw <-
  tibble(file_name = online_buysell_within_files) %>%
  mutate(file_content = purrr::map(file_name, data.table::fread)) %>%
  unnest(cols = c(file_content)) %>%
  mutate(
    across(where(is.character), ~na_if(.x, '')),
    condition = stringr::str_to_sentence(condition),
    effort_level = stringr::str_to_sentence(effort),
    component = stringr::str_to_sentence(component),
    experiment_name = "buysell_within",
    attention_check2 = as.integer(attention_check2)
  ) %>%
  group_by(id) %>%
  mutate(
    attention_check1 = as.integer(any(attention_check == 1, na.rm = TRUE))
  ) %>%
  fill(attention_check2, .direction = 'updown') %>%
  ungroup()

# Buyer/Seller order from trial sequence
order_by_id <- online_buysell_within_raw %>%
  filter(block == "main", condition %in% c("Buyer", "Seller")) %>%
  group_by(id, condition) %>%
  summarise(first_trial = min(trial, na.rm = TRUE), .groups = "drop") %>%
  arrange(id, first_trial) %>%
  group_by(id) %>%
  summarise(order = paste(condition, collapse = ""), .groups = "drop")

# Attention-check answers per id
attn_by_id <- online_buysell_within_raw %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    attention_check1 = max(attention_check1, na.rm = TRUE),
    attention_check2 = dplyr::first(attention_check2[!is.na(attention_check2)], default = NA_integer_),
    .groups = "drop"
  )

# Keep only rated trials and attach order
online_buysell_within_all <- online_buysell_within_raw %>%
  drop_na(rating) %>%
  left_join(order_by_id, by = "id") %>%
  select(
    experiment_name, id, attention_check1, attention_check2,
    block, trial, order, condition, effort_level, component, rating, rt
  )

# Exclude: answered inconsistently with role order
online_buysell_within_exclude_attention <- attn_by_id %>%
  left_join(order_by_id, by = "id") %>%
  mutate(
    expected = case_when(
      order == "SellerBuyer" ~ 1L,
      order == "BuyerSeller" ~ 2L,
      TRUE ~ NA_integer_
    ),
    should_exclude = attention_check1 == 1L & !is.na(attention_check2) & attention_check2 != expected
  ) %>%
  filter(should_exclude) %>%
  pull(id)

# Exclude: no post-survey
online_buysell_within_exclude_no_post <- setdiff(
  online_buysell_within_all$id,
  online_effort_post_survey3$id
)

online_buysell_within_exclude <- unique(c(
  online_buysell_within_exclude_attention,
  online_buysell_within_exclude_no_post
))

# Filter analysis datasets
online_buysell_within_filtered <- online_buysell_within_all %>%
  dplyr::filter(!id %in% online_buysell_within_exclude)

online_buysell_within_post <- online_effort_post_survey3 %>%
  dplyr::filter(!id %in% online_buysell_within_exclude)

online_buysell_within_main <-
  online_buysell_within_filtered %>%
  dplyr::filter(block == 'main') %>%
  dplyr::left_join(
    dplyr::select(online_buysell_within_post, id, gender, age, belief_liking, belief_value),
    by = "id"
  ) %>%
  dplyr::select(
    experiment_name, id, gender, age,
    trial, condition, effort_level, rating, rt,
    belief_liking, belief_value
  )

n_distinct(online_buysell_within_main$id)

# Supplementary Experiments ----

## Supplementary T (Task Test) ----

supp_task_demo <- read_csv('data/raw/supplementary/task-demo.csv')
supp_task_demo <- supp_task_demo %>% group_by(subjID) %>% slice(1) %>% ungroup()
supp_task_performance <- read_csv('data/raw/supplementary/task-perf.csv')

supplementary_task_data <-
  read_csv('data/raw/supplementary/task-main.csv') %>%
  left_join(supp_task_demo) %>%
  left_join(supp_task_performance) %>%
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
  select(-scale_text, -scale_type, -steps_req, -effort_trials, rating = scale_rating, rt = scale_RT)

## Supplementary 1 ----

supplementary_price_liking_raw_data <- read_csv('data/raw/supplementary/supp1.csv')
supplementary_price_liking_raw_data$Q170 <- supplementary_price_liking_raw_data$Q170 - 5
supplementary_price_liking_raw_data$priceFirst <- supplementary_price_liking_raw_data$`DO-BL-PriceFirst`
supplementary_price_liking_raw_data$likeFirst <- supplementary_price_liking_raw_data$`DO-BL-DefaultQuestionBlock`

supplementary_price_order_data <- supplementary_price_liking_raw_data %>%
  dplyr::select(ID, priceFirst) %>%
  separate(priceFirst, paste0('a', as.character(1:22)), sep = '\\|') %>%
  dplyr::select(-a1, -a2) %>%
  gather(key = 'order', value = 'ideograph', a3:a22) %>%
  separate(order, into = c('drop', 'order1'), 1, convert = TRUE) %>%
  dplyr::select(-drop)

supplementary_liking_order_data <- supplementary_price_liking_raw_data %>%
  dplyr::select(ID, likeFirst) %>%
  separate(likeFirst, paste0('a', as.character(1:22)), sep = '\\|') %>%
  dplyr::select(-a1, -a2) %>%
  gather(key = 'order', value = 'ideograph', a3:a22) %>%
  separate(order, into = c('drop', 'order1'), 1, convert = TRUE) %>%
  dplyr::select(-drop)

supplementary_price_responses <- supplementary_price_liking_raw_data %>%
  dplyr::select(ID, Q161:Q236) %>%
  gather(key = 'ideograph', value = 'resp', Q161:Q236)

supplementary_price_responses %<>% inner_join(supplementary_price_order_data, by = c('ID', 'ideograph')) %>%
  dplyr::arrange(ID, order1)

supplementary_price_responses$liking <- (supplementary_price_responses$order1 + 1) %% 2
supplementary_price_responses$qg <- (supplementary_price_responses$order1 + 1) %/% 2

supplementary_liking_responses <- supplementary_price_liking_raw_data %>%
  dplyr::select(ID, V1.1:Q95) %>%
  gather(key = 'ideograph', value = 'resp', V1.1:Q95)

supplementary_liking_responses %<>% inner_join(supplementary_liking_order_data, by = c('ID', 'ideograph')) %>% dplyr::arrange(ID, order1)
supplementary_liking_responses$liking <- (supplementary_liking_responses$order1) %% 2
supplementary_liking_responses$qg <- (supplementary_liking_responses$order1 + 1) %/% 2

supplementary_liking_responses$cond <- 'liking_first'
supplementary_price_responses$cond <- 'price_first'

supplementary_liking_responses <- supplementary_liking_responses %>% 
  dplyr::select(-ideograph, -order1) %>% 
  pivot_wider(names_from = liking, values_from = resp)

supplementary_price_responses  <- supplementary_price_responses %>% 
  dplyr::select(-ideograph, -order1) %>% 
  pivot_wider(names_from = liking, values_from = resp)

supplementary_price_liking_data <- rbind(supplementary_liking_responses, supplementary_price_responses)
supplementary_price_liking_data$Liking <- supplementary_price_liking_data$`1`
supplementary_price_liking_data$Price <- supplementary_price_liking_data$`0`
supplementary_price_liking_data <- select(supplementary_price_liking_data, -c(`0`, `1`), id = ID)
supplementary_price_liking_data$cond <- recode(supplementary_price_liking_data$cond, liking_first = 'Liking First', price_first = 'Price First')

## Supplementary 2 ----

supplementary_price_magnitude_data <-
  read_csv('data/raw/supplementary/supp2.csv', col_types = cols(.default = 'c')) %>%
  slice(-1) %>%
  select(id = SubID, magnitude = Values, val1:val5, matches('[VL]\\d\\.')) %>%
  mutate(across(val1:val5, str_extract, '\\d+')) %>%
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

# Save Prepared Data ----

write_csv(core1_main_data, 'data/prepared/core/core1.csv')
write_csv(core2_data, 'data/prepared/core/core2.csv')
write_csv(online_ratingclicks_data, 'data/prepared/online/ratingclicks.csv')
write_csv(online_taskloadclicks_data, 'data/prepared/online/taskloadclicks.csv')
write_csv(online_observe_data, 'data/prepared/online/observe.csv')
write_csv(supplementary_task_data, 'data/prepared/supplementary/tasktest.csv')
write_csv(supplementary_price_liking_data, 'data/prepared/supplementary/supp1.csv')
write_csv(supplementary_price_magnitude_data, 'data/prepared/supplementary/supp2.csv')
write_csv(supplementary_participate_observe_data, 'data/prepared/supplementary/participate_observe.csv')
write_csv(online_buysell_within_all, 'data/prepared/online/buysell_within.csv')
write_csv(online_buysell_within_main, 'data/prepared/online/buysell_within_main.csv')
write_csv(online_ratingclicks_within_post, 'data/prepared/online/ratingclicks_within_post.csv')
write_csv(online_ratingclicks_within_taskload, 'data/prepared/online/ratingclicks_within_taskload.csv')
write_csv(online_ratingclicks_within_main, 'data/prepared/online/ratingclicks_within_main.csv')
write_csv(supp_buysell_data, 'data/prepared/supplementary/buysell_between.csv')
