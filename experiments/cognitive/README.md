# Cognitive Effort (Experiment 5)

Internal name `ratingclicks2_cognitive`, the `experiment_name` carried in the task and
survey data. Object-collection effort/rating experiment built with jsPsych 7 + Pavlovia.
Each trial presents one Brady et al. object stimulus together with a fixed
number of mental operations the participant must perform to "collect" the
object; after collection the participant rates the object. Two effort task
variants — **cogletters** (alphabet-offset transcription) and **subtractn**
(serial subtraction) — are assigned between-subjects, as is the rating type
(value vs liking). Daisy-chained with SONA -> Pavlovia -> Qualtrics -> SONA.

## Folder layout

Place these files in your Pavlovia project root:

- `index.html`
- `main.js`
- `jspsych/`
- `lib/` (must contain `jspsych-7-pavlovia-2022.1.1.js`)
- `stimuli/` with Brady et al. image strips `s_obj1.jpg` through `s_obj540.jpg`
  (the code crops one of 12 color exemplars from each strip at runtime)

## Between-subjects assignments

Two independent between-subjects assignments are computed at session start:

| Assignment | URL override | Random fallback | Stored in data as |
|------------|--------------|-----------------|-------------------|
| Effort task | `?effort_task=cogletters\|subtractn` | `Math.floor(pHash / 2) % 2` (bit 1) | `effort_task` |
| Rating type | `?condition=value\|liking` | `pHash % 2` (bit 0) | `assigned_condition` |

The two assignments use different bits of the same hash so they are independent.
A participant who returns under the same `id` always gets the same task
and condition (deterministic hashing).

## Key behaviour

- **Effort definition (uniform across tasks)**: Three levels (`Low` / `Medium`
  / `High`) of magnitude **1 / 3 / 7**. For cogletters this is the alphabet
  offset (+1, +3, +7); for subtractn it is the subtraction step (-1, -3, -7).
  Each trial requires `OPERATIONS_PER_TRIAL = 5` correct operations (5 source
  letters or 5 correct subtractions). No time limit — the trial runs until
  completion; trial duration is tracked in `effort_trial_duration`.
- **Effort sequence**: 18 stimuli, balanced across the three levels (6 each)
  in shuffled blocks with no immediate repeats across block boundaries.
- **Image presentation**: The clear object stimulus is shown for the first
  `EFFORT_PREVIEW_MS = 4000` ms of each effort trial and then fades —
  leaving an empty bounding-box frame in its place for the remainder of the
  trial. Mirrors the brief-preview pattern of the clicking studies and
  forces the participant to hold the object in mind while collecting it.
- **Walk scene**: Mirrors the physical clicking-task layout — a stick
  figure walks along a 900-px ground line toward the goal frame on the
  right, advancing one step per completed operation. Practice trials hide
  the object and show a downward arrow at the figure's arrival spot
  instead.
- **Trial structure**:
  1. Fixation (500 ms)
  2. 5-second "Ready?" countdown showing the trial-specific rule
  3. Effort scene — walk scene on top (4-s object preview, then the image
     fades to its empty bounding frame); prompt + task display (letters
     strip or running code + input) centred below; figure advances per
     completed operation until all 5 are done (no cap)
  4. Fixation, then rating screen (object reappears for 4 s on the rating
     screen, then fades — forces a quick gestalt judgment), then 15-second
     rest
- **Rating**: One slider per trial (between-subjects: value or liking). Same
  1–5 continuous scale shape as the clicking studies (value scale carries dollar
  ticks $100–$500; liking scale carries "really don't like / really like"
  anchors).
- **Practice**: 3 practice trials — one per effort level — using the assigned
  task type and non-experiment objects (`PRACTICE_OBJ_INDICES = [72, 266, 425]`).
- **Memory check**: At the end of the session, a 10-item recognition grid
  (5 seen + 5 unseen foils from the active pool). Hits / false alarms / d'
  computed in `R/prepare_data.R`.

## Configuration

Edit `CONFIG` at the top of `main.js`.

| Key | Default | Purpose |
|-----|---------|---------|
| `DEBUG` | `false` | Local CSV save, skip Pavlovia (also set via `?debug=1`) |
| `ACTIVE_STIMULI_POOL` | `'high_value'` | Active curated object set (`high_value` or `low_value`) |
| `N_STIMULI` | `18` | Stimuli per participant (balanced 6 × 3 levels) |
| `EFFORT_LEVELS` | `{low:1, medium:3, high:7}` | Magnitude per level (sign applied per task) |
| `OPERATIONS_PER_TRIAL` | `5` | Stimuli (cogletters) or steps (subtractn) per object |
| `EFFORT_PREVIEW_MS` | `4000` | Object visibility at the start of each effort trial; then fades to an empty frame |
| `COGLETTERS.KEYS_PER_STIMULUS` | `2` | Letter + offset letter |
| `COGLETTERS.MAX_OFFSET` | `7` | Bounds the source alphabet (`a..s`) |
| `SUBTRACTN.FLASH_MS` | `220` | Neutral input-flash duration on wrong Enter |
| `SCALE_MIN` / `SCALE_MAX` / `SCALE_STEP` | `1` / `5` / `0` | Rating slider (continuous when step = 0) |
| `RATING_DESIGN` | `'between'` | `'within'` (both Value + Liking per trial) or `'between'` (one rating type per participant) |
| `RATING_IMG_VISIBLE_MS` | `4000` | Rating-screen image fade window (matches the effort-screen preview) |
| `REST_MS` | `15000` | Inter-trial rest period (ms) |
| `QUALTRICS_URL` | `'https://...'` | Daisy-chain endpoint after the experiment |

## URL parameters

| Param | Effect |
|-------|--------|
| `id`, `experiment_id`, `experiment_name`, `credit_token` | Forwarded from SONA via Pavlovia, then on to Qualtrics |
| `condition=value` or `condition=liking` | Override the random value/liking assignment |
| `effort_task=cogletters` or `effort_task=subtractn` | Override the random task assignment |
| `debug=1` | Skip Pavlovia init/finish; download CSV locally on completion |

## Debug recipes

```
?debug=1                                            # random task + random condition
?debug=1&effort_task=cogletters&condition=value     # specific arm: cogletters/value
?debug=1&effort_task=subtractn&condition=liking     # specific arm: subtractn/liking
?debug=1&id=test01                                  # deterministic random for fixed id
```

Local testing:

```bash
cd experiments/cognitive
python -m http.server 8000
# then open http://localhost:8000?debug=1
```

## Saved data fields

One `trial_summary` row per object trial with the following columns.

**Session-level** (added by `jsPsych.data.addProperties`, present on every row):
`id`, `experiment_name`, `experiment_id`, `date`, `effort_task`
(`cogletters` | `subtractn`), `n_trials` (18), `active_stimuli_pool`,
`rating_design` (`between` | `within`), `assigned_condition`
(`Value` | `Liking` | `both`).

**Common per-trial** (every trial, both arms):
`trial`, `effort_level` (`low`/`medium`/`high`), `magnitude` (1/3/7),
`img` (`s_obj<N>`), `color_variant`, `operations_completed` (always 5,
since the trial runs until completion), `operations_total` (5),
`effort_completed` (always 1, since no trial is capped),
`effort_trial_duration` (seconds, decimal — primary effort-cost measure),
`value_rating` / `value_rt` and/or `liking_rating` / `liking_rt`
(populated per `assigned_condition`; the unused one is NA),
`scale_order` (for the `within` design only).

**Cogletters arm only**:
`effort_source` (the 5-letter source string),
`effort_matches`, `effort_mismatches`,
`effort_keystrokes` — JSON-stringified array of
`{ t, key, expected, stimChar, subStep, match }` records, `t` in ms since
task start.

**Subtractn arm only**:
`effort_start_number` (4-digit starting code),
`effort_first_attempt_correct` (count of steps the participant got right
on the first attempt, 0–5),
`effort_total_attempts`, `effort_incorrect_attempts`,
`effort_steps` — JSON-stringified per-step record array including each
attempt's `value`, `dt_step_ms`, `dt_prev_ms`, `canonicalCorrect`, etc.

**Memory check** (one row, `trial_id = 'memory_check'`):
`mem_objects_shown`, `mem_selected`, `mem_hits`, `mem_false_alarms`,
`mem_correct_rejections`, `mem_misses`, `mem_rt`.

Practice trials are saved separately under `trial_id = 'practice_summary'`
with the same effort fields and are dropped from analysis by
`R/prepare_data.R`.

## Data pipeline

`R/prepare_data.R` reads `data/raw/`, extracts `trial_summary` rows, computes
the memory-check d' and the exclusions (no completed post-survey, d' ≤ 0, or
summed effort time above the task mean by more than 2.5 SD), then **splits on
`effort_task`** and writes two prepared CSVs:

- `data/cogletters.csv`
- `data/subtractn.csv`

Plus `data/memory.csv` for the recognition-test summary.

## Randomisation

- **Condition sequencing** — `buildEffortSequence(n)` produces a balanced
  sequence (each block of three is a Fisher–Yates shuffle of the three
  levels) with a fix-up that prevents back-to-back repeats at block
  boundaries. With `N_STIMULI = 18` this yields exactly 6 trials per
  effort level.
- **Stimulus colour** — `colorVariant(objIdx, pHash)` deterministically
  hashes the participant + object index modulo 12 to pick one of the
  Brady colour variants.
- **Task assignment** — `Math.floor(pHash / 2) % 2` (bit 1 of `pHash`).
- **Rating-type assignment** — `pHash % 2` (bit 0 of `pHash`).
- **Source letters / starting code** — `randomSource(length)` and
  `randomStart(n, steps)` use `Math.random()` per trial.
