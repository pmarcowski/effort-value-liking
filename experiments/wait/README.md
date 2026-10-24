# Waiting (Experiment S6)

Internal name `ratingclicks2_cognitive_nopreview_wait`, the `experiment_name` carried in
the task and survey data. Pure-**wait** (temporal-cost) variant. Effort is eliminated entirely: instead of
a cognitive task, each trial is a passive **wait** of varying duration (the
"temporal cost"), after which the participant rates one object. This isolates
temporal cost from effort and from the preview/target perceptual gap, testing
whether the price↑ / liking-flat dissociation seen in the effort studies is
driven by how long you wait rather than by effort or the preview gap.

Built with jsPsych 7 + Pavlovia. Daisy-chained
**Prolific → Pavlovia → Qualtrics → Prolific**.

## Design

| Factor | Type | Levels |
|--------|------|--------|
| **Wait duration** (temporal cost) | within | `low` / `medium` / `high` = **10 / 20 / 40 s** (ratio 2×) |
| **Preview** | between | `preview` (object shown `PREVIEW_MS` at trial start, then hidden) vs `nopreview` (object first seen at rating) |
| **Rating type** | between | `Value` (price) vs `Liking` — one type per participant |

18 object trials, balanced 6/6/6 across the three wait levels (shuffled blocks,
no back-to-back repeats). The wait durations bracket the pooled median
time-on-task observed in the cognitive effort studies (≈14/21/31 s), set to a
clean doubling (10 / 20 / 40 s) for a 2× dose-response. Change them in one place:
`CONFIG.WAIT_LEVELS`.

## Folder layout

Place these files in your Pavlovia project root:

- `index.html`
- `main.js`
- `jspsych/`
- `lib/` (must contain `jspsych-7-pavlovia-2022.1.1.js`)
- `stimuli/` with Brady et al. image strips `s_obj1.jpg` through `s_obj540.jpg`
  (the code crops one of 12 color exemplars from each strip at runtime)

## Between-subjects assignments

Two independent between-subjects assignments are computed at session start from
the participant-id hash (`pHash`). No quotas or cell-counting — purely stochastic
per-participant assignment, approximately balanced across the four
`preview × rating` cells. A participant who returns under the same id always gets
the same cell (deterministic hashing).

| Assignment | URL override | Random fallback | Stored in data as |
|------------|--------------|-----------------|-------------------|
| Rating type | `?condition=value\|liking` | `pHash % 2` (bit 0) | `assigned_condition` |
| Preview | `?preview=preview\|nopreview` | `Math.floor(pHash / 2) % 2` (bit 1) | `preview_assignment` |

## Per-trial flow

**preview condition**

1. Fixation cross (500 ms)
2. **Object shown** centred for `PREVIEW_MS = 4000` ms
3. Fixation cross held for the trial's **wait** (10 / 20 / 40 s) — caption
   "Waiting to access the object…", **no timer**
4. Rating screen: object reappears for `RATING_IMG_VISIBLE_MS = 4000` ms then
   fades; one rating (value or liking)
5. Inter-trial spacer: a 3 s sham "Loading next trial…" loading bar (`LOADING_MS`)

**nopreview condition**

1. Fixation cross (500 ms)
2. Fixation cross held for the **wait** (10 / 20 / 40 s) — object not shown
3. Rating screen: object appears for the first time (4 s then fades); one rating
4. Inter-trial spacer: a 3 s sham loading bar

The wait (temporal cost) is identical across preview conditions; `preview` only
adds the initial 4 s exposure. There is **no countdown/timer anywhere** — the
wait shows only a fixation cross and a "Waiting to access the object" caption with
animated dots (an indeterminate activity cue that conveys no remaining time, so
it reassures without inviting clock-watching). The 15 s rest of the effort
studies is replaced by a brief 3 s determinate loading bar between trials.

### Passive attention QA

Because the wait has no task and no timer, a participant could disengage (e.g.
switch tabs) undetected. Each wait records, unobtrusively, whether the
tab/window lost focus: `wait_focus_lost` (count) and `wait_blur_ms` (total ms
unfocused). They are analyzed as a compliance measure and never exclude or
reject a participant.

## Rating

One slider per trial (between-subjects: value or liking). 1–5 continuous scale
(value scale carries dollar ticks $100–$500; liking carries "really don't like /
really like" anchors).

## Memory check

There are no practice trials. The session ends with a 10-item recognition grid
(5 seen + 5 foils), used as the attention/exclusion filter (d'). In `nopreview`
the object is seen once, for about 4 s at rating; in `preview` it is seen twice.

## Configuration

Edit `CONFIG` at the top of `main.js`.

| Key | Default | Purpose |
|-----|---------|---------|
| `DEBUG` | `false` | Local CSV save, skip Pavlovia (also set via `?debug=1`) |
| `ACTIVE_STIMULI_POOL` | `'high_value'` | Active curated object set (`high_value` or `low_value`) |
| `N_STIMULI` | `18` | Stimuli per participant (balanced 6 × 3 levels) |
| `WAIT_LEVELS` | `{low:10, medium:20, high:40}` | Wait duration (seconds) per level |
| `PREVIEW_MS` | `4000` | Object preview window at trial start (preview condition) |
| `RATING_IMG_VISIBLE_MS` | `4000` | Rating-screen image fade window |
| `LOADING_MS` | `3000` | Inter-trial sham "Loading next trial" loading bar (ms) |
| `SCALE_MIN` / `SCALE_MAX` / `SCALE_STEP` | `1` / `5` / `0` | Rating slider (continuous when step = 0) |
| `RATING_DESIGN` | `'between'` | `'between'` (one rating type per participant) or `'within'` (both) |
| `RECRUITMENT` | `'prolific'` | Recruitment platform label |
| `QUALTRICS_URL` | `'https://...'` | Survey endpoint after the experiment |

## URL parameters

| Param | Effect |
|-------|--------|
| `PROLIFIC_PID`, `STUDY_ID`, `SESSION_ID` | Forwarded from Prolific via Pavlovia, then on to Qualtrics (data linkage) |
| `condition=value` or `condition=liking` | Override the random rating-type assignment |
| `preview=preview` or `preview=nopreview` | Override the random preview assignment |
| `debug=1` | Skip Pavlovia init/finish; download CSV locally on completion |

## Recruitment daisy chain (Prolific → Pavlovia → Qualtrics → Prolific)

The JS only captures the Prolific identifiers and forwards them to Qualtrics. The
return hop to Prolific is configured **in Qualtrics**, not in the code.

1. **Prolific study URL** → Pavlovia with
   `?PROLIFIC_PID={{%PROLIFIC_PID%}}&STUDY_ID={{%STUDY_ID%}}&SESSION_ID={{%SESSION_ID%}}`.
2. **Pavlovia** sets `participantId = PROLIFIC_PID`, runs, then redirects to
   `QUALTRICS_URL` forwarding `PROLIFIC_PID`, `STUDY_ID`, `SESSION_ID`,
   `experiment_name`, `id` (see `buildRedirectURL`).
3. **Qualtrics** (the study's own copy of the shared survey): capture
   `PROLIFIC_PID` / `STUDY_ID` / `SESSION_ID` as embedded data
   (for data linkage), and set the end-of-survey redirect to the **fixed**
   `https://app.prolific.com/submissions/complete?cc=<COMPLETION_CODE>` — one
   completion code per study, from Prolific. `SESSION_ID` is not needed in the
   redirect.

## Debug recipes

```
?debug=1                                       # random preview + random condition
?debug=1&preview=preview&condition=value       # specific cell: preview / value
?debug=1&preview=nopreview&condition=liking    # specific cell: nopreview / liking
?debug=1&id=test01                             # deterministic assignment for a fixed id
```

Local testing:

```bash
cd experiments/wait
python -m http.server 8000
# then open http://localhost:8000?debug=1
```

## Saved data fields

One `trial_summary` row per object trial.

**Session-level** (added by `jsPsych.data.addProperties`, present on every row):
`id`, `experiment_name`, `recruitment` (`prolific`), `prolific_pid`, `study_id`,
`session_id`, `date`, `effort_task` (`wait`), `preview_assignment`
(`preview` | `nopreview`), `n_trials` (18), `active_stimuli_pool`,
`rating_design`, `assigned_condition` (`Value` | `Liking` | `both`).

**Per-trial**:
`trial`, `effort_level` (`low`/`medium`/`high`), `wait_seconds` (10/20/40 — the
temporal-cost IV), `effort_trial_duration` (actual elapsed wait, seconds ≈
target), `wait_focus_lost`, `wait_blur_ms` (attention QA), `img` (`s_obj<N>`),
`color_variant`, `value_rating` / `value_rt` and/or `liking_rating` / `liking_rt`
(populated per `assigned_condition`; the unused one is NA), `scale_order`
(`within` design only).

**Memory check** (one row, `trial_id = 'memory_check'`):
`mem_objects_shown`, `mem_selected`, `mem_hits`, `mem_false_alarms`,
`mem_correct_rejections`, `mem_misses`, `mem_rt`.

## Data pipeline

The `## Waiting (Experiment S6) ----` block of `R/prepare_data.R` reads `data/raw/`
and the study's own survey export (`data/raw/post_survey_wait.csv`), keeps
`trial_summary` rows, resolves each id to its earliest complete session, computes
the memory-check d', excludes participants without a completed survey or with
d' ≤ 0, pseudonymises the Prolific ids, and writes `data/main.csv` and
`data/memory.csv`. The wait columns (`wait_seconds`, `preview_assignment`,
`wait_focus_lost`, `wait_blur_ms`) carry through; there are no cognitive-task fields.

## Randomisation

- **Wait-level sequencing** — `buildEffortSequence(n)` produces a balanced
  sequence (each block of three is a Fisher–Yates shuffle of the three levels)
  with a fix-up preventing back-to-back repeats at block boundaries. With
  `N_STIMULI = 18` this yields exactly 6 trials per wait level.
- **Stimulus colour** — `colorVariant(objIdx, pHash)` deterministically hashes
  participant + object index modulo 12 to pick one of the Brady colour variants.
- **Rating-type assignment** — `pHash % 2` (bit 0).
- **Preview assignment** — `Math.floor(pHash / 2) % 2` (bit 1).
