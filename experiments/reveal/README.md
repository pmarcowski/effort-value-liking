# Effort-Contingent Reveal (Experiments 4 and S7)

Internal name `ratingclicks2_physical_scrambled_reveal`, the `experiment_name` carried in
the task and survey data. Physical-effort variant in which the object's **perceptual reveal is made contingent on
the effort**. The participant presses `[SPACE]` to walk a stick figure toward a scrambled
object, then rates it on price or liking. The implementation supports progressive and
endpoint reveal schedules. The progressive collection was run through Prolific and the
endpoint collection through SONA. The paper reports the progressive collection as
Experiment 4 and the endpoint collection as Experiment S7, the post-effort target reveal.

Built with jsPsych 8 + Pavlovia. The task supports both the
**Prolific → Pavlovia → Qualtrics → Prolific** and
**SONA → Pavlovia → Qualtrics → SONA** chains (see
[Recruitment](#recruitment-daisy-chain)).

## Design

| Factor | Type | Levels |
|--------|------|--------|
| **Reveal schedule** | between | `progressive` (resolves as you walk) / `endpoint` (stays scrambled, resolves on arrival) |
| **Rating type** | between | `Value` (price) vs `Liking` — one per participant |
| **Effort** | within | `low` / `medium` / `high` = **30 / 100 / 270** key presses |

18 object trials, balanced 6/6/6 across effort levels (shuffled blocks, no back-to-back
repeats), preceded by one practice walk that is not rated. The reveal schedule can be pinned
to a single arm for a collection — see [Single-schedule collections](#single-schedule-collections).

**Progressive collection.** Run on Prolific with `FORCE_REVEAL: 'progressive'` and
`RECRUITMENT: 'prolific'`. The export contains 142 Pavlovia files; resolving repeat
participation to each ID's earliest complete session gives 131 task completers, every one with
`reveal_assignment = progressive` and `reveal_source = forced`. Of these, 25 have no matching
completed post-survey; among the other 106, 1 has d′ ≤ 0 and 3 have a summed walk time above
the schedule mean by more than 2.5 SD. The analytic sample is the remaining 102 (53 Liking,
49 Value).

**Endpoint collection.** Run through SONA with `FORCE_REVEAL: 'endpoint'` and
`RECRUITMENT: 'sona'`. The export contains 146 Pavlovia files; resolving repeat participation
gives 120 task completers. Of these, 19 have no matching completed post-survey; among the other
101, 2 have d′ ≤ 0 and 2 have a summed walk time above the schedule mean by more than 2.5 SD,
one participant meeting both. The analytic sample is the remaining 98 (50 Liking, 48 Value).

In both collections each analytic participant contributes their first complete exposure, and
prep asserts one recorded task configuration across every session.

**Effort dose.** The three levels deliver exactly 30 / 100 / 270 accepted presses: the press
count is primary and the figure's per-press displacement is derived from it. The other online
clicking studies in this repo delivered 30 / 95 / 213, so this study's `effort_trial_duration`
and effort slopes are not poolable with theirs.

## Folder layout

Place these in the Pavlovia project root:

- `index.html`
- `main.js`
- `jspsych/` — jsPsych 8 core + plugins
- `lib/` — must contain `jspsych-7-pavlovia-2022.1.1.js`
- `stimuli/` — Brady et al. (2013) strips `s_obj1.jpg` … `s_obj540.jpg`; the code crops one
  of 12 colour exemplars from each strip at runtime

`index.html` loads `main.js?v=N`. Bump `N` on every redeploy that changes `main.js`: the file
is cached, so a participant who has opened the study before otherwise runs the previous
version — with no visible sign that they did.

## Between-subjects assignment

Both factors come from independent bits of the participant-id hash (`pHash`), so a
participant returning under the same id always lands in the same cell. No quotas.

| Assignment | Random source | URL override (debug only) | Stored as |
|---|---|---|---|
| Rating type | `pHash % 2` | `?condition=value\|liking` | `assigned_condition` |
| Reveal schedule | `Math.floor(pHash / 2) % 2` | `?reveal=progressive\|endpoint` | `reveal_assignment` |

Both overrides are **debug-only**: in a live session they parse to empty, so a participant
handed a crafted URL still gets their hash-assigned cell. (`?debug=1` does select a cell, but
it also skips Pavlovia entirely — no upload, no credit — so such a session never enters the
dataset.)

### Single-schedule collections

`CONFIG.FORCE_REVEAL` pins the reveal arm for a whole collection and, unlike `?reveal=`, applies in
production. Precedence is `?reveal=` (debug) → `FORCE_REVEAL` → hash; the outcome is recorded
per session as `reveal_source` (`debug_url` | `forced` | `hash`), so a pinned collection stays
distinguishable from a randomised one once the two are pooled, and a live pinned session logs
a console warning.

The two collections confound reveal schedule with collection, date, and recruitment platform:
progressive was collected through Prolific and endpoint through SONA. They are therefore
prepared and analyzed separately, with the walk-time cutoff computed within each schedule and no
reveal-schedule contrast. The rating type has no equivalent knob: pinning it would remove the
price control the dissociation is measured against.

## Per-trial flow

1. Fixation (500 ms)
2. **Walk.** The object sits scrambled in the target frame. Each accepted `[SPACE]` press
   advances the figure and the reveal by `1 / stepsRequired`.
   - `progressive` — tiles resolve toward the **true object**, which is fully clear on arrival
   - `endpoint` — the tiles hold their scramble; the object is unchanged and
     non-diagnostic for the whole walk, then resolves over a 400 ms ramp on arrival
3. The resolved object is held for `COLLECTED_MSG_MS` (2 s) — timed from when the reveal
   finishes settling, so both arms get the same fully-resolved exposure
4. Fixation, then the rating screen: the object remains visible until the participant
   submits or `RATING_IMG_VISIBLE_MS` (4 s) elapses, when it fades; one slider rating
5. Fixation, then a 15 s rest with countdown

Clear-object exposure is 2 s fixed after arrival plus up to 4 s on the rating screen.
The walk and rating response have no completion time limit.

### The reveal renderer

The object is an N×N grid of tiles. Tile *position* `i` shows object content `perm[i]`, so a
permutation with every tile displaced is a block scramble. Each tile stacks two crops — a
source below and a target above — and only the target's opacity is written:

```
opacity_i(p) = clamp((p - t_i) / REVEAL_WINDOW, 0, 1)
```

with thresholds `t_i` spread over `[0, 1 - REVEAL_WINDOW]` and `p` the fraction of the walk
completed. Every tile is therefore fully hidden at `p = 0` and fully revealed at `p = 1`, and
about `REVEAL_WINDOW / spacing` tiles (14 at the defaults) are mid-fade at any moment — so
change is continuous rather than a sequence of discrete pops, at every effort level.

In the progressive arm, total change is fixed at one image's worth however it is spread, so a
single press moves each live tile by ~27.8% of its fade at low effort and ~3.1% at high — the
reveal is continuous at every dose, but a press is not equally perceptible across doses, and
cannot be.

One renderer serves both arms. The tiles always cross-fade from the scramble to the true
object; the arms differ only in what drives `p`:

| arm | `p` during the walk | `p` on arrival |
|---|---|---|
| `progressive` | `stepsTaken / stepsRequired` | already 1 |
| `endpoint` | held at 0 — the scramble is unchanged | ramped 0 → 1 over `REVEAL_RAMP_MS` |

So the endpoint arm receives exactly the same cross-fade, compressed into 400 ms at the
target instead of spread across the walk.

The scramble carries a Chebyshev displacement floor of `REVEAL_MIN_DISPLACEMENT` tiles from
the identity: a uniform permutation has one fixed point on average, and a fixed tile would
show true content from `p = 0`. The sampler shuffles once and repairs violating tiles by
swapping them with a random partner, a median of 9 swaps. Rejection sampling cannot serve
here — on a 10×10 grid whole-permutation acceptance is ~0.00025, so a bounded retry loop
essentially always fails.

Permutations and thresholds are seeded per participant × object, so the exact stimulus is
reproducible from the logged `reveal_seed`, and are drawn per trial: one arrangement reused
across 18 trials would be partly learnable, which matters most in the endpoint arm, where the
same scramble is on screen for the whole walk.

### Key-press handling

Presses are counted on `keydown` with `e.repeat` ignored (so holding the key does nothing)
and a wall-clock debounce of `DEBOUNCE_MS` (183 ms). Two properties are load-bearing: a
rejected press does not restart the clock, or fast pressing stalls progress indefinitely; and
the gate is wall-clock rather than frame-counted, or the achievable press rate becomes
`refresh / 11` and the physical cost of a fixed dose depends on the participant's monitor.
`DEBOUNCE_MS` must be at least `REVEAL_TRANSITION_MS` (150 ms) or the reveal lags the walk;
the validator enforces it. Rejected presses are counted in `presses_rejected`, which is
descriptive only.

## Rating

One slider per trial (between-subjects: Value or Liking), 1–5 continuous. The value scale
carries dollar ticks $100–$500; the liking scale carries "really don't like it / really like
it" anchors. The OK button enables only after the participant moves the slider.

## Memory check

The session ends with a 10-item recognition grid (5 seen + 5 unseen foils). The practice
object is drawn from outside both the trial whitelist and the foil pool, so it never enters
the check.

In both schedules a participant at chance (d′ ≤ 0) is excluded. Because reveal schedule is
confounded with collection and recruitment platform, the analyses do not compare schedules.

## Configuration

Edit `CONFIG` at the top of `main.js`.

| Key | Default | Purpose |
|-----|---------|---------|
| `DEBUG` | `false` | Skip Pavlovia, save CSV locally (also via `?debug=1`) |
| `ACTIVE_STIMULI_POOL` | `'high_value'` | Curated object set (`high_value` or `low_value`) |
| `N_STIMULI` | `18` | Trials per participant (balanced 6 × 3 levels) |
| `EFFORT_STEPS` | `{low:30, medium:100, high:270}` | Required key presses per level |
| `DEBOUNCE_MS` | `183` | Minimum interval between accepted presses |
| `REVEAL_GRID_N` | `10` | Tiles per side (10 → 100 tiles) |
| `REVEAL_WINDOW` | `0.12` | Width of a tile's fade, in units of walk progress |
| `REVEAL_MIN_DISPLACEMENT` | `2` | Chebyshev floor on scramble displacement |
| `REVEAL_RAMP_MS` | `400` | Endpoint arm: duration of the arrival reveal |
| `RATING_IMG_VISIBLE_MS` | `4000` | Rating-screen image fade window |
| `COLLECTED_MSG_MS` | `2000` | Fully-resolved viewing after the reveal completes |
| `REST_MS` | `15000` | Rest between trials |
| `PRACTICE_OBJ_INDEX` | `72` | Practice object; must be outside the trial and foil pools |
| `PRACTICE_EFFORT_LEVEL` | `'low'` | Effort level of the practice walk (30 presses) |
| `SCALE_MIN` / `SCALE_MAX` / `SCALE_STEP` | `1` / `5` / `0` | Slider (continuous when step = 0) |
| `RATING_DESIGN` | `'between'` | `'between'` (one rating type) or `'within'` (both) |
| `FORCE_REVEAL` | `'endpoint'` | Pins the reveal arm for a live collection; `null` randomises from the hash |
| `RECRUITMENT` | `'prolific'` | `'prolific'` or `'sona'` — selects params and survey |
| `QUALTRICS_URLS` | Prolific set, SONA empty | Survey per platform; the selected one must be non-empty |

The configuration is validated at load. A bad value aborts with an actionable message before
anything renders rather than failing mid-session — including an empty `QUALTRICS_URLS` entry
for the selected platform; missing Prolific `PROLIFIC_PID` / `STUDY_ID` / `SESSION_ID` or SONA
`id` / `experiment_id` / `credit_token` parameters; and a `REVEAL_MIN_DISPLACEMENT` no
permutation could satisfy (the centre tile cannot move further than
`floor((REVEAL_GRID_N - 1) / 2)`).

## URL parameters

| Param | Effect |
|-------|--------|
| `PROLIFIC_PID`, `STUDY_ID`, `SESSION_ID` | Captured and forwarded to Qualtrics (Prolific chain) |
| `id`, `experiment_id`, `credit_token` | Captured and forwarded to Qualtrics (SONA chain) |
| `debug=1` | Skip Pavlovia init/finish; download the CSV locally on completion |
| `condition=value\|liking` | Override the rating-type assignment — **debug only** |
| `reveal=progressive\|endpoint` | Override the reveal assignment — **debug only** |
| `turbo=N` | Divide every effort level by N so a 270-press trial is testable — **debug only** |

## Recruitment daisy chain

`CONFIG.RECRUITMENT` selects both the parameter set captured from the URL and the survey the
participant is forwarded to. The return hop is configured **in Qualtrics**, not here.

**Prolific.** In the Prolific study's **Data collection**, select **I'll use URL parameters**
and keep the default names `PROLIFIC_PID`, `STUDY_ID`, and `SESSION_ID`, which Prolific
appends to the Pavlovia run URL (with the older manual-template field, append
`?PROLIFIC_PID={{%PROLIFIC_PID%}}&STUDY_ID={{%STUDY_ID%}}&SESSION_ID={{%SESSION_ID%}}`
exactly once). Pavlovia sets `participantId = PROLIFIC_PID`, runs, then redirects to
`QUALTRICS_URLS.prolific` forwarding all three IDs plus `experiment_name` and `id`. The
survey's end-of-survey redirect returns the participant to Prolific with the study's own
completion code, `C1AQ970X` for the progressive collection.

**SONA.** SONA → Pavlovia with
`?experiment_id=<SONA_EXPERIMENT_ID>&credit_token=<SONA_CREDIT_TOKEN>&id=%SURVEY_CODE%`.
SONA replaces `%SURVEY_CODE%` for each participant. Pavlovia forwards `experiment_id`,
`credit_token`, `experiment_name`, and `id` to `QUALTRICS_URLS.sona`. The Qualtrics
end-of-survey redirect pipes those embedded fields back to SONA:

```
https://ucsd.sona-systems.com/webstudy_credit.aspx?experiment_id=${e://Field/experiment_id}&credit_token=${e://Field/credit_token}&survey_code=${e://Field/id}
```

SONA displays its completion URL with `survey_code=XXXX`; `XXXX` marks the value the external
study must replace. It is not a literal survey code.

### The shared survey

`SV_6J1fEpvNX3nMgYJ` is shared with the SONA studies in this repo. At the top of its Survey
Flow, declare these Embedded Data fields with the exact spelling shown:

```
PROLIFIC_PID
STUDY_ID
SESSION_ID
experiment_name
id
experiment_id
credit_token
```

After the survey blocks, give every study routed through this survey its own branch on
`experiment_name`, and a Prolific study also on its `STUDY_ID`, each ending in that study's own
redirect: the Prolific completion URL with the study's code, or the SONA credit URL above. Do
not replace the survey's ending wholesale and do not route merely on a non-empty
`PROLIFIC_PID`: either choice can credit the wrong study.

Sharing also pools the responses; `experiment_name` separates them at export.

Only the platform being run has a URL set in `QUALTRICS_URLS`; the other stays empty, and the
validator refuses to start on an empty one.

Before opening a study, test its survey branch directly with synthetic URL parameters, then run
one test submission through the complete chain and check the Pavlovia CSV, the Qualtrics
response, and the platform's completion status.

In `?debug=1` no Pavlovia node is pushed at all: no session is opened, so **no credit is
consumed on either platform** and no data is uploaded.

## Debug recipes

```
?debug=1                                          # random cell
?debug=1&reveal=progressive&condition=liking      # specific cell
?debug=1&reveal=endpoint&condition=value&turbo=10 # same, with a short walk
?debug=1&id=test01                                # deterministic cell for a fixed id
```

In debug a badge in the top-left shows the assigned arm and where it came from (`?reveal`,
`CONFIG.FORCE_REVEAL`, or `id hash`), the rating type, the step counts, and the raw query
string. Read the arm off the badge rather than off the animation: a badge reading `id hash`
when an override was passed means the parameter did not survive the URL, usually an
HTML-escaped `&amp;` making everything after the first parameter parse under the wrong name.
An override that is present but unrecognised aborts rather than falling back to the hash.

Local testing:

```bash
cd experiments/reveal
python -m http.server 8000
# open http://localhost:8000?debug=1
```

## Saved data fields

**Session-level** (on every row): `id`, `experiment_name`, `recruitment`, `prolific_pid`,
`study_id`, `session_id`, `experiment_id`, `date`, `effort_task` (`walk`),
`reveal_assignment` (`progressive` | `endpoint`), `reveal_source` (`hash` | `forced` |
`debug_url`), `assigned_condition` (`Value` | `Liking` | `both`), `rating_design`,
`active_stimuli_pool`, `n_trials`, `viewport_width`, `debug`.

`viewport_width` is recorded because the walk scene is a fixed 900 px block on a page that
does not scroll horizontally: below roughly 940 px the target frame is cropped, and the
memory grid needs 814 px. Check its distribution before analysis.

**Per trial** (`trial_id = 'trial_summary'`, 18 rows): `trial`, `effort_level`,
`steps_required`, `effort_trial_duration` (seconds from the moment the scene is painted and
keys go live to the final accepted press), `presses_rejected`, `walk_focus_lost`,
`walk_blur_ms`, `reveal_seed`, `stimulus_load_error`, `walk_error`, `img`, `color_variant`,
`value_rating` / `value_rt` and/or `liking_rating` / `liking_rt` (the unused pair is NA),
`scale_order`.

`walk_focus_lost` and `walk_blur_ms` count tab/window focus losses and their total duration
during the walk. The walk is self-paced and can exceed a minute, so a tab switch inflates
`effort_trial_duration` exactly like slow pressing — which matters because that column is
both a reported measure and the quantity `time_outlier_ids()` cuts on.

**Practice** (`trial_id = 'practice_summary'`, 1 row): the trial-summary walk fields except
`walk_focus_lost` and `walk_blur_ms`, plus `effort_level` and `img`. Prep drops these along
with the raw `walk` / `practice_walk` rows, since every branch filters on
`trial_id == 'trial_summary'`.

**Memory check** (`trial_id = 'memory_check'`, 1 row): `mem_objects_shown`, `mem_selected`,
`mem_hits`, `mem_false_alarms`, `mem_correct_rejections`, `mem_misses`, `mem_rt`.

## Data pipeline

Prep lives in the `## Effort-Contingent Reveal (Experiment 4) ----` block of `R/prepare_data.R`,
mirroring the waiting study's block: read `data/raw/`, keep
`trial_summary` rows, resolve each ID to its earliest complete session, and resolve surveys to
one response per study and participant ID from
`experiments/qualtrics/post_survey_combined.csv`, the union `R/build_qualtrics.R` writes from
the raw survey exports. Both reveal schedules use the same inclusion rule: a participant's
earliest complete task exposure enters when a matching completed survey is present, d′ is above
zero, and the summed walk time does not exceed the schedule mean by more than 2.5 SD. It then
writes the combined prepared files into `data/`:

| File | Contract |
|------|----------|
| `main.csv` | Analytic samples: 102 progressive + 98 endpoint participants, 3,600 trial rows |
| `main_complete.csv` | All 131 progressive + 120 endpoint earliest complete sessions, 4,518 trial rows, with inclusion and diagnostic fields |
| `memory.csv` | One memory-check row for each of the 200 analytic participants |

Five constraints it holds to:

- A failed stimulus decode does not erase completed physical effort. Its real
  `effort_trial_duration` remains in the summed walk time the exclusion reads, while its
  uninterpretable rating is held out of rating analyses. A walk that did not complete fails
  integrity checks.
- `effort_trial_duration` and `effort_task` are the consumable names — `time_outlier_ids()`
  groups on those; `effort_time` does not reach the exclusion pipeline.
- `id` stays **character** (Prolific hex PIDs or SONA survey codes) and is pseudonymised at
  write time to `r001`-style progressive or `e001`-style endpoint codes.
- `repeat_complete` identifies participants with more than one completed session; the earliest
  complete session supplies task ratings. `viewport_width` supports the narrow-viewport
  diagnostic without excluding those sessions from the primary sample.
- Inclusion requires a matching reveal survey the participant carried to the end (an abandoned
  response counts as no survey), d′ above zero, and a summed walk time within the cutoff
  computed for the schedule; repeat completion and viewport width remain diagnostics and do not
  alter either analytic sample.

Analysis lives in the `# Experiment 4: Effort-Contingent Reveal` section of `analysis/main.qmd`.
The progressive and endpoint collections are analyzed in separate subsections using the same model
and reporting style. No reveal-schedule contrast is estimated because schedule is confounded
with collection and recruitment platform.

## Randomisation

- **Effort sequencing** — `buildEffortSequence(n)` shuffles blocks of the three levels with a
  fix-up preventing back-to-back repeats at block boundaries; with `N_STIMULI = 18` this gives
  exactly 6 trials per level.
- **Object selection** — 18 objects drawn from the active whitelist, shuffled per participant,
  so object identity is counterbalanced against effort level between subjects.
- **Stimulus colour** — `colorVariant(objIdx, pHash)` picks one of the 12 Brady colour
  variants deterministically per participant × object.
- **Scramble** — seeded per participant × object; see the renderer section.
- **Cell assignment** — bits 0 and 1 of `pHash`.
