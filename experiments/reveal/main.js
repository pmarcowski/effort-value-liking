/**
 * Effort & Rating Experiment — effort-contingent unscrambling (jsPsych 8 + Pavlovia)
 *
 * Physical-effort branch: the participant presses [SPACE] to walk a stick figure
 * toward a scrambled object, then rates it on price or liking. The renderer supports
 * two reveal schedules, collected in separate forced waves:
 *
 *   progressive — the object resolves toward its true form as the walk progresses
 *   endpoint    — the object remains scrambled throughout the walk and resolves
 *                 toward its true form only on arrival
 *
 * Effort varies within subjects: 30 / 100 / 270 required key presses.
 *
 * Stimuli: Brady et al. (2013) color-rotated object image strips (s_obj*.jpg).
 * Daisy chain: Prolific|SONA -> Pavlovia -> Qualtrics -> Prolific|SONA
 */

// ======================== CONFIGURATION ========================

var CONFIG = {
    DEBUG: false,

    // Stimuli — Brady et al. (2013) strips: s_obj{N}.jpg
    ACTIVE_STIMULI_POOL: 'high_value', // 'low_value' or 'high_value'
    STIMULI_POOLS: {
        low_value: {
            // Fixed 18 low-value everyday objects for trials
            whitelist: [
                3,   14,  15,  35,  60,  65,
                105, 120, 125, 150, 155, 170,
                200, 230, 240, 300, 310, 350
            ],
            // Additional low-value foils for memory check
            foils: [
                10, 30, 40, 55, 70, 80, 90, 100, 110, 115,
                180, 190, 290, 330, 340, 370, 390, 410, 450, 500
            ]
        },
        high_value: {
            // Fixed 18 higher-value objects (roughly $100-$500) for trials
            whitelist: [
                4,   6,   7,   8,   22,  31,
                34,  50,  54,  75,  77,  85,
                87,  88,  93,  131, 145, 163
            ],
            // Additional higher-value foils for memory check
            foils: [
                32,  79,  95,  122, 123, 142, 157, 160, 161, 164,
                185, 201, 203, 211, 214, 216, 224, 228, 229, 232
            ]
        }
    },
    N_STIMULI: 18,               // per participant; balanced across the 3 effort levels (6 each)
    N_COLOR_VARIANTS: 12,
    STRIP_WIDTH_PX: 1895,
    STRIP_HEIGHT_PX: 155,
    STIMULI_PATH: 'stimuli/',

    // ---- effort ----
    // Required key presses per level, delivered exactly: the press count is primary
    // and the figure's per-press displacement is derived from it, so the dose never
    // depends on how the pixel step rounds.
    EFFORT_LEVEL_KEYS: ['low', 'medium', 'high'],
    EFFORT_STEPS: { low: 30, medium: 100, high: 270 },
    DEBOUNCE_MS: 183,            // minimum interval between accepted presses

    // ---- reveal ----
    REVEAL_GRID_N: 10,           // tiles per side (10 -> 100 tiles)
    REVEAL_WINDOW: 0.12,         // width of a tile's fade, in units of walk progress
    REVEAL_MIN_DISPLACEMENT: 2,  // Chebyshev floor on every tile's scramble displacement
    REVEAL_RAMP_MS: 400,         // endpoint arm: duration of the arrival reveal

    // Timing (ms)
    FIXATION_MS: 500,
    RATING_IMG_VISIBLE_MS: 4000, // object visibility on the rating screen before it fades
    COLLECTED_MSG_MS: 2000,      // fully-resolved viewing after the reveal completes
    INTER_RATING_MS: 1000,       // pause between ratings (within design only)
    REST_MS: 15000,              // rest after ratings

    // Display (px)
    WALK_IMG_SIZE: 200,
    RATING_IMG_SIZE: 250,

    // Rating scale
    SCALE_MIN: 1,
    SCALE_MAX: 5,
    SCALE_STEP: 0,               // 0 = continuous (step 0.01), >0 = discrete
    SCALE_SHOW_TICKS: true,

    // Memory check
    N_MEMORY_OLD: 5,
    N_MEMORY_NEW: 5,

    // Practice — one walk in the participant's assigned reveal arm, no rating.
    // The object must sit outside both the trial whitelist and the foil pool so it
    // never enters the recognition check.
    PRACTICE_OBJ_INDEX: 72,
    PRACTICE_EFFORT_LEVEL: 'low',

    // Design
    RATING_DESIGN: 'between',    // 'within' (both Value + Liking) or 'between' (one per participant)

    // Pins the reveal arm for a whole wave; null randomises it from the participant
    // hash. Applies in production, unlike ?reveal=, and the outcome is recorded per
    // session as `reveal_source`. Two waves pinned to opposite arms confound the
    // reveal factor with wave. The rating type has no equivalent knob: pinning it
    // would remove the price control the dissociation is measured against.
    FORCE_REVEAL: 'endpoint',    // null | 'progressive' | 'endpoint'

    // Recruitment / redirect. RECRUITMENT selects both the parameter set captured
    // from the URL and the survey the participant is forwarded to; the return hop
    // to the recruitment platform is configured in Qualtrics, not here. Only the
    // platform actually being run has a survey URL set — the other stays empty and
    // the load-time validator refuses to start with an unset one.
    //
    // This survey is shared by Prolific and SONA studies in this line. Its Survey
    // Flow must branch on the exact study identifier before the platform-specific
    // End of Survey element; a single wholesale redirect miscredits another study.
    EXPERIMENT_NAME: 'ratingclicks2_physical_scrambled_reveal',
    RECRUITMENT: 'prolific',     // 'prolific' | 'sona'
    QUALTRICS_URLS: {
        prolific: 'https://ucsd.co1.qualtrics.com/jfe/form/SV_6J1fEpvNX3nMgYJ',
        sona: ''
    }
};

// Rendering constants. REVEAL_TRANSITION_MS is both the CSS transition applied to a
// tile's target layer and the settle wait before the post-reveal hold begins, so the
// two cannot drift apart.
var REVEAL_TRANSITION_MS = 150;
var REVEAL_TILE_BLEED_PX = 1;

// ======================== URL PARAMS ========================

var urlParams = new URLSearchParams(window.location.search);
if (urlParams.get('debug') === '1') CONFIG.DEBUG = true;

// Assignment overrides and the effort divisor are DEBUG-ONLY. A leaked URL must not
// let a participant choose their own cell, and turbo must never shorten a live run.
// Values are trimmed: a trailing space survives a copy-paste and would otherwise
// change the arm silently.
function debugParam(name) {
    if (!CONFIG.DEBUG) return '';
    return (urlParams.get(name) || '').trim().toLowerCase();
}

var conditionOverride = debugParam('condition');
var revealOverride    = debugParam('reveal');
var turboDivisor      = 1;
if (CONFIG.DEBUG) {
    var turboParam = parseInt(urlParams.get('turbo'), 10);
    if (turboParam > 1) turboDivisor = turboParam;
}

var PARTICIPANT = {
    // Prolific identifiers
    prolific_pid:    urlParams.get('PROLIFIC_PID') || '',
    study_id:        urlParams.get('STUDY_ID') || '',
    session_id:      urlParams.get('SESSION_ID') || '',
    // SONA identifiers
    experiment_id:   urlParams.get('experiment_id') || '',
    credit_token:    urlParams.get('credit_token') || '',
    // Pavlovia/data participant id: Prolific PID, else the SONA/generic ?id=
    id:              urlParams.get('PROLIFIC_PID') || urlParams.get('id') || '',
    // Routing and export identity are configuration, not participant input.
    experiment_name: CONFIG.EXPERIMENT_NAME
};

// ======================== STATE ========================

var pHash = 0;
var assignedCondition = '';
var assignedReveal    = '';
var revealSource      = '';       // 'hash' | 'forced' | 'debug_url'
var trialList = [];
var trialIndex = 0;
var scaleCounter = 0;
var scaleOrder = '';
var currentWalk = {};             // per-trial walk outcome, carried into trial_summary
var currentRatings = {};
var memoryCheckList = [];

// ======================== HELPERS — GENERAL ========================

/** Abort with a message the operator can act on, and show it rather than a blank page. */
function fail(msg) {
    var el = document.getElementById('jspsych-display');
    if (el) {
        el.innerHTML = '<div style="max-width:720px;margin:40px auto;color:#ff6b6b;' +
            'font-family:monospace;white-space:pre-wrap;line-height:1.5;">' +
            'CONFIGURATION ERROR\n\n' + msg + '</div>';
    }
    throw new Error('[' + CONFIG.EXPERIMENT_NAME + '] ' + msg);
}

function hashCode(str) {
    var h = 0;
    for (var i = 0; i < str.length; i++) {
        h = ((h << 5) - h) + str.charCodeAt(i);
        h |= 0;
    }
    return Math.abs(h);
}

function shuffle(arr) {
    var a = arr.slice();
    for (var i = a.length - 1; i > 0; i--) {
        var j = Math.floor(Math.random() * (i + 1));
        var tmp = a[i]; a[i] = a[j]; a[j] = tmp;
    }
    return a;
}

/** Seeded PRNG (linear congruential) — every deterministic draw in the reveal
 *  renderer comes from this one generator. */
function seededRandom(seed) {
    var s = Math.abs(seed) || 1;
    return function () {
        s = (s * 1664525 + 1013904223) & 0x7fffffff;
        return s / 0x80000000;
    };
}

/** Balanced effort sequence: blocks of k levels, each block a random permutation,
 *  no back-to-back repeats across block boundaries. */
function buildEffortSequence(n) {
    var levels = CONFIG.EFFORT_LEVEL_KEYS;
    var k = levels.length;
    var seq = [];
    var last = null;

    while (seq.length < n) {
        var block = shuffle(levels).slice(0, Math.min(k, n - seq.length));
        if (block[0] === last) {
            for (var i = 1; i < block.length; i++) {
                if (block[i] !== last) {
                    var tmp = block[0]; block[0] = block[i]; block[i] = tmp;
                    break;
                }
            }
            if (block[0] === last) {
                var others = levels.filter(function (l) { return l !== last; });
                block[0] = others[Math.floor(Math.random() * others.length)];
            }
        }
        seq = seq.concat(block);
        last = seq[seq.length - 1];
    }

    return seq;
}

/** Required presses for a level. `turbo` is a debug-only divisor so a 270-press
 *  trial can be exercised end to end; it is 1 in any live session. */
function stepsFor(level) {
    return Math.max(1, Math.ceil(CONFIG.EFFORT_STEPS[level] / turboDivisor));
}

function colorVariant(objIdx, hash) {
    return hashCode(String(hash) + '_' + String(objIdx)) % CONFIG.N_COLOR_VARIANTS;
}

/** CSS background properties clipping one color variant from a strip image. */
function stimBgCSS(objIdx, variant, size) {
    var scale = size / CONFIG.STRIP_HEIGHT_PX;
    var vw = CONFIG.STRIP_WIDTH_PX / CONFIG.N_COLOR_VARIANTS;
    return 'background-image:url(\'' + stimulusURL(objIdx) + '\');' +
        'background-size:' + (CONFIG.STRIP_WIDTH_PX * scale) + 'px ' + (CONFIG.STRIP_HEIGHT_PX * scale) + 'px;' +
        'background-position:-' + (variant * vw * scale) + 'px center;' +
        'background-repeat:no-repeat;';
}

function stimDiv(objIdx, variant, size) {
    return '<div style="width:' + size + 'px;height:' + size + 'px;overflow:hidden;' +
        'display:inline-block;' + stimBgCSS(objIdx, variant, size) + '"></div>';
}

function getActiveStimuliPool() {
    var pools = CONFIG.STIMULI_POOLS || {};
    if (CONFIG.ACTIVE_STIMULI_POOL && pools[CONFIG.ACTIVE_STIMULI_POOL]) {
        return pools[CONFIG.ACTIVE_STIMULI_POOL];
    }
    var keys = Object.keys(pools);
    if (keys.length > 0) return pools[keys[0]];
    return { whitelist: null, foils: null };
}

function stimulusURL(objIdx) {
    return CONFIG.STIMULI_PATH + 's_obj' + objIdx + '.jpg';
}

// ======================== HELPERS — REVEAL RENDERER ========================
//
// The object is drawn as an N x N grid of tiles. Tile *position* i shows the object
// content at index `perm[i]`, so a permutation with every tile displaced is a block
// scramble. Each tile holds two stacked crops — a source below and a target above —
// and only the target's opacity is written:
//
//     opacity_i(p) = clamp((p - t_i) / REVEAL_WINDOW, 0, 1)
//
// with thresholds t_i spread over [0, 1 - REVEAL_WINDOW]. Every tile is therefore at
// 0 when p = 0 and 1 when p = 1, and ~REVEAL_WINDOW/spacing tiles are mid-fade at any
// moment, so progress is continuous rather than a sequence of discrete pops.
//
// One renderer serves both arms: the tiles always cross-fade from the scramble to the
// true object, and the arms differ only in what drives p.
//     progressive   p = stepsTaken / stepsRequired   the object resolves as you walk
//     endpoint      p held at 0 for the whole walk, then ramped 0 -> 1 on arrival

/** Chebyshev distance between two tile indices on the grid. */
function tileDistance(a, b, gridN) {
    return Math.max(Math.abs(Math.floor(a / gridN) - Math.floor(b / gridN)),
                    Math.abs((a % gridN) - (b % gridN)));
}

/** Per-tile fade thresholds: a uniform grid over [0, 1 - REVEAL_WINDOW], shuffled.
 *  Drawn independently of the scramble permutations so the order in which tiles
 *  resolve carries no spatial structure. */
function buildRevealThresholds(nTiles, rng) {
    var span = 1 - CONFIG.REVEAL_WINDOW;
    var t = [];
    for (var i = 0; i < nTiles; i++) {
        t.push(nTiles > 1 ? (i / (nTiles - 1)) * span : 0);
    }
    for (var j = t.length - 1; j > 0; j--) {
        var k = Math.floor(rng() * (j + 1));
        var tmp = t[j]; t[j] = t[k]; t[k] = tmp;
    }
    return t;
}

/** A permutation in which every tile sits at least REVEAL_MIN_DISPLACEMENT away
 *  from its counterpart in each reference arrangement.
 *
 *  Rejection sampling cannot produce this. On a 10x10 grid, whole-permutation
 *  acceptance against the identity alone is ~0.00025, and against the identity AND a
 *  second permutation it is effectively zero (0 successes in 2,000,000 draws). So:
 *  shuffle once, then repair violating tiles by swapping each with a random partner.
 *  Median 9 swaps against the identity alone, 20 against the identity and a second
 *  permutation. */
function buildScramblePermutation(refs, rng) {
    var gridN = CONFIG.REVEAL_GRID_N;
    var n = gridN * gridN;
    var minD = CONFIG.REVEAL_MIN_DISPLACEMENT;

    var perm = [];
    for (var i = 0; i < n; i++) perm.push(i);
    for (var j = n - 1; j > 0; j--) {
        var k = Math.floor(rng() * (j + 1));
        var tmp = perm[j]; perm[j] = perm[k]; perm[k] = tmp;
    }

    function violates(idx) {
        for (var r = 0; r < refs.length; r++) {
            if (tileDistance(perm[idx], refs[r][idx], gridN) < minD) return true;
        }
        return false;
    }

    var swaps = 0;
    var maxSwaps = 200 * n;
    for (;;) {
        var bad = [];
        for (var i2 = 0; i2 < n; i2++) if (violates(i2)) bad.push(i2);
        if (bad.length === 0) return perm;

        var a = bad[Math.floor(rng() * bad.length)];
        var b = Math.floor(rng() * n);
        var swap = perm[a]; perm[a] = perm[b]; perm[b] = swap;

        if (++swaps > maxSwaps) {
            fail('scramble permutation did not converge in ' + maxSwaps + ' swaps.\n' +
                 'REVEAL_MIN_DISPLACEMENT (' + minD + ') is too large for ' +
                 'REVEAL_GRID_N (' + gridN + ') — lower it or use a finer grid.');
        }
    }
}

function identityPermutation() {
    var n = CONFIG.REVEAL_GRID_N * CONFIG.REVEAL_GRID_N;
    var p = [];
    for (var i = 0; i < n; i++) p.push(i);
    return p;
}

/** Background CSS for the sub-rectangle of the object crop holding tile content
 *  `contentIdx`. Unlike stimBgCSS this states the vertical offset explicitly: the
 *  'center' shorthand only resolves to zero because the background height equals the
 *  full box, which is not true inside a tile. */
function stimTileBgCSS(objIdx, variant, boxSize, contentIdx, tile) {
    var gridN = CONFIG.REVEAL_GRID_N;
    var scale = boxSize / CONFIG.STRIP_HEIGHT_PX;
    var vw = CONFIG.STRIP_WIDTH_PX / CONFIG.N_COLOR_VARIANTS;
    var x = -(variant * vw * scale) - ((contentIdx % gridN) * tile);
    var y = -(Math.floor(contentIdx / gridN) * tile);
    return 'background-image:url(\'' + stimulusURL(objIdx) + '\');' +
        'background-size:' + (CONFIG.STRIP_WIDTH_PX * scale) + 'px ' + (CONFIG.STRIP_HEIGHT_PX * scale) + 'px;' +
        'background-position:' + x + 'px ' + y + 'px;' +
        'background-repeat:no-repeat;';
}

/** Markup for one tile grid. Tiles overlap by REVEAL_TILE_BLEED_PX so that
 *  fractional tile sizes cannot open a seam; the grid container clips the overhang.
 *  Each tile's source layer is fully opaque, so overlapping tiles never composite
 *  translucently against the background. */
function buildTileGridHTML(objIdx, variant, boxSize, sourcePerm, targetPerm) {
    var gridN = CONFIG.REVEAL_GRID_N;
    var tile = boxSize / gridN;
    var span = tile + REVEAL_TILE_BLEED_PX;
    var html = '';

    for (var i = 0; i < gridN * gridN; i++) {
        var left = (i % gridN) * tile;
        var top = Math.floor(i / gridN) * tile;
        html += '<div style="position:absolute;left:' + left + 'px;top:' + top + 'px;' +
            'width:' + span + 'px;height:' + span + 'px;overflow:hidden;">' +
            '<div style="position:absolute;left:0;top:0;width:100%;height:100%;' +
            stimTileBgCSS(objIdx, variant, boxSize, sourcePerm[i], tile) + '"></div>' +
            '<div class="reveal-target" style="position:absolute;left:0;top:0;width:100%;height:100%;' +
            'opacity:0;transition:opacity ' + REVEAL_TRANSITION_MS + 'ms linear;' +
            stimTileBgCSS(objIdx, variant, boxSize, targetPerm[i], tile) + '"></div>' +
            '</div>';
    }
    return html;
}

/** Render a pass into `container` and return the state applyReveal() advances. */
function mountRevealPass(container, trial, sourcePerm, targetPerm, thresholds) {
    container.innerHTML = buildTileGridHTML(
        trial.objIndex, trial.colorVariant, CONFIG.WALK_IMG_SIZE, sourcePerm, targetPerm);

    var targets = container.querySelectorAll('.reveal-target');
    var expected = CONFIG.REVEAL_GRID_N * CONFIG.REVEAL_GRID_N;
    if (targets.length !== expected) {
        fail('tile grid built ' + targets.length + ' tiles, expected ' + expected);
    }

    // Tiles visited in threshold order so a progress update touches only the ones
    // inside the fade window.
    var order = [];
    for (var i = 0; i < expected; i++) order.push(i);
    order.sort(function (a, b) { return thresholds[a] - thresholds[b]; });

    return { targetEls: targets, thresholds: thresholds, order: order, lo: 0, hi: 0 };
}

/** Advance the reveal to progress `p` (monotonically non-decreasing within a pass). */
function applyReveal(state, p) {
    var w = CONFIG.REVEAL_WINDOW;
    var order = state.order;
    var thr = state.thresholds;
    var els = state.targetEls;

    while (state.lo < order.length && thr[order[state.lo]] + w <= p) {
        els[order[state.lo]].style.opacity = '1';
        state.lo++;
    }
    while (state.hi < order.length && thr[order[state.hi]] < p) {
        state.hi++;
    }
    for (var i = state.lo; i < state.hi; i++) {
        var idx = order[i];
        var o = (p - thr[idx]) / w;
        els[idx].style.opacity = (o < 0 ? 0 : o > 1 ? 1 : o).toFixed(3);
    }
}

/** Force the pass to its resolved state, independent of floating-point drift in the
 *  threshold arithmetic. */
function completeReveal(state) {
    for (var i = 0; i < state.targetEls.length; i++) {
        state.targetEls[i].style.opacity = '1';
    }
    state.lo = state.hi = state.order.length;
}

// ======================== TRIAL & MEMORY LIST BUILDERS ========================

function buildTrialList(hash) {
    var activePool = getActiveStimuliPool();
    var selected = shuffle(activePool.whitelist).slice(0, CONFIG.N_STIMULI);
    var levels = buildEffortSequence(CONFIG.N_STIMULI);

    return selected.map(function (obj, idx) {
        return {
            objIndex: obj,
            colorVariant: colorVariant(obj, hash),
            effortLevel: levels[idx],
            stepsRequired: stepsFor(levels[idx])
        };
    });
}

function buildPracticeTrial(hash) {
    return {
        objIndex: CONFIG.PRACTICE_OBJ_INDEX,
        colorVariant: colorVariant(CONFIG.PRACTICE_OBJ_INDEX, hash),
        effortLevel: CONFIG.PRACTICE_EFFORT_LEVEL,
        stepsRequired: stepsFor(CONFIG.PRACTICE_EFFORT_LEVEL)
    };
}

function buildMemoryCheckList(trials, hash) {
    var activePool = getActiveStimuliPool();
    var seenItems = trials.map(function (t) {
        return { objIndex: t.objIndex, colorVariant: t.colorVariant, isOld: true };
    });
    var oldItems = shuffle(seenItems).slice(0, CONFIG.N_MEMORY_OLD);

    // Pool sizes are guaranteed by validateConfig, so foils are taken directly. A
    // "whitelist minus seen" fallback would silently yield zero foils whenever the
    // whitelist is exactly N_STIMULI long, leaving a recognition test with no
    // unseen items and a d' that still looks numerically plausible.
    var newItems = shuffle(activePool.foils).slice(0, CONFIG.N_MEMORY_NEW).map(function (idx) {
        return { objIndex: idx, colorVariant: colorVariant(idx, hash), isOld: false };
    });

    return shuffle(oldItems.concat(newItems));
}

function buildRedirectURL() {
    var base = CONFIG.QUALTRICS_URLS[CONFIG.RECRUITMENT];
    if (CONFIG.RECRUITMENT === 'sona') {
        return base +
            '?experiment_id=' + encodeURIComponent(PARTICIPANT.experiment_id) +
            '&credit_token=' + encodeURIComponent(PARTICIPANT.credit_token) +
            '&experiment_name=' + encodeURIComponent(PARTICIPANT.experiment_name) +
            '&id=' + encodeURIComponent(PARTICIPANT.id);
    }
    return base +
        '?PROLIFIC_PID=' + encodeURIComponent(PARTICIPANT.prolific_pid) +
        '&STUDY_ID=' + encodeURIComponent(PARTICIPANT.study_id) +
        '&SESSION_ID=' + encodeURIComponent(PARTICIPANT.session_id) +
        '&experiment_name=' + encodeURIComponent(PARTICIPANT.experiment_name) +
        '&id=' + encodeURIComponent(PARTICIPANT.id);
}

/** SVG stick figure with sinusoidal limb animation; `frame` indexes the stride. */
function figureSVG(frame) {
    var t = (frame % 11) / 11 * Math.PI * 2;
    var leg = Math.sin(t) * 14;
    var arm = Math.sin(t + Math.PI) * 11;
    return '<svg width="30" height="60" viewBox="0 0 30 60">' +
        '<circle cx="15" cy="7" r="6" fill="none" stroke="#fff" stroke-width="2"/>' +
        '<line x1="15" y1="13" x2="15" y2="36" stroke="#fff" stroke-width="2"/>' +
        '<line x1="15" y1="22" x2="' + (15 + arm) + '" y2="32" stroke="#fff" stroke-width="2"/>' +
        '<line x1="15" y1="22" x2="' + (15 - arm) + '" y2="32" stroke="#fff" stroke-width="2"/>' +
        '<line x1="15" y1="36" x2="' + (15 + leg) + '" y2="56" stroke="#fff" stroke-width="2"/>' +
        '<line x1="15" y1="36" x2="' + (15 - leg) + '" y2="56" stroke="#fff" stroke-width="2"/>' +
        '</svg>';
}

// ======================== VALIDATION ========================

/** Refuse to start on a configuration that cannot produce a valid session. Runs
 *  before any trial is built, so an operator sees the cause rather than a hang. */
function validateConfig() {
    var gridN = CONFIG.REVEAL_GRID_N;
    if (!Number.isInteger(gridN) || gridN < 2) {
        fail('REVEAL_GRID_N must be an integer >= 2 (got ' + gridN + ')');
    }
    if (!(CONFIG.REVEAL_WINDOW > 0 && CONFIG.REVEAL_WINDOW < 1)) {
        fail('REVEAL_WINDOW must lie in (0, 1) (got ' + CONFIG.REVEAL_WINDOW + ')');
    }
    // The centre tile can move no further than floor((gridN - 1) / 2), so any larger
    // floor is unsatisfiable for every permutation — catch it here rather than as a
    // non-convergence deep inside a trial.
    var maxDisplacement = Math.floor((gridN - 1) / 2);
    if (!Number.isInteger(CONFIG.REVEAL_MIN_DISPLACEMENT) ||
        CONFIG.REVEAL_MIN_DISPLACEMENT < 1 ||
        CONFIG.REVEAL_MIN_DISPLACEMENT > maxDisplacement) {
        fail('REVEAL_MIN_DISPLACEMENT must be an integer in [1, ' + maxDisplacement + '] ' +
             'for REVEAL_GRID_N ' + gridN + ' (got ' + CONFIG.REVEAL_MIN_DISPLACEMENT + ') — ' +
             'the centre tile cannot be displaced further than that');
    }
    if (!(CONFIG.REVEAL_RAMP_MS > 0)) {
        fail('REVEAL_RAMP_MS must be positive (got ' + CONFIG.REVEAL_RAMP_MS + ')');
    }
    if (CONFIG.FORCE_REVEAL !== null &&
        CONFIG.FORCE_REVEAL !== 'progressive' && CONFIG.FORCE_REVEAL !== 'endpoint') {
        fail('FORCE_REVEAL must be null, "progressive" or "endpoint" (got ' +
             JSON.stringify(CONFIG.FORCE_REVEAL) + ')');
    }

    CONFIG.EFFORT_LEVEL_KEYS.forEach(function (level) {
        var steps = CONFIG.EFFORT_STEPS[level];
        if (!Number.isInteger(steps) || steps < 1) {
            fail('EFFORT_STEPS.' + level + ' must be a positive integer (got ' + steps + ')');
        }
    });
    if (CONFIG.N_STIMULI % CONFIG.EFFORT_LEVEL_KEYS.length !== 0) {
        fail('N_STIMULI (' + CONFIG.N_STIMULI + ') must divide evenly across the ' +
             CONFIG.EFFORT_LEVEL_KEYS.length + ' effort levels');
    }
    // Every opacity write rides a REVEAL_TRANSITION_MS cross-fade, so the reveal only
    // keeps pace with the walk while accepted presses are at least that far apart.
    if (!(CONFIG.DEBOUNCE_MS >= REVEAL_TRANSITION_MS)) {
        fail('DEBOUNCE_MS must be at least REVEAL_TRANSITION_MS (' + REVEAL_TRANSITION_MS +
             ') so the reveal keeps up with the walk (got ' + CONFIG.DEBOUNCE_MS + ')');
    }

    var pool = getActiveStimuliPool();
    if (!pool.whitelist || pool.whitelist.length < CONFIG.N_STIMULI) {
        fail('the "' + CONFIG.ACTIVE_STIMULI_POOL + '" pool holds ' +
             ((pool.whitelist || []).length) + ' trial objects but N_STIMULI is ' + CONFIG.N_STIMULI);
    }
    if (!pool.foils || pool.foils.length < CONFIG.N_MEMORY_NEW) {
        fail('the "' + CONFIG.ACTIVE_STIMULI_POOL + '" pool holds ' +
             ((pool.foils || []).length) + ' foils but the memory check needs ' +
             CONFIG.N_MEMORY_NEW + ' unseen items');
    }

    if (CONFIG.EFFORT_STEPS[CONFIG.PRACTICE_EFFORT_LEVEL] === undefined) {
        fail('PRACTICE_EFFORT_LEVEL "' + CONFIG.PRACTICE_EFFORT_LEVEL + '" is not an effort level');
    }
    if (pool.whitelist.indexOf(CONFIG.PRACTICE_OBJ_INDEX) !== -1 ||
        pool.foils.indexOf(CONFIG.PRACTICE_OBJ_INDEX) !== -1) {
        fail('PRACTICE_OBJ_INDEX (' + CONFIG.PRACTICE_OBJ_INDEX + ') appears in the "' +
             CONFIG.ACTIVE_STIMULI_POOL + '" trial or foil pool — the practice object must be ' +
             'outside both so it never enters the recognition check');
    }

    if (['prolific', 'sona'].indexOf(CONFIG.RECRUITMENT) === -1) {
        fail('RECRUITMENT must be "prolific" or "sona" (got "' + CONFIG.RECRUITMENT + '")');
    }
    if (!CONFIG.DEBUG && !CONFIG.QUALTRICS_URLS[CONFIG.RECRUITMENT]) {
        fail('QUALTRICS_URLS.' + CONFIG.RECRUITMENT + ' is empty.\n\n' +
             'Set the selected survey URL before launching, or run locally with ?debug=1.');
    }
    if (!CONFIG.DEBUG && CONFIG.RECRUITMENT === 'prolific') {
        var missingProlificParams = [
            ['PROLIFIC_PID', PARTICIPANT.prolific_pid],
            ['STUDY_ID', PARTICIPANT.study_id],
            ['SESSION_ID', PARTICIPANT.session_id]
        ].filter(function (entry) { return !entry[1]; })
          .map(function (entry) { return entry[0]; });
        if (missingProlificParams.length > 0) {
            fail('Missing required Prolific URL parameter(s): ' + missingProlificParams.join(', ') +
                 '.\n\nStart through a Prolific participant link, or run locally with ?debug=1.');
        }
    }
    if (!CONFIG.DEBUG && CONFIG.RECRUITMENT === 'sona') {
        var missingSonaParams = [
            ['id', PARTICIPANT.id],
            ['experiment_id', PARTICIPANT.experiment_id],
            ['credit_token', PARTICIPANT.credit_token]
        ].filter(function (entry) { return !entry[1]; })
          .map(function (entry) { return entry[0]; });
        if (missingSonaParams.length > 0) {
            fail('Missing required SONA URL parameter(s): ' + missingSonaParams.join(', ') +
                 '.\n\nStart through a SONA participant link, or run locally with ?debug=1.');
        }
    }
}

validateConfig();

// ======================== DERIVED STATE ========================

pHash = hashCode(PARTICIPANT.id || String(Math.random()));

// Two independent bits of the participant hash drive the two between-subjects
// factors, so a participant returning under the same id lands in the same cell.
if (CONFIG.RATING_DESIGN === 'between') {
    if (conditionOverride === 'value' || conditionOverride === 'liking') {
        assignedCondition = (conditionOverride === 'value') ? 'Value' : 'Liking';
    } else {
        assignedCondition = (pHash % 2 === 0) ? 'Value' : 'Liking';
    }
}
// An override that is present but unrecognised must not fall through to the hash —
// a typo would otherwise hand back a random arm that looks like a working override.
if (revealOverride && revealOverride !== 'progressive' && revealOverride !== 'endpoint') {
    fail('?reveal=' + revealOverride + ' is not a reveal schedule — use "progressive" or "endpoint"');
}
if (conditionOverride && conditionOverride !== 'value' && conditionOverride !== 'liking') {
    fail('?condition=' + conditionOverride + ' is not a rating type — use "value" or "liking"');
}

// Precedence: the debug URL override first, so both arms stay testable without
// editing this file; then FORCE_REVEAL, which pins a whole live wave; then the hash.
if (revealOverride) {
    assignedReveal = revealOverride;
    revealSource = 'debug_url';
} else if (CONFIG.FORCE_REVEAL) {
    assignedReveal = CONFIG.FORCE_REVEAL;
    revealSource = 'forced';
} else {
    assignedReveal = (Math.floor(pHash / 2) % 2 === 0) ? 'progressive' : 'endpoint';
    revealSource = 'hash';
}
// A pinned arm is deliberate, but it is invisible on screen and silently turns the
// between-subjects factor into a constant, so say so where an operator can see it.
if (revealSource === 'forced' && !CONFIG.DEBUG) {
    console.warn('[' + CONFIG.EXPERIMENT_NAME + '] FORCE_REVEAL is set — every participant ' +
                 'in this wave is assigned "' + CONFIG.FORCE_REVEAL + '" and the reveal ' +
                 'factor is not randomised.');
}

var ratingsPerTrial = (CONFIG.RATING_DESIGN === 'within') ? 2 : 1;

// Debug badge. The assigned cell is otherwise invisible until the data is inspected,
// so a URL whose override silently failed to parse (an HTML-escaped "&amp;", a
// copy-pasted trailing space) looks identical to one that worked.
if (CONFIG.DEBUG) {
    document.addEventListener('DOMContentLoaded', function () {
        var badge = document.createElement('div');
        badge.style.cssText = 'position:fixed;top:0;left:0;z-index:9999;padding:6px 10px;' +
            'background:#3a2a00;color:#ffcc55;font:12px/1.5 monospace;white-space:pre;' +
            'border-bottom-right-radius:4px;opacity:0.9;';
        badge.textContent =
            'DEBUG — no Pavlovia, no credit, no upload\n' +
            'reveal    ' + assignedReveal + '  (from ' +
                { debug_url: '?reveal', forced: 'CONFIG.FORCE_REVEAL', hash: 'id hash' }[revealSource] +
                ')\n' +
            'rating    ' + (assignedCondition || 'both') +
                (conditionOverride ? '  (from ?condition)' : '  (from id hash)') + '\n' +
            'steps     ' + CONFIG.EFFORT_LEVEL_KEYS.map(function (l) {
                return l + ' ' + stepsFor(l);
            }).join(' / ') + (turboDivisor > 1 ? '  (turbo /' + turboDivisor + ')' : '') + '\n' +
            'query     ' + (window.location.search || '(none)');
        document.body.appendChild(badge);
    });
}

// ======================== jsPSYCH INIT ========================

var jsPsych = initJsPsych({
    display_element: 'jspsych-display',
    on_finish: function () {
        if (CONFIG.DEBUG) {
            jsPsych.data.get().localSave(
                'csv', CONFIG.EXPERIMENT_NAME + '_' + (PARTICIPANT.id || 'debug') + '.csv');
        } else {
            window.location.href = buildRedirectURL();
        }
    }
});

var timeline = [];

// ======================== PAVLOVIA INIT ========================

// The Pavlovia plugin (2022.1.1) stores its beforeunload callback as a local
// variable in _init() and never assigns it to PavloviaPlugin._beforeunloadCallback,
// so _finish() silently fails to remove the listener and Chrome shows a "Leave site?"
// dialog on redirect — stranding the participant. Intercept addEventListener once to
// capture the reference, then restore it.
//
// In debug no Pavlovia node is pushed at all: no session is opened, so no credit is
// consumed on either recruitment platform, and no data is uploaded.
if (!CONFIG.DEBUG) {
    (function () {
        var origAdd = window.addEventListener;
        window.addEventListener = function (type, fn, opts) {
            if (type === 'beforeunload') {
                jsPsychPavlovia._beforeunloadCallback = fn;
                window.addEventListener = origAdd; // restore after capture
            }
            return origAdd.call(this, type, fn, opts);
        };
    })();

    timeline.push({
        type: jsPsychPavlovia,
        command: 'init',
        participantId: PARTICIPANT.id || undefined
    });
}

// ======================== SETUP ========================

var practiceTrial = null;

timeline.push({
    type: jsPsychCallFunction,
    func: function () {
        trialList = buildTrialList(pHash);
        practiceTrial = buildPracticeTrial(pHash);
        memoryCheckList = buildMemoryCheckList(trialList, pHash);
        trialIndex = 0;

        jsPsych.data.addProperties({
            id: PARTICIPANT.id,
            experiment_name: PARTICIPANT.experiment_name,
            recruitment: CONFIG.RECRUITMENT,
            prolific_pid: PARTICIPANT.prolific_pid,
            study_id: PARTICIPANT.study_id,
            session_id: PARTICIPANT.session_id,
            experiment_id: PARTICIPANT.experiment_id,
            date: new Date().toISOString(),
            effort_task: 'walk',
            reveal_assignment: assignedReveal,
            // How the arm was chosen. A pinned wave and a randomised one are otherwise
            // indistinguishable once they are pooled, and the difference decides whether
            // the reveal contrast is randomised or between-wave.
            reveal_source: revealSource,
            assigned_condition: assignedCondition || 'both',
            rating_design: CONFIG.RATING_DESIGN,
            active_stimuli_pool: CONFIG.ACTIVE_STIMULI_POOL,
            n_trials: CONFIG.N_STIMULI,
            // The walk scene is a fixed 900 px block and the page does not scroll
            // horizontally, so a narrow window crops the target frame — the reveal
            // itself. Recorded so affected sessions are identifiable.
            viewport_width: window.innerWidth,
            debug: CONFIG.DEBUG ? 1 : 0
        });
    }
});

// ======================== PRELOAD STIMULI ========================

timeline.push({
    type: jsPsychHtmlKeyboardResponse,
    stimulus: '<div style="text-align:center;font-size:20px;">Loading resources...</div>',
    choices: 'NO_KEYS',
    trial_duration: null,
    on_load: function () {
        var urls = trialList.map(function (t) { return stimulusURL(t.objIndex); });
        urls.push(stimulusURL(CONFIG.PRACTICE_OBJ_INDEX));
        memoryCheckList.forEach(function (m) {
            if (!m.isOld) urls.push(stimulusURL(m.objIndex));
        });

        var idx = 0, done = 0, total = urls.length, batch = 4, maxRetries = 2;

        function load(url, retries) {
            var img = new Image();
            img.onload = function () { done++; next(); };
            img.onerror = function () {
                if (retries > 0) { load(url, retries - 1); }
                else { done++; next(); }
            };
            img.src = url;
        }
        function next() {
            if (done >= total) { jsPsych.finishTrial(); return; }
            while (idx < total && idx - done < batch) { load(urls[idx++], maxRetries); }
        }
        next();
    },
    data: { trial_id: 'preload' }
});

// ======================== INSTRUCTIONS ========================

// The two arms are described accurately and as similarly as the manipulation allows;
// the wording difference is part of the design and is preregistered.
var revealBlurb = (assignedReveal === 'progressive')
    ? 'Each object starts out scrambled at the target location. It will gradually come ' +
      'into focus as you walk, and is fully clear once you reach it.'
    : 'Each object starts out scrambled at the target location. It stays scrambled while ' +
      'you walk, and is revealed once you reach it.';

// Compensation is issued by the survey the final redirect lands on, not by finishing
// here, so both the instructions and the end screen have to say so: a participant who
// stops at the end screen has already uploaded their data but never reaches the code.
var COMPLETION_TERM = (CONFIG.RECRUITMENT === 'prolific') ? 'completion code' : 'course credit';

var instructionsHTML =
    '<div style="max-width:700px;margin:0 auto;text-align:left;line-height:1.7;">' +
    '<h3 style="text-align:center;">TASK INSTRUCTIONS</h3>' +
    '<p>Thank you for agreeing to participate in today\'s experiment!</p>' +
    '<p>Please make sure to read all following instructions carefully.</p>' +
    '<p>You will be asked to collect a number of objects. Each object is associated ' +
    'with a different level of effort, i.e., number of steps needed to collect it.</p>' +
    '<p>' + revealBlurb + ' Collect it by pressing <b>[SPACE]</b> for each step ' +
    'necessary. Try performing the steps as quickly as you can.</p>';

if (CONFIG.RATING_DESIGN === 'within') {
    instructionsHTML +=
        '<p>After each object is collected you will be asked two separate questions about it. ' +
        'Use the scale presented on the screen to answer these questions. Please move the mouse ' +
        'to indicate your opinion.</p>' +
        '<p>One question will ask you to indicate the value that you think that object has. ' +
        'The other will ask you to indicate how much you like or dislike it. These two questions ' +
        'are separate so please consider them independently.</p>';
} else {
    instructionsHTML +=
        '<p>After each object is collected you will be asked a question about it. ' +
        'Use the scale presented on the screen to answer. Please move the mouse to ' +
        'indicate your opinion.</p>';
}

instructionsHTML +=
    '<p>Use the mouse to navigate the scale and confirm your choice. There will be a ' +
    '15 second rest period following the collection and rating of each object.</p>' +
    '<p>You will start with one practice object, which you will not be asked to rate.</p>' +
    '<p>The study ends with a short survey. You must complete that survey to receive your ' +
    COMPLETION_TERM + '.</p>' +
    '</div>';

timeline.push({
    type: jsPsychHtmlButtonResponse,
    stimulus: instructionsHTML,
    choices: ['Next'],
    data: { trial_id: 'instructions' }
});

// ======================== FIXATION (reused) ========================

var fixation = {
    type: jsPsychHtmlKeyboardResponse,
    stimulus: '<div style="font-size:48px;color:#fff;">+</div>',
    choices: 'NO_KEYS',
    trial_duration: CONFIG.FIXATION_MS,
    data: { trial_id: 'fixation' }
};

// ======================== WALK SCENE ========================

var WALK_W = 900;
var WALK_GROUND_Y = 260;
var WALK_IMG_LEFT = WALK_W - CONFIG.WALK_IMG_SIZE - 30;
var WALK_IMG_TOP = WALK_GROUND_Y - CONFIG.WALK_IMG_SIZE - 10;
var WALK_FIGURE_W = 30;
var WALK_FIGURE_H = 60;
var WALK_RANGE = WALK_IMG_LEFT - WALK_FIGURE_W;

function buildWalkHTML() {
    return '<div id="walk-scene" style="position:relative;width:' + WALK_W + 'px;height:340px;' +
        'overflow:hidden;margin:0 auto;">' +
        // Target frame (always visible)
        '<div style="position:absolute;left:' + WALK_IMG_LEFT + 'px;top:' + WALK_IMG_TOP + 'px;' +
        'width:' + CONFIG.WALK_IMG_SIZE + 'px;height:' + CONFIG.WALK_IMG_SIZE + 'px;' +
        'border:2px solid rgba(255,255,255,0.4);box-sizing:border-box;"></div>' +
        // Tile grid; clips the tiles' overlap bleed to the frame
        '<div id="walk-tiles" style="position:absolute;left:' + WALK_IMG_LEFT + 'px;top:' + WALK_IMG_TOP + 'px;' +
        'width:' + CONFIG.WALK_IMG_SIZE + 'px;height:' + CONFIG.WALK_IMG_SIZE + 'px;overflow:hidden;"></div>' +
        // Ground line
        '<div style="position:absolute;left:0;top:' + WALK_GROUND_Y + 'px;width:100%;height:3px;background:#fff;"></div>' +
        // Stick figure
        '<div id="walk-figure" style="position:absolute;top:' + (WALK_GROUND_Y - WALK_FIGURE_H) + 'px;left:0px;">' +
        figureSVG(0) + '</div>' +
        // Status text
        '<div id="walk-text" style="position:absolute;top:' + (WALK_GROUND_Y + 16) + 'px;width:100%;text-align:center;' +
        'font-size:22px;font-weight:600;">' +
        '<span style="color:#888;">Get ready...</span></div>' +
        '</div>';
}

// ======================== WALK TRIAL ========================

/** One walk. `getTrial` supplies the trial spec; practice trials differ only in the
 *  data row they write and in not being followed by a rating. */
function makeWalkTrial(getTrial, isPractice) {
    var dataTrialId = isPractice ? 'practice_walk' : 'walk';

    return {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: buildWalkHTML,
        choices: 'NO_KEYS',
        trial_duration: null,
        data: { trial_id: dataTrialId },
        on_load: function () {
            var trial = getTrial();
            var tilesEl  = document.getElementById('walk-tiles');
            var figureEl = document.getElementById('walk-figure');
            var textEl   = document.getElementById('walk-text');
            if (!tilesEl || !figureEl || !textEl) {
                fail('walk scene is missing an element (tiles/figure/text) — markup and ' +
                     'on_load have drifted apart');
            }

            currentWalk = {
                effort_trial_duration: null,
                steps_required: trial.stepsRequired,
                presses_rejected: 0,
                reveal_seed: 0,
                walk_focus_lost: 0,
                walk_blur_ms: 0,
                stimulus_load_error: 0,
                walk_error: ''
            };

            var stepsTaken = 0;
            var lastAccepted = -Infinity;
            var startTime = 0;
            var collected = false;
            var finished = false;
            var revealState = null;
            var sigmaA = null, thresholds = null;
            var rampFrame = null;

            // The walk is self-paced and can run past a minute, and its duration is both
            // a reported measure and the quantity time_outlier_ids() cuts on. A tab
            // switch inflates it exactly like slow pressing, so record it.
            var blurStart = 0;
            var listeners = [];

            function markBlur() {
                if (blurStart === 0) { blurStart = performance.now(); currentWalk.walk_focus_lost++; }
            }
            function markFocus() {
                if (blurStart > 0) {
                    currentWalk.walk_blur_ms += Math.round(performance.now() - blurStart);
                    blurStart = 0;
                }
            }
            function onVisibility() { if (document.hidden) markBlur(); else markFocus(); }
            function addL(target, type, fn) {
                target.addEventListener(type, fn);
                listeners.push([target, type, fn]);
            }

            addL(window, 'blur', markBlur);
            addL(window, 'focus', markFocus);
            addL(document, 'visibilitychange', onVisibility);

            function cleanup() {
                markFocus(); // close any open blur interval
                listeners.forEach(function (l) { l[0].removeEventListener(l[1], l[2]); });
                listeners = [];
                document.removeEventListener('keydown', onKeyDown);
                if (rampFrame !== null) cancelAnimationFrame(rampFrame);
            }

            function endTrial(extra) {
                if (finished) return;
                finished = true;
                cleanup();
                var payload = {
                    trial_id: dataTrialId,
                    effort_trial_duration: currentWalk.effort_trial_duration,
                    steps_required: currentWalk.steps_required,
                    presses_rejected: currentWalk.presses_rejected,
                    walk_focus_lost: currentWalk.walk_focus_lost,
                    walk_blur_ms: currentWalk.walk_blur_ms
                };
                if (extra) Object.keys(extra).forEach(function (k) { payload[k] = extra[k]; });
                jsPsych.finishTrial(payload);
            }

            // An exception inside a handler would otherwise leave the participant on a
            // trial with no time limit and no accepted keys — i.e. stuck forever.
            function guarded(fn) {
                return function (arg) {
                    try { fn(arg); }
                    catch (err) {
                        currentWalk.walk_error = String((err && err.message) || err).slice(0, 200);
                        endTrial({ walk_error: currentWalk.walk_error });
                    }
                };
            }

            function drawFigure() {
                var p = Math.min(stepsTaken / trial.stepsRequired, 1);
                figureEl.style.left = (p * WALK_RANGE) + 'px';
                // Rest pose on arrival so the figure ends standing rather than mid-stride.
                figureEl.innerHTML = figureSVG(collected ? 0 : stepsTaken);
            }

            function setText(html) { textEl.innerHTML = html; }

            /** Hold the fully-resolved object for the same duration in both arms: the
             *  wait starts once the reveal has finished settling, not on arrival. */
            function holdResolved() {
                setTimeout(function () {
                    endTrial();
                }, REVEAL_TRANSITION_MS + CONFIG.COLLECTED_MSG_MS);
            }

            /** Endpoint arm: the tiles have held their scramble for the whole walk, so
             *  the reveal is the same cross-fade the progressive arm spreads across the
             *  walk, compressed into REVEAL_RAMP_MS on arrival. */
            function runArrivalReveal() {
                var rampStart = performance.now();
                var step = guarded(function () {
                    var q = (performance.now() - rampStart) / CONFIG.REVEAL_RAMP_MS;
                    if (q >= 1) {
                        rampFrame = null;
                        completeReveal(revealState);
                        holdResolved();
                        return;
                    }
                    applyReveal(revealState, q);
                    rampFrame = requestAnimationFrame(step);
                });
                rampFrame = requestAnimationFrame(step);
            }

            function onCollected() {
                collected = true;
                currentWalk.effort_trial_duration = (performance.now() - startTime) / 1000;
                drawFigure();
                setText('<span style="color:#00dd44;">Object collected!</span>');
                if (assignedReveal === 'endpoint') {
                    runArrivalReveal();
                } else {
                    completeReveal(revealState);
                    holdResolved();
                }
            }

            var onKeyDown = guarded(function (e) {
                if (e.code !== 'Space') return;
                e.preventDefault();
                if (e.repeat || collected) return;

                var now = performance.now();
                // Debounce on elapsed time, and do NOT reset the clock on a rejected
                // press: a rejection-resetting gate lets fast pressing stall progress
                // indefinitely, and a frame-based one makes the effort cost depend on
                // the participant's monitor refresh rate.
                if (now - lastAccepted < CONFIG.DEBOUNCE_MS) {
                    currentWalk.presses_rejected++;
                    return;
                }
                lastAccepted = now;
                stepsTaken++;

                // Only the progressive arm couples the reveal to the walk; in the
                // endpoint arm the scramble is held until arrival.
                if (assignedReveal === 'progressive') {
                    applyReveal(revealState, Math.min(stepsTaken / trial.stepsRequired, 1));
                }
                drawFigure();

                if (stepsTaken >= trial.stepsRequired) onCollected();
            });

            function startWalk() {
                setText('<span style="color:#ff4444;">Walk to collect object...</span>');
                document.addEventListener('keydown', onKeyDown);
                startTime = performance.now();
            }

            // Build the scramble from a seed fixed by participant and object, so the
            // stimulus is reproducible from the logged fields. Drawn per trial rather
            // than per session: a single arrangement reused across 18 trials would be
            // partly learnable, which would erode the endpoint arm over the session.
            var seed = hashCode(String(pHash) + '_reveal_' + String(trial.objIndex));
            var rng = seededRandom(seed);
            currentWalk.reveal_seed = seed;

            thresholds = buildRevealThresholds(CONFIG.REVEAL_GRID_N * CONFIG.REVEAL_GRID_N, rng);
            var identity = identityPermutation();
            sigmaA = buildScramblePermutation([identity], rng);

            function mountWalkPass() {
                revealState = mountRevealPass(tilesEl, trial, sigmaA, identity, thresholds);
            }

            // Decode before the first tile paints and before the clock starts:
            // measuring effort from a moment the participant could not yet act on
            // would fold network latency into effort_trial_duration.
            function beginAfterFirstPaint() {
                mountWalkPass();
                requestAnimationFrame(function () {
                    requestAnimationFrame(guarded(startWalk));
                });
            }

            var strip = new Image();
            strip.src = stimulusURL(trial.objIndex);
            // Two-argument then(), not .then().catch(): a single trailing catch would
            // also swallow anything thrown while mounting the tiles and mislabel it as
            // a stimulus failure.
            strip.decode().then(guarded(beginAfterFirstPaint), function () {
                currentWalk.stimulus_load_error = 1;
                if (CONFIG.DEBUG) {
                    fail('could not decode ' + stimulusURL(trial.objIndex) +
                         ' — check that stimuli/ is present and complete');
                }
                // In a live session a missing stimulus must not strand the participant:
                // run the trial, flag the row, and exclude it in analysis.
                guarded(beginAfterFirstPaint)();
            });
        }
    };
}

// ======================== PRACTICE ========================

timeline.push({
    type: jsPsychHtmlButtonResponse,
    stimulus:
        '<div style="max-width:700px;margin:0 auto;text-align:center;line-height:1.7;">' +
        '<h3>PRACTICE</h3>' +
        '<p>You will now collect one practice object.</p>' +
        '<p>You will not be asked to rate it.</p></div>',
    choices: ['Begin'],
    data: { trial_id: 'practice_instructions' }
});

timeline.push(fixation);

timeline.push(makeWalkTrial(function () { return practiceTrial; }, true));

timeline.push({
    type: jsPsychCallFunction,
    func: function () {},
    on_finish: function (data) {
        data.trial_id = 'practice_summary';
        data.effort_level = practiceTrial.effortLevel;
        data.steps_required = currentWalk.steps_required;
        data.effort_trial_duration = currentWalk.effort_trial_duration;
        data.presses_rejected = currentWalk.presses_rejected;
        data.reveal_seed = currentWalk.reveal_seed;
        data.stimulus_load_error = currentWalk.stimulus_load_error;
        data.walk_error = currentWalk.walk_error;
        data.img = 's_obj' + practiceTrial.objIndex;
        data.color_variant = practiceTrial.colorVariant;
    }
});

timeline.push({
    type: jsPsychHtmlButtonResponse,
    stimulus:
        '<div style="max-width:600px;margin:0 auto;text-align:center;line-height:1.7;">' +
        '<p style="font-size:20px;">Practice complete.</p>' +
        '<p>The main experiment will now begin.</p></div>',
    choices: ['Begin'],
    data: { trial_id: 'practice_end' }
});

// ======================== CONDITION INTRO (between only) ========================

if (CONFIG.RATING_DESIGN === 'between') {
    var condIntroText = (assignedCondition === 'Value')
        ? 'For each object you collect, you will be asked to indicate the value that you ' +
          'think the object has. Please use the scale to indicate your answer.'
        : 'For each object you collect, you will be asked to indicate how much you like or ' +
          'dislike it. Please use the scale to indicate your answer.';

    timeline.push({
        type: jsPsychHtmlButtonResponse,
        stimulus:
            '<div style="max-width:700px;margin:0 auto;text-align:center;line-height:1.7;">' +
            '<p style="font-size:20px;">' + condIntroText + '</p></div>',
        choices: ['Next'],
        data: { trial_id: 'condition_intro' }
    });
}

timeline.push(fixation);

// ======================== RATING SCREEN ========================

function buildRatingHTML(trial, scaleType) {
    var isValue = (scaleType === 'Value');
    var thumbColor = isValue ? '#4488ff' : '#44cc66';
    var question = isValue
        ? 'What is the value of this object?'
        : 'How much do you like this object?';
    var randomStart = Math.floor(Math.random() * (CONFIG.SCALE_MAX - CONFIG.SCALE_MIN + 1)) + CONFIG.SCALE_MIN;
    var sliderStep = CONFIG.SCALE_STEP > 0 ? CONFIG.SCALE_STEP : 0.01;
    var tickStep = CONFIG.SCALE_STEP > 0 ? CONFIG.SCALE_STEP : 1;

    var thumbR = 11;
    var nTicks = Math.floor((CONFIG.SCALE_MAX - CONFIG.SCALE_MIN) / tickStep) + 1;

    var overlayHTML = '';
    if (CONFIG.SCALE_SHOW_TICKS) {
        for (var t = 0; t < nTicks; t++) {
            var frac = nTicks > 1 ? t / (nTicks - 1) : 0.5;
            var pos = 'calc(' + thumbR + 'px + ' + frac + ' * (100% - ' + (2 * thumbR) + 'px))';
            overlayHTML += '<span style="position:absolute;left:' + pos + ';top:50%;' +
                'transform:translate(-50%,-50%);width:2px;height:14px;background:#aaa;pointer-events:none;"></span>';
            if (isValue) {
                overlayHTML += '<span style="position:absolute;left:' + pos + ';bottom:100%;margin-bottom:4px;' +
                    'transform:translateX(-50%);font-size:13px;color:#aaa;white-space:nowrap;pointer-events:none;">' +
                    '$' + (100 * (CONFIG.SCALE_MIN + t * tickStep)) + '</span>';
            }
        }
    }

    var bottomHTML;
    if (isValue) {
        bottomHTML = CONFIG.SCALE_SHOW_TICKS ? '' :
            '<div style="display:flex;justify-content:space-between;padding:0 ' + thumbR + 'px;' +
            'font-size:13px;color:#aaa;margin-top:6px;">' +
            '<span>$' + (100 * CONFIG.SCALE_MIN) + '</span>' +
            '<span>$' + (100 * CONFIG.SCALE_MAX) + '</span></div>';
    } else {
        bottomHTML = '<div style="display:flex;justify-content:space-between;padding:0 ' + thumbR + 'px;' +
            'font-size:12px;color:#aaa;margin-top:6px;">' +
            '<span>really don\'t like it</span><span>really like it</span></div>';
    }

    var sliderTopPad = CONFIG.SCALE_SHOW_TICKS ? 'padding-top:24px;' : '';

    return '<div style="text-align:center;">' +
        '<div id="rating-img" style="margin:20px auto;display:inline-block;transition:opacity 0.3s;">' +
        stimDiv(trial.objIndex, trial.colorVariant, CONFIG.RATING_IMG_SIZE) + '</div>' +
        '<div style="font-size:22px;margin:24px 0 20px;color:#fff;">' + question + '</div>' +
        '<div style="max-width:500px;margin:0 auto;' + sliderTopPad + '">' +
        '<div style="position:relative;">' +
        '<input type="range" id="rating-slider" min="' + CONFIG.SCALE_MIN + '" max="' + CONFIG.SCALE_MAX +
        '" step="' + sliderStep + '" value="' + randomStart +
        '" style="width:100%;cursor:pointer;--thumb-color:' + thumbColor + ';">' +
        overlayHTML +
        '</div>' +
        bottomHTML + '</div>' +
        '<button id="ok-btn" disabled style="margin-top:20px;padding:12px 40px;font-size:16px;' +
        'cursor:pointer;border:1px solid rgba(255,255,255,0.2);background:#333;color:#888;' +
        'transition:background 0.15s,color 0.15s;">OK</button>' +
        '</div>';
}

// ======================== TRIAL PROCEDURE ========================

var trialProcedure = {
    timeline: [

        // PRE — reset per-trial state
        {
            type: jsPsychCallFunction,
            func: function () {
                scaleCounter = 0;
                scaleOrder = '';
                currentRatings = {
                    value_rating: null, value_rt: null,
                    liking_rating: null, liking_rt: null
                };
            }
        },

        makeWalkTrial(function () { return trialList[trialIndex]; }, false),

        // RATINGS LOOP
        {
            timeline: [
                fixation,

                {
                    type: jsPsychHtmlKeyboardResponse,
                    stimulus: function () {
                        return buildRatingHTML(trialList[trialIndex], jsPsych.evaluateTimelineVariable('scale_type'));
                    },
                    choices: 'NO_KEYS',
                    trial_duration: null,
                    data: { trial_id: 'rating' },
                    on_load: function () {
                        var scaleType = jsPsych.evaluateTimelineVariable('scale_type');
                        var slider = document.getElementById('rating-slider');
                        var okBtn = document.getElementById('ok-btn');
                        var startTime = performance.now();
                        var interacted = false;

                        // Held so it can be cancelled: the id is reused by the next
                        // rating screen, and under the 'within' design that screen can
                        // arrive before this timer fires.
                        var fadeTimer = setTimeout(function () {
                            var img = document.getElementById('rating-img');
                            if (img) img.style.opacity = '0';
                        }, CONFIG.RATING_IMG_VISIBLE_MS);

                        function enableOK() {
                            if (!interacted) {
                                interacted = true;
                                okBtn.disabled = false;
                                okBtn.style.background = '#555';
                                okBtn.style.color = '#fff';
                                okBtn.style.borderColor = '#888';
                            }
                        }
                        slider.addEventListener('input', enableOK);
                        slider.addEventListener('mousedown', enableOK);
                        slider.addEventListener('touchstart', enableOK);

                        okBtn.addEventListener('click', function () {
                            clearTimeout(fadeTimer);
                            var rating = parseFloat(slider.value);
                            var rt = (performance.now() - startTime) / 1000;

                            if (scaleType === 'Value') {
                                currentRatings.value_rating = rating;
                                currentRatings.value_rt = rt;
                            } else {
                                currentRatings.liking_rating = rating;
                                currentRatings.liking_rt = rt;
                            }
                            scaleOrder += scaleType;

                            jsPsych.finishTrial({
                                trial_id: 'rating',
                                scale_type: scaleType,
                                rating: rating,
                                rt_seconds: rt
                            });
                        });
                    }
                },

                fixation,

                // Inter-rating prompt (within only) or rest with countdown
                {
                    type: jsPsychHtmlKeyboardResponse,
                    stimulus: '<div id="post-content" style="text-align:center;color:#fff;"></div>',
                    choices: 'NO_KEYS',
                    trial_duration: null,
                    data: { trial_id: 'post' },
                    on_load: function () {
                        scaleCounter++;
                        var isRest = (scaleCounter >= ratingsPerTrial);
                        var duration = isRest ? CONFIG.REST_MS : CONFIG.INTER_RATING_MS;
                        var container = document.getElementById('post-content');

                        if (isRest) {
                            container.innerHTML =
                                '<div id="rest-counter" style="font-size:64px;margin-bottom:16px;"></div>' +
                                '<div style="font-size:22px;">Rest period before advancing<br>to next object...</div>';
                            var counterEl = document.getElementById('rest-counter');
                            var restStart = performance.now();
                            (function tick() {
                                var remaining = Math.max(0, (duration - (performance.now() - restStart)) / 1000);
                                counterEl.textContent = remaining.toFixed(1);
                                if (remaining > 0) requestAnimationFrame(tick);
                            })();
                        } else {
                            container.innerHTML =
                                '<div style="font-size:22px;">Now please answer<br>the next separate question</div>';
                        }

                        setTimeout(function () { jsPsych.finishTrial({ trial_id: 'post' }); }, duration);
                    }
                }
            ],
            timeline_variables: (CONFIG.RATING_DESIGN === 'within')
                ? [{ scale_type: 'Value' }, { scale_type: 'Liking' }]
                : [{ scale_type: assignedCondition }],
            randomize_order: (CONFIG.RATING_DESIGN === 'within')
        },

        // SAVE — combined summary row for this trial
        {
            type: jsPsychCallFunction,
            func: function () {},
            on_finish: function (data) {
                var t = trialList[trialIndex];
                data.trial_id = 'trial_summary';
                data.trial = trialIndex + 1;
                data.effort_level = t.effortLevel;
                data.steps_required = currentWalk.steps_required;
                data.effort_trial_duration = currentWalk.effort_trial_duration;
                data.presses_rejected = currentWalk.presses_rejected;
                data.walk_focus_lost = currentWalk.walk_focus_lost;
                data.walk_blur_ms = currentWalk.walk_blur_ms;
                data.reveal_seed = currentWalk.reveal_seed;
                data.stimulus_load_error = currentWalk.stimulus_load_error;
                data.walk_error = currentWalk.walk_error;
                data.img = 's_obj' + t.objIndex;
                data.color_variant = t.colorVariant;
                data.value_rating = currentRatings.value_rating;
                data.value_rt = currentRatings.value_rt;
                data.liking_rating = currentRatings.liking_rating;
                data.liking_rt = currentRatings.liking_rt;
                data.scale_order = scaleOrder;
                trialIndex++;
            }
        },

        fixation
    ],

    loop_function: function () {
        return trialIndex < trialList.length;
    }
};

timeline.push(trialProcedure);

// ======================== MEMORY CHECK ========================

timeline.push({
    type: jsPsychHtmlButtonResponse,
    stimulus:
        '<div style="max-width:700px;margin:0 auto;text-align:center;line-height:1.7;">' +
        '<h3>MEMORY CHECK</h3>' +
        '<p>You will now see a set of objects.</p>' +
        '<p>Click on the ones you saw during the experiment, then press <b>Done</b>.</p></div>',
    choices: ['Begin'],
    data: { trial_id: 'memory_check_instructions' }
});

timeline.push({
    type: jsPsychHtmlKeyboardResponse,
    stimulus: function () {
        var gridSize = 150;
        var cols = 5;
        var html = '<div style="text-align:center;">' +
            '<p style="font-size:20px;margin-bottom:20px;color:#fff;">Click on the objects you saw during the experiment</p>' +
            '<div id="mem-grid" style="display:inline-grid;grid-template-columns:repeat(' + cols + ',1fr);gap:16px;margin:0 auto;">';

        for (var i = 0; i < memoryCheckList.length; i++) {
            var item = memoryCheckList[i];
            html += '<div class="mem-item" data-idx="' + i + '" ' +
                'style="width:' + gridSize + 'px;height:' + gridSize + 'px;overflow:hidden;' +
                'border:4px solid transparent;cursor:pointer;transition:border-color 0.15s;' +
                stimBgCSS(item.objIndex, item.colorVariant, gridSize) + '"></div>';
        }

        html += '</div>' +
            '<button id="mem-done-btn" style="margin-top:24px;padding:12px 40px;font-size:16px;' +
            'cursor:pointer;background:#555;color:#fff;border:1px solid #888;">Done</button>' +
            '</div>';
        return html;
    },
    choices: 'NO_KEYS',
    trial_duration: null,
    data: { trial_id: 'memory_check' },
    on_load: function () {
        var selected = {};
        var startTime = performance.now();

        var items = document.querySelectorAll('.mem-item');
        items.forEach(function (el) {
            el.addEventListener('click', function () {
                var idx = parseInt(el.getAttribute('data-idx'), 10);
                if (selected[idx]) {
                    delete selected[idx];
                    el.style.borderColor = 'transparent';
                } else {
                    selected[idx] = true;
                    el.style.borderColor = '#00dd44';
                }
            });
        });

        document.getElementById('mem-done-btn').addEventListener('click', function () {
            var rt = (performance.now() - startTime) / 1000;
            var selectedIndices = Object.keys(selected).map(Number);
            var hits = 0, falseAlarms = 0, misses = 0, correctRejections = 0;
            var selectedObjects = [];

            for (var i = 0; i < memoryCheckList.length; i++) {
                var isSelected = selectedIndices.indexOf(i) !== -1;
                var isOld = memoryCheckList[i].isOld;
                if (isOld && isSelected) hits++;
                else if (isOld && !isSelected) misses++;
                else if (!isOld && isSelected) falseAlarms++;
                else correctRejections++;
                if (isSelected) selectedObjects.push('s_obj' + memoryCheckList[i].objIndex);
            }

            var allObjects = memoryCheckList.map(function (m) { return 's_obj' + m.objIndex; });

            jsPsych.finishTrial({
                trial_id: 'memory_check',
                mem_objects_shown: JSON.stringify(allObjects),
                mem_selected: JSON.stringify(selectedObjects),
                mem_hits: hits,
                mem_false_alarms: falseAlarms,
                mem_correct_rejections: correctRejections,
                mem_misses: misses,
                mem_rt: rt
            });
        });
    }
});

// ======================== END SCREEN ========================

timeline.push({
    type: jsPsychHtmlButtonResponse,
    stimulus:
        '<div style="text-align:center;max-width:600px;margin:0 auto;line-height:1.8;">' +
        '<p style="font-size:20px;">This phase of the study has ended.</p>' +
        '<p>Press <b>[Continue]</b> to go to the short final survey. You must complete it to ' +
        'receive your ' + COMPLETION_TERM + ', so please do not close this window.</p></div>',
    choices: ['Continue'],
    data: { trial_id: 'end' }
});

// ======================== PAVLOVIA FINISH ========================

if (!CONFIG.DEBUG) {
    timeline.push({ type: jsPsychPavlovia, command: 'finish' });
}

// ======================== START ========================

document.addEventListener('DOMContentLoaded', function () {
    jsPsych.run(timeline);
});
