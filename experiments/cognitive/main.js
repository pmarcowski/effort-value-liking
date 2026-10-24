/**
 * Effort & Rating Experiment (jsPsych 7 + Pavlovia)
 *
 * Object-collection task. Each trial pairs one object image with a fixed number
 * of mental operations the participant must perform to "collect" it; after
 * collection the participant rates the object on either value or liking.
 *
 * Effort task is one of two cognitive variants (cogletters / subtractn),
 * assigned between-subjects. Liking vs Value rating is also between-subjects.
 * Effort magnitude (1 / 3 / 7) varies within-subjects across the 18 trials.
 *
 * Stimuli: Brady et al. (2013) color-rotated object image strips (s_obj*.jpg).
 *
 * Daisy chain: Sona -> Pavlovia -> Qualtrics -> Sona
 * URL params forwarded: id, experiment_id, experiment_name, credit_token
 *                       (experiment_name falls back to 'ratingclicks2_cognitive' if not in URL)
 * URL overrides:        condition (value|liking), effort_task (cogletters|subtractn), debug
 */

// ======================== CONFIGURATION ========================

var CONFIG = {
    DEBUG: false,

    // Stimuli — Brady et al. (2013) strips: s_obj{N}.jpg
    STIMULI_RANGE: [1, 540],
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
            foils: [
                32,  79,  95,  122, 123, 142, 157, 160, 161, 164,
                185, 201, 203, 211, 214, 216, 224, 228, 229, 232
            ]
        }
    },
    N_STIMULI: 18,               // per participant; balanced across the 3 effort levels (6 of each)
    N_COLOR_VARIANTS: 12,
    STRIP_WIDTH_PX: 1895,
    STRIP_HEIGHT_PX: 155,
    STIMULI_PATH: 'stimuli/',

    // ---- canonical effort definition (shared across cogletters + subtractn) ----
    EFFORT_LEVEL_KEYS: ['low', 'medium', 'high'],
    EFFORT_LEVELS: {
        low:    { magnitude: 1, label: 'Low effort'    },
        medium: { magnitude: 3, label: 'Medium effort' },
        high:   { magnitude: 7, label: 'High effort'   }
    },
    OPERATIONS_PER_TRIAL: 5,     // 5 stimuli (cogletters) or 5 steps (subtractn) per object

    // Task-specific knobs
    COGLETTERS: { KEYS_PER_STIMULUS: 2, MAX_OFFSET: 7, FLASH_OK_MS: 80, FLASH_ERR_MS: 140 },
    SUBTRACTN:  { FLASH_MS: 220 },

    // Countdown stage (matches taskload variants)
    COUNTDOWN_SECONDS:     5,
    COUNTDOWN_INTERVAL_MS: 1000,
    COUNTDOWN_START_DELAY: 500,

    // Timing (ms)
    FIXATION_MS: 500,
    EFFORT_PREVIEW_MS: 4000,     // object visibility at the start of each effort trial; then fades to an empty frame
    RATING_IMG_VISIBLE_MS: 4000, // object visibility on the rating screen before fade (matches preview window)
    INTER_RATING_MS: 1000,       // pause between ratings (within only)
    REST_MS: 15000,              // rest after ratings
    COMPLETE_HOLD_MS: 600,       // brief pause after task completion before advancing

    // Display (px)
    EFFORT_IMG_SIZE: 220,
    RATING_IMG_SIZE: 250,

    // Rating scale
    SCALE_MIN: 1,
    SCALE_MAX: 5,
    SCALE_STEP: 0,               // 0 = continuous (step 0.01), >0 = discrete
    SCALE_SHOW_TICKS: true,

    // Memory check
    N_MEMORY_OLD: 5,
    N_MEMORY_NEW: 5,

    // Design
    RATING_DESIGN: 'between',    // 'within' (both Value + Liking) or 'between' (one per participant)

    // Guided sampling: set to an array of {task, condition} cells to direct
    // participants into specific cells for balanced coverage. Hash picks which
    // cell deterministically. Set to null for normal random assignment.
    GUIDED_CELLS: null,

    // Redirect
    QUALTRICS_URL: 'https://ucsd.co1.qualtrics.com/jfe/form/SV_6J1fEpvNX3nMgYJ'
};

// ======================== URL PARAMS ========================

var urlParams = new URLSearchParams(window.location.search);
if (urlParams.get('debug') === '1') CONFIG.DEBUG = true;
var conditionOverride = urlParams.get('condition');
var taskOverride      = (urlParams.get('effort_task') || '').toLowerCase();

var PARTICIPANT = {
    id:              urlParams.get('id') || '',
    experiment_id:   urlParams.get('experiment_id') || '',
    experiment_name: urlParams.get('experiment_name') || 'ratingclicks2_cognitive',
    credit_token:    urlParams.get('credit_token') || ''
};

// ======================== STATE ========================

var pHash = 0;
var assignedCondition = '';
var assignedTask      = '';
var trialList = [];
var trialIndex = 0;
var scaleCounter = 0;
var scaleOrder = '';
var currentEffortData = {};
var currentRatings = {};
var memoryCheckList = [];
var practiceIndex = 0;

// ======================== HELPERS — GENERAL ========================

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

/** Balanced effort sequence: blocks of k levels, each block a random permutation, no back-to-back. */
function buildEffortSequence(n) {
    var levels = CONFIG.EFFORT_LEVEL_KEYS.slice();
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

function colorVariant(objIdx, hash) {
    return hashCode(String(hash) + '_' + String(objIdx)) % CONFIG.N_COLOR_VARIANTS;
}

function getStimulusUrl(objIdx) {
    return CONFIG.STIMULI_PATH + 's_obj' + objIdx + '.jpg';
}

/** CSS background properties to clip one color variant from a strip image. */
function stimBgCSS(objIdx, variant, size) {
    var scale = size / CONFIG.STRIP_HEIGHT_PX;
    var vw = CONFIG.STRIP_WIDTH_PX / CONFIG.N_COLOR_VARIANTS;
    return 'background-image:url(\'' + getStimulusUrl(objIdx) + '\');' +
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

// ======================== HELPERS — TASK-SPECIFIC ========================

function letterAtOffset(ch, offset) {
    return String.fromCharCode(ch.charCodeAt(0) + offset);
}

function expectedSequenceFor(ch, offset) {
    return [ch, letterAtOffset(ch, offset)];
}

/** Random unique letters from 'a..s' (so source + MAX_OFFSET never wraps past 'z'). */
function randomSource(length) {
    var pool = 'abcdefghijklmnopqrstuvwxyz'.slice(0, 26 - CONFIG.COGLETTERS.MAX_OFFSET).split('');
    for (var i = pool.length - 1; i > 0; i--) {
        var j = Math.floor(Math.random() * (i + 1));
        var tmp = pool[i]; pool[i] = pool[j]; pool[j] = tmp;
    }
    return pool.slice(0, length).join('');
}

/** Random 4-digit start such that all `steps` subtractions of `n` keep the result in [1000, 9999]. */
function randomStart(n, steps) {
    var minStart = 1000 + n * steps;
    var maxStart = 9999;
    return Math.floor(Math.random() * (maxStart - minStart + 1)) + minStart;
}

/** Render the cogletters source strip; completed letters drop out as cursor advances. */
function renderStrip(stripEl, state) {
    if (!stripEl) return;
    var html = '';
    for (var i = state.cursor; i < CONFIG.OPERATIONS_PER_TRIAL; i++) {
        var ch = state.source[i];
        var cls = 'src-char';
        cls += (i === state.cursor) ? ' current' : ' upcoming';
        html += '<span class="' + cls + '">' + ch + '</span>';
    }
    stripEl.innerHTML = html;
}

/** Brief flash on the current letter; null clearAfterMs leaves it until renderStrip replaces it. */
function flashCurrent(stripEl, cls, clearAfterMs) {
    if (!stripEl) return;
    var cur = stripEl.querySelector('.src-char.current');
    if (!cur) return;
    cur.classList.add(cls);
    if (clearAfterMs != null) {
        setTimeout(function () { cur.classList.remove(cls); }, clearAfterMs);
    }
}

// ======================== HELPERS — WALKER PROGRESS BAR ========================

/** Stick-figure SVG; `frame` indexes a sinusoidal limb pose for a per-step stride. */
function figureSVG(frame) {
    var t = (frame % 11) / 11 * Math.PI * 2;
    var leg = Math.sin(t) * 14;
    var arm = Math.sin(t + Math.PI) * 11;
    return '<svg viewBox="0 0 30 60" preserveAspectRatio="xMidYMid meet">' +
        '<circle cx="15" cy="7" r="6" fill="none" stroke="#fff" stroke-width="2"/>' +
        '<line x1="15" y1="13" x2="15" y2="36" stroke="#fff" stroke-width="2"/>' +
        '<line x1="15" y1="22" x2="' + (15 + arm) + '" y2="32" stroke="#fff" stroke-width="2"/>' +
        '<line x1="15" y1="22" x2="' + (15 - arm) + '" y2="32" stroke="#fff" stroke-width="2"/>' +
        '<line x1="15" y1="36" x2="' + (15 + leg) + '" y2="56" stroke="#fff" stroke-width="2"/>' +
        '<line x1="15" y1="36" x2="' + (15 - leg) + '" y2="56" stroke="#fff" stroke-width="2"/>' +
        '</svg>';
}

/**
 * Walk-scene markup mirroring `ratingclicks2/` physical: figure on a ground
 * line, goal frame on the right. `opts` toggles which goal-area elements are
 * rendered so practice trials can hide the real object and show a downward
 * arrow at the arrival spot instead.
 *
 * opts = { showObject, showFrame, showArrow }   (all default to non-practice)
 */
function buildWalkSceneHTML(trial, opts) {
    opts = opts || {};
    var showObject = opts.showObject !== false;
    var showFrame  = opts.showFrame  !== false;
    var showArrow  = !!opts.showArrow;
    var goalSize   = CONFIG.EFFORT_IMG_SIZE;

    var html = '<div class="walk-scene" id="walk-scene">';
    if (showFrame) {
        html += '<div class="walk-goal-frame"></div>';
    }
    if (showObject) {
        html += '<div class="walk-img" id="walk-img" style="' +
                stimBgCSS(trial.objIndex, trial.colorVariant, goalSize) + '"></div>';
    }
    if (showArrow) {
        html += '<div class="walk-goal-arrow">' +
                  '<svg viewBox="0 0 48 64" preserveAspectRatio="xMidYMid meet">' +
                    '<polygon points="4,4 44,4 24,60" fill="#fff"/>' +
                  '</svg>' +
                '</div>';
    }
    html += '<div class="walk-ground"></div>';
    html += '<div class="walk-figure" id="progress-figure">' + figureSVG(0) + '</div>';
    html += '</div>';
    return html;
}

function updateProgressBar(completed, total) {
    var figure = document.getElementById('progress-figure');
    if (!figure) return;
    // Track geometry matches the .walk-scene CSS: 900 wide, figure 30, goal
    // frame 220 with 30 right-padding. Figure arrives with its right edge
    // flush against the goal's left edge on the final step.
    var TRACK = 900, FIG_W = 30, GOAL_W = CONFIG.EFFORT_IMG_SIZE, RIGHT_PAD = 30;
    var travel = TRACK - FIG_W - GOAL_W - RIGHT_PAD;
    figure.style.left = (Math.min(completed, total) / total * travel) + 'px';
    // Rest pose on arrival so the figure ends standing upright (not mid-stride).
    figure.innerHTML = figureSVG(completed >= total ? 0 : completed);
}

// ======================== TRIAL & MEMORY LIST BUILDERS ========================

function buildTrialList(hash) {
    var activePool = getActiveStimuliPool();
    var pool;
    if (activePool.whitelist && activePool.whitelist.length > 0) {
        pool = activePool.whitelist.slice();
    } else {
        pool = [];
        for (var i = CONFIG.STIMULI_RANGE[0]; i <= CONFIG.STIMULI_RANGE[1]; i++) pool.push(i);
    }
    if (pool.length < CONFIG.N_STIMULI) {
        console.warn('Stimulus pool (' + pool.length + ') smaller than N_STIMULI (' + CONFIG.N_STIMULI + ')');
    }
    var selected = shuffle(pool).slice(0, CONFIG.N_STIMULI);
    var levels = buildEffortSequence(CONFIG.N_STIMULI);

    return selected.map(function (obj, idx) {
        var level     = levels[idx];
        var magnitude = CONFIG.EFFORT_LEVELS[level].magnitude;
        return {
            objIndex: obj,
            colorVariant: colorVariant(obj, hash),
            effortLevel: level,
            magnitude: magnitude,
            taskPayload: (assignedTask === 'cogletters')
                ? { source: randomSource(CONFIG.OPERATIONS_PER_TRIAL) }
                : { startNumber: randomStart(magnitude, CONFIG.OPERATIONS_PER_TRIAL) }
        };
    });
}

function buildMemoryCheckList(trialList, hash) {
    var activePool = getActiveStimuliPool();
    var seenItems = trialList.map(function (t) {
        return { objIndex: t.objIndex, colorVariant: t.colorVariant, isOld: true };
    });
    var oldItems = shuffle(seenItems).slice(0, CONFIG.N_MEMORY_OLD);

    var foilPool;
    if (activePool.foils && activePool.foils.length > 0) {
        foilPool = activePool.foils.slice();
    } else {
        var seenIndices = trialList.map(function (t) { return t.objIndex; });
        var sourcePool = (activePool.whitelist && activePool.whitelist.length > 0)
            ? activePool.whitelist : [];
        if (sourcePool.length === 0) {
            for (var i = CONFIG.STIMULI_RANGE[0]; i <= CONFIG.STIMULI_RANGE[1]; i++) sourcePool.push(i);
        }
        foilPool = sourcePool.filter(function (idx) {
            return seenIndices.indexOf(idx) === -1;
        });
    }
    var foilSelected = shuffle(foilPool).slice(0, CONFIG.N_MEMORY_NEW);
    var newItems = foilSelected.map(function (idx) {
        return { objIndex: idx, colorVariant: colorVariant(idx, hash), isOld: false };
    });

    return shuffle(oldItems.concat(newItems));
}

function buildRedirectURL() {
    return CONFIG.QUALTRICS_URL +
        '?experiment_id=' + encodeURIComponent(PARTICIPANT.experiment_id) +
        '&credit_token=' + encodeURIComponent(PARTICIPANT.credit_token) +
        '&experiment_name=' + encodeURIComponent(PARTICIPANT.experiment_name) +
        '&id=' + encodeURIComponent(PARTICIPANT.id);
}

// ======================== DERIVED STATE ========================

pHash = hashCode(PARTICIPANT.id || String(Math.random()));

// Guided sampling: when active, pick a target cell deterministically from the hash.
// URL overrides still take precedence. Set GUIDED_CELLS to null to disable.
var guidedCell = null;
if (CONFIG.GUIDED_CELLS && CONFIG.GUIDED_CELLS.length > 0
    && !conditionOverride && taskOverride === '') {
    guidedCell = CONFIG.GUIDED_CELLS[pHash % CONFIG.GUIDED_CELLS.length];
}

// Rating-condition assignment (between-subjects)
if (CONFIG.RATING_DESIGN === 'between') {
    if (conditionOverride && conditionOverride.toLowerCase() === 'value') {
        assignedCondition = 'Value';
    } else if (conditionOverride && conditionOverride.toLowerCase() === 'liking') {
        assignedCondition = 'Liking';
    } else if (guidedCell) {
        assignedCondition = guidedCell.condition;
    } else {
        assignedCondition = (pHash % 2 === 0) ? 'Value' : 'Liking';
    }
}
var ratingsPerTrial = (CONFIG.RATING_DESIGN === 'within') ? 2 : 1;

// Task assignment (between-subjects; uses bit 1 of pHash for independence from condition on bit 0)
if (taskOverride === 'cogletters' || taskOverride === 'subtractn') {
    assignedTask = taskOverride;
} else if (guidedCell) {
    assignedTask = guidedCell.task;
} else {
    assignedTask = (Math.floor(pHash / 2) % 2 === 0) ? 'cogletters' : 'subtractn';
}

// ======================== jsPSYCH INIT ========================

var jsPsych = initJsPsych({
    display_element: 'jspsych-display',
    on_finish: function () {
        if (CONFIG.DEBUG) {
            jsPsych.data.get().localSave('csv', 'ratingclicks2_cognitive_' + (PARTICIPANT.id || 'debug') + '.csv');
        } else {
            window.location.href = buildRedirectURL();
        }
    }
});

var timeline = [];

// ======================== PAVLOVIA INIT ========================

// Fix: Pavlovia plugin (2022.1.1) stores beforeunload callback as a local
// variable in _init(), never assigning it to PavloviaPlugin._beforeunloadCallback.
// This means _finish() silently fails to remove the listener, causing Chrome to
// show a "Leave site?" dialog on redirect — which strands the participant.
// We intercept addEventListener once to capture the reference, then restore it.
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

timeline.push({
    type: jsPsychCallFunction,
    func: function () {
        trialList = buildTrialList(pHash);
        memoryCheckList = buildMemoryCheckList(trialList, pHash);
        trialIndex = 0;
        practiceIndex = 0;

        jsPsych.data.addProperties({
            id: PARTICIPANT.id,
            experiment_name: PARTICIPANT.experiment_name,
            experiment_id: PARTICIPANT.experiment_id,
            date: new Date().toISOString(),
            effort_task: assignedTask,
            n_trials: CONFIG.N_STIMULI,
            active_stimuli_pool: CONFIG.ACTIVE_STIMULI_POOL,
            rating_design: CONFIG.RATING_DESIGN,
            assigned_condition: assignedCondition || 'both'
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
        var urls = trialList.map(function (t) { return getStimulusUrl(t.objIndex); });
        memoryCheckList.forEach(function (m) {
            if (!m.isOld) urls.push(getStimulusUrl(m.objIndex));
        });
        PRACTICE_OBJ_INDICES.forEach(function (idx) { urls.push(getStimulusUrl(idx)); });
        urls = Array.from(new Set(urls));
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

var instructionsHTML =
    '<div style="max-width:700px;margin:0 auto;text-align:left;line-height:1.7;">' +
    '<h3 style="text-align:center;">TASK INSTRUCTIONS</h3>' +
    '<p>Thank you for agreeing to participate in today\'s experiment!</p>' +
    '<p>Please make sure to read all following instructions carefully.</p>' +
    '<p>You will be asked to collect a number of objects. Each object is associated ' +
    'with a different level of effort, i.e., number of mental operations needed to ' +
    'collect it.</p>' +
    '<p>When an object appears, you will see it briefly at the start of the trial. ' +
    'The object then hides, leaving an empty frame on the screen — try to keep the ' +
    'object in mind while you continue collecting it.</p>' +
    '<p>Collect each object by performing all necessary operations. When the required ' +
    'number of operations is reached, you will proceed to the next one. Try performing ' +
    'the operations as quickly and accurately as you can.</p>';

if (CONFIG.RATING_DESIGN === 'within') {
    instructionsHTML +=
        '<p>After each object is collected you will be asked two separate questions about it. ' +
        'Use the scale presented on the screen to answer these questions. Please move the mouse ' +
        'to indicate your opinion.</p>' +
        '<p>One question will ask you to indicate the value that you think the object has. The ' +
        'other will ask how much you like or dislike it. These two questions are separate so ' +
        'please consider them independently.</p>';
} else {
    instructionsHTML +=
        '<p>After each object is collected you will be asked a question about it. Use the scale ' +
        'presented on the screen to answer. Please move the mouse to indicate your opinion.</p>';
}

instructionsHTML +=
    '<p>Use the mouse to navigate the scale and confirm your choice. There will be a ' +
    '15-second rest period following the collection and rating of each object.</p>' +
    '<p>After the main task you will be redirected to a brief survey about your experience.</p>' +
    '</div>';

timeline.push({
    type: jsPsychHtmlButtonResponse,
    stimulus: instructionsHTML,
    choices: ['Next'],
    data: { trial_id: 'instructions' }
});

// Task-specific second screen — describes the mechanic of the assigned variant.
var taskInstructionsHTML;
if (assignedTask === 'cogletters') {
    taskInstructionsHTML =
        '<div style="max-width:700px;margin:0 auto;text-align:left;line-height:1.7;">' +
        '<h3 style="text-align:center;">HOW TO COLLECT EACH OBJECT</h3>' +
        '<p>On each trial you will see ' + CONFIG.OPERATIONS_PER_TRIAL + ' letters in a row. ' +
        'For every letter you must type:</p>' +
        '<ol style="margin:8px 0 8px 20px;">' +
        '<li>the letter itself, and</li>' +
        '<li>the letter that sits a fixed number of positions later in the alphabet.</li>' +
        '</ol>' +
        '<p>That fixed number is the <b>effort level</b> for that object — it stays the same ' +
        'within a trial but changes across trials. You will encounter three levels:</p>' +
        '<ul style="margin:8px 0 8px 20px;">' +
        '<li><b>Low</b> (+1): if you see <code>a</code>, type <code>a</code> then <code>b</code>.</li>' +
        '<li><b>Medium</b> (+3): if you see <code>a</code>, type <code>a</code> then <code>d</code>.</li>' +
        '<li><b>High</b> (+7): if you see <code>a</code>, type <code>a</code> then <code>h</code>.</li>' +
        '</ul>' +
        '<p>An alphabet reference is shown beside the letters during each trial. Correct ' +
        'keystrokes flash briefly; mistakes flash and do not advance the cursor. The object is ' +
        'collected once you complete all ' + CONFIG.OPERATIONS_PER_TRIAL + ' letters.</p>' +
        '</div>';
} else {
    taskInstructionsHTML =
        '<div style="max-width:700px;margin:0 auto;text-align:left;line-height:1.7;">' +
        '<h3 style="text-align:center;">HOW TO COLLECT EACH OBJECT</h3>' +
        '<p>On each trial you will see a 4-digit code. For every step you must:</p>' +
        '<ol style="margin:8px 0 8px 20px;">' +
        '<li>subtract a fixed number from the code shown, and</li>' +
        '<li>type the result and press <b>[Enter]</b>.</li>' +
        '</ol>' +
        '<p>If your answer is correct, the displayed code updates to that new value and you ' +
        'continue subtracting from it. If your answer is incorrect, the input flashes briefly ' +
        'and the displayed code does not change — try again from the same code.</p>' +
        '<p><b>Do this in your head.</b> Please do not use a calculator, paper, or any other aid.</p>' +
        '<p>That fixed number is the <b>effort level</b> for that object — it stays the same ' +
        'within a trial but changes across trials. You will encounter three levels:</p>' +
        '<ul style="margin:8px 0 8px 20px;">' +
        '<li><b>Low</b> (-1): subtract 1 from the code shown.</li>' +
        '<li><b>Medium</b> (-3): subtract 3 from the code shown.</li>' +
        '<li><b>High</b> (-7): subtract 7 from the code shown.</li>' +
        '</ul>' +
        '<p>The object is collected once you complete ' + CONFIG.OPERATIONS_PER_TRIAL + ' correct ' +
        'subtractions.</p>' +
        '</div>';
}

timeline.push({
    type: jsPsychHtmlButtonResponse,
    stimulus: taskInstructionsHTML,
    choices: ['Next'],
    data: { trial_id: 'task_instructions' }
});

// Practice intro screen
timeline.push({
    type: jsPsychHtmlButtonResponse,
    stimulus:
        '<div style="max-width:600px;margin:0 auto;text-align:center;line-height:1.7;">' +
        '<p style="font-size:20px;">You will now complete a few practice trials.</p>' +
        '<p>Use them to familiarise yourself with the task.</p></div>',
    choices: ['Begin'],
    data: { trial_id: 'practice_intro' }
});

// ======================== FIXATION ========================

var fixation = {
    type: jsPsychHtmlKeyboardResponse,
    stimulus: '<div style="font-size:48px;color:#fff;">+</div>',
    choices: 'NO_KEYS',
    trial_duration: CONFIG.FIXATION_MS,
    data: { trial_id: 'fixation' }
};

// ======================== EFFORT SCENE BUILDERS ========================

/** Countdown stage shown before each effort trial. */
function buildCountdownHTML(trial) {
    var blurb;
    if (assignedTask === 'cogletters') {
        blurb = 'For each letter, type it AND the letter ' + trial.magnitude +
                ' position' + (trial.magnitude === 1 ? '' : 's') + ' later.';
    } else {
        blurb = 'Subtract ' + trial.magnitude + ' from the code shown.';
    }
    return '<div id="stage">' +
        '<div class="ready">Ready?</div>' +
        '<div class="countdown" id="cd">' + CONFIG.COUNTDOWN_SECONDS + '</div>' +
        '<div class="hint"><strong>' + blurb + '</strong></div>' +
        '</div>';
}

function buildCogletterTaskHTML(trial, isPractice) {
    // Trial-screen prompt matches the warmup blurb so the participant sees
    // the exact same wording during the countdown and the active task.
    var prompt = 'For each letter, type it AND the letter ' + trial.magnitude +
                 ' position' + (trial.magnitude === 1 ? '' : 's') + ' later.';

    var rowLengths = [6, 4, 6, 4, 6];
    var letterIdx = 0;
    var gridRows = rowLengths.map(function (len) {
        var cells = '';
        for (var i = 0; i < len; i++) {
            cells += '<div class="alphabet-cell">' + String.fromCharCode(97 + letterIdx++) + '</div>';
        }
        return '<div class="alphabet-row">' + cells + '</div>';
    }).join('');

    var sceneOpts = {
        showObject: !isPractice,
        showFrame:  !isPractice,
        showArrow:   isPractice
    };

    // 3-column grid (1fr auto 1fr) places the trial-center in the middle
    // column — truly page-centered because equal 1fr flanks balance it. The
    // alphabet sits at the start of the right 1fr column, so it appears
    // just to the right of the centered prompt+letters with a fixed gap.
    return buildWalkSceneHTML(trial, sceneOpts) +
        '<div style="display:grid;grid-template-columns:1fr auto 1fr;column-gap:24px;align-items:start;">' +
            '<div></div>' +
            '<div class="trial-center">' +
                '<div class="effort-title">' + prompt + '</div>' +
                '<div class="source-wrap"><div class="source-strip" id="cog-strip"></div></div>' +
            '</div>' +
            '<div class="alphabet-grid" style="justify-self:start;">' + gridRows + '</div>' +
        '</div>';
}

function buildSubtractTaskHTML(trial, isPractice) {
    // Trial-screen prompt matches the warmup blurb. The "Press [Enter]..."
    // mechanic hint is placed directly under the input field so it sits next
    // to where the participant types — not buried at the top of the column.
    var prompt = 'Subtract ' + trial.magnitude + ' from the code shown.';
    var addl   = 'Press [Enter] after each answer.<br>' +
                 'Each correct answer updates the code for the next step.';

    var sceneOpts = {
        showObject: !isPractice,
        showFrame:  !isPractice,
        showArrow:   isPractice
    };

    return buildWalkSceneHTML(trial, sceneOpts) +
        '<div style="display:flex;justify-content:center;">' +
            '<div class="trial-center">' +
                '<div class="effort-title">' + prompt + '</div>' +
                '<div class="running-number" id="running">' + trial.taskPayload.startNumber + '</div>' +
                '<input type="text" inputmode="numeric" autocomplete="off" ' +
                    'maxlength="4" class="step-input" id="step-input" ' +
                    'aria-label="Your answer">' +
                '<div class="task-hint">' + addl + '</div>' +
            '</div>' +
        '</div>';
}

function buildEffortHTML(trial, isPractice) {
    var taskHTML = (assignedTask === 'cogletters')
        ? buildCogletterTaskHTML(trial, isPractice)
        : buildSubtractTaskHTML(trial, isPractice);
    var practiceHeader = isPractice
        ? '<div style="text-align:center;font-size:12px;letter-spacing:0.1em;text-transform:uppercase;color:#9aa0a6;margin-bottom:12px;">Practice</div>'
        : '';
    return '<div style="max-width:920px;margin:0 auto;">' +
        practiceHeader +
        taskHTML +
        '</div>';
}

// ======================== TASK RUNNERS ========================

/**
 * Returns a handle: { cleanup, getSummary }. Calls onComplete() when the
 * participant finishes all OPERATIONS_PER_TRIAL stimuli before the cap.
 */
function runCogletterTask(trial, onComplete, getElapsedMs) {
    var stripEl = document.getElementById('cog-strip');
    var state = {
        source:     trial.taskPayload.source,
        offset:     trial.magnitude,
        cursor:     0,
        subCursor:  0,
        active:     true,
        keystrokes: []
    };
    renderStrip(stripEl, state);
    updateProgressBar(0, CONFIG.OPERATIONS_PER_TRIAL);

    var pendingTimeouts = [];
    function schedule(delayMs, fn) {
        var id = setTimeout(fn, delayMs);
        pendingTimeouts.push(id);
    }

    function keyHandler(e) {
        if (!state.active) return;
        if (e.ctrlKey || e.metaKey || e.altKey) return;
        var k = (e.key || '').toLowerCase();
        if (!/^[a-z]$/.test(k)) return;
        e.preventDefault();

        var stimChar = state.source[state.cursor];
        var expected = expectedSequenceFor(stimChar, state.offset)[state.subCursor];
        var match    = (k === expected);

        state.keystrokes.push({
            t:        Math.round(getElapsedMs()),
            key:      k,
            expected: expected,
            stimChar: stimChar,
            subStep:  state.subCursor,
            match:    match
        });

        if (!match) {
            flashCurrent(stripEl, 'err', CONFIG.COGLETTERS.FLASH_ERR_MS);
            return;
        }

        flashCurrent(stripEl, 'ok', null);
        schedule(CONFIG.COGLETTERS.FLASH_OK_MS, function () {
            if (!state.active) return;
            state.subCursor++;
            if (state.subCursor >= CONFIG.COGLETTERS.KEYS_PER_STIMULUS) {
                state.subCursor = 0;
                state.cursor++;
                updateProgressBar(state.cursor, CONFIG.OPERATIONS_PER_TRIAL);
                if (state.cursor >= CONFIG.OPERATIONS_PER_TRIAL) {
                    state.active = false;
                    onComplete();
                    return;
                }
            }
            renderStrip(stripEl, state);
        });
    }
    window.addEventListener('keydown', keyHandler);

    return {
        cleanup: function () {
            state.active = false;
            pendingTimeouts.forEach(function (id) { clearTimeout(id); });
            pendingTimeouts = [];
            window.removeEventListener('keydown', keyHandler);
        },
        getSummary: function () {
            var matches    = state.keystrokes.filter(function (k) { return  k.match; }).length;
            var mismatches = state.keystrokes.filter(function (k) { return !k.match; }).length;
            return {
                effort_source:          state.source,
                effort_matches:         matches,
                effort_mismatches:      mismatches,
                operations_completed:   state.cursor,
                operations_total:       CONFIG.OPERATIONS_PER_TRIAL,
                effort_completed:       state.cursor >= CONFIG.OPERATIONS_PER_TRIAL ? 1 : 0,
                effort_trial_duration:  getElapsedMs() / 1000,
                effort_keystrokes:      JSON.stringify(state.keystrokes)
            };
        }
    };
}

/**
 * Returns a handle: { cleanup, getSummary }. Calls onComplete() when the
 * participant correctly answers OPERATIONS_PER_TRIAL subtractions before the cap.
 */
function runSubtractTask(trial, onComplete, getElapsedMs) {
    var inputEl   = document.getElementById('step-input');
    var runningEl = document.getElementById('running');
    var state = {
        n:           trial.magnitude,
        startNumber: trial.taskPayload.startNumber,
        displayed:   trial.taskPayload.startNumber,
        stepIdx:     0,
        active:      true,
        steps:       [],
        currentStep: null
    };
    updateProgressBar(0, CONFIG.OPERATIONS_PER_TRIAL);

    var flashTimer = null;

    function beginStep() {
        state.currentStep = {
            stepIdx:           state.stepIdx,
            canonicalExpected: state.startNumber - state.n * (state.stepIdx + 1),
            displayedBefore:   state.displayed,
            stepStartMs:       Math.round(getElapsedMs()),
            attempts:          [],
            firstCorrectMs:    null
        };
    }

    function flashInputNeutral() {
        if (!inputEl) return;
        inputEl.classList.add('flash');
        if (flashTimer) clearTimeout(flashTimer);
        flashTimer = setTimeout(function () {
            if (inputEl) inputEl.classList.remove('flash');
            flashTimer = null;
        }, CONFIG.SUBTRACTN.FLASH_MS);
    }

    function submitAttempt() {
        if (!state.active || !inputEl) return;
        var raw   = inputEl.value;
        var value = raw === '' ? null : parseInt(raw, 10);
        var t     = Math.round(getElapsedMs());

        var canonicalExpected = state.currentStep.canonicalExpected;
        var localExpected     = state.displayed - state.n;
        var canonicalCorrect  = value === canonicalExpected;
        var localCorrect      = value === localExpected;

        var prevAttempts       = state.currentStep.attempts;
        var prevAttempt        = prevAttempts[prevAttempts.length - 1] || null;
        var prevAttemptCorrect = (prevAttempt && prevAttempt.value !== null && value !== null)
            ? value === prevAttempt.value - state.n
            : null;

        var dt_step_ms = t - state.currentStep.stepStartMs;
        var dt_prev_ms = prevAttempt ? t - prevAttempt.t : null;
        var attemptIdx = prevAttempts.length;

        state.currentStep.attempts.push({
            attemptIdx:         attemptIdx,
            t:                  t,
            dt_step_ms:         dt_step_ms,
            dt_prev_ms:         dt_prev_ms,
            raw:                raw,
            value:              value,
            canonicalCorrect:   canonicalCorrect,
            localCorrect:       localCorrect,
            prevAttemptCorrect: prevAttemptCorrect
        });

        if (canonicalCorrect) {
            var firstAttempt = state.currentStep.attempts[0];
            state.currentStep.firstCorrectMs        = t;
            state.currentStep.attemptsToCorrect     = state.currentStep.attempts.length;
            state.currentStep.firstAttemptCorrect   = firstAttempt.canonicalCorrect;
            state.currentStep.timeToFirstAttempt_ms = firstAttempt.dt_step_ms;
            state.currentStep.timeToCorrect_ms      = t - state.currentStep.stepStartMs;
            state.steps.push(state.currentStep);

            state.displayed = canonicalExpected;
            state.stepIdx++;
            if (runningEl) runningEl.textContent = state.displayed;
            updateProgressBar(state.stepIdx, CONFIG.OPERATIONS_PER_TRIAL);
            inputEl.value = '';
            inputEl.classList.remove('flash');

            if (state.stepIdx >= CONFIG.OPERATIONS_PER_TRIAL) {
                state.active = false;
                onComplete();
            } else {
                beginStep();
                inputEl.focus();
            }
        } else {
            flashInputNeutral();
            inputEl.value = '';
            inputEl.focus();
        }
    }

    function onKeydown(e) {
        if (!state.active) return;
        if (e.key === 'Enter') {
            e.preventDefault();
            submitAttempt();
            return;
        }
        if (e.ctrlKey || e.metaKey || e.altKey) return;
        var allowed = ['Backspace', 'Delete', 'ArrowLeft', 'ArrowRight', 'Home', 'End', 'Tab'];
        if (allowed.indexOf(e.key) !== -1) return;
        if (/^[0-9]$/.test(e.key)) return;
        e.preventDefault();
    }

    function onInput(e) {
        var cleaned = e.target.value.replace(/\D/g, '');
        if (cleaned.length > 4) cleaned = cleaned.slice(0, 4);
        if (cleaned !== e.target.value) e.target.value = cleaned;
        if (e.target.classList.contains('flash')) e.target.classList.remove('flash');
    }

    if (inputEl) {
        inputEl.addEventListener('keydown', onKeydown);
        inputEl.addEventListener('input',   onInput);
        inputEl.focus();
    }
    beginStep();

    return {
        cleanup: function () {
            state.active = false;
            if (flashTimer) { clearTimeout(flashTimer); flashTimer = null; }
            if (inputEl) {
                inputEl.removeEventListener('keydown', onKeydown);
                inputEl.removeEventListener('input',   onInput);
            }
        },
        getSummary: function () {
            var totalAttempts = state.steps.reduce(function (sum, step) {
                return sum + step.attempts.length;
            }, 0);
            var incorrectAttempts = state.steps.reduce(function (sum, step) {
                return sum + step.attempts.filter(function (a) { return !a.canonicalCorrect; }).length;
            }, 0);
            var firstAttemptCorrect = state.steps.filter(function (step) {
                return step.attempts.length > 0 && step.attempts[0].canonicalCorrect;
            }).length;
            // The current-but-incomplete step (if any) also contributed attempts.
            if (state.currentStep && state.currentStep.attempts.length > 0 &&
                state.steps.indexOf(state.currentStep) === -1) {
                totalAttempts     += state.currentStep.attempts.length;
                incorrectAttempts += state.currentStep.attempts.length;
            }
            return {
                effort_start_number:           state.startNumber,
                effort_first_attempt_correct:  firstAttemptCorrect,
                effort_total_attempts:         totalAttempts,
                effort_incorrect_attempts:     incorrectAttempts,
                operations_completed:          state.stepIdx,
                operations_total:              CONFIG.OPERATIONS_PER_TRIAL,
                effort_completed:              state.stepIdx >= CONFIG.OPERATIONS_PER_TRIAL ? 1 : 0,
                effort_trial_duration:         getElapsedMs() / 1000,
                effort_steps:                  JSON.stringify(state.steps)
            };
        }
    };
}

// ======================== EFFORT TRIAL FACTORY ========================

/**
 * Builds a single jsPsych trial: countdown -> effort scene -> finish on completion.
 *
 * No time limit — the trial only ends when the participant completes all
 * OPERATIONS_PER_TRIAL operations. The object image is shown for the first
 * EFFORT_PREVIEW_MS milliseconds and then fades, leaving an empty frame.
 */
function makeEffortTrial(getTrialData, isPractice, dataTrialId) {
    return {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: function () { return buildCountdownHTML(getTrialData()); },
        choices: 'NO_KEYS',
        trial_duration: null,
        data: { trial_id: dataTrialId },
        on_load: function () {
            var trial   = getTrialData();
            var cdEl    = document.getElementById('cd');
            var n       = CONFIG.COUNTDOWN_SECONDS;
            var timeouts = [];
            var finished = false;
            var trialStart = 0;
            var taskHandle = null;

            function elapsedMs() { return trialStart > 0 ? performance.now() - trialStart : 0; }

            function schedule(delayMs, fn) {
                var id = setTimeout(fn, delayMs);
                timeouts.push(id);
                return id;
            }

            function clearAllTimers() {
                timeouts.forEach(function (id) { clearTimeout(id); });
                timeouts = [];
            }

            function finalizeTrial() {
                if (finished) return;
                finished = true;
                clearAllTimers();
                if (taskHandle && taskHandle.cleanup) taskHandle.cleanup();

                var summary = (taskHandle && taskHandle.getSummary)
                    ? taskHandle.getSummary()
                    : {
                        operations_completed:  0,
                        operations_total:      CONFIG.OPERATIONS_PER_TRIAL,
                        effort_completed:      0,
                        effort_trial_duration: elapsedMs() / 1000
                    };
                Object.keys(summary).forEach(function (k) { currentEffortData[k] = summary[k]; });

                setTimeout(function () { jsPsych.finishTrial(); }, CONFIG.COMPLETE_HOLD_MS);
            }

            function startTask() {
                if (finished) return;
                var displayRoot = document.querySelector('.jspsych-content');
                if (!displayRoot) { finalizeTrial(); return; }
                displayRoot.innerHTML = buildEffortHTML(trial, isPractice);
                trialStart = performance.now();

                // Brief object preview, then fade — the empty frame stays as
                // a placeholder while the participant collects the object.
                // (Practice trials render no #walk-img, so the lookup returns null and
                // the opacity update is skipped — the down-arrow stays visible throughout.)
                schedule(CONFIG.EFFORT_PREVIEW_MS, function () {
                    var img = document.getElementById('walk-img');
                    if (img) img.style.opacity = '0';
                });

                taskHandle = (assignedTask === 'cogletters')
                    ? runCogletterTask(trial, finalizeTrial, elapsedMs)
                    : runSubtractTask(trial,  finalizeTrial, elapsedMs);
            }

            // Countdown: COUNTDOWN_SECONDS -> 0 -> COUNTDOWN_START_DELAY -> startTask
            (function tick() {
                if (cdEl) cdEl.textContent = n;
                if (n === 0) {
                    schedule(CONFIG.COUNTDOWN_START_DELAY, startTask);
                    return;
                }
                n--;
                schedule(CONFIG.COUNTDOWN_INTERVAL_MS, tick);
            })();
        }
    };
}

// ======================== PRACTICE TRIALS ========================

var PRACTICE_OBJ_INDICES = [72, 266, 425];

var practiceTrials = CONFIG.EFFORT_LEVEL_KEYS.map(function (level, idx) {
    var magnitude = CONFIG.EFFORT_LEVELS[level].magnitude;
    var objIdx    = PRACTICE_OBJ_INDICES[idx];
    return {
        objIndex: objIdx,
        colorVariant: colorVariant(objIdx, pHash),
        effortLevel: level,
        magnitude: magnitude,
        taskPayload: (assignedTask === 'cogletters')
            ? { source: randomSource(CONFIG.OPERATIONS_PER_TRIAL) }
            : { startNumber: randomStart(magnitude, CONFIG.OPERATIONS_PER_TRIAL) }
    };
});

var practiceProcedure = {
    timeline: [
        fixation,
        makeEffortTrial(
            function () { return practiceTrials[practiceIndex]; },
            true,
            'practice_effort'
        ),
        {
            type: jsPsychCallFunction,
            func: function () {},
            on_finish: function (data) {
                var t = practiceTrials[practiceIndex];
                data.trial_id      = 'practice_summary';
                data.practice_trial = practiceIndex + 1;
                data.effort_level  = t.effortLevel;
                data.magnitude     = t.magnitude;
                data.img           = 's_obj' + t.objIndex;
                data.color_variant = t.colorVariant;
                Object.keys(currentEffortData).forEach(function (k) { data[k] = currentEffortData[k]; });
                currentEffortData = {};
                practiceIndex++;
            }
        }
    ],
    loop_function: function () {
        return practiceIndex < practiceTrials.length;
    }
};

timeline.push(practiceProcedure);

// Practice -> main transition
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
          'think the object has. Please use the slider to indicate your answer.'
        : 'For each object you collect, you will be asked to indicate how much you like or ' +
          'dislike it. Please use the slider to indicate your answer.';

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
    var randomStartVal = Math.floor(Math.random() * (CONFIG.SCALE_MAX - CONFIG.SCALE_MIN + 1)) + CONFIG.SCALE_MIN;
    var sliderStep = CONFIG.SCALE_STEP > 0 ? CONFIG.SCALE_STEP : 0.01;
    var tickStep   = CONFIG.SCALE_STEP > 0 ? CONFIG.SCALE_STEP : 1;

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
        '" step="' + sliderStep + '" value="' + randomStartVal +
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
                currentEffortData = {};
                currentRatings = {
                    value_rating: null, value_rt: null,
                    liking_rating: null, liking_rt: null
                };
            }
        },

        // EFFORT
        makeEffortTrial(
            function () { return trialList[trialIndex]; },
            false,
            'effort'
        ),

        // RATINGS LOOP
        {
            timeline: [
                fixation,

                // Rating screen
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

                        // Fade the clear image after the rating-viewing window elapses
                        setTimeout(function () {
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

                // Post-rating: inter-rating prompt (within only) or rest with countdown
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
                data.trial_id      = 'trial_summary';
                data.trial         = trialIndex + 1;
                data.effort_level  = t.effortLevel;
                data.magnitude     = t.magnitude;
                data.img           = 's_obj' + t.objIndex;
                data.color_variant = t.colorVariant;
                Object.keys(currentEffortData).forEach(function (k) { data[k] = currentEffortData[k]; });
                data.value_rating  = currentRatings.value_rating;
                data.value_rt      = currentRatings.value_rt;
                data.liking_rating = currentRatings.liking_rating;
                data.liking_rt     = currentRatings.liking_rt;
                data.scale_order   = scaleOrder;
                trialIndex++;
            }
        },

        // Fixation between trials
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
                var idx = parseInt(el.getAttribute('data-idx'));
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
        '<p>You will now be redirected to a brief survey about your experience. ' +
        'Please complete it to finish the study and receive credit.</p>' +
        '<p>Press <b>[Continue]</b> to proceed.</p></div>',
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
