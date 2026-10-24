/**
 * Wait & Rating Experiment (jsPsych 7 + Pavlovia) — pure-wait (temporal-cost) variant
 *
 * Effort is eliminated entirely: instead of a cognitive task, each trial is a
 * passive WAIT of varying duration (the "temporal cost"). After the wait the
 * participant rates one object on either value (price) or liking. This isolates
 * temporal cost from effort and from the preview/target perceptual gap.
 *
 * Wait duration varies within-subjects across the 18 trials (low / medium /
 * high = 10 / 20 / 40 s, ratio 2x). Two between-subjects factors:
 *   - preview: 'preview'   = object shown for PREVIEW_MS at trial start, then
 *                            hidden (fixation cross) for the wait, re-shown at rating;
 *              'nopreview' = only the fixation cross during the wait; object
 *                            first appears on the rating screen.
 *   - rating:  Value (price) vs Liking, one type per participant.
 *
 * Stimuli: Brady et al. (2013) color-rotated object image strips (s_obj*.jpg).
 *
 * Daisy chain: Prolific -> Pavlovia -> Qualtrics -> Prolific
 * URL params forwarded to Qualtrics: PROLIFIC_PID, STUDY_ID, SESSION_ID, experiment_name, id
 *                       (experiment_name falls back to 'ratingclicks2_cognitive_nopreview_wait')
 * URL overrides:        condition (value|liking), preview (preview|nopreview), debug
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

    // ---- level definition ----
    // Three within-subjects levels. Effort is eliminated: the only thing that
    // varies across levels is the passive WAIT duration (the temporal cost).
    EFFORT_LEVEL_KEYS: ['low', 'medium', 'high'],
    WAIT_LEVELS: {
        low:    { seconds: 10, label: 'Short wait'  },
        medium: { seconds: 20, label: 'Medium wait' },
        high:   { seconds: 40, label: 'Long wait'   }
    },

    // Timing (ms)
    FIXATION_MS: 500,
    PREVIEW_MS: 4000,            // object visibility at trial start (preview condition only)
    RATING_IMG_VISIBLE_MS: 4000, // object visibility on the rating screen before fade
    INTER_RATING_MS: 1000,       // pause between the two ratings (within design only)
    LOADING_MS: 3000,            // sham "Loading next trial" loading bar between trials

    // Display (px)
    RATING_IMG_SIZE: 250,        // also used for the preview image

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

    // Guided sampling: set to an array of {condition, preview} cells to direct
    // participants into specific cells for balanced coverage. Hash picks which
    // cell deterministically. Set to null for normal random assignment
    // (independent hash bits -> approx. balanced across all 4 cells).
    GUIDED_CELLS: null,

    // Recruitment / redirect — Prolific -> Pavlovia -> Qualtrics -> Prolific.
    // Pavlovia forwards the Prolific params to Qualtrics (for data linkage); the
    // Qualtrics end-of-survey redirect points back to Prolific with the study's
    // fixed completion code (configured in Qualtrics, not here).
    RECRUITMENT: 'prolific',
    QUALTRICS_URL: 'https://ucsd.co1.qualtrics.com/jfe/form/SV_cGgIcJboo4CGMCy'
};

// ======================== URL PARAMS ========================

var urlParams = new URLSearchParams(window.location.search);
if (urlParams.get('debug') === '1') CONFIG.DEBUG = true;
var conditionOverride = urlParams.get('condition');
var previewOverride   = (urlParams.get('preview') || '').toLowerCase();

var PARTICIPANT = {
    // Prolific identifiers (forwarded to Qualtrics for data linkage)
    prolific_pid:    urlParams.get('PROLIFIC_PID') || '',
    study_id:        urlParams.get('STUDY_ID') || '',
    session_id:      urlParams.get('SESSION_ID') || '',
    // Pavlovia/data participant id = Prolific PID (falls back to a generic ?id=)
    id:              urlParams.get('PROLIFIC_PID') || urlParams.get('id') || '',
    experiment_name: urlParams.get('experiment_name') || 'ratingclicks2_cognitive_nopreview_wait'
};

// ======================== STATE ========================

var pHash = 0;
var assignedCondition = '';
var assignedPreview   = '';
var trialList = [];
var trialIndex = 0;
var scaleCounter = 0;
var scaleOrder = '';
var currentEffortData = {};
var currentRatings = {};
var memoryCheckList = [];

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
        var level = levels[idx];
        return {
            objIndex: obj,
            colorVariant: colorVariant(obj, hash),
            effortLevel: level,
            waitSeconds: CONFIG.WAIT_LEVELS[level].seconds
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
    // Forward Prolific identifiers to Qualtrics for data linkage; the Qualtrics
    // end-of-survey redirect returns the participant to Prolific (fixed cc code).
    return CONFIG.QUALTRICS_URL +
        '?PROLIFIC_PID=' + encodeURIComponent(PARTICIPANT.prolific_pid) +
        '&STUDY_ID=' + encodeURIComponent(PARTICIPANT.study_id) +
        '&SESSION_ID=' + encodeURIComponent(PARTICIPANT.session_id) +
        '&experiment_name=' + encodeURIComponent(PARTICIPANT.experiment_name) +
        '&id=' + encodeURIComponent(PARTICIPANT.id);
}

// ======================== DERIVED STATE ========================

pHash = hashCode(PARTICIPANT.id || String(Math.random()));

// Guided sampling: when active, pick a target cell deterministically from the hash.
// URL overrides still take precedence. Set GUIDED_CELLS to null to disable.
var guidedCell = null;
if (CONFIG.GUIDED_CELLS && CONFIG.GUIDED_CELLS.length > 0
    && !conditionOverride && previewOverride === '') {
    guidedCell = CONFIG.GUIDED_CELLS[pHash % CONFIG.GUIDED_CELLS.length];
}

// Rating-condition assignment (between-subjects; bit 0 of pHash)
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

// Preview assignment (between-subjects; bit 1 of pHash, independent of condition on bit 0)
if (previewOverride === 'preview' || previewOverride === 'nopreview') {
    assignedPreview = previewOverride;
} else if (guidedCell) {
    assignedPreview = guidedCell.preview;
} else {
    assignedPreview = (Math.floor(pHash / 2) % 2 === 0) ? 'preview' : 'nopreview';
}

// ======================== jsPSYCH INIT ========================

var jsPsych = initJsPsych({
    display_element: 'jspsych-display',
    on_finish: function () {
        if (CONFIG.DEBUG) {
            jsPsych.data.get().localSave('csv', 'ratingclicks2_cognitive_nopreview_wait_' + (PARTICIPANT.id || 'debug') + '.csv');
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

        jsPsych.data.addProperties({
            id: PARTICIPANT.id,
            experiment_name: PARTICIPANT.experiment_name,
            recruitment: CONFIG.RECRUITMENT,
            prolific_pid: PARTICIPANT.prolific_pid,
            study_id: PARTICIPANT.study_id,
            session_id: PARTICIPANT.session_id,
            date: new Date().toISOString(),
            effort_task: 'wait',
            preview_assignment: assignedPreview,
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
        // Preload trial objects and memory-check foils.
        var urls = trialList.map(function (t) { return getStimulusUrl(t.objIndex); });
        memoryCheckList.forEach(function (m) {
            if (!m.isOld) urls.push(getStimulusUrl(m.objIndex));
        });
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
    '<p>You will be shown a series of objects, one at a time. Before you can access ' +
    'and rate each object, you will need to wait for different lengths of time.</p>';

instructionsHTML +=
    '<p>At the start of each trial, the object may or may not be shown briefly before ' +
    'the wait begins. When the wait is over, the object will appear on the screen and ' +
    'you can rate it.</p>';

if (CONFIG.RATING_DESIGN === 'within') {
    instructionsHTML +=
        '<p>For each object you will be asked two separate questions about it. ' +
        'Use the scale presented on the screen to answer these questions. Please move the mouse ' +
        'to indicate your opinion.</p>' +
        '<p>One question will ask you to indicate the value that you think the object has. The ' +
        'other will ask how much you like or dislike it. These two questions are separate so ' +
        'please consider them independently.</p>';
} else {
    instructionsHTML +=
        '<p>For each object you will be asked a question about it. Use the scale ' +
        'presented on the screen to answer. Please move the mouse to indicate your opinion.</p>';
}

instructionsHTML +=
    '<p>Use the mouse to navigate the scale and confirm your choice. After each rating, ' +
    'there is a brief pause before the next trial begins.</p>' +
    '<p>After the main task you will be redirected to a brief survey about your experience.</p>' +
    '</div>';

timeline.push({
    type: jsPsychHtmlButtonResponse,
    stimulus: instructionsHTML,
    choices: ['Next'],
    data: { trial_id: 'instructions' }
});

// ======================== FIXATION ========================

var fixation = {
    type: jsPsychHtmlKeyboardResponse,
    stimulus: '<div style="font-size:48px;color:#fff;">+</div>',
    choices: 'NO_KEYS',
    trial_duration: CONFIG.FIXATION_MS,
    data: { trial_id: 'fixation' }
};

// ======================== WAIT SCENE BUILDERS ========================

/** Centered fixation cross (same markup as the standalone fixation trial). */
function fixationCrossHTML() {
    return '<div style="font-size:48px;color:#fff;">+</div>';
}

/** Preview screen: the object shown centered (preview condition only). */
function buildPreviewHTML(trial) {
    return '<div style="text-align:center;">' +
        '<div style="margin:20px auto;display:inline-block;">' +
        stimDiv(trial.objIndex, trial.colorVariant, CONFIG.RATING_IMG_SIZE) +
        '</div></div>';
}

/**
 * Wait screen shown for the trial's wait duration: a fixation cross with a
 * neutral "Waiting to access the object" caption whose animated dots signal an
 * ongoing wait without showing remaining time (no timer / no clock-watching).
 * Wording is condition-agnostic: in preview the same object reappears, in
 * nopreview it appears for the first time — either way the participant is
 * waiting to access it.
 */
function buildWaitHTML() {
    return '<div style="text-align:center;">' +
        fixationCrossHTML() +
        '<div class="wait-caption">Waiting to access the object' +
        '<span class="wait-dots"><span>.</span><span>.</span><span>.</span></span></div>' +
        '</div>';
}

// ======================== WAIT TRIAL FACTORY ========================

/**
 * Builds a single jsPsych trial: a passive WAIT of the trial's duration, shown
 * as a fixation cross with a neutral caption (no effort, no task). The wait IS
 * the temporal-cost manipulation. Records the wait duration and a passive
 * attention probe (tab/window focus loss) into currentEffortData.
 */
function makeWaitTrial(getTrialData, dataTrialId) {
    var startMs = 0;
    var focusLost = 0;
    var blurMs = 0;
    var blurStart = 0;
    var listeners = [];

    function markBlur() {
        if (blurStart === 0) { blurStart = performance.now(); focusLost++; }
    }
    function markFocus() {
        if (blurStart > 0) { blurMs += performance.now() - blurStart; blurStart = 0; }
    }
    function onVisibility() { if (document.hidden) markBlur(); else markFocus(); }
    function addL(target, type, fn) {
        target.addEventListener(type, fn);
        listeners.push([target, type, fn]);
    }
    function removeAll() {
        listeners.forEach(function (l) { l[0].removeEventListener(l[1], l[2]); });
        listeners = [];
    }

    return {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: function () { return buildWaitHTML(); },
        choices: 'NO_KEYS',
        trial_duration: function () { return getTrialData().waitSeconds * 1000; },
        data: { trial_id: dataTrialId },
        on_load: function () {
            startMs = performance.now();
            focusLost = 0; blurMs = 0; blurStart = 0;
            addL(window, 'blur', markBlur);
            addL(window, 'focus', markFocus);
            addL(document, 'visibilitychange', onVisibility);
        },
        on_finish: function () {
            markFocus(); // close any open blur interval
            removeAll();
            var trial = getTrialData();
            currentEffortData.wait_seconds          = trial.waitSeconds;
            currentEffortData.effort_trial_duration = (performance.now() - startMs) / 1000;
            currentEffortData.wait_focus_lost       = focusLost;
            currentEffortData.wait_blur_ms          = Math.round(blurMs);
        }
    };
}

// ======================== PREVIEW TRIAL FACTORY ========================

/** Object preview shown for PREVIEW_MS at trial start (preview condition only). */
function makePreviewTrial(getTrialData, dataTrialId) {
    return {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: function () { return buildPreviewHTML(getTrialData()); },
        choices: 'NO_KEYS',
        trial_duration: CONFIG.PREVIEW_MS,
        data: { trial_id: dataTrialId }
    };
}

// ======================== CONDITION INTRO (between only) ========================

if (CONFIG.RATING_DESIGN === 'between') {
    var condIntroText = (assignedCondition === 'Value')
        ? 'For each object shown, you will be asked to indicate the value that you ' +
          'think the object has. Please use the slider to indicate your answer.'
        : 'For each object shown, you will be asked to indicate how much you like or ' +
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

        // FIXATION before the (optional) preview and the wait
        fixation,

        // PREVIEW (preview condition only): object shown briefly, then hidden
        {
            timeline: [
                makePreviewTrial(function () { return trialList[trialIndex]; }, 'preview')
            ],
            conditional_function: function () { return assignedPreview === 'preview'; }
        },

        // WAIT — the temporal-cost manipulation (fixation cross held for the wait)
        makeWaitTrial(
            function () { return trialList[trialIndex]; },
            'wait'
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

                // Post-rating: inter-rating prompt (within only) or a sham
                // "Loading next trial" loading bar between trials.
                {
                    type: jsPsychHtmlKeyboardResponse,
                    stimulus: '<div id="post-content" style="text-align:center;color:#fff;"></div>',
                    choices: 'NO_KEYS',
                    trial_duration: null,
                    data: { trial_id: 'post' },
                    on_load: function () {
                        scaleCounter++;
                        var isSpacer = (scaleCounter >= ratingsPerTrial);
                        var duration = isSpacer ? CONFIG.LOADING_MS : CONFIG.INTER_RATING_MS;
                        var container = document.getElementById('post-content');

                        if (isSpacer) {
                            container.innerHTML =
                                '<div class="loading-label">Loading next trial…</div>' +
                                '<div class="loading-track"><div class="loading-fill" id="loading-fill"></div></div>';
                            var fill = document.getElementById('loading-fill');
                            var start = performance.now();
                            (function tick() {
                                var p = Math.min(1, (performance.now() - start) / duration);
                                if (fill) fill.style.width = (p * 100) + '%';
                                if (p < 1) requestAnimationFrame(tick);
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
