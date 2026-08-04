;;; resurface.el --- Resurface material for rereading and review  -*- lexical-binding: t; -*-
;; Author: vmargb
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: notes, spaced-repetition, org, feynman
;; URL: https://github.com/vmargb/resurface.el

;;; Commentary:
;;
;; resurface.el brings material back for review on a schedule, instead of
;; leaving it to whenever you happen to remember it exists.  It offers two
;; independent strategies, pick whichever fits what you're reviewing:
;;
;;   - Leitner  (`resurface-leitner'): whole note files, resurfaced
;;     on a schedule driven by a rolling FAMILIARITY signal rather than a
;;     single pass/fail rating, see the ./docs/ for more info
;;   - Incremental Reading (`resurface-ir', resurface-ir.el): a priority
;;     queue of whole sources (PDFs, EPUBs, web pages) you read a little
;;     of each day, passages you select while reading are captured,
;;     reworded, and filed as ordinary Leitner cards under that source's
;;     group.  Reuses the same scheduler as resurface-leitner.
;;
;; Both share one JSON index file, FILES ARE NEVER MODIFIED, all
;; scheduling metadata lives externally in `resurface-index-file'.
;;
;; See `resurface--leitner-decide' and `resurface--choose-next-review'
;; for the two functions this all runs through.
;;
;; Quick start -- Leitner (whole files):
;;   M-x resurface-leitner                  Open the group dashboard
;;   M-x resurface-leitner-add-group        Add a new group
;;   M-x resurface-leitner-add-file         Add current buffer's file to a group
;;   M-x resurface-leitner-start-session    Review all due files (C-u: one group)
;;   M-x resurface-leitner-review-graduated Browse graduated files (C-u: one group)
;;
;; Quick start -- Incremental Reading (sources + capture, resurface-ir.el):
;;   M-x resurface-ir                       Open the source-queue dashboard
;;   M-x resurface-ir-add-source            Register a PDF/EPUB/URL, set its priority
;;   M-x resurface-ir-start-session         Open today's sources by priority, one at a time
;;   M-x resurface-ir-capture               Reword the active selection into a Leitner card
;;   M-x resurface-ir-review-finished       Browse finished sources, reactivate any of them
;;
;; Every mode has a `?' binding for its keymap.  See README.org for the full workflow
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'json)
(require 'tabulated-list)


;; ===========================================================================
;;  Customisation

(defgroup resurface nil
  "Familiarity-based spaced repetition for note files."
  :group 'applications
  :prefix "leitner-")

(defcustom resurface-index-file
  (expand-file-name "var/resurface.json" user-emacs-directory)
  "Path to the JSON file storing all review metadata.
Note files are never touched, so all state lives here."
  :type 'file
  :group 'resurface)

(defcustom resurface-leitner-intervals [1 3 7 14 30 60 90]
  "Ideal review interval (days) for each Leitner box.
These are targets the scheduler aims for, not exact deadlines,
`resurface-leitner-interval-tolerance'."
  :type '(vector integer)
  :group 'resurface)

(defcustom resurface-leitner-default-group "General"
  "Default group name when none is specified."
  :type 'string
  :group 'resurface)

(defcustom resurface-leitner-session-max-items nil
  "Maximum number of files to queue in a single review session.
Nil disables the cap every due file is queued"
  :type '(choice (const :tag "No limit" nil)
                  (integer :tag "Max items per session"))
  :group 'resurface)

;; enable things like olivetti, writetoom etc...
(defcustom resurface-leitner-before-review-hook nil
  "Hook run right after a note file is revealed in a review session.")

(defcustom resurface-leitner-after-session-hook nil
  "Hook run immediately after a review session is fully completed.")

;; ---------------------------------------------------------------------
;;  Familiarity Scheduler tuning

(defcustom resurface-leitner-ema-alpha-min 0.15
  "Smallest EMA smoothing weight.
used when a rating arrives right after the last one,
little new evidence, since nothing has been forgotten yet
`resurface--leitner-alpha'."
  :type 'float
  :group 'resurface)

(defcustom resurface-leitner-ema-alpha-max 0.65
  "Largest EMA smoothing weight.
used when a rating arrives well after the item's ideal intervals
strong evidence either way, having survived or failed a long gap
`resurface--leitner-alpha'."
  :type 'float
  :group 'resurface)

(defcustom resurface-leitner-min-reviews 3
  "Base number of ratings needed since the last box move before a Box 1
item can move again.  Scaled down for higher boxes and scaled up when
recent ratings have been inconsistent, `resurface--leitner-required-reviews'."
  :type 'integer
  :group 'resurface)

(defcustom resurface-leitner-min-reviews-box-decay 0.25
  "Fraction shaved off the required-reviews count per box above Box 1.
0.25 means Box 2 needs 75% of Box 1's requirement, Box 3 needs 75% of
that.  Higher boxes have already proven themselves
repeatedly and shouldn't have to re-prove it as hard."
  :type 'float
  :group 'resurface)

(defcustom resurface-leitner-variance-weight 1.0
  "How strongly rating inconsistency inflates the required-reviews count.
Variance is normalised to [0, 1], a value of 1.0 means maximally
inconsistent ratings can up to double how much evidence is required
before this item's box is allowed to move."
  :type 'float
  :group 'resurface)

(defcustom resurface-leitner-promote-threshold 0.80
  "Confidence needed to promote a Box 1 item by one box.
Promoting from the last box graduates the item instead.  The
effective threshold decays for higher boxes,
`resurface--leitner-promote-threshold' and
`resurface-leitner-threshold-decay'."
  :type 'float
  :group 'resurface)

(defcustom resurface-leitner-threshold-decay 0.15
  "Exponential decay rate of the promotion threshold per box above Box 1.
Higher boxes have already earned trust and need less confidence to
advance further, `resurface--leitner-promote-threshold'."
  :type 'float
  :group 'resurface)

(defcustom resurface-leitner-promote-threshold-floor 0.30
  "Lower bound the decayed promotion threshold is never allowed below.
Keeps very high boxes from becoming trivially easy to promote out of."
  :type 'float
  :group 'resurface)

(defcustom resurface-leitner-demote-threshold -0.40
  "Confidence at or below where an item should be demoted.
How many boxes it drops is adaptive, `resurface--leitner-demote-drop'
and `resurface-leitner-demote-max-drop'."
  :type 'float
  :group 'resurface)

(defcustom resurface-leitner-demote-max-drop 3
  "Most boxes a single demotion can drop an item by.
A confidence right at `resurface-leitner-demote-threshold' always
drops exactly 1 box, severity then scales up to this cap as confidence
approaches the theoretical floor of -1.0, `resurface--leitner-demote-drop'."
  :type 'integer
  :group 'resurface)

(defcustom resurface-leitner-interval-tolerance 0.20
  "Fractional slack allowed around a box's ideal interval, each way.
0.20 on a 30-day box permits scheduling anywhere from 24 to 36
days out, whichever balances workload best."
  :type 'float
  :group 'resurface)

(defcustom resurface-leitner-workload-weight 0.2
  "Weight of same-day review load against deviation from the ideal date.
Spacing cost is normalised to [0, 1] across the tolerance window,
`resurface--leitner-choose-next-review' Higher values push
harder toward quiet days, 0 disables workload-awareness entirely."
  :type 'float
  :group 'resurface)

;; ===========================================================================
;;  Internal State
;;
;; resurface--data  =  ((:groups . HASH-TABLE)  (:dirty . BOOL))
;;   HASH-TABLE  : group-name (string) -> group-alist
;;   group-alist : ((:name . STRING) (:items . LIST-OF-ITEM-ALISTS))
;;   item-alist  : ((:path . STRING) (:box . INT)
;;                  (:confidence . FLOAT)   ; EMA in [-1, +1], O(1) to update
;;                  (:variance . FLOAT)     ; EMA of squared error, in [0, 1]
;;                  (:reviews-since-transition . INT) ; since last box move
;;                  (:last-reviewed . INT) (:next-review . INT)
;;                  (:added . INT)
;;                  (:graduated . INT-OR-NIL)   ; Unix ts when graduated
;;                  (:paused . BOOL))           ; t after a Partial rating
;;
;; :confidence and :variance are the whole state, two floats per item
;; regardless of how long it's existed, updated in O(1) per review
;; instead of a rolling history array that has to be rescanned.
;;
;; resurface--data  also carries a SECOND top-level key, :ir-sources, used
;; by Incremental Reading mode (`resurface-ir.el', `resurface-enable-ir-mode').
;; this file keeps the raw JSON sexp under :ir-sources-raw so a round-trip
;; save/load never drops it just because that optional file wasn't required.
;; dispatch is in `resurface--data->json-sexp'/`resurface--json-sexp->data'.

(defvar resurface--data nil
  "In-memory Resurface index (Leitner groups).
Nil until first initialisation.")

;; :queue (group-name . item-alist) pairs left to review, :reviewed count,
;; :total count, :group-filter string-or-nil
(defvar resurface--leitner-session nil
  "Active review session state, or nil when idle.")


;; ====================================================================
;;  Small Utilities

(defun resurface--now ()
  "Return the current time as a Unix timestamp (integer)."
  (floor (float-time)))

(defun resurface--day-number (ts)
  "Return the epoch-day number containing Unix timestamp TS."
  (floor ts 86400))

(defun resurface--leitner-num-boxes ()
  "Return the number of Leitner boxes."
  (length resurface-leitner-intervals))

(defun resurface--leitner-box-days (box)
  "Ideal review interval in days for BOX (1-indexed)."
  (aref resurface-leitner-intervals (1- box)))

(defun resurface--format-ts (ts)
  "Format Unix timestamp TS as YYYY-MM-DD, or \"Never\" for 0 or nil."
  (if (or (null ts) (= ts 0)) "Never"
    (format-time-string "%Y-%m-%d" (seconds-to-time ts))))

(defun resurface--shuffle (seq)
  "Return a shuffled copy of SEQ using fisher-yates algorithm."
  (let ((v (vconcat seq)))
    (dotimes (i (length v))
      (let ((j (+ i (random (- (length v) i)))))
        (cl-rotatef (aref v i) (aref v j))))
    (append v nil)))

(defun resurface--age-str (ts)
  "Return a short \"how long ago\" string for Unix timestamp TS."
  (let ((days (/ (- (resurface--now) ts) 86400.0)))
    (if (< days 1) "<1d" (format "%dd" (floor days)))))

;; ===========================================================================
;;  Generic Collection Helpers
;;
;; Leitner groups are, underneath, "a hash-table of id -> alist with an
;; :items list": the traversal and filtering logic over that shape doesn't
;; care which domain it's in, so it's written once here.
;; wrappers supplying their own hash-table and predicates, see
;; `resurface--leitner-due-pairs'.

(defun resurface--collection-all-pairs (table)
  "All (id . item-alist) pairs across every collection in hash-table TABLE.
TABLE maps some id to an alist containing an :items list."
  (let (result)
    (maphash (lambda (id coll)
               (dolist (item (cdr (assq :items coll)))
                 (push (cons id item) result)))
             table)
    result))

(defun resurface--collection-filter-pairs (table pred &optional match-id)
  "Pairs from TABLE whose item satisfies PRED.
When MATCH-ID is non-nil, only pairs whose id is `equal' to it are kept."
  (seq-filter
   (lambda (pair)
     (and (or (null match-id) (equal (car pair) match-id))
          (funcall pred (cdr pair))))
   (resurface--collection-all-pairs table)))

(defun resurface--collection-top-overdue-pairs (pairs overdue-fn n)
  "Return the N most overdue of PAIRS, ranked by OVERDUE-FN on each item."
  (seq-take
   (sort (copy-sequence pairs)
         (lambda (a b) (> (funcall overdue-fn (cdr a)) (funcall overdue-fn (cdr b)))))
   n))

(defun resurface--build-item (item &rest overrides)
  "Copy ITEM with OVERRIDES (a :key value plist) applied."
  (let ((new (copy-alist item)))
    (while overrides
      (let ((key (pop overrides)) (val (pop overrides)))
        (if (assq key new) (setcdr (assq key new) val)
          (push (cons key val) new))))
    new))


;; ===========================================================================
;;  Scheduling Engine
;;
;; Two pieces, both independent: a confidence engine (an O(1) EMA
;; over ratings -> a score -> promote/demote/graduate, demotion severity
;; itself adaptive) and a day-picker (ideal interval + current workload
;; -> an actual date).  Leitner uses both directly.

(defun resurface--ema-alpha (delta-days alpha-min alpha-max tau)
  "Time-aware EMA smoothing weight for a gap of DELTA-DAYS since review.
Saturates from ALPHA-MIN (reviewed almost immediately, nothing proven
yet) toward ALPHA-MAX (reviewed after a full TAU day gap or more, real
evidence about retention either way).  Nil or non-positive DELTA-DAYS
(first ever rating, nothing to protect) yields ALPHA-MAX outright."
  (if (or (null delta-days) (<= delta-days 0))
      alpha-max
    (+ alpha-min (* (- alpha-max alpha-min)
                     (- 1.0 (exp (- (/ delta-days (max 0.0001 tau)))))))))

(defun resurface--ema-update (prev-confidence prev-variance rating alpha)
  "Return (NEW-CONFIDENCE . NEW-VARIANCE), an O(1) update of both EMAs.
RATING is +1 or -1, ALPHA the time-aware weight for this step,
Variance tracks how far RATING fell from the standing prediction,
normalised to [0, 1], and feeds `resurface--required-evidence'
so inconsistent ratings demand more evidence before a box is allowed to move."
  (let* ((err      (- rating prev-confidence))
         (new-conf (+ prev-confidence (* alpha err)))
         (new-var  (+ (* (- 1.0 alpha) prev-variance) (* alpha (/ (* err err) 4.0)))))
    (cons (max -1.0 (min 1.0 new-conf))
          (max 0.0 (min 1.0 new-var)))))

(defun resurface--required-evidence (box variance base-reviews box-decay variance-weight)
  "Minimum ratings needed since the last box move before BOX can move again.
Shrinks geometrically with BOX by BOX-DECAY (an item several boxes up
has already proven itself repeatedly) and grows with VARIANCE, scaled
by VARIANCE-WEIGHT (inconsistent recent ratings need more evidence
before the trend is trusted).  Always at least 1."
  (let* ((base   (* base-reviews (expt (- 1.0 box-decay) (max 0 (1- box)))))
         (scaled (* base (+ 1.0 (* variance-weight variance)))))
    (max 1 (round scaled))))

(defun resurface--promote-threshold (box base-threshold decay floor)
  "Confidence needed to promote out of BOX.
Decays exponentially by DECAY per box above Box 1 from BASE-THRESHOLD,
floored at FLOOR so high boxes never become trivially easy to clear."
  (max floor (* base-threshold (exp (- (* decay (max 0 (1- box))))))))

(defun resurface--demote-drop (confidence demote-threshold max-drop)
  "Boxes to drop for CONFIDENCE at or below DEMOTE-THRESHOLD.
Severity scales linearly from 1 box right at the threshold up to
MAX-DROP boxes as CONFIDENCE approaches the theoretical floor of -1.0,
so one bad session costs little but a run of failures can send an item
back near Box 1 in a single hit."
  (let* ((span      (max 0.0001 (- demote-threshold -1.0)))
         (overshoot (/ (max 0.0 (- demote-threshold confidence)) span))
         (severity  (min 1.0 overshoot)))
    (max 1 (round (+ 1.0 (* severity (1- max-drop)))))))

(defun resurface--confidence-decide
    (confidence variance reviews old-box last-box
     required-n promote-th demote-th max-drop)
  "Return (ACTION . NEW-BOX) for CONFIDENCE/VARIANCE at OLD-BOX of LAST-BOX.
REVIEWS must be at least REQUIRED-N before acting either way.  ACTION
is one of `demote', `promote', `graduate', or `remain'."
  (if (< reviews required-n)
      (cons 'remain old-box)
    (cond
     ((<= confidence demote-th)
      (cons 'demote (max 1 (- old-box (resurface--demote-drop confidence demote-th max-drop)))))
     ((>= confidence promote-th)
      (if (= old-box last-box) (cons 'graduate old-box) (cons 'promote (1+ old-box))))
     (t (cons 'remain old-box)))))

(defun resurface--day-loads (pairs due-ts-fn exclude-p)
  "Hash-table of epoch-day -> how many PAIRS are next due that day.
DUE-TS-FN maps an item to its next-review ts, or nil."
  (let ((loads (make-hash-table :test #'eql)))
    (dolist (pair pairs)
      (unless (funcall exclude-p pair)
        (let ((ts (funcall due-ts-fn (cdr pair))))
          (when (and ts (> ts 0))
            (let ((day (resurface--day-number ts)))
              (puthash day (1+ (or (gethash day loads) 0)) loads))))))
    loads))

(defun resurface--choose-next-review (ideal-days tolerance workload-weight loads)
  "Pick a Unix ts around IDEAL-DAYS out, balancing spacing against LOADS.
Considers every day within TOLERANCE of IDEAL-DAYS and prefers one both
close to ideal and lightly loaded, weighted by WORKLOAD-WEIGHT."
  (let* ((slack (max 0 (round (* ideal-days tolerance))))
         (lo    (max 1 (- ideal-days slack)))
         (hi    (+ ideal-days slack))
         (today (resurface--day-number (resurface--now)))
         best best-cost)
    (cl-loop for offset from lo to hi do
      (let* ((day     (+ today offset))
             (load    (or (gethash day loads) 0))
             (spacing (if (> slack 0) (/ (float (- offset ideal-days)) slack) 0.0))
             (cost    (+ (* spacing spacing) (* workload-weight load))))
        (when (or (null best-cost)
                  (< cost best-cost)
                  (and (= cost best-cost) best (< (abs (- offset ideal-days)) (abs (- best ideal-days)))))
          (setq best offset best-cost cost))))
    (* (+ today best) 86400)))

(defun resurface--item-due-p (finished next-review)
  "Non-nil when an item is due: not FINISHED and NEXT-REVIEW has passed."
  (and (not finished) (or (null next-review) (= next-review 0)
                           (<= next-review (resurface--now)))))

(defun resurface--item-days-until-due (next-review)
  "Days until NEXT-REVIEW.  Negative = overdue, 0 = never reviewed."
  (if (or (null next-review) (= next-review 0)) 0.0
    (/ (- next-review (resurface--now)) 86400.0)))

;; ---------------------------------------------------------------------
;;  Leitner instantiation

(defun resurface--leitner-alpha (box delta-days)
  "Time-aware EMA weight for a rating landing DELTA-DAYS after the last one.
Uses BOX's own ideal interval as the natural time-scale
`resurface--leitner-box-days', so reviewing right on schedule lands
comfortably mid-range between the alpha bounds, reviewing early barely
moves confidence, and reviewing late moves it hard."
  (resurface--ema-alpha delta-days
                         resurface-leitner-ema-alpha-min
                         resurface-leitner-ema-alpha-max
                         (float (resurface--leitner-box-days box))))

(defun resurface--leitner-required-reviews (box variance)
  "See `resurface--required-evidence', using Leitner's tuning knobs."
  (resurface--required-evidence
   box variance resurface-leitner-min-reviews
   resurface-leitner-min-reviews-box-decay resurface-leitner-variance-weight))

(defun resurface--leitner-promote-threshold (box)
  "`resurface--promote-threshold' for BOX, using Leitner's tuning knobs."
  (resurface--promote-threshold
   box resurface-leitner-promote-threshold
   resurface-leitner-threshold-decay resurface-leitner-promote-threshold-floor))

(defun resurface--leitner-demote-drop (confidence)
  "`resurface--demote-drop' using CONFIDENCE, using Leitner's tuning knobs."
  (resurface--demote-drop
   confidence resurface-leitner-demote-threshold resurface-leitner-demote-max-drop))

(defun resurface--leitner-decide (confidence variance reviews old-box last-box)
  "`resurface--confidence-decide', using Leitner's thresholds.
Threshold and required-evidence values are themselves box/variance
dependent, so they're resolved here rather than passed as constants."
  (resurface--confidence-decide
   confidence variance reviews old-box last-box
   (resurface--leitner-required-reviews old-box variance)
   (resurface--leitner-promote-threshold old-box)
   resurface-leitner-demote-threshold
   resurface-leitner-demote-max-drop))

(defun resurface--leitner-day-loads (exclude-path)
  "Day-loads across every active item except EXCLUDE-PATH."
  (resurface--day-loads
   (resurface--leitner-all-pairs)
   (lambda (item) (cdr (assq :next-review item)))
   (lambda (pair) (or (equal (cdr (assq :path (cdr pair))) exclude-path)
                       (cdr (assq :graduated (cdr pair)))))))

(defun resurface--leitner-choose-next-review (box exclude-path)
  "Pick BOX's next review date, workload-balanced, `resurface--choose-next-review'."
  (resurface--choose-next-review
   (resurface--leitner-box-days box) resurface-leitner-interval-tolerance
   resurface-leitner-workload-weight (resurface--leitner-day-loads exclude-path)))


;; ===========================================================================
;;  Item Lifecycle

(defun resurface--leitner-make-item (path)
  "Return a fresh item-alist for PATH placed in Box 1, due immediately."
  (list (cons :path          (expand-file-name path))
        (cons :box           1)
        (cons :confidence     0.0)
        (cons :variance       0.0)
        (cons :reviews-since-transition 0)
        (cons :last-reviewed 0)
        (cons :next-review   0)
        (cons :added         (resurface--now))
        (cons :graduated     nil)
        (cons :paused        nil)))

(defun resurface--leitner-item-graduated-p (item)
  "Non-nil when ITEM has been graduated (fully mastered)."
  (cdr (assq :graduated item)))

(defun resurface--leitner-item-paused-p (item)
  "Non-nil when ITEM was last rated Partial."
  (cdr (assq :paused item)))

(defun resurface--leitner-item-due-p (item)
  "Non-nil when ITEM is due.  Graduated items never are."
  (resurface--item-due-p (resurface--leitner-item-graduated-p item)
                          (cdr (assq :next-review item))))

(defun resurface--leitner-item-days-until-due (item)
  "Days until ITEM is next due; see `resurface--item-days-until-due'."
  (resurface--item-days-until-due (cdr (assq :next-review item))))

(defun resurface--leitner-item-confidence (item)
  "Return ITEM's current confidence score, a stored EMA (O(1) to read)."
  (cdr (assq :confidence item)))

(defun resurface--leitner-item-confidence-str (item)
  "ITEM's confidence as a short string, or \"—\" if never reviewed."
  (if (= (cdr (assq :last-reviewed item)) 0)
      "—"
    (format "%+.2f" (cdr (assq :confidence item)))))

(defun resurface--leitner-item-evidence-str (item)
  "ITEM's progress toward its next possible box move, e.g. \"2/3\"."
  (let* ((box  (cdr (assq :box item)))
         (var  (cdr (assq :variance item)))
         (n    (cdr (assq :reviews-since-transition item)))
         (need (resurface--leitner-required-reviews box var)))
    (format "%d/%d" (min n need) need)))

(defun resurface--leitner-item-evidence-note (item)
  "Return a plain-language sentence about ITEM's evidence state."
  (let* ((box  (cdr (assq :box item)))
         (var  (cdr (assq :variance item)))
         (n    (cdr (assq :reviews-since-transition item)))
         (need (resurface--leitner-required-reviews box var)))
    (if (< n need)
        (format "%d of %d ratings collected since the last move, %d more before this can move box"
                n need (- need n))
      (format "%d of %d ratings collected, every review re-evaluates the box now"
              n need))))

(defun resurface--leitner-outcome-label (outcome old-item new-item)
  "Human-readable line describing what OUTCOME did, from OLD-ITEM to NEW-ITEM."
  (pcase outcome
    ((or 'familiar 'unfamiliar)
     (let* ((old-box  (cdr (assq :box old-item)))
            (new-box  (cdr (assq :box new-item)))
            (grad-now (and (cdr (assq :graduated new-item))
                           (not (cdr (assq :graduated old-item)))))
            (n        (cdr (assq :reviews-since-transition new-item)))
            (need     (resurface--leitner-required-reviews
                       new-box (cdr (assq :variance new-item))))
            (verb     (if (eq outcome 'familiar) "Familiar" "Unfamiliar")))
       (cond
        (grad-now (propertize (format "%s -> Graduated! Removed from active queue." verb)
                              'face 'success))
        ((/= new-box old-box)
         (format "%s -> confidence %s crossed the line: Box %d -> %d (evidence reset)"
                 verb (resurface--leitner-item-confidence-str new-item) old-box new-box))
        ((>= n need)
         (format "%s -> confidence %s, not decisive enough, Box %d unchanged"
                 verb (resurface--leitner-item-confidence-str new-item) new-box))
        (t
         (format "%s -> still building evidence (%d/%d), Box %d unchanged"
                 verb n need new-box)))))
    ('partial (propertize "Paused, due again tomorrow" 'face 'font-lock-doc-face))
    ('revised (propertize "Revised, box unchanged" 'face 'font-lock-doc-face))
    ('skip "Skipped")))

(defun resurface--leitner-item-rate (item outcome)
  "Return a NEW item-alist for ITEM rated with OUTCOME.
OUTCOME is `familiar', `unfamiliar', `reset', `skip', `partial' (paused), or
`revised' (box untouched, but counts as a completed review today)."
  (let* ((path      (cdr (assq :path item)))
         (old-box   (cdr (assq :box item)))
         (last-box  (resurface--leitner-num-boxes))
         (last-rev  (cdr (assq :last-reviewed item)))
         (now       (resurface--now)))
    (pcase outcome
      ((or 'familiar 'unfamiliar)
       (let* ((rating     (if (eq outcome 'familiar) 1 -1))
              (delta-days (if (> last-rev 0) (/ (- now last-rev) 86400.0) nil))
              (alpha      (resurface--leitner-alpha old-box delta-days))
              (updated    (resurface--ema-update
                           (cdr (assq :confidence item)) (cdr (assq :variance item))
                           rating alpha))
              (new-conf   (car updated))
              (new-var    (cdr updated))
              (reviews    (1+ (cdr (assq :reviews-since-transition item))))
              (decision   (resurface--leitner-decide new-conf new-var reviews old-box last-box))
              (action     (car decision))
              (new-box    (cdr decision)))
         (if (eq action 'graduate)
             (resurface--build-item item
               :confidence   new-conf
               :variance     new-var
               :reviews-since-transition 0
               :last-reviewed now :graduated now :paused nil)
           (resurface--build-item item
             :box          new-box
             :confidence   new-conf
             :variance     new-var
             :reviews-since-transition (if (memq action '(promote demote)) 0 reviews)
             :last-reviewed now
             :graduated    nil
             :paused       nil
             :next-review  (resurface--leitner-choose-next-review new-box path)))))
      ('reset
       (resurface--build-item item
         :box 1 :confidence 0.0 :variance 0.0 :reviews-since-transition 0
         :graduated nil :paused nil :next-review now))
      ('skip item)
      ('partial
       (resurface--build-item item
         :last-reviewed now :paused t :next-review (+ now 86400)))
      ('revised
       (resurface--build-item item
         :last-reviewed now :paused nil
         :next-review (resurface--leitner-choose-next-review old-box path))))))


;; ===========================================================================
;;  Org prompt extraction

(defun resurface--leitner-extract-prompt (path)
  "Scan the first 1000 bytes of PATH for a review prompt.
Looks for #+LEITNER_PROMPT: first, then falls back to #+TITLE:."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path nil 1 1000)
      (goto-char (point-min))
      (cond
       ((re-search-forward "^#\\+LEITNER_PROMPT:\\s-*\\(.*\\)$" nil t)
        (match-string-no-properties 1))
       ((re-search-forward "^#\\+TITLE:\\s-*\\(.*\\)$" nil t)
        (match-string-no-properties 1))
       (t nil)))))  ; nil for non-org files

(defun resurface--leitner-insert-prompt-keyword (path prompt)
  "Insert a #+LEITNER_PROMPT: line containing PROMPT into PATH.
Places it after an existing #+TITLE: line (matched case-insensitively,
so Denote's lowercase #+title: is handled too)."
  (cl-flet ((splice ()
              (goto-char (point-min))
              (if (re-search-forward "^#\\+[Tt][Ii][Tt][Ll][Ee]:.*$"
                                      (+ (point-min) 1000) t)
                  (progn (end-of-line)
                         (insert (format "\n#+LEITNER_PROMPT: %s" prompt)))
                (goto-char (point-min))
                (insert (format "#+LEITNER_PROMPT: %s\n" prompt)))))
    (let ((existing-buf (get-file-buffer path)))
      (if existing-buf
          (with-current-buffer existing-buf
            (save-excursion (splice))
            (save-buffer))
        (with-temp-buffer
          (insert-file-contents path)
          (splice)
          (write-region (point-min) (point-max) path nil 'silent))))))


;; ===========================================================================
;;  Groups & Index Accessors

(defun resurface--ensure-data ()
  "Initialise `resurface--data', loading from disk when available."
  (unless resurface--data
    (if (file-exists-p (expand-file-name resurface-index-file))
        (resurface-load)
      (setq resurface--data (resurface--empty-data)))))

(defun resurface--leitner-groups-ht ()
  "Return the groups hash-table."
  (cdr (assq :groups resurface--data)))

(defun resurface--leitner-group-names ()
  "Return a sorted list of all group names."
  (sort (hash-table-keys (resurface--leitner-groups-ht)) #'string<))

(defun resurface--leitner-get-group (name)
  "Return the group alist for NAME, or nil."
  (gethash name (resurface--leitner-groups-ht)))

(defun resurface--leitner-get-or-create-group (name)
  "Return the group alist for NAME, creating it if it does not exist."
  (or (resurface--leitner-get-group name)
      (let ((g (list (cons :name name) (cons :items nil))))
        (puthash name g (resurface--leitner-groups-ht))
        g)))

(defun resurface--leitner-group-items (name)
  "Return the items list for group NAME (may be nil)."
  (cdr (assq :items (resurface--leitner-get-group name))))

(defun resurface--leitner-find-item (group-name path)
  "Return the item in GROUP-NAME with :path PATH, or nil."
  (seq-find (lambda (it) (equal (cdr (assq :path it)) path))
            (resurface--leitner-group-items group-name)))

;; setcdr+assq mutates in-place: gethash returns the actual cons-cell list
;; stored in the hash table, so setcdr on one of its cells updates it too

(defun resurface--leitner-set-group-items (group-name items)
  "Replace the items list of GROUP-NAME with ITEMS (mutates in-place)."
  (let ((g (gethash group-name (resurface--leitner-groups-ht))))
    (when g (setcdr (assq :items g) items))))

(defun resurface--leitner-prepend-item (group-name item)
  "Add ITEM to the front of GROUP-NAME's items list."
  (resurface--leitner-get-or-create-group group-name)
  (resurface--leitner-set-group-items
   group-name (cons item (resurface--leitner-group-items group-name))))

(defun resurface--leitner-replace-item (group-name path new-item)
  "Replace the item with :path = PATH in GROUP-NAME with NEW-ITEM."
  (resurface--leitner-set-group-items
   group-name
   (mapcar (lambda (it) (if (equal (cdr (assq :path it)) path) new-item it))
           (resurface--leitner-group-items group-name))))

(defun resurface--mark-dirty ()
  "Mark the index as having an unsaved change."
  (resurface--ensure-data)
  (setcdr (assq :dirty resurface--data) t))

(defun resurface--persist ()
  "Mark the index dirty and write it to disk."
  (resurface--mark-dirty)
  (resurface-save))

(defun resurface--leitner-all-pairs ()
  "All (group-name . item-alist) pairs across every group."
  (resurface--collection-all-pairs (resurface--leitner-groups-ht)))

(defun resurface--leitner-due-pairs (&optional group-name)
  "Due (group-name . item-alist) pairs, optionally filtered to GROUP-NAME."
  (resurface--collection-filter-pairs
   (resurface--leitner-groups-ht) #'resurface--leitner-item-due-p group-name))

;; ranks items only for `resurface-leitner-session-max-items', it has no
;; effect on whether something counts as due (`resurface--leitner-item-due-p' alone)
(defun resurface--leitner-item-overdue-amount (item)
  "Return how overdue ITEM is, in days.
Never-reviewed items rank lowest (0.0) here, behind files that sat
past their interval, they're due, but not yet neglected backlog."
  (max 0.0 (- (resurface--leitner-item-days-until-due item))))

(defun resurface--leitner-top-overdue-pairs (pairs n)
  "Return the N most overdue of PAIRS (as from `resurface--leitner-due-pairs')."
  (resurface--collection-top-overdue-pairs pairs #'resurface--leitner-item-overdue-amount n))

(defun resurface--leitner-graduated-pairs (&optional group-name)
  "Graduated (group-name . item-alist) pairs, optionally filtered to GROUP-NAME."
  (resurface--collection-filter-pairs
   (resurface--leitner-groups-ht) #'resurface--leitner-item-graduated-p group-name))

(defun resurface--leitner-group-next-due-str (active)
  "Return a string for when the next item in ACTIVE becomes due.
ACTIVE is any non-graduated items for a group."
  (let ((min-days nil))
    (dolist (item active)
      (unless (resurface--leitner-item-due-p item)
        (let ((d (resurface--leitner-item-days-until-due item)))
          (when (> d 0)
            (setq min-days (if min-days (min min-days d) d))))))
    (cond
     (min-days (propertize (if (< min-days 1) "<1d" (format "%dd" (floor min-days)))
                            'face 'font-lock-keyword-face))
     (active   (propertize "now" 'face 'warning))
     (t        "—"))))

(defun resurface--leitner-path-registered-p (path &optional group-name)
  "Return non-nil if PATH is already registered (optionally in GROUP-NAME)."
  (let ((abs (expand-file-name path)))
    (seq-find
     (lambda (pair)
       (and (or (null group-name) (equal (car pair) group-name))
            (equal (cdr (assq :path (cdr pair))) abs)))
     (resurface--leitner-all-pairs))))


;; ===========================================================================
;;  Persistence
;;
;; read with json-key-type 'string so object keys come back as plain
;; strings.  On write, json-encode calls symbol-name on symbol keys, so a
;; symbol-keyed alist serialises fine as-is

(defun resurface--empty-data ()
  "Return a fresh, empty index data structure."
  (list (cons :groups           (make-hash-table :test #'equal))
        (cons :ir-sources       nil)
        (cons :ir-sources-raw   nil)
        (cons :dirty            nil)))

(defun resurface--json-safe-alist (val)
  "Return VAL if it's usable as an alist (a proper list, including nil)."
  (if (listp val) val nil))

(defun resurface--ir-sources-json-sexp-for-save ()
  "Return the JSON sexp to write for the top-level \"ir_sources\" key.
When `resurface-ir.el' is loaded this serialises the live source list,
otherwise whatever was last read comes back out untouched."
  (if (fboundp 'resurface--ir-sources->json-sexp)
      (resurface--ir-sources->json-sexp)
    (cdr (assq :ir-sources-raw resurface--data))))

(defun resurface--data->json-sexp ()
  "Convert `resurface--data' to a JSON-encodable sexp."
  (let (groups-list)
    (maphash
     (lambda (gname g)
       (let* ((items (cdr (assq :items g)))
              (encoded-items
               (vconcat
                (mapcar
                 (lambda (item)
                   (list (cons 'path          (cdr (assq :path item)))
                         (cons 'box           (cdr (assq :box  item)))
                         (cons 'confidence    (cdr (assq :confidence item)))
                         (cons 'variance      (cdr (assq :variance item)))
                         (cons 'reviews_since_transition
                               (cdr (assq :reviews-since-transition item)))
                         (cons 'last_reviewed (cdr (assq :last-reviewed item)))
                         (cons 'next_review   (or (cdr (assq :next-review item)) 0))
                         (cons 'added         (cdr (assq :added item)))
                         (cons 'graduated     (or (cdr (assq :graduated item)) :json-false))
                         (cons 'paused        (if (cdr (assq :paused item)) t :json-false))))
                 items))))
         (push (cons gname (list (cons 'name  gname) (cons 'items encoded-items)))
               groups-list)))
     (resurface--leitner-groups-ht))
    (list (cons 'version       5)
          (cons 'box_intervals resurface-leitner-intervals)
          (cons 'groups        groups-list)
          (cons 'ir_sources    (resurface--ir-sources-json-sexp-for-save)))))

(defun resurface--leitner-seed-confidence-from-history (history)
  "Best-effort initial EMA confidence for legacy HISTORY (newest first).
Only used once, when loading a pre-EMA (version < 5) index in
`resurface--json-sexp->data', new saves never carry a history array."
  (if (null history)
      0.0
    (let ((n (length history)) (i 0) (wsum 0.0) (rsum 0.0))
      (dolist (r (reverse history))
        (cl-incf i)
        (let ((w (/ (float i) n)))
          (cl-incf wsum w)
          (cl-incf rsum (* w r))))
      (/ rsum wsum))))

(defun resurface--json-sexp->data (sexp)
  "Parse SEXP (from `json-read' with string keys) into internal data."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (group-pair (resurface--json-safe-alist (cdr (assoc "groups" sexp))))
      (let* ((gname      (car group-pair))
             (gdata      (cdr group-pair))
             (raw-items  (cdr (assoc "items" gdata)))
             (items-list (if (vectorp raw-items) (append raw-items nil) nil))
             (items
              (mapcar
               (lambda (raw)
                 (let* ((grad    (cdr (assoc "graduated" raw)))
                        (paused  (cdr (assoc "paused" raw)))
                        (box     (cdr (assoc "box" raw)))
                        (lr      (or (cdr (assoc "last_reviewed" raw)) 0))
                        (raw-nr  (assoc "next_review" raw))
                        (nr      (cond (raw-nr (cdr raw-nr))
                                        ((= lr 0) 0)
                                        (t (+ lr (* 86400 (resurface--leitner-box-days
                                                            (min box (resurface--leitner-num-boxes))))))))
                        ;; version < 5 indices stored a rolling history array
                        ;; instead of a confidence/variance EMA, seed one from
                        ;; it below so old data keeps working after upgrade.
                        (raw-conf (assoc "confidence" raw))
                        (raw-var  (assoc "variance" raw))
                        (raw-rst  (assoc "reviews_since_transition" raw))
                        (legacy-h (let ((h (cdr (assoc "history" raw))))
                                    (if (vectorp h) (append h nil) nil)))
                        (confidence (if raw-conf (cdr raw-conf)
                                      (resurface--leitner-seed-confidence-from-history legacy-h)))
                        (variance   (if raw-var (cdr raw-var) (if legacy-h 0.5 0.0)))
                        (reviews    (if raw-rst (cdr raw-rst) 0)))
                   ;; JSON false/null both come back as nil.  Legacy "question"
                   ;; field is ignored, prompts are read live via `resurface--leitner-extract-prompt'.
                   (list (cons :path          (cdr (assoc "path" raw)))
                         (cons :box           box)
                         (cons :confidence    confidence)
                         (cons :variance      variance)
                         (cons :reviews-since-transition reviews)
                         (cons :last-reviewed lr)
                         (cons :next-review   nr)
                         (cons :added         (cdr (assoc "added" raw)))
                         (cons :graduated     (if (or (null grad) (eq grad :json-false)) nil grad))
                         (cons :paused        (and paused (not (eq paused :json-false)))))))
               items-list)))
        (puthash gname (list (cons :name gname) (cons :items items)) ht)))
    (list (cons :groups       ht)
          ;; passthrough for "ir_sources", see the :ir-sources comment above
          (cons :ir-sources (if (fboundp 'resurface--json-sexp->ir-sources)
                                 (resurface--json-sexp->ir-sources sexp)
                               nil))
          (cons :ir-sources-raw (unless (fboundp 'resurface--json-sexp->ir-sources)
                                   (cdr (assoc "ir_sources" sexp))))
          (cons :dirty        nil))))

;;;###autoload
(defun resurface-save ()
  "Save the Resurface index (Leitner groups) to `resurface-index-file'."
  (interactive)
  (resurface--ensure-data)
  (let* ((full (expand-file-name resurface-index-file))
         (dir  (file-name-directory full)))
    (when dir (make-directory dir t))
    (let ((json-encoding-pretty-print t))
      (with-temp-file full
        (insert (json-encode (resurface--data->json-sexp)))))
    (setcdr (assq :dirty resurface--data) nil)
    (message "Resurface: saved to %s" (abbreviate-file-name full))))

;;;###autoload
(defun resurface-load ()
  "Load the Resurface index from `resurface-index-file'."
  (interactive)
  (let ((full (expand-file-name resurface-index-file)))
    (if (not (file-exists-p full))
        (progn
          (setq resurface--data (resurface--empty-data))
          (message "Resurface: no index found, starting fresh."))
      (condition-case err
          (let ((json-object-type 'alist)
                (json-array-type  'vector)
                (json-key-type    'string))
            (setq resurface--data
                  (resurface--json-sexp->data (json-read-file full)))
            (message "Resurface: loaded %d Leitner group(s)."
                     (hash-table-count (resurface--leitner-groups-ht))))
        (error
         (message "Resurface: failed to load: %s" (error-message-string err))
         (setq resurface--data (resurface--empty-data)))))))

(add-hook 'kill-emacs-hook
          (lambda ()
            (when (and resurface--data (cdr (assq :dirty resurface--data)))
              (resurface-save))))


;; ===========================================================================
;;  Generic Retirement/Graduation Browser
;;
;; Leitner's graduated-file browser is built on generic tabulated-list
;; list every item that has finished review, let the person send
;; any one of them back into rotation, refresh the owning dashboard when
;; they do.  Domain-specific behaviour (like Leitner being able to open
;; the underlying file) stays a per-domain option rather than being
;; forced to match, so other modes can reuse this macro too.

(cl-defmacro resurface--define-retirement-browser
    (&key mode mode-map mode-lighter filter-var
          browser-command bring-back-command help-command help-prefix
          row-open-command row-open-fn
          buffer-title-fn collection-label item-label state-label
          noun state-verb
          pairs-fn collection-names-fn collection-name-fn item-display-fn
          item-key-fn timestamp-fn find-item-fn replace-item-fn
          bring-back-fn success-message-fn refresh-dashboard-fn)
  "Define a read-only tabulated-list browser for a \"finished\" review state."
  (let* ((column-fn      (intern (format "%s--column-format" mode)))
         (entries-fn     (intern (format "%s--entries" mode)))
         (header-fn      (intern (format "%s--build-header" mode)))
         (collection-key (downcase collection-label))
         (open-hint      (if row-open-command "RET open   " "")))
    `(progn
       (defun ,column-fn ()
         (vector (list ,collection-label 16 t)
                 (list ,item-label       36 t)
                 (list ,state-label      12 t)
                 '("Age" 6 t)))

       (defun ,entries-fn (&optional collection-name)
         "Build tabulated-list entries, optionally filtered to COLLECTION-NAME."
         (mapcar
          (lambda (pair)
            (let* ((cid  (car pair))
                   (item (cdr pair))
                   (ts   (funcall ,timestamp-fn item)))
              (list (cons cid (funcall ,item-key-fn item))
                    (vector (funcall ,collection-name-fn cid)
                            (funcall ,item-display-fn item)
                            (resurface--format-ts ts)
                            (resurface--age-str ts)))))
          (funcall ,pairs-fn collection-name)))

       (defun ,header-fn (collection-name)
         "Build the header-line string, optionally filtered to COLLECTION-NAME."
         (let ((n (length (funcall ,pairs-fn collection-name))))
           (propertize
            (format "  %d %s %s%s%s   |   %sr bring back   g refresh   q close"
                    n ,state-verb ,noun (if (= n 1) "" "s")
                    (if collection-name (format "  (%s: %s)" ,collection-key collection-name) "")
                    ,open-hint)
            'face '(:inherit shadow :slant italic))))

       (defvar-local ,filter-var nil
         "The collection this browser buffer is filtered to, or nil for all.")

       (defvar ,mode-map
         (let ((map (make-sparse-keymap)))
           (set-keymap-parent map tabulated-list-mode-map)
           ,@(when row-open-command
               `((define-key map (kbd "RET") #',row-open-command)))
           (define-key map (kbd "r") #',bring-back-command)
           (define-key map (kbd "g") #'revert-buffer)
           (define-key map (kbd "q") #'quit-window)
           (define-key map (kbd "?") #',help-command)
           map)
         ,(format "Keymap for `%s'." mode))

       (define-derived-mode ,mode tabulated-list-mode ,mode-lighter
         ,(format "Browse every %s %s and decide whether it stays that way." state-verb noun)
         (setq tabulated-list-format (,column-fn))
         (tabulated-list-init-header))

       ;;;###autoload
       (defun ,browser-command (&optional collection-name)
         ,(format "Browse every %s %s and decide whether it stays retired.
With a prefix argument, limit the browser to one %s instead of every one."
                  state-verb noun collection-key)
         (interactive
          (list (when current-prefix-arg
                  (completing-read ,(format "Limit to %s: " collection-key)
                                    (funcall ,collection-names-fn) nil t))))
         (resurface--ensure-data)
         (let ((buf (get-buffer-create (funcall ,buffer-title-fn collection-name))))
           (with-current-buffer buf
             (,mode)
             (setq ,filter-var collection-name
                   tabulated-list-entries (lambda () (,entries-fn collection-name))
                   header-line-format     (,header-fn collection-name))
             (setq-local revert-buffer-function
                         (lambda (_auto _noconfirm)
                           (setq header-line-format (,header-fn collection-name))
                           (tabulated-list-print t)))
             (tabulated-list-print t))
           (switch-to-buffer buf)))

       ,@(when row-open-command
           `((defun ,row-open-command ()
               ,(format "Open the item under point in `%s'." mode)
               (interactive)
               (let ((id (tabulated-list-get-id)))
                 (when id (funcall ,row-open-fn (car id) (cdr id)))))))

       (defun ,bring-back-command ()
         ,(format "Send the %s under point back into rotation." noun)
         (interactive)
         (let* ((id   (tabulated-list-get-id))
                (cid  (car id))
                (iid  (cdr id))
                (item (funcall ,find-item-fn cid iid)))
           (unless item (user-error "Resurface: no %s on current line" ,noun))
           (funcall ,replace-item-fn cid iid (funcall ,bring-back-fn item))
           (resurface--persist)
           (setq header-line-format (,header-fn ,filter-var))
           (tabulated-list-print t)
           (funcall ,refresh-dashboard-fn)
           (message "%s" (funcall ,success-message-fn item))))

       (defun ,help-command ()
         ,(format "Echo `%s' keybindings." mode)
         (interactive)
         (message ,(format "%s: %sr bring back   g refresh   q close" help-prefix open-hint))))))


;; ===========================================================================
;;  Generic List-Mode Scaffolding
;;
;; Every dashboard and detail-view browser needs a keymap, a mode, and a
;; help command whose text lists that keymap's bindings.Entries and header
;; content stay out of its scope, dashboards vs. detail views compute those too differently.

(cl-defmacro resurface--define-list-mode
    (&key mode mode-map mode-lighter keys column-format-fn
          entries header-text dynamic-columns help-command)
  "Define a tabulated-list mode, keymap, and help command."
  `(progn
     (defvar ,mode-map
       (let ((map (make-sparse-keymap)))
         (set-keymap-parent map tabulated-list-mode-map)
         ,@(mapcar (lambda (k) `(define-key map (kbd ,(nth 0 k)) #',(nth 1 k))) keys)
         map)
       ,(format "Keymap for `%s'." mode))

     (define-derived-mode ,mode tabulated-list-mode ,mode-lighter nil
       (setq tabulated-list-format (funcall ,column-format-fn))
       ,@(when entries `((setq tabulated-list-entries ,entries)))
       ,@(when header-text
           `((setq header-line-format
                   (propertize ,header-text 'face '(:inherit shadow :slant italic)))))
       ,@(when dynamic-columns
           `((setq-local revert-buffer-function
                         (lambda (_auto _noconfirm)
                           (resurface--ensure-data)
                           (setq tabulated-list-format (funcall ,column-format-fn))
                           (tabulated-list-init-header)
                           (tabulated-list-print t)))))
       (tabulated-list-init-header))

     (defun ,help-command ()
       "Echo keybindings."
       (interactive)
       (message ,(mapconcat (lambda (k) (nth 2 k))
                             (seq-filter (lambda (k) (nth 2 k)) keys)
                             "  ")))))

(defun resurface--open-list-buffer (name mode-fn)
  "Create/reuse buffer NAME, run MODE-FN in it, print, and switch to it."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (funcall mode-fn)
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(defun resurface--refresh-buffer-if-live (name)
  "Redraw buffer NAME's tabulated list if that buffer exists and is live."
  (let ((buf (get-buffer name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (tabulated-list-print t)))))


;; ===========================================================================
;;  Healthcheck
;;
;; for each missing file: [r]emap prompts for its new location (keeps all
;; SR history), [p]rune removes the entry, [s]kip leaves it stale for now
;; nothing is written until the interactive pass is complete
;;;###autoload
(defun resurface-leitner-healthcheck ()
  "Check all files, interactively remap moved/renamed ones, prune gone ones."
  (interactive)
  (resurface--ensure-data)
  (let (missing)
    (maphash (lambda (gname _g)
               (dolist (item (resurface--leitner-group-items gname))
                 (unless (file-exists-p (cdr (assq :path item)))
                   (push (cons gname item) missing))))
             (resurface--leitner-groups-ht))
    (if (null missing)
        (message "Leitner: All tracked files are intact.")
      (let ((remapped 0) (pruned 0) (skipped 0))
        (dolist (pair missing)
          (let* ((gname    (car pair))
                 (item     (cdr pair))
                 (old-path (cdr (assq :path item)))
                 (choice   (read-char-choice
                            (format "Missing: %s\n  [r]emap  [p]rune  [s]kip? " old-path)
                            '(?r ?p ?s))))
            (pcase choice
              (?r (setcdr (assq :path item)
                          (expand-file-name
                           (read-file-name
                            (format "New path for %s: " (file-name-nondirectory old-path))
                            (file-name-directory old-path))))
                  (cl-incf remapped))
              (?p (resurface--leitner-set-group-items
                   gname (seq-remove (lambda (it) (equal it item))
                                      (resurface--leitner-group-items gname)))
                  (cl-incf pruned))
              (?s (cl-incf skipped)))))
        (when (> (+ remapped pruned) 0)
          (resurface--persist))
        (message "Leitner healthcheck: %d remapped, %d pruned, %d skipped."
                 remapped pruned skipped)))))


;; ===========================================================================
;;  Adding / Removing / Resetting Files

(defun resurface--leitner-read-group-name (&optional prompt)
  "PROMPT for a group name with completion."
  (let* ((names   (resurface--leitner-group-names))
         (default resurface-leitner-default-group)
         (pr      (or prompt (format "Group (default %s): " default))))
    (completing-read pr names nil nil nil nil default)))

;; #+LEITNER_PROMPT isn't asked for in batch mode (no sane way to prompt
;; per file across a whole Dired selection), run `resurface-leitner-add-file' again
;; from the file's own buffer afterwards, or add the keyword by hand.
;;;###autoload
(defun resurface-leitner-add-file (&optional file group)
  "Add FILE to GROUP for spaced repetition.
Called interactively from a `dired' buffer, bulk-adds all marked files to
a single group; folders and already-registered files are skipped."
  (interactive)
  (resurface--ensure-data)
  (if (and (called-interactively-p 'any) (derived-mode-p 'dired-mode))
      (let* ((files (seq-filter (lambda (f) (not (file-directory-p f)))
                                (dired-get-marked-files)))
             (_ (unless files (user-error "Leitner: no files marked in Dired")))
             (grp     (resurface--leitner-read-group-name))
             (added   0)
             (skipped 0))
        (dolist (f files)
          (let ((abs (expand-file-name f)))
            (if (resurface--leitner-path-registered-p abs grp)
                (cl-incf skipped)
              (resurface--leitner-prepend-item grp (resurface--leitner-make-item abs))
              (cl-incf added))))
        (when (> added 0)
          (resurface--persist)
          (resurface--leitner-maybe-refresh-dashboard))
        (message "Leitner: added %d file(s) to group '%s'%s."
                 added grp
                 (if (> skipped 0)
                     (format ", skipped %d already registered" skipped)
                   "")))
    (let* ((target (expand-file-name
                    (or file
                        (buffer-file-name)
                        (read-file-name "File to add: " nil nil t))))
           (grp (or group (resurface--leitner-read-group-name)))
           (prompt
            (when (called-interactively-p 'any)
              (let ((s (read-string
                        (format "#+LEITNER_PROMPT for '%s' (RET to skip): "
                                (file-name-nondirectory target)))))
                (unless (string-empty-p s) s)))))
      (if (resurface--leitner-path-registered-p target grp)
          (message "Leitner: '%s' is already in group '%s'."
                   (file-name-nondirectory target) grp)
        (when prompt
          (resurface--leitner-insert-prompt-keyword target prompt))
        (resurface--leitner-prepend-item grp (resurface--leitner-make-item target))
        (resurface--persist)
        (message "Leitner: added '%s' to group '%s' (Box 1)%s."
                 (file-name-nondirectory target) grp
                 (if prompt " with #+LEITNER_PROMPT" ""))
        (resurface--leitner-maybe-refresh-dashboard)))))

;;;###autoload
(defun resurface-leitner-remove-file (&optional file)
  "Remove FILE from the Leitner index, defaults to the current buffer's file."
  (interactive)
  (resurface--ensure-data)
  (let ((abs (expand-file-name
              (or file (buffer-file-name) (read-file-name "File to remove: ")))))
    (maphash (lambda (gname _g)
               (resurface--leitner-set-group-items
                gname
                (seq-remove (lambda (it) (equal (cdr (assq :path it)) abs))
                            (resurface--leitner-group-items gname))))
             (resurface--leitner-groups-ht))
    (resurface--persist)
    (message "Leitner: removed '%s'." (file-name-nondirectory abs))
    (resurface--leitner-maybe-refresh-dashboard)))

;;;###autoload
(defun resurface-leitner-add-group (name)
  "Create a new empty group called NAME."
  (interactive "sNew group name: ")
  (resurface--ensure-data)
  (if (resurface--leitner-get-group name)
      (message "Leitner: group '%s' already exists." name)
    (resurface--leitner-get-or-create-group name)
    (resurface--persist)
    (message "Leitner: group '%s' created." name)
    (resurface--leitner-maybe-refresh-dashboard)))


;; ===========================================================================
;;  Review Session
;;
;; review order: the due list is shuffled by `resurface--shuffle', then stable
;; sorted by box number, groups items by box while randomising within it
;;;###autoload
(defun resurface-leitner-start-session (&optional group-name)
  "Start a Leitner review session for all currently due files.
With an optional prefix argument, prompt to limit review to one GROUP-NAME."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Limit session to group: "
                            (resurface--leitner-group-names) nil t))))
  (resurface--ensure-data)
  (when (and resurface--leitner-session
             (not (yes-or-no-p "A session is already running.  Start a new one? ")))
    (user-error "Session aborted"))
  (let* ((due       (resurface--leitner-due-pairs group-name))
         (due-count (length due))
         (gsuffix   (if group-name (format " in '%s'" group-name) ""))
         (capped    (and (integerp resurface-leitner-session-max-items)
                          (> resurface-leitner-session-max-items 0)
                          (> due-count resurface-leitner-session-max-items)))
         (due       (if capped
                        (resurface--leitner-top-overdue-pairs due resurface-leitner-session-max-items)
                      due)))
    (if (null due)
        (message "Resurface: nothing due%s, great work!" gsuffix)
      (let ((queue (sort (resurface--shuffle due)
                          (lambda (a b)
                            (< (cdr (assq :box (cdr a))) (cdr (assq :box (cdr b))))))))
        (setq resurface--leitner-session
              (list (cons :queue        queue)
                    (cons :reviewed     0)
                    (cons :total        (length queue))
                    (cons :group-filter group-name)
                    (cons :capped       capped)))
        (if capped
            (message "Rusurface: %d due%s: queuing today's top %d most overdue.  Session starting..."
                     due-count gsuffix (length queue))
          (message "Resurface: %d file%s due%s.  Session starting..."
                   (length queue) (if (= (length queue) 1) "" "s") gsuffix))
        (resurface--leitner-session-advance)))))

(defun resurface--leitner-session-advance ()
  "Show the front card for the next queue item, or finish the session."
  (let ((queue (cdr (assq :queue resurface--leitner-session))))
    (if (null queue)
        (resurface--leitner-session-finish)
      (let* ((pair (car queue))
             (path (cdr (assq :path (cdr pair)))))
        (if (not (file-exists-p path))
            (progn
              (message "Leitner: file missing, skipping, %s"
                       (file-name-nondirectory path))
              (setcdr (assq :queue resurface--leitner-session) (cdr queue))
              (resurface--leitner-session-advance))
          (resurface--leitner-show-front-card pair))))))

(defun resurface--leitner-session-record (outcome)
  "Record OUTCOME (familiar/unfamiliar/partial/skip/revised) and advance."
  (unless resurface--leitner-session
    (user-error "Leitner: no active session"))
  (let* ((queue    (cdr (assq :queue resurface--leitner-session)))
         (pair     (car queue))
         (gname    (car pair))
         (item     (cdr pair))
         (path     (cdr (assq :path item)))
         (new-item (resurface--leitner-item-rate item outcome)))
    (resurface--leitner-replace-item gname path new-item)
    (cl-incf (cdr (assq :reviewed resurface--leitner-session)))
    (setcdr (assq :queue resurface--leitner-session) (cdr queue))
    (when resurface-leitner-review-minor-mode
      (resurface-leitner-review-minor-mode -1))
    (message "Leitner: %s  (%d / %d done)"
             (resurface--leitner-outcome-label outcome item new-item)
             (cdr (assq :reviewed resurface--leitner-session))
             (cdr (assq :total    resurface--leitner-session)))
    (resurface--leitner-session-advance)))

(defun resurface--leitner-session-finish ()
  "Clean up and save after all items have been reviewed.
When the session was trimmed by `resurface-leitner-session-max-items', reports
how many files are still due so the remaining backlog stays visible."
  (let* ((n            (cdr (assq :reviewed resurface--leitner-session)))
         (capped       (cdr (assq :capped       resurface--leitner-session)))
         (group-filter (cdr (assq :group-filter resurface--leitner-session)))
         (remaining    (and capped (length (resurface--leitner-due-pairs group-filter)))))
    (setq resurface--leitner-session nil)
    (resurface-save)
    (resurface-leitner)
    (run-hooks 'resurface-leitner-after-session-hook)
    (if (and capped (> remaining 0))
        (message "Leitner: session complete, %d file%s reviewed.  %d more due file%s waiting, run `resurface-leitner-start-session' again when you're ready. Index saved."
                 n (if (= n 1) "" "s")
                 remaining (if (= remaining 1) "" "s"))
      (message "Leitner: session complete, %d file%s reviewed. Index saved."
                n (if (= n 1) "" "s")))))


;; ===========================================================================
;;  Front Card: a pause before revealing

(defconst resurface--leitner-front-buf "*Resurface: Leitner Review*"
  "Name of the front-card buffer shown before revealing the file.")

(defvar-local resurface--leitner-front-item  nil "Item being previewed.")
(defvar-local resurface--leitner-front-group nil "Group name of the item being previewed.")

(defvar resurface-leitner-front-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'resurface-leitner-front-reveal)
    (define-key map (kbd "s")   #'resurface-leitner-front-skip)
    (define-key map (kbd "q")   #'resurface-leitner-front-quit)
    (define-key map (kbd "?")   #'resurface-leitner-front-help)
    map)
  "Keymap for `resurface-leitner-front-mode'.")

(define-derived-mode resurface-leitner-front-mode special-mode "Resurface-Leitner"
  "Read-only buffer shown before revealing a note file."
  :interactive nil)

(defun resurface--leitner-show-front-card (pair)
  "Display the front card for PAIR (group-name . item-alist)."
  (let* ((gname    (car pair))
         (item     (cdr pair))
         (path     (cdr (assq :path item)))
         (box      (cdr (assq :box  item)))
         (lr       (cdr (assq :last-reviewed item)))
         (reviewed (cdr (assq :reviewed resurface--leitner-session)))
         (total    (cdr (assq :total    resurface--leitner-session)))
         (fname    (file-name-sans-extension (file-name-nondirectory path)))
         (interval (resurface--leitner-box-days box))
         (progress (resurface--leitner-item-evidence-str item))
         (evidence (resurface--leitner-item-evidence-note item))
         (buf      (get-buffer-create resurface--leitner-front-buf)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (resurface-leitner-front-mode)
        (setq resurface--leitner-front-item  item
              resurface--leitner-front-group gname)
        (cl-flet ((ins (str &optional face)
                    (insert (if face (propertize str 'face face) str))))
          (let* ((width   (max 50 (- (window-width) 4)))
                 (rule    (concat "  " (make-string width ?-) "\n")))
            (ins "\n")
            (ins (format "  LEITNER  %d / %d\n" (1+ reviewed) total) '(:weight bold))
            (ins rule 'shadow)
            (ins "\n")
            (ins (format "  Group:          %s\n" gname))
            (ins (format "  Box:            %d  (ideal ~%d day%s)\n"
                         box interval (if (= interval 1) "" "s")))
            (ins (format "  Last reviewed:  %s\n" (resurface--format-ts lr)))
            (ins (format "  Confidence:     %s\n" (resurface--leitner-item-confidence-str item)))
            (ins (format "  Evidence:       %s\n" progress))
            (ins (format "                  %s\n" evidence) 'shadow)
            (ins "\n\n")
            (ins (concat "  " fname) '(:weight bold :height 1.2))
            (ins "\n\n\n")
            ;; The prompt line only appears when one is found in the file.
            (let ((prompt (resurface--leitner-extract-prompt path)))
              (when (and prompt (not (string-empty-p prompt)))
                (ins "  Prompt / Question:\n" 'shadow)
                (ins (format "  %s\n\n\n" prompt) '(:weight bold :height 1.1))))
            (ins "  Take a moment to notice what you remember before rereading.\n" '(:slant italic))
            (ins "  When ready, press SPC to open your notes.\n" '(:slant italic))
            (ins "\n")
            (ins "\n")
            (ins rule 'shadow)
            (ins "  [SPC] Reveal     [s] Skip     [q] Quit\n" 'shadow)))
        (goto-char (point-min))))
    (switch-to-buffer buf)))

(defun resurface-leitner-front-reveal ()
  "Reveal the note file for the current front card."
  (interactive)
  (unless resurface--leitner-front-item
    (user-error "Leitner: no front card active"))
  (let* ((item  resurface--leitner-front-item)
         (gname resurface--leitner-front-group)
         (path  (cdr (assq :path item)))
         (fc    (current-buffer)))
    (find-file path)
    (setq-local resurface--leitner-review-item  item)
    (setq-local resurface--leitner-review-group gname)
    (resurface-leitner-review-minor-mode 1)
    (kill-buffer fc)
    (run-hooks 'resurface-leitner-before-review-hook)))

(defun resurface-leitner-front-skip ()
  "Skip the current front card without revealing."
  (interactive)
  (unless resurface--leitner-session (user-error "Leitner: no active session"))
  (let* ((queue (cdr (assq :queue resurface--leitner-session)))
         (pair  (car queue))
         (gname (car pair))
         (item  (cdr pair))
         (path  (cdr (assq :path item))))
    (resurface--leitner-replace-item gname path
                           (resurface--leitner-item-rate item 'skip))
    (cl-incf (cdr (assq :reviewed resurface--leitner-session)))
    (setcdr (assq :queue resurface--leitner-session) (cdr queue))
    (message "Leitner: skipped '%s'." (file-name-nondirectory path))
    (resurface--leitner-session-advance)))

(defun resurface-leitner-front-quit ()
  "Quit the session from the front card."
  (interactive)
  (when (yes-or-no-p "Quit this Leitner session (Progress so far is saved.)?")
    (setq resurface--leitner-session nil)
    (resurface-save)
    (kill-buffer (current-buffer))
    (resurface-leitner)
    (message "Leitner: session ended.  Index saved.")))

(defun resurface-leitner-front-help ()
  "Show front-card keybindings in the echo area."
  (interactive)
  (message "Leitner front: [SPC] Reveal   [s] Skip   [q] Quit"))


;; ===========================================================================
;;  Review Minor Mode: rating from within the note file

(defvar-local resurface--leitner-review-item  nil "Item under review (buffer-local).")
(defvar-local resurface--leitner-review-group nil "Group of the item under review (buffer-local).")

(defvar resurface-leitner-review-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c l f") #'resurface-leitner-rate-familiar)
    (define-key map (kbd "C-c l u") #'resurface-leitner-rate-unfamiliar)
    (define-key map (kbd "C-c l p") #'resurface-leitner-rate-partial)
    (define-key map (kbd "C-c l r") #'resurface-leitner-rate-revised)
    (define-key map (kbd "C-c l s") #'resurface-leitner-rate-skip)
    (define-key map (kbd "C-c l q") #'resurface-leitner-quit-session)
    (define-key map (kbd "C-c l ?") #'resurface-leitner-review-help)
    map)
  "Keymap active while reviewing a revealed note file.")

(define-minor-mode resurface-leitner-review-minor-mode
  "Active while a note file is open for review.
The file is fully editable, so you can revise freely and rate your
familiarity with it when done."
  :lighter " Resurface-Leitner"
  :keymap resurface-leitner-review-minor-mode-map
  (if resurface-leitner-review-minor-mode
      (progn
        (setq header-line-format (resurface--leitner-build-review-header))
        (add-hook 'kill-buffer-hook #'resurface--leitner-on-review-buffer-kill nil t))
    (kill-local-variable 'header-line-format)
    (remove-hook 'kill-buffer-hook #'resurface--leitner-on-review-buffer-kill t)))

(defun resurface--leitner-build-review-header ()
  "Construct the header-line string for the file currently under review."
  (when (and resurface--leitner-review-item resurface--leitner-session)
    (let ((box      (cdr (assq :box resurface--leitner-review-item)))
          (conf     (resurface--leitner-item-confidence-str resurface--leitner-review-item))
          (progress (resurface--leitner-item-evidence-str resurface--leitner-review-item))
          (reviewed (cdr (assq :reviewed resurface--leitner-session)))
          (total    (cdr (assq :total    resurface--leitner-session))))
      (concat
       (propertize (format " LEITNER  %d/%d " (1+ reviewed) total) 'face '(:weight bold))
       (propertize (format "  %s" resurface--leitner-review-group) 'face 'mode-line)
       (propertize (format "  Box %d  Conf %s  [%s] " box conf progress) 'face '(:slant italic))
       (propertize "    C-c l f Familiar   C-c l u Unfamiliar   C-c l p Partial   C-c l r Revised   C-c l s Skip   C-c l q Quit"
                   'face '(:inherit shadow))))))

(defun resurface--leitner-on-review-buffer-kill ()
  "Warn when a review buffer is killed mid-session."
  (when (and resurface-leitner-review-minor-mode resurface--leitner-session)
    (message "Leitner: review buffer killed -- use M-x resurface-leitner-start-session to resume.")))

;;;###autoload
(defun resurface-leitner-rate-familiar ()
  "Rate current review item FAMILIAR.
Feeds the confidence engine; only enough consecutive Familiar evidence
promotes the box, see \"The Familiarity Scheduler\" in the Commentary."
  (interactive)
  (resurface--leitner-session-record 'familiar))

;;;###autoload
(defun resurface-leitner-rate-unfamiliar ()
  "Rate current review item UNFAMILIAR.
Feeds the confidence engine, only sustained Unfamiliar evidence demotes
or resets the box, a single rating never does."
  (interactive)
  (resurface--leitner-session-record 'unfamiliar))

;;;###autoload
(defun resurface-leitner-rate-partial ()
  "Rate current review item PARTIAL: only read part of it, or ran out of time.
Box untouched, the file marked paused and scheduled to come due again tomorrow."
  (interactive)
  (resurface--leitner-session-record 'partial))

;;;###autoload
(defun resurface-leitner-rate-revised ()
  "Rate current review item REVISED: you revised or added to your notes.
Unlike `resurface-leitner-rate-partial' the review counts as complete, the file
returns on its normal box interval, new content and all."
  (interactive)
  (resurface--leitner-session-record 'revised))

;;;###autoload
(defun resurface-leitner-rate-skip ()
  "Skip current review item (keep its box and history)."
  (interactive)
  (resurface--leitner-session-record 'skip))

;;;###autoload
(defun resurface-leitner-quit-session ()
  "End the current review session early and save progress."
  (interactive)
  (when (yes-or-no-p "Quit this Leitner session  (Progress so far is saved.)?")
    (when resurface-leitner-review-minor-mode
      (resurface-leitner-review-minor-mode -1))
    (setq resurface--leitner-session nil)
    (resurface-save)
    (resurface-leitner)
    (message "Leitner: session ended.  Index saved.")))

(defun resurface-leitner-review-help ()
  "Echo review keybindings in minibuffer."
  (interactive)
  (message "Leitner: C-c l f Familiar   C-c l u Unfamiliar   C-c l p Partial   C-c l r Revised   C-c l s Skip   C-c l q Quit   C-c l ? Help"))


;; ===========================================================================
;;  Dashboard, main entry point showing each group

(defun resurface--leitner-menu-format ()
  "Column format vector, box columns sized to `resurface-leitner-intervals'."
  (vconcat
   (list (list "Group"  22 t)
         (list "Files"   7 t)
         (list "Due"     5 t)
         (list "Pause"   6 t)
         (list "Next"    6 t))
   (cl-loop for i from 1 to (resurface--leitner-num-boxes)
            collect (list (format "B%d" i) 5 t))
   (list (list "Grad" 5 t))))

(resurface--define-list-mode
 :mode              resurface-leitner-menu-mode
 :mode-map          resurface-leitner-menu-mode-map
 :mode-lighter      "Leitner"
 :column-format-fn  #'resurface--leitner-menu-format
 :entries           #'resurface--leitner-menu-entries
 :header-text       "  Leitner: press ? for keybindings"
 :dynamic-columns   t
 :help-command      resurface-leitner-menu-help
 :keys (("RET" resurface-leitner-menu-view-group    "RET view")
        ("r"   resurface-leitner-menu-start-session "r review")
        ("s"   resurface-leitner-start-session      "s review-all")
        ("G"   resurface-leitner-review-graduated   "G graduated")
        ("a"   resurface-leitner-add-file           "a add-file")
        ("A"   resurface-leitner-add-group          "A new-group")
        ("R"   resurface-leitner-menu-rename-group  "R rename")
        ("d"   resurface-leitner-menu-delete-group  "d delete")
        ("S"   resurface-save                       "S save")
        ("g"   revert-buffer                        "g refresh")
        ("?"   resurface-leitner-menu-help          nil)))

(defun resurface--leitner-menu-entries ()
  "Tabulated-list entries for the dashboard."
  (resurface--ensure-data)
  (let ((nb (resurface--leitner-num-boxes)))
    (mapcar
     (lambda (gname)
       (let* ((items  (resurface--leitner-group-items gname))
              (n      (length items))
              (active (seq-filter (lambda (it) (not (resurface--leitner-item-graduated-p it))) items))
              (due    (length (seq-filter #'resurface--leitner-item-due-p active)))
              (paused (length (seq-filter #'resurface--leitner-item-paused-p active)))
              (next   (resurface--leitner-group-next-due-str active))
              (grad   (- n (length active)))
              (boxes  (make-vector nb 0)))
         (dolist (item active)
           (let ((b (1- (min nb (cdr (assq :box item))))))
             (aset boxes b (1+ (aref boxes b)))))
         (list gname
               (vconcat
                (list gname
                      (number-to-string n)
                      (if (> due 0) (propertize (number-to-string due) 'face 'warning) "0")
                      (if (> paused 0)
                          (propertize (number-to-string paused) 'face 'font-lock-doc-face)
                        "0")
                      next)
                (cl-loop for i from 0 below nb
                         collect (number-to-string (aref boxes i)))
                (list (if (> grad 0) (propertize (number-to-string grad) 'face 'success) "0"))))))
     (resurface--leitner-group-names))))

;;;###autoload
(defun resurface-leitner ()
  "Open the Leitner group dashboard (whole-file review)."
  (interactive)
  (resurface--ensure-data)
  (resurface--open-list-buffer "*Resurface: Leitner*" #'resurface-leitner-menu-mode))

(defun resurface-leitner-menu-view-group ()
  "Open the detail view for the group on this dashboard line."
  (interactive)
  (let ((gname (tabulated-list-get-id)))
    (when gname (resurface-leitner-view-group gname))))

(defun resurface-leitner-menu-start-session ()
  "Start a review session for the group on this dashboard line."
  (interactive)
  (let ((gname (tabulated-list-get-id)))
    (when gname (resurface-leitner-start-session gname))))

(defun resurface-leitner-menu-delete-group ()
  "Delete the group on this line, with confirmation."
  (interactive)
  (let ((gname (tabulated-list-get-id)))
    (when (and gname
               (yes-or-no-p (format "Delete group '%s' and all its entries? " gname)))
      (remhash gname (resurface--leitner-groups-ht))
      (resurface--persist)
      (tabulated-list-print t))))

(defun resurface-leitner-menu-rename-group ()
  "Rename the group on this dashboard line."
  (interactive)
  (let ((old-name (tabulated-list-get-id)))
    (unless old-name
      (user-error "Leitner: no group on current line"))
    (let ((new-name (string-trim (read-string (format "Rename group '%s' to: " old-name) old-name))))
      (cond
       ((string-empty-p new-name)
        (user-error "Leitner: group name cannot be empty"))
       ((equal old-name new-name)
        (message "Leitner: name unchanged."))
       ((resurface--leitner-get-group new-name)
        (user-error "Leitner: group '%s' already exists" new-name))
       (t
        (let ((group-alist (resurface--leitner-get-group old-name)))
          (setcdr (assq :name group-alist) new-name)
          (puthash new-name group-alist (resurface--leitner-groups-ht)) ; move to new hash key
          (remhash old-name (resurface--leitner-groups-ht))
          (let ((detail-buf (get-buffer (format "*Resurface: Leitner: %s*" old-name))))
            (when detail-buf
              (with-current-buffer detail-buf
                (setq resurface--leitner-gv-group new-name)
                (rename-buffer (format "*Resurface: Leitner: %s*" new-name) t)
                (setq header-line-format (resurface--leitner-gv-build-header new-name))
                (tabulated-list-print t))))
          (resurface--persist)
          (tabulated-list-print t)
          (message "Leitner: group renamed to '%s'." new-name)))))))

(defun resurface--leitner-maybe-refresh-dashboard ()
  "Silently refresh the dashboard buffer if it is alive."
  (resurface--refresh-buffer-if-live "*Resurface: Leitner*"))


;; ===========================================================================
;;  Group Detail View  (file list + status for one group)

(defvar-local resurface--leitner-gv-group nil "Group name this detail view is showing.")

(defun resurface--leitner-gv-column-format ()
  "Column format vector for the group detail view."
  (vector
   '("File"          32 t)
   '("Box"            5 t)
   '("Conf"            6 t)
   '("Evid"            7 t)
   '("Last Reviewed" 14 t)
   '("Due in"        10 nil)
   '("Due?"           5 nil)))

(resurface--define-list-mode
 :mode              resurface-leitner-group-view-mode
 :mode-map          resurface-leitner-group-view-mode-map
 :mode-lighter      "Resurface-Leitner-Group"
 :column-format-fn  #'resurface--leitner-gv-column-format
 :help-command      resurface-leitner-gv-help
 :keys (("RET" resurface-leitner-gv-open-file  "RET open")
        ("r"   resurface-leitner-gv-reset-file "r reset")
        ("d"   resurface-leitner-gv-remove-file "d remove")
        ("a"   resurface-leitner-gv-add-file   "a add")
        ("q"   quit-window                     "q close")
        ("?"   resurface-leitner-gv-help       nil)))

(defun resurface--leitner-gv-build-header (group-name)
  "Header-line string for the GROUP-NAME detail view."
  (let* ((items    (resurface--leitner-group-items group-name))
         (n        (length items))
         (grad     (length (seq-filter #'resurface--leitner-item-graduated-p items)))
         (active   (- n grad))
         (due      (length (seq-filter #'resurface--leitner-item-due-p items)))
         (paused   (length (seq-filter #'resurface--leitner-item-paused-p items))))
    (propertize
     (format "  %s   |   %d active  %d graduated   |   %d due%s   |   RET open  r reset  d remove  a add  q close"
             group-name active grad due
             (if (> paused 0) (format "   |   %d paused" paused) ""))
     'face 'mode-line)))

(defun resurface--leitner-gv-refresh (group-name)
  "Refresh this group-view buffer's header and table for GROUP-NAME."
  (setq header-line-format (resurface--leitner-gv-build-header group-name))
  (tabulated-list-print t))

(defun resurface--leitner-gv-entries (group-name)
  "Tabulated-list entries for GROUP-NAME in group view."
  (mapcar
   (lambda (item)
     (let* ((path   (cdr (assq :path item)))
            (box    (cdr (assq :box  item)))
            (lr     (cdr (assq :last-reviewed item)))
            (grad   (cdr (assq :graduated item)))
            (paused (resurface--leitner-item-paused-p item))
            (due-p  (resurface--leitner-item-due-p item))
            (days   (resurface--leitner-item-days-until-due item))
            (conf   (resurface--leitner-item-confidence-str item))
            (evid   (resurface--leitner-item-evidence-str item))
            (due-str
             (cond (grad       (propertize "—"       'face 'shadow))
                   ((= lr 0)   (propertize "new"     'face 'warning))
                   (paused     (propertize (if due-p "paused, due" (format "paused (%.0fd)" days))
                                           'face 'font-lock-doc-face))
                   (due-p      (propertize "overdue" 'face 'warning))
                   (t          (format "%.0fd" days))))
            (status
             (cond (grad   (propertize "Grad" 'face 'success))
                   (paused (propertize "Paused" 'face 'font-lock-doc-face))
                   (due-p  (propertize "Yes"  'face 'warning))
                   (t      "")))
            (base-vec
             (vector
              (file-name-nondirectory path)
              (if grad (propertize (number-to-string box) 'face 'shadow)
                (number-to-string box))
              conf
              evid
              (resurface--format-ts lr)
              due-str
              status)))
       (list path base-vec)))
   (resurface--leitner-group-items group-name)))

;;;###autoload
(defun resurface-leitner-view-group (group-name)
  "Open the detail view listing all files in GROUP-NAME."
  (interactive
   (list (completing-read "View group: " (resurface--leitner-group-names) nil t)))
  (resurface--ensure-data)
  (let ((buf (get-buffer-create (format "*Resurface: Leitner: %s*" group-name))))
    (with-current-buffer buf
      (resurface-leitner-group-view-mode)
      (setq resurface--leitner-gv-group      group-name
            header-line-format     (resurface--leitner-gv-build-header group-name)
            tabulated-list-entries (lambda () (resurface--leitner-gv-entries group-name)))
      (setq-local revert-buffer-function
                  (lambda (_a _n)
                    (setq tabulated-list-format (resurface--leitner-gv-column-format))
                    (tabulated-list-init-header)
                    (resurface--leitner-gv-refresh group-name)))
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(defun resurface-leitner-gv-open-file ()
  "Open the file on the current detail-view line."
  (interactive)
  (let ((path (tabulated-list-get-id)))
    (when path (find-file path))))

(defun resurface-leitner-gv-remove-file ()
  "Remove the file on the current line from the Leitner index."
  (interactive)
  (let* ((path  (tabulated-list-get-id))
         (gname resurface--leitner-gv-group))
    (when (and path
               (yes-or-no-p (format "Remove '%s' from Leitner? "
                                    (file-name-nondirectory path))))
      (resurface--leitner-set-group-items
       gname
       (seq-remove (lambda (it) (equal (cdr (assq :path it)) path))
                   (resurface--leitner-group-items gname)))
      (resurface--persist)
      (resurface--leitner-gv-refresh gname)
      (resurface--leitner-maybe-refresh-dashboard)
      (message "Leitner: removed '%s'." (file-name-nondirectory path)))))

(defun resurface-leitner-gv-reset-file ()
  "Reset the file on the current line to Box 1, reactivating it if graduated."
  (interactive)
  (let* ((path  (tabulated-list-get-id))
         (gname resurface--leitner-gv-group)
         (item  (resurface--leitner-find-item gname path)))
    (when (and item
               (yes-or-no-p
                (format (if (cdr (assq :graduated item))
                            "Reactivate '%s' and reset to Box 1? "
                          "Reset '%s' to Box 1? ")
                        (file-name-nondirectory path))))
      ;; This is the explicit "reset" action, not a rating, it always
      ;; forces Box 1, clears :graduated, and clears the confidence history
      (resurface--leitner-replace-item gname path (resurface--leitner-item-rate item 'reset))
      (resurface--persist)
      (resurface--leitner-gv-refresh gname)
      (resurface--leitner-maybe-refresh-dashboard)
      (message "Leitner: '%s' reset to Box 1 (active)." (file-name-nondirectory path)))))

(defun resurface-leitner-gv-add-file ()
  "Add a file to the group shown in this detail view."
  (interactive)
  (resurface-leitner-add-file nil resurface--leitner-gv-group)
  (resurface--leitner-gv-refresh resurface--leitner-gv-group))


;; ===========================================================================
;;  Graduated Browser
;;
;; Lists every graduated file (across all groups, or just one) so you can
;; check them manually and send anything shaky back to Box 1.  Files you
;; don't touch here simply stay graduated.  An instantiation of
;; `resurface--define-retirement-browser'.

(resurface--define-retirement-browser
 :mode                 resurface-leitner-graduated-mode
 :mode-map             resurface-leitner-graduated-mode-map
 :mode-lighter         "Resurface-Leitner-Grad"
 :filter-var           resurface--leitner-grad-group
 :browser-command      resurface-leitner-review-graduated
 :bring-back-command   resurface-leitner-grad-bring-back
 :help-command         resurface-leitner-grad-help
 :help-prefix          "Leitner graduated"
 :row-open-command     resurface-leitner-grad-open-file
 :row-open-fn          (lambda (_cid path) (find-file path))
 :buffer-title-fn      (lambda (name)
                         (if name (format "*Resurface: Leitner Graduated (%s)*" name)
                           "*Resurface: Leitner Graduated*"))
 :collection-label     "Group"
 :item-label           "File"
 :state-label          "Graduated"
 :noun                 "file"
 :state-verb           "graduated"
 :pairs-fn             #'resurface--leitner-graduated-pairs
 :collection-names-fn  #'resurface--leitner-group-names
 :collection-name-fn   #'identity
 :item-display-fn      (lambda (item) (file-name-nondirectory (cdr (assq :path item))))
 :item-key-fn          (lambda (item) (cdr (assq :path item)))
 :timestamp-fn         (lambda (item) (cdr (assq :graduated item)))
 :find-item-fn         #'resurface--leitner-find-item
 :replace-item-fn      #'resurface--leitner-replace-item
 :bring-back-fn        (lambda (item) (resurface--leitner-item-rate item 'reset))
 :success-message-fn   (lambda (item)
                         (format "Leitner: '%s' is back in the rotation at Box 1."
                                 (file-name-nondirectory (cdr (assq :path item)))))
 :refresh-dashboard-fn #'resurface--leitner-maybe-refresh-dashboard)


;; ===========================================================================
;;  Evil-mode Compatibility
;;
;; Keys `g', `?', `s', `a', `d', `A', `S' are intercepted by evil by default;
;; `evil-set-initial-state' MODE 'emacs tells evil to leave each buffer alone
;; so our own keymaps are consulted.  Tabulated-list navigation (TAB, arrows,
;; n/p) still works since those live in `tabulated-list-mode-map'.  We then
;; add back `j'/`k' as plain evil motion on top.
;;
;; `resurface--evil-tabulated-compat' is the shared helper for this.

(defun resurface--evil-tabulated-compat (major-modes maps)
  "Make evil defer to our own bindings in MAJOR-MODES and MAPS.
MAJOR-MODES (single-letter-command tabulated-list buffers) get their
initial evil state set to `emacs', MAPS (their keymaps) get `j'/`k'
added back on top so line motion still feels like evil."
  (dolist (mode major-modes)
    (evil-set-initial-state mode 'emacs))
  (dolist (map maps)
    (define-key map (kbd "j") #'evil-next-line)
    (define-key map (kbd "k") #'evil-previous-line)))

(with-eval-after-load 'evil
  (resurface--evil-tabulated-compat
   '(resurface-leitner-menu-mode
     resurface-leitner-group-view-mode
     resurface-leitner-graduated-mode)
   (list resurface-leitner-menu-mode-map
         resurface-leitner-group-view-mode-map
         resurface-leitner-graduated-mode-map))
  (evil-set-initial-state 'resurface-leitner-front-mode 'emacs)  ; SPC/s also live in evil-motion-state-map
  (evil-make-overriding-map resurface-leitner-review-minor-mode-map 'normal)
  (add-hook 'resurface-leitner-review-minor-mode-hook #'evil-normalize-keymaps))


(provide 'resurface)
;;; resurface.el ends here
