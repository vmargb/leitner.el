;;; resurface.el --- Resurface material for rereading, drilling, and review  -*- lexical-binding: t; -*-
;; Author: vmargb
;; Version: 0.2.0
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
;;     single pass/fail rating, see "The Familiarity Scheduler" for more info.
;;   - Drill    (`resurface-drill', resurface-drill.el): for sentence mining
;;     and rereading sentences (e.g. lines from a language-learning text) until
;;     they naturally make sense, with no right/wrong grading, only
;;     `clear' vs `opaque'.
;;
;; Both share one JSON index file; FILES ARE NEVER MODIFIED, all
;; scheduling metadata lives externally in `resurface-index-file'.
;;
;; -----------------------------------------
;; Familiarity Scheduler (Leitner mode)
;;
;; A single session's rating is a noisy, one-off observation, not a
;; reliable measurement, so no single Familiar/Unfamiliar rating changes
;; a files box directly.  Ratings only ever modify the familiarity history
;; a box change only occurs when the confidence estimate crosses a threshold
;;
;;   1. Every rating is appended to a short rolling history (`resurface-
;;      leitner-history-length').
;;   2. That history is turned into a CONFIDENCE score in [-1, +1] by a
;;      recency-weighted average (`resurface-leitner-weighting').
;;   3. Confidence only moves the box once it clears a threshold.  Then the
;;      history is cleared so the next transition needs its own evidence
;;   4. Unliked standard Leitner, each box now has an IDEAL interval, not
;;      an exact deadline.  The actual next-review is chosen from a small window
;;      around that ideal (`resurface-leitner-interval-tolerance'), which self-balances
;;      by picking the day which is both close to ideal and lightly loaded
;;      (`resurface-leitner-workload-weight'), so reviews spread out
;;      instead of piling up on the same day.
;;
;; Quick start -- Leitner (whole files):
;;   M-x resurface-leitner                  Open the group dashboard
;;   M-x resurface-leitner-add-group        Add a new group
;;   M-x resurface-leitner-add-file         Add current buffer's file to a group
;;   M-x resurface-leitner-start-session    Review all due files (C-u: one group)
;;   M-x resurface-leitner-review-graduated Browse graduated files (C-u: one group)
;;
;; Quick start -- Drill (sentences/chunks, resurface-drill.el):
;;   M-x resurface-drill                    Open the drill block dashboard
;;   M-x resurface-drill-add-block          Add a new drill block
;;   M-x resurface-drill-add-sentence       Add a sentence/chunk to drill
;;   M-x resurface-drill-start-session      Drill all due sentences (C-u: one block)
;;   M-x resurface-drill-review-retired     Browse retired sentences (C-u: one block)
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
These are targets the scheduler aims for, not exact deadlines, see
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

(defcustom resurface-leitner-history-length 5
  "Window of number of most recent ratings kept per item.
Confidence is recomputed from this window, older ratings are
discarded, so there is nothing further to keep in sync."
  :type 'integer
  :group 'resurface)

(defcustom resurface-leitner-weighting 'linear
  "How much more recent ratings should count than older ones.
`linear' weights position i of n as i/n.  `exponential' doubles
the weight each step closer to the present.  `uniform' gives
every rating in the window equal weight."
  :type '(choice (const linear) (const exponential) (const uniform))
  :group 'resurface)

(defcustom resurface-leitner-promote-threshold 0.80
  "Confidence at or above where an item should be promoted by one box.
Promoting from the last box graduates the item instead."
  :type 'float
  :group 'resurface)

(defcustom resurface-leitner-demote-threshold -0.40
  "Confidence at or below where an item should be demoted by one box.
Superseded by `resurface-leitner-reset-threshold' when both apply."
  :type 'float
  :group 'resurface)

(defcustom resurface-leitner-reset-threshold -0.80
  "Confidence at or below where an item is reset straight to Box 1.
Checked before `resurface-leitner-demote-threshold', so consistent
unfamiliarity always wins over a milder demotion."
  :type 'float
  :group 'resurface)

(defcustom resurface-leitner-interval-tolerance 0.20
  "Fractional slack allowed around a box's ideal interval, each way.
0.20 on a 30-day box permits scheduling anywhere from 24 to 36
days out, whichever balances workload best."
  :type 'float
  :group 'resurface)

(defcustom resurface-leitner-workload-weight 0.2
  "Weight of same-day review load against deviation from the ideal date.
Spacing cost is normalised to [0, 1] across the tolerance window (see
`resurface--leitner-choose-next-review'), so this is denominated in
\"how many extra same-day reviews is drifting all the way to the edge
of the window worth\": the default 0.2 means about 5 extra reviews on
a day justify using the full tolerance to avoid it.  Higher values push
harder toward quiet days; 0 disables workload-awareness entirely."
  :type 'float
  :group 'resurface)

;; ---------------------------------------------------------------------------
;;  Drill mode (optional companion feature, see resurface-drill.el)

(defcustom resurface-enable-drill-mode nil
  "Whether to load Drill mode (`resurface-drill.el') alongside Leitner mode."
  :type 'boolean
  :group 'resurface
  :set (lambda (sym val)
         (set-default sym val)
         (when val (require 'resurface-drill))))

;; ===========================================================================
;;  Internal State
;;
;; resurface--data  =  ((:groups . HASH-TABLE)  (:dirty . BOOL))
;;   HASH-TABLE  : group-name (string) -> group-alist
;;   group-alist : ((:name . STRING) (:items . LIST-OF-ITEM-ALISTS))
;;   item-alist  : ((:path . STRING) (:box . INT)
;;                  (:history . LIST-OF-1-OR-MINUS-1)  ; newest-first, capped
;;                  (:last-reviewed . INT) (:next-review . INT)
;;                  (:added . INT)
;;                  (:graduated . INT-OR-NIL)   ; Unix ts when graduated
;;                  (:paused . BOOL))           ; t after a Partial rating
;;
;; No confidence, momentum, or streak count is ever stored, it is always
;; recomputed from :history, in `resurface--leitner-confidence'.
;;
;; resurface--data  also carries a THIRD top-level key, :drill-blocks, used
;; by Drill Mode (`resurface-drill.el', a separate, optional file
;; `resurface-enable-drill-mode')  this file only touches it as an opaque
;; hash-table it hands off to resurface-drill.el's own accessors, the
;; dispatch is in `resurface--data->json-sexp'/`resurface--json-sexp->data'.

(defvar resurface--data nil
  "In-memory Resurface index (Leitner groups + drill blocks).
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


;; ===========================================================================
;;  The Familiarity Scheduler
;;
;; Turns a rolling window of Familiar/Unfamiliar ratings into a confidence
;; score, decides whether that confidence is enough to move the box
;; when a review needs a new date, picks the day nearest the box's
;; ideal interval that isn't already overloaded with other reviews.
;; Every step is a pure function of the item's stored history
;; nothing is cached.

(defun resurface--leitner-weight (position n)
  "Weight of the rating at 1-indexed POSITION out of N (oldest = 1)."
  (pcase resurface-leitner-weighting
    ('uniform     1.0)
    ('exponential (expt 2.0 (1- position)))
    (_            (/ (float position) n)))) ; linear, the default

(defun resurface--leitner-confidence (history)
  "Return a confidence score in [-1, +1] for HISTORY, newest first.
An empty HISTORY (not enough evidence yet) is neutral: 0.0."
  (if (null history)
      0.0
    (let ((n (length history)) (position 0) (wsum 0.0) (rsum 0.0))
      (dolist (rating (reverse history)) ; walk oldest -> newest
        (cl-incf position)
        (let ((w (resurface--leitner-weight position n)))
          (cl-incf wsum w)
          (cl-incf rsum (* w rating))))
      (/ rsum wsum))))

(defun resurface--leitner-push-history (history rating)
  "Prepend RATING (1 or -1) to HISTORY, capped to `resurface-leitner-history-length'."
  (seq-take (cons rating history) resurface-leitner-history-length))

(defun resurface--leitner-decide (confidence n old-box last-box)
  "Return (ACTION . NEW-BOX) for CONFIDENCE at OLD-BOX of LAST-BOX boxes.
N is how many ratings CONFIDENCE was computed from.  Fewer than
`resurface-leitner-history-length' means there isn't enough evidence yet
ACTION is one of `reset', `demote', `promote', `graduate', or `remain'."
  (if (< n resurface-leitner-history-length)
      (cons 'remain old-box)
    (cond
     ((<= confidence resurface-leitner-reset-threshold)  (cons 'reset 1))
     ((<= confidence resurface-leitner-demote-threshold)  (cons 'demote (max 1 (1- old-box))))
     ((>= confidence resurface-leitner-promote-threshold)
      (if (= old-box last-box) (cons 'graduate old-box) (cons 'promote (1+ old-box))))
     (t (cons 'remain old-box)))))

(defun resurface--leitner-day-loads (exclude-path)
  "Hash-table of epoch-day -> number of active items next due that day.
EXCLUDE-PATH is left out, since it's the item currently being rescheduled."
  (let ((loads (make-hash-table :test #'eql)))
    (dolist (pair (resurface--leitner-all-pairs))
      (let* ((item (cdr pair))
             (nr   (cdr (assq :next-review item))))
        (when (and (not (equal (cdr (assq :path item)) exclude-path))
                   (not (cdr (assq :graduated item)))
                   nr (> nr 0))
          (let ((day (resurface--day-number nr)))
            (puthash day (1+ (or (gethash day loads) 0)) loads)))))
    loads))

(defun resurface--leitner-choose-next-review (box exclude-path)
  "Pick a Unix timestamp to next review BOX, balancing spacing and workload.
Considers every day within `resurface-leitner-interval-tolerance' of the
box's ideal interval, and returns the ideal day itself when workload
weighting is disabled or the window collapses to one candidate."
  (let* ((ideal  (resurface--leitner-box-days box))
         (slack  (max 0 (round (* ideal resurface-leitner-interval-tolerance))))
         (lo     (max 1 (- ideal slack)))
         (hi     (+ ideal slack))
         (today  (resurface--day-number (resurface--now)))
         (loads  (resurface--leitner-day-loads exclude-path))
         best best-cost)
    (cl-loop for offset from lo to hi do
      (let* ((day      (+ today offset))
             (load     (or (gethash day loads) 0))
             (spacing  (if (> slack 0) (/ (float (- offset ideal)) slack) 0.0))
             (cost     (+ (* spacing spacing)
                          (* resurface-leitner-workload-weight load))))
        (when (or (null best-cost)
                  (< cost best-cost)
                  (and (= cost best-cost) best (< (abs (- offset ideal)) (abs (- best ideal)))))
          (setq best offset best-cost cost))))
    (* (+ today best) 86400)))


;; ===========================================================================
;;  Item Lifecycle

(defun resurface--leitner-make-item (path)
  "Return a fresh item-alist for PATH placed in Box 1, due immediately."
  (list (cons :path          (expand-file-name path))
        (cons :box           1)
        (cons :history        nil)
        (cons :last-reviewed 0)
        (cons :next-review   0)
        (cons :added         (resurface--now))
        (cons :graduated     nil)
        (cons :paused        nil)))

(defun resurface--leitner-build-item (item &rest overrides)
  "Return a copy of ITEM with OVERRIDES (a :key value plist) applied."
  (let ((new (copy-alist item)))
    (while overrides
      (let ((key (pop overrides)) (val (pop overrides)))
        (if (assq key new) (setcdr (assq key new) val)
          (push (cons key val) new))))
    new))

(defun resurface--leitner-item-graduated-p (item)
  "Return non-nil when ITEM has been graduated (fully mastered)."
  (cdr (assq :graduated item)))

(defun resurface--leitner-item-paused-p (item)
  "Return non-nil when ITEM was last rated Partial."
  (cdr (assq :paused item)))

(defun resurface--leitner-item-due-p (item)
  "Return non-nil when ITEM is due for review.  Graduated items never are."
  (and (not (resurface--leitner-item-graduated-p item))
       (let ((nr (cdr (assq :next-review item))))
         (or (null nr) (= nr 0) (<= nr (resurface--now))))))

(defun resurface--leitner-item-days-until-due (item)
  "Days until ITEM is next due.  Negative = overdue, 0 = never reviewed."
  (let ((nr (cdr (assq :next-review item))))
    (if (or (null nr) (= nr 0)) 0.0
      (/ (- nr (resurface--now)) 86400.0))))

(defun resurface--leitner-item-confidence (item)
  "Return ITEM's current confidence score, recomputed from its history."
  (resurface--leitner-confidence (cdr (assq :history item))))

(defun resurface--leitner-item-confidence-str (item)
  "Return a short display string for ITEM's confidence, or \"—\" with no history."
  (let ((h (cdr (assq :history item))))
    (if (null h) "—" (format "%+.2f" (resurface--leitner-confidence h)))))

(defun resurface--leitner-item-window-str (item)
  "Return ITEM's evidence window as \"F U F . .\", oldest to newest."
  (let* ((filled  (mapcar (lambda (r) (if (> r 0) "F" "U"))
                          (reverse (cdr (assq :history item)))))
         (missing (max 0 (- resurface-leitner-history-length (length filled)))))
    (mapconcat #'identity (append filled (make-list missing "·")) " ")))

(defun resurface--leitner-item-evidence-note (item)
  "Return a plain-language sentence about ITEM's evidence-window state."
  (let* ((n    (length (cdr (assq :history item))))
         (need (- resurface-leitner-history-length n)))
    (if (> need 0)
        (format "%d of %d ratings collected, %d more before this can move box"
                n resurface-leitner-history-length need)
      (format "%d of %d ratings collected, window full, every review re-evaluates"
              resurface-leitner-history-length resurface-leitner-history-length))))

(defun resurface--leitner-outcome-label (outcome old-item new-item)
  "Human-readable line describing what OUTCOME did, from OLD-ITEM to NEW-ITEM.
This distinguishes three states a rating can leave things in:
still collecting evidence, a full window that wasn't enough to act on,
or a threshold that just fired, since all."
  (pcase outcome
    ((or 'familiar 'unfamiliar)
     (let* ((old-box  (cdr (assq :box old-item)))
            (new-box  (cdr (assq :box new-item)))
            (grad-now (and (cdr (assq :graduated new-item))
                           (not (cdr (assq :graduated old-item)))))
            (n        (length (cdr (assq :history new-item))))
            (verb     (if (eq outcome 'familiar) "Familiar" "Unfamiliar")))
       (cond
        (grad-now (propertize (format "%s -> Graduated! Removed from active queue." verb)
                              'face 'success))
        ((/= new-box old-box)
         (format "%s -> confidence %s crossed the line: Box %d -> %d (evidence window reset)"
                 verb (resurface--leitner-item-confidence-str new-item) old-box new-box))
        ((= n resurface-leitner-history-length)
         (format "%s -> confidence %s (window full), not decisive enough, Box %d unchanged"
                 verb (resurface--leitner-item-confidence-str new-item) new-box))
        (t
         (format "%s -> still building evidence (%d/%d), Box %d unchanged"
                 verb n resurface-leitner-history-length new-box)))))
    ('partial (propertize "Paused, due again tomorrow" 'face 'font-lock-doc-face))
    ('revised (propertize "Revised, box unchanged" 'face 'font-lock-doc-face))
    ('skip "Skipped")))

(defun resurface--leitner-item-rate (item outcome)
  "Return a NEW item-alist for ITEM rated with OUTCOME.
OUTCOME is `familiar', `unfamiliar', `reset', `skip', `partial' (paused), or
`revised' (box untouched, but counts as a completed review today)."
  (let* ((path     (cdr (assq :path item)))
         (old-box  (cdr (assq :box item)))
         (last-box (resurface--leitner-num-boxes))
         (now      (resurface--now)))
    (pcase outcome
      ((or 'familiar 'unfamiliar)
       (let* ((history  (resurface--leitner-push-history
                          (cdr (assq :history item))
                          (if (eq outcome 'familiar) 1 -1)))
              (decision (resurface--leitner-decide
                         (resurface--leitner-confidence history) (length history) old-box last-box))
              (action   (car decision))
              (new-box  (cdr decision)))
         (if (eq action 'graduate)
             (resurface--leitner-build-item item
               :last-reviewed now :graduated now :paused nil)
           (resurface--leitner-build-item item
             :box          new-box
             :history      (if (memq action '(promote demote reset)) nil history)
             :last-reviewed now
             :graduated    nil
             :paused       nil
             :next-review  (resurface--leitner-choose-next-review new-box path)))))
      ('reset
       (resurface--leitner-build-item item
         :box 1 :history nil :graduated nil :paused nil :next-review now))
      ('skip item)
      ('partial
       (resurface--leitner-build-item item
         :last-reviewed now :paused t :next-review (+ now 86400)))
      ('revised
       (resurface--leitner-build-item item
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
  (let (result)
    (maphash (lambda (gname g)
               (dolist (item (cdr (assq :items g)))
                 (push (cons gname item) result)))
             (resurface--leitner-groups-ht))
    result))

(defun resurface--leitner-due-pairs (&optional group-name)
  "Due (group-name . item-alist) pairs, optionally filtered to GROUP-NAME."
  (seq-filter
   (lambda (pair)
     (and (or (null group-name) (equal (car pair) group-name))
          (resurface--leitner-item-due-p (cdr pair))))
   (resurface--leitner-all-pairs)))

;; ranks items only for `resurface-leitner-session-max-items', it has no
;; effect on whether something counts as due (`resurface--leitner-item-due-p' alone)
(defun resurface--leitner-item-overdue-amount (item)
  "Return how overdue ITEM is, in days.
Never-reviewed items rank lowest (0.0) here, behind files that sat
past their interval, they're due, but not yet neglected backlog."
  (max 0.0 (- (resurface--leitner-item-days-until-due item))))

(defun resurface--leitner-top-overdue-pairs (pairs n)
  "Return the N most overdue of PAIRS (as from `resurface--leitner-due-pairs')."
  (seq-take
   (sort (copy-sequence pairs)
         (lambda (a b) (> (resurface--leitner-item-overdue-amount (cdr a))
                          (resurface--leitner-item-overdue-amount (cdr b)))))
   n))

(defun resurface--leitner-graduated-pairs (&optional group-name)
  "Graduated (group-name . item-alist) pairs, optionally filtered to GROUP-NAME."
  (seq-filter
   (lambda (pair)
     (and (or (null group-name) (equal (car pair) group-name))
          (resurface--leitner-item-graduated-p (cdr pair))))
   (resurface--leitner-all-pairs)))

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
        (cons :drill-blocks     (make-hash-table :test #'equal))
        (cons :drill-blocks-raw nil)
        (cons :dirty            nil)))

(defun resurface--json-safe-alist (val)
  "Return VAL if it's usable as an alist (a proper list, including nil)."
  (if (listp val) val nil))

(defun resurface--drill-blocks-json-sexp-for-save ()
  "Return the JSON sexp to write for the top-level \"drill_blocks\" key.
When `resurface-drill.el' is loaded, this serialises the live, in-memory
hash-table (`resurface--drill-blocks->json-sexp'), same as always."
  (if (fboundp 'resurface--drill-blocks->json-sexp)
      (resurface--drill-blocks->json-sexp)
    (cdr (assq :drill-blocks-raw resurface--data))))

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
                         (cons 'history       (vconcat (cdr (assq :history item))))
                         (cons 'last_reviewed (cdr (assq :last-reviewed item)))
                         (cons 'next_review   (or (cdr (assq :next-review item)) 0))
                         (cons 'added         (cdr (assq :added item)))
                         (cons 'graduated     (or (cdr (assq :graduated item)) :json-false))
                         (cons 'paused        (if (cdr (assq :paused item)) t :json-false))))
                 items))))
         (push (cons gname (list (cons 'name  gname) (cons 'items encoded-items)))
               groups-list)))
     (resurface--leitner-groups-ht))
    (list (cons 'version       3)
          (cons 'box_intervals resurface-leitner-intervals)
          (cons 'groups        groups-list)
          (cons 'drill_blocks  (resurface--drill-blocks-json-sexp-for-save)))))

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
                        (raw-h   (cdr (assoc "history" raw))))
                   ;; JSON false/null both come back as nil.  Legacy "question"
                   ;; field is ignored, prompts are read live via `resurface--leitner-extract-prompt'.
                   (list (cons :path          (cdr (assoc "path" raw)))
                         (cons :box           box)
                         (cons :history       (if (vectorp raw-h) (append raw-h nil) nil))
                         (cons :last-reviewed lr)
                         (cons :next-review   nr)
                         (cons :added         (cdr (assoc "added" raw)))
                         (cons :graduated     (if (or (null grad) (eq grad :json-false)) nil grad))
                         (cons :paused        (and paused (not (eq paused :json-false)))))))
               items-list)))
        (puthash gname (list (cons :name gname) (cons :items items)) ht)))
    (list (cons :groups       ht)
          ;; parse "drill_blocks" into a real hash-table if drill blocks loaded
          ;; otherwise stash the raw sexp untouched under :drill-blocks-raw
          ;; drill rebuilds the real hash-table from it `resurface--drill-blocks-sync-after-load'.
          (cons :drill-blocks (if (fboundp 'resurface--json-sexp->drill-blocks-ht)
                                   (resurface--json-sexp->drill-blocks-ht sexp)
                                 (make-hash-table :test #'equal)))
          (cons :drill-blocks-raw (unless (fboundp 'resurface--json-sexp->drill-blocks-ht)
                                     (cdr (assoc "drill_blocks" sexp))))
          (cons :dirty        nil))))

;;;###autoload
(defun resurface-save ()
  "Save the Resurface index (Leitner groups + drill blocks) to `resurface-index-file'."
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
        (message "Leitner: nothing due%s, great work!" gsuffix)
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
            (message "Leitner: %d due%s: queuing today's top %d most overdue.  Session starting..."
                     due-count gsuffix (length queue))
          (message "Leitner: %d file%s due%s.  Session starting..."
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
    (resurface--leitner-maybe-refresh-dashboard)
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
         (window   (resurface--leitner-item-window-str item))
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
            (ins (format "  Evidence:       [%s]\n" window))
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
          (window   (resurface--leitner-item-window-str resurface--leitner-review-item))
          (reviewed (cdr (assq :reviewed resurface--leitner-session)))
          (total    (cdr (assq :total    resurface--leitner-session))))
      (concat
       (propertize (format " LEITNER  %d/%d " (1+ reviewed) total) 'face '(:weight bold))
       (propertize (format "  %s" resurface--leitner-review-group) 'face 'mode-line)
       (propertize (format "  Box %d  Conf %s  [%s] " box conf window) 'face '(:slant italic))
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
    (message "Leitner: session ended.  Index saved.")))

(defun resurface-leitner-review-help ()
  "Echo review keybindings in minibuffer."
  (interactive)
  (message "Leitner: C-c l f Familiar   C-c l u Unfamiliar   C-c l p Partial   C-c l r Revised   C-c l s Skip   C-c l q Quit   C-c l ? Help"))


;; ===========================================================================
;;  Dashboard, main entry point showing each group

(defvar resurface-leitner-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'resurface-leitner-menu-view-group)    ; open group detail
    (define-key map (kbd "r")   #'resurface-leitner-menu-start-session) ; review this group
    (define-key map (kbd "a")   #'resurface-leitner-add-file)
    (define-key map (kbd "A")   #'resurface-leitner-add-group)
    (define-key map (kbd "d")   #'resurface-leitner-menu-delete-group)
    (define-key map (kbd "R")   #'resurface-leitner-menu-rename-group)
    (define-key map (kbd "s")   #'resurface-leitner-start-session)      ; review ALL groups
    (define-key map (kbd "S")   #'resurface-save)
    (define-key map (kbd "g")   #'revert-buffer)
    (define-key map (kbd "G")   #'resurface-leitner-review-graduated)   ; browse graduated files
    (define-key map (kbd "?")   #'resurface-leitner-menu-help)
    map)
  "Keymap for the Leitner group dashboard.")

(defun resurface--leitner-menu-format ()
  "Build the tabulated-list column format from `resurface-leitner-intervals'."
  (vconcat
   (list (list "Group"  22 t)
         (list "Files"   7 t)
         (list "Due"     5 t)
         (list "Pause"   6 t)
         (list "Next"    6 t))
   (cl-loop for i from 1 to (resurface--leitner-num-boxes)
            collect (list (format "B%d" i) 5 t))
   (list (list "Grad" 5 t))))

(define-derived-mode resurface-leitner-menu-mode tabulated-list-mode "Leitner"
  "Group overview: due counts and box distribution for each group."
  (setq tabulated-list-format   (resurface--leitner-menu-format))
  (setq tabulated-list-entries  #'resurface--leitner-menu-entries)
  (setq header-line-format
        (propertize "  Leitner: press ? for keybindings"
                    'face '(:inherit shadow :slant italic)))
  (setq-local revert-buffer-function
              (lambda (_auto _noconfirm)
                (resurface--ensure-data)
                (setq tabulated-list-format (resurface--leitner-menu-format)) ; box count may have changed
                (tabulated-list-init-header)
                (tabulated-list-print t)))
  (tabulated-list-init-header))

(defun resurface--leitner-menu-entries ()
  "Compute tabulated-list entries for the dashboard."
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
  (let ((buf (get-buffer-create "*Resurface: Leitner*")))
    (with-current-buffer buf
      (resurface-leitner-menu-mode)
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(defun resurface-leitner-menu-view-group ()
  "Open the group detail view for the group on the current dashboard line."
  (interactive)
  (let ((gname (tabulated-list-get-id)))
    (when gname (resurface-leitner-view-group gname))))

(defun resurface-leitner-menu-start-session ()
  "Start a review session for the group on the current dashboard line."
  (interactive)
  (let ((gname (tabulated-list-get-id)))
    (when gname (resurface-leitner-start-session gname))))

(defun resurface-leitner-menu-delete-group ()
  "Delete the group on the current line, with confirmation."
  (interactive)
  (let ((gname (tabulated-list-get-id)))
    (when (and gname
               (yes-or-no-p (format "Delete group '%s' and all its entries? " gname)))
      (remhash gname (resurface--leitner-groups-ht))
      (resurface--persist)
      (tabulated-list-print t))))

(defun resurface-leitner-menu-rename-group ()
  "Rename the group name on the current dashboard line."
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

(defun resurface-leitner-menu-help ()
  "Echo dashboard keybindings to minibuffer."
  (interactive)
  (message
   "Leitner: RET view  r review  s review-all  G graduated  a add-file  A new-group  R rename  d delete  S save  g refresh"))

(defun resurface--leitner-maybe-refresh-dashboard ()
  "Silently refresh the dashboard buffer if it is alive."
  (let ((buf (get-buffer "*Resurface: Leitner*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (tabulated-list-print t)))))


;; ===========================================================================
;;  Group Detail View  (file list + status for one group)

(defvar resurface-leitner-group-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'resurface-leitner-gv-open-file)
    (define-key map (kbd "r")   #'resurface-leitner-gv-reset-file)
    (define-key map (kbd "d")   #'resurface-leitner-gv-remove-file)
    (define-key map (kbd "a")   #'resurface-leitner-gv-add-file)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "?")   #'resurface-leitner-gv-help)
    map)
  "Keymap for the group detail view.")

(defvar-local resurface--leitner-gv-group nil "Group name this detail view is showing.")

(define-derived-mode resurface-leitner-group-view-mode tabulated-list-mode "Resurface-Leitner-Group"
  "Detail view for one Leitner group: all files and their review status."
  (setq tabulated-list-format (resurface--leitner-gv-column-format))
  (tabulated-list-init-header))

(defun resurface--leitner-gv-column-format ()
  "Return the column format vector for the group detail view.
The Window columns width tracks `resurface-leitner-history-length' so
it never truncates."
  (vector
   '("File"          32 t)
   '("Box"            5 t)
   '("Conf"            6 t)
   (list "Window" (+ 4 (* 2 resurface-leitner-history-length)) t)
   '("Last Reviewed" 14 t)
   '("Due in"        10 nil)
   '("Due?"           5 nil)))

(defun resurface--leitner-gv-build-header (group-name)
  "Build the header-line string for the GROUP-NAME detail view."
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
  "Build tabulated-list entries for GROUP-NAME in group view."
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
            (window (resurface--leitner-item-window-str item))
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
              window
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
      (setq tabulated-list-format (resurface--leitner-gv-column-format))
      (tabulated-list-init-header)
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

(defun resurface-leitner-gv-help ()
  "Echo group-detail keybindings."
  (interactive)
  (message "Leitner group: RET open   r reset   d remove   a add   q close"))


;; ===========================================================================
;;  Graduated Browser
;;
;; Lists every graduated file (across all groups, or just one) so you can
;; check them manually and send anything shaky back to Box 1.  Files you
;; don't touch here simply stay graduated.

(defun resurface--age-str (ts)
  "Return a short \"how long ago\" string for Unix timestamp TS."
  (let ((days (/ (- (resurface--now) ts) 86400.0)))
    (if (< days 1) "<1d" (format "%dd" (floor days)))))

(defun resurface--leitner-grad-column-format ()
  "Column format for the graduated browser."
  [("Group"     16 t)
   ("File"      36 t)
   ("Graduated" 12 t)
   ("Age"        6 t)])

(defun resurface--leitner-grad-entries (&optional group-name)
  "Build tabulated-list entries for the graduated browser.
When GROUP-NAME is non-nil, only that group's graduated files are listed."
  (mapcar
   (lambda (pair)
     (let* ((gname (car pair))
            (item  (cdr pair))
            (path  (cdr (assq :path item)))
            (grad  (cdr (assq :graduated item))))
       (list (cons gname path)
             (vector gname (file-name-nondirectory path)
                     (resurface--format-ts grad) (resurface--age-str grad)))))
   (resurface--leitner-graduated-pairs group-name)))

(defun resurface--leitner-grad-build-header (group-name)
  "Build the header-line string for the graduated browser for GROUP-NAME."
  (let ((n (length (resurface--leitner-graduated-pairs group-name))))
    (propertize
     (format "  %d graduated file%s%s   |   RET open   r bring back   g refresh   q close"
             n (if (= n 1) "" "s")
             (if group-name (format "  (group: %s)" group-name) ""))
     'face '(:inherit shadow :slant italic))))

(defvar-local resurface--leitner-grad-group nil
  "The group this graduated-browser buffer is filtered to, or nil for all groups.")

(defvar resurface-leitner-graduated-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'resurface-leitner-grad-open-file)
    (define-key map (kbd "r")   #'resurface-leitner-grad-bring-back)
    (define-key map (kbd "g")   #'revert-buffer)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "?")   #'resurface-leitner-grad-help)
    map)
  "Keymap for the graduated browser.")

(define-derived-mode resurface-leitner-graduated-mode tabulated-list-mode "Resurface-Leitner-Grad"
  "Browse graduated files and decide whether each one stays retired."
  (setq tabulated-list-format (resurface--leitner-grad-column-format))
  (tabulated-list-init-header))

;;;###autoload
(defun resurface-leitner-review-graduated (&optional group-name)
  "Browse every graduated file and decide whether it stays retired.
With a prefix argument, limit the browser to one GROUP-NAME instead of
every group.  Press r on a file to send it back to Box 1."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Limit to group: " (resurface--leitner-group-names) nil t))))
  (resurface--ensure-data)
  (let ((buf (get-buffer-create
              (if group-name
                  (format "*Resurface: Leitner Graduated (%s)*" group-name)
                "*Resurface: Leitner Graduated*"))))
    (with-current-buffer buf
      (resurface-leitner-graduated-mode)
      (setq resurface--leitner-grad-group      group-name
            tabulated-list-entries   (lambda () (resurface--leitner-grad-entries group-name))
            header-line-format       (resurface--leitner-grad-build-header group-name))
      (setq-local revert-buffer-function
                  (lambda (_auto _noconfirm)
                    (setq header-line-format (resurface--leitner-grad-build-header group-name))
                    (tabulated-list-print t)))
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(defun resurface-leitner-grad-open-file ()
  "Open the file under point in the graduated browser."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id (find-file (cdr id)))))

(defun resurface-leitner-grad-bring-back ()
  "Send the graduated file under point back to Box 1 (explicit reset, always Box 1)."
  (interactive)
  (let* ((id    (tabulated-list-get-id))
         (gname (car id))
         (path  (cdr id))
         (item  (resurface--leitner-find-item gname path)))
    (unless item (user-error "Leitner: no file on current line"))
    (resurface--leitner-replace-item gname path (resurface--leitner-item-rate item 'reset))
    (resurface--persist)
    (setq header-line-format (resurface--leitner-grad-build-header resurface--leitner-grad-group))
    (tabulated-list-print t)
    (resurface--leitner-maybe-refresh-dashboard)
    (message "Leitner: '%s' is back in the rotation at Box 1." (file-name-nondirectory path))))

(defun resurface-leitner-grad-help ()
  "Echo graduated-browser keybindings."
  (interactive)
  (message "Leitner graduated: RET open   r bring back   g refresh   q close"))


;; ===========================================================================
;;  Evil-mode Compatibility
;;
;; Keys `g', `?', `s', `a', `d', `A', `S' are intercepted by evil by default;
;; `evil-set-initial-state' MODE 'emacs tells evil to leave each buffer alone
;; so our own keymaps are consulted.  Tabulated-list navigation (TAB, arrows,
;; n/p) still works since those live in `tabulated-list-mode-map'.  We then
;; add back `j'/`k' as plain evil motion on top.
;;
;; Drill mode modes get the same treatment, but from `resurface-drill.el' itself
(with-eval-after-load 'evil
  (evil-set-initial-state 'resurface-leitner-menu-mode 'emacs)
  (evil-set-initial-state 'resurface-leitner-group-view-mode 'emacs)
  (evil-set-initial-state 'resurface-leitner-graduated-mode 'emacs)
  (evil-set-initial-state 'resurface-leitner-front-mode 'emacs)  ; SPC/s also live in evil-motion-state-map
  (dolist (map (list resurface-leitner-menu-mode-map
                     resurface-leitner-group-view-mode-map
                     resurface-leitner-graduated-mode-map))
    (define-key map (kbd "j") #'evil-next-line)
    (define-key map (kbd "k") #'evil-previous-line))
  (evil-make-overriding-map resurface-leitner-review-minor-mode-map 'normal)
  (add-hook 'resurface-leitner-review-minor-mode-hook #'evil-normalize-keymaps))


(provide 'resurface)
;;; resurface.el ends here
