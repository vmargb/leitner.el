;;; resurface.el --- Resurface material for rereading, drilling, and review  -*- lexical-binding: t; -*-
;; Author: vmargb
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: notes, spaced-repetition, org, feynman
;; URL: https://github.com/vmargb/resurface.el

;;; Commentary:
;;
;; resurface.el brings material back for review on a schedule, instead of
;; leaving it to whenever you happen to remember it exists.  It offers two
;; independent strategies, pick whichever fits what you're reviewing:
;;
;;   - Leitner  (`resurface-leitner'): the classic Leitner box system, run
;;     over WHOLE NOTE FILES instead of individual flashcards.  Write notes
;;     as usual; this surfaces them for review on a Leitner schedule.
;;   - Drill    (`resurface-drill'): rereading short sentences or chunks
;;     (e.g. lines from a language-learning text) until they stop feeling
;;     like work, with no right/wrong grading -- only `clear' vs `opaque'.
;;
;; Both share one JSON index file; FILES ARE NEVER MODIFIED -- all
;; scheduling metadata lives externally in `resurface-index-file'.
;;
;; Quick start -- Leitner (whole files):
;;   M-x resurface-leitner                  Open the group dashboard
;;   M-x resurface-leitner-add-group        Add a new group
;;   M-x resurface-leitner-add-file         Add current buffer's file to a group
;;   M-x resurface-leitner-start-session    Review all due files (C-u: one group)
;;   M-x resurface-leitner-review-graduated Browse graduated files (C-u: one group)
;;
;; Quick start -- Drill (sentences/chunks):
;;   M-x resurface-drill                    Open the drill block dashboard
;;   M-x resurface-drill-add-block          Add a new drill block
;;   M-x resurface-drill-add-sentence       Add a sentence/chunk to drill
;;   M-x resurface-drill-start-session      Drill all due sentences (C-u: one block)
;;   M-x resurface-drill-review-retired     Browse retired sentences (C-u: one block)
;;
;; Every mode has a `?' binding for its keymap.  See README.org for the full
;; review workflow and customisation reference.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'json)
(require 'tabulated-list)


;; ===========================================================================
;;  Customisation

(defgroup resurface nil
  "Leitner spaced repetition for note files."
  :group 'applications
  :prefix "leitner-")

(defcustom resurface-index-file
  (expand-file-name "var/resurface.json" user-emacs-directory)
  "Path to the JSON file storing all review metadata.
Your note files are never touched, so all state lives here."
  :type 'file
  :group 'resurface)

(defcustom resurface-leitner-intervals [1 3 7 14 30 60 90]
  "Review intervals (days) for each Leitner box.
Index 0 = Box 1 (reviewed most frequently)."
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

(defcustom resurface-leitner-before-review-hook nil
  "Hook run right after a note file is revealed in a review session.")

(defcustom resurface-leitner-after-session-hook nil
  "Hook run immediately after a review session is fully completed.")

;; ---------------------------------------------------------------------------
;;  Drill mode (rereading drills, see the Drill Mode section further down)

(defcustom resurface-drill-intervals '((active . 1) (stabilizing . 4) (maintenance . 14))
  "Resurfacing interval (days) for each drill mode.
Unlike `resurface-leitner-intervals', movement between these three modes is
driven by exposure counts and elapsed time, not by whether you \"got it
right\", see `resurface-drill-promote-exposures' and
`resurface-drill-min-days-per-mode'."
  :type '(alist :key-type symbol :value-type integer)
  :group 'resurface)

(defcustom resurface-drill-promote-exposures 3
  "The number of consecutive `clear' outcomes needed.
To advance a sentence to the next mode: active -> stabilizing -> maintenance ->
retired.  An `opaque' outcome resets this count back to zero."
  :type 'integer
  :group 'resurface)

(defcustom resurface-drill-min-days-per-mode 3
  "The minimum number of days a sentence must sit in its current mode.
Before it can be promoted, even once it already has enough
`clear' exposures.  It stops an easy sentence from graduating on day one."
  :type 'integer
  :group 'resurface)

(defcustom resurface-drill-default-block "General"
  "Default drill block name when none is specified."
  :type 'string
  :group 'resurface)

(defcustom resurface-drill-session-max-items nil
  "Maximum number of sentences to queue in a single drill session.
Nil disables the cap, every due sentence is queued."
  :type '(choice (const :tag "No limit" nil)
                  (integer :tag "Max items per session"))
  :group 'resurface)


;; ===========================================================================
;;  Internal State
;;
;; resurface--data  =  ((:groups . HASH-TABLE)  (:dirty . BOOL))
;;   HASH-TABLE  : group-name (string) -> group-alist
;;   group-alist : ((:name . STRING) (:items . LIST-OF-ITEM-ALISTS))
;;   item-alist  : ((:path . STRING) (:box . INT)
;;                  (:last-reviewed . INT) (:added . INT)
;;                  (:graduated . INT-OR-NIL)   ; Unix ts when graduated
;;                  (:paused . BOOL))           ; t after a Partial rating
;;
;; resurface--data  also carries a THIRD top-level key, :drill-blocks, used
;; by Drill Mode (see that section further down) for rereading short
;; sentences/chunks rather than whole files:
;;   HASH-TABLE  : block-id (string) -> block-alist
;;   block-alist : ((:id . STRING) (:name . STRING) (:items . LIST-OF-ITEMS))
;;   item-alist  : ((:id . STRING) (:text . STRING) (:note . STRING-OR-NIL)
;;                  (:mode . SYMBOL)             ; active | stabilizing | maintenance
;;                  (:exposures . INT)           ; consecutive `clear' outcomes, this mode
;;                  (:total-exposures . INT)     ; lifetime rep count (clear + opaque), stat only
;;                  (:mode-entered . INT)        ; Unix ts entering the current mode
;;                  (:last-drilled . INT) (:added . INT)
;;                  (:min-sessions . INT)        ; per-item copy of the promote threshold
;;                  (:retired . INT-OR-NIL))     ; Unix ts when retired
;;
(defvar resurface--data nil
  "In-memory Resurface index (Leitner groups + drill blocks).
Nil until first initialisation.")

;; :queue (group-name . item-alist) pairs left to review, :reviewed count,
;; :total count, :group-filter string-or-nil
(defvar resurface--leitner-session nil
  "Active review session state, or nil when idle.")


;; ===========================================================================
;;  Small Utilities

(defun resurface--now ()
  "Return the current time as a Unix timestamp (integer)."
  (floor (float-time)))

(defun resurface--leitner-num-boxes ()
  "Return the number of Leitner boxes."
  (length resurface-leitner-intervals))

(defun resurface--leitner-box-days (box)
  "Review interval in days for BOX (1-indexed)."
  (aref resurface-leitner-intervals (1- box)))

(defun resurface--leitner-box-secs (box)
  "Review interval in seconds for BOX (1-indexed)."
  (* (resurface--leitner-box-days box) 86400))

(defun resurface--leitner-item-interval-secs (item)
  "Effective review interval in seconds for ITEM."
  (resurface--leitner-box-secs (cdr (assq :box item))))

(defun resurface--leitner-item-due-p (item)
  "Return non-nil when ITEM is due for review.
Graduated items are never due."
  (and (not (cdr (assq :graduated item)))
       (let ((lr (cdr (assq :last-reviewed item))))
         (or (= lr 0)
             (>= (- (resurface--now) lr) (resurface--leitner-item-interval-secs item))))))

(defun resurface--leitner-item-days-until-due (item)
  "Days until ITEM is next due.  Negative = overdue, 0 = never reviewed."
  (let ((lr (cdr (assq :last-reviewed item))))
    (if (= lr 0) 0
      (/ (- (resurface--leitner-item-interval-secs item)
            (- (resurface--now) lr))
         86400.0))))

(defun resurface--leitner-make-item (path)
  "Return a fresh item-alist for PATH placed in Box 1."
  (list (cons :path          (expand-file-name path))
        (cons :box           1)
        (cons :last-reviewed 0)
        (cons :added         (resurface--now))
        (cons :graduated     nil)
        (cons :paused        nil)))

(defun resurface--leitner-item-graduated-p (item)
  "Return non-nil when ITEM has been graduated (fully mastered)."
  (cdr (assq :graduated item)))

(defun resurface--leitner-item-paused-p (item)
  "Return non-nil when ITEM was last rated Partial.
Cleared the next time the item is rated with any other outcome."
  (cdr (assq :paused item)))

(defun resurface--leitner-item-rate (item outcome)
  "Return a NEW item-alist for ITEM rated with OUTCOME.
OUTCOME is one of `good', `reset' (Bad), `skip', `partial', `revised'.
`reset' always sends the item back to Box 1."
  (let* ((old-box (cdr (assq :box item)))
         (last-box (resurface--leitner-num-boxes))
         (graduating (and (eq outcome 'good) (= old-box last-box)))
         (new-box (pcase outcome
                    ('good (if graduating last-box (1+ old-box)))
                    ('reset 1)
                    ((or 'skip 'partial 'revised) old-box)))
         ;; partial backdates :last-reviewed so the effective interval is
         ;; satisfied exactly one day from now
         (interval (resurface--leitner-item-interval-secs item)))
    (list (cons :path          (cdr (assq :path item)))
          (cons :box           new-box)
          (cons :last-reviewed (cond
                                 ((eq outcome 'skip)
                                  (cdr (assq :last-reviewed item)))
                                 ((eq outcome 'partial)
                                  (max 0 (- (resurface--now)
                                            (max 0 (- interval 86400)))))
                                 (t (resurface--now))))
          (cons :added         (cdr (assq :added item)))
          (cons :graduated     (if graduating (resurface--now) nil))
          (cons :paused        (eq outcome 'partial)))))

(defun resurface--format-ts (ts)
  "Format Unix timestamp TS as YYYY-MM-DD, or \"Never\" for 0."
  (if (= ts 0) "Never"
    (format-time-string "%Y-%m-%d" (seconds-to-time ts))))

(defun resurface--shuffle (seq)
  "Return a shuffled copy of SEQ using fisher-yates."
  (let ((v (vconcat seq)))
    (dotimes (i (length v))
      (let ((j (+ i (random (- (length v) i)))))
        (cl-rotatef (aref v i) (aref v j))))
    (append v nil)))

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
    (when g
      (setcdr (assq :items g) items))))

(defun resurface--leitner-prepend-item (group-name item)
  "Add ITEM to the front of GROUP-NAME's items list."
  (resurface--leitner-get-or-create-group group-name)
  (resurface--leitner-set-group-items
   group-name
   (cons item (resurface--leitner-group-items group-name))))

(defun resurface--leitner-replace-item (group-name path new-item)
  "Replace the item with :path = PATH in GROUP-NAME with NEW-ITEM."
  (resurface--leitner-set-group-items
   group-name
   (mapcar (lambda (it)
             (if (equal (cdr (assq :path it)) path) new-item it))
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
(defun resurface--leitner-item-overdue-amount (group-name item)
  "Return how overdue ITEM (in GROUP-NAME) is, in days.
Never-reviewed items rank lowest (0.0) here, behind files that sat
past their interval, they're due, but not yet neglected backlog."
  (max 0.0 (- (resurface--leitner-item-days-until-due item))))

(defun resurface--leitner-top-overdue-pairs (pairs n)
  "Return the N most overdue of PAIRS (as from `resurface--leitner-due-pairs')."
  (seq-take
   (sort (copy-sequence pairs)
         (lambda (a b)
           (> (resurface--leitner-item-overdue-amount (car a) (cdr a))
              (resurface--leitner-item-overdue-amount (car b) (cdr b)))))
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
  (list (cons :groups       (make-hash-table :test #'equal))
        (cons :drill-blocks (make-hash-table :test #'equal))
        (cons :dirty        nil)))

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
                         (cons 'last_reviewed (cdr (assq :last-reviewed item)))
                         (cons 'added         (cdr (assq :added item)))
                         (cons 'graduated     (or (cdr (assq :graduated item))
                                                  :json-false))
                         (cons 'paused        (if (cdr (assq :paused item))
                                                  t :json-false))))
                 items))))
             (push (cons gname (list (cons 'name  gname)
                                     (cons 'items encoded-items)))
                   groups-list)))
     (resurface--leitner-groups-ht))
    (list (cons 'version       2)
          (cons 'box_intervals resurface-leitner-intervals)
          (cons 'groups        groups-list)
          (cons 'drill_blocks  (resurface--drill-blocks->json-sexp)))))

(defun resurface--json-sexp->data (sexp)
  "Parse SEXP (from `json-read' with string keys) into internal data."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (group-pair (cdr (assoc "groups" sexp)))
      (let* ((gname      (car group-pair))
             (gdata      (cdr group-pair))
             (raw-items  (cdr (assoc "items" gdata)))
             (items-list (if (vectorp raw-items) (append raw-items nil) nil))
             (items
              (mapcar
               (lambda (raw)
                 (let ((grad   (cdr (assoc "graduated" raw)))
                       (paused (cdr (assoc "paused" raw))))
                   ;; JSON false/null both come back as nil, Legacy "question" field
                   ;; is intentionally ignored, prompts are read live via `resurface--leitner-extract-prompt'
                   (list (cons :path          (cdr (assoc "path"          raw)))
                         (cons :box           (cdr (assoc "box"           raw)))
                         (cons :last-reviewed (cdr (assoc "last_reviewed" raw)))
                         (cons :added         (cdr (assoc "added"         raw)))
                         (cons :graduated     (if (or (null grad)
                                                      (eq grad :json-false))
                                                  nil grad))
                         (cons :paused        (and paused
                                                    (not (eq paused :json-false)))))))
               items-list)))
        (puthash gname
                 (list (cons :name  gname)
                       (cons :items items))
                 ht)))
    (list (cons :groups       ht)
          (cons :drill-blocks (resurface--json-sexp->drill-blocks-ht sexp))
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

(defun resurface--leitner-all-tracked-paths ()
  "Return a list of all absolute paths currently in the index."
  (let (paths)
    (maphash (lambda (gname _g)
               (dolist (item (resurface--leitner-group-items gname))
                 (push (cdr (assq :path item)) paths)))
             (resurface--leitner-groups-ht))
    paths))

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
  "Record OUTCOME (good/reset/partial/skip/revised) for the current item and advance."
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
    (let* ((grad-p (cdr (assq :graduated new-item)))
           (label  (pcase outcome
                     ('good (if grad-p
                               (propertize "Graduated! Removed from active queue."
                                           'face 'success)
                             (format "Good -> Box %d" (cdr (assq :box new-item)))))
                     ('reset  "Bad -> Box 1")
                     ('partial (propertize "Paused, due again tomorrow"
                                           'face 'font-lock-doc-face))
                     ('revised (propertize "Revised, box unchanged"
                                           'face 'font-lock-doc-face))
                     ('skip "Skipped"))))
      (message "Leitner: %s  (%d / %d done)"
               label
               (cdr (assq :reviewed resurface--leitner-session))
               (cdr (assq :total    resurface--leitner-session))))
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
    (if (and capped (> remaining 0))
        (message "Leitner: session complete, %d file%s reviewed.  %d more due file%s waiting, run `resurface-leitner-start-session' again when you're ready. Index saved."
                 n (if (= n 1) "" "s")
                 remaining (if (= remaining 1) "" "s"))
      (message "Leitner: session complete, %d file%s reviewed. Index saved."
                n (if (= n 1) "" "s")))))

;; ===========================================================================
;;  Front Card: recall before reveal

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
         (buf      (get-buffer-create resurface--leitner-front-buf)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (resurface-leitner-front-mode)
        (setq resurface--leitner-front-item  item
              resurface--leitner-front-group gname)
        ;; Layout: header bar / meta / blank / concept name (big, centred) /
        ;; blank / prompt lines / key hints.
        (cl-flet ((ins (str &optional face)
                    (insert (if face (propertize str 'face face) str))))
          (let* ((width   (max 50 (- (window-width) 4)))
                 (rule    (concat "  " (make-string width ?-) "\n")))
            (ins "\n")
            (ins (format "  LEITNER  %d / %d\n" (1+ reviewed) total) '(:weight bold))
            (ins rule 'shadow)
            (ins "\n")
            (ins (format "  Group:          %s\n" gname))
            (ins (format "  Box:            %d  (every %d day%s)\n"
                         box interval (if (= interval 1) "" "s")))
            (ins (format "  Last reviewed:  %s\n" (resurface--format-ts lr)))
            (ins "\n\n")
            (ins (concat "  " fname) '(:weight bold :height 1.2))
            (ins "\n\n\n")
            ;; The prompt line only appears when one is found in the file.
            (let ((prompt (resurface--leitner-extract-prompt path)))
              (when (and prompt (not (string-empty-p prompt)))
                (ins "  Prompt / Question:\n" 'shadow)
                (ins (format "  %s\n\n\n" prompt) '(:weight bold :height 1.1))))
            (ins "  Recall from memory before revealing.\n" '(:slant italic))
            (ins "  When ready, press SPC to open your notes.\n" '(:slant italic))
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
    (kill-buffer fc)))

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
    (define-key map (kbd "C-c l g") #'resurface-leitner-rate-good)
    (define-key map (kbd "C-c l b") #'resurface-leitner-rate-bad)
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
recall when done."
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
          (reviewed (cdr (assq :reviewed resurface--leitner-session)))
          (total    (cdr (assq :total    resurface--leitner-session))))
      (concat
       (propertize (format " LEITNER  %d/%d " (1+ reviewed) total) 'face '(:weight bold))
       (propertize (format "  %s" resurface--leitner-review-group) 'face 'mode-line)
       (propertize (format "  Box %d " box)
                   'face '(:slant italic))
       (propertize "    C-c l g Good   C-c l b Bad   C-c l p Partial   C-c l r Revised   C-c l s Skip   C-c l q Quit"
                   'face '(:inherit shadow))))))

(defun resurface--leitner-on-review-buffer-kill ()
  "Warn when a review buffer is killed mid-session."
  (when (and resurface-leitner-review-minor-mode resurface--leitner-session)
    (message "Leitner: review buffer killed -- use M-x resurface-leitner-start-session to resume.")))

;;;###autoload
(defun resurface-leitner-rate-good ()
  "Rate current review item GOOD (move up one box)."
  (interactive)
  (resurface--leitner-session-record 'good))

;;;###autoload
(defun resurface-leitner-rate-bad ()
  "Rate current review item BAD (reset straight to Box 1)."
  (interactive)
  (resurface--leitner-session-record 'reset))

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
  "Skip current review item (keep its box)."
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
  (message "Leitner: C-c l g Good   C-c l b Bad   C-c l p Partial   C-c l r Revised   C-c l s Skip   C-c l q Quit   C-c l ? Help"))

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
  "Return the column format vector for the group detail view."
  [("File"          36 t)
   ("Box"            5 t)
   ("Last Reviewed" 14 t)
   ("Due in"        10 nil)
   ("Due?"           5 nil)])

(defun resurface--leitner-gv-build-header (group-name)
  "Build the header-line string for the GROUP-NAME detail view."
  (let* ((items  (resurface--leitner-group-items group-name))
         (n      (length items))
         (grad   (length (seq-filter #'resurface--leitner-item-graduated-p items)))
         (active (- n grad))
         (due    (length (seq-filter #'resurface--leitner-item-due-p items)))
         (paused (length (seq-filter #'resurface--leitner-item-paused-p items))))
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
      ;; 'reset is the only outcome that both forces Box 1 *and* clears
      ;; :graduated, there is no separate 'bad outcome.
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
  "Send the graduated file under point back to Box 1.
Same as rating it Reset from a normal review: box resets to 1, the
graduated flag clears."
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
;;  Drill Mode: rereading drills (no recall grading)
;;
;;  Drill Data Accessors

(defun resurface--drill-blocks-ht ()
  "Return the drill-blocks hash-table."
  (cdr (assq :drill-blocks resurface--data)))

(defun resurface--drill-block-names ()
  "Return a sorted list of all drill block names.
the hash-table itself is keyed by block *id* (so a block stays
identifiable even if renamed later)."
  (sort (mapcar (lambda (b) (cdr (assq :name b)))
                (hash-table-values (resurface--drill-blocks-ht)))
        #'string<))

(defun resurface--drill-new-id ()
  "Return a fresh, reasonably unique id string."
  (format "%d-%04d" (resurface--now) (random 10000)))

(defun resurface--drill-get-block (block-id)
  "Return the block-alist for BLOCK-ID, or nil."
  (gethash block-id (resurface--drill-blocks-ht)))

(defun resurface--drill-get-block-by-name (name)
  "Return the block-alist whose :name is NAME, or nil."
  (seq-find (lambda (b) (equal (cdr (assq :name b)) name))
            (hash-table-values (resurface--drill-blocks-ht))))

(defun resurface--drill-get-or-create-block (name)
  "Return the block-alist for NAME, creating a fresh block if needed."
  (or (resurface--drill-get-block-by-name name)
      (let* ((id (resurface--drill-new-id))
             (b  (list (cons :id id) (cons :name name) (cons :items nil))))
        (puthash id b (resurface--drill-blocks-ht))
        b)))

(defun resurface--drill-block-items (block-id)
  "Return the items list for BLOCK-ID (may be nil)."
  (cdr (assq :items (resurface--drill-get-block block-id))))

;; setcdr+assq mutates in-place, same trick as `resurface--leitner-set-group-items'
(defun resurface--drill-set-block-items (block-id items)
  "Replace the items list of BLOCK-ID with ITEMS (mutates in-place)."
  (let ((b (gethash block-id (resurface--drill-blocks-ht))))
    (when b (setcdr (assq :items b) items))))

(defun resurface--drill-find-item (block-id item-id)
  "Return the item in BLOCK-ID with :id ITEM-ID, or nil."
  (seq-find (lambda (it) (equal (cdr (assq :id it)) item-id))
            (resurface--drill-block-items block-id)))

(defun resurface--drill-prepend-item (block-id item)
  "Add ITEM to the front of BLOCK-ID's items list."
  (resurface--drill-set-block-items
   block-id (cons item (resurface--drill-block-items block-id))))

(defun resurface--drill-replace-item (block-id item-id new-item)
  "Replace the item with :id = ITEM-ID in BLOCK-ID with NEW-ITEM."
  (resurface--drill-set-block-items
   block-id
   (mapcar (lambda (it) (if (equal (cdr (assq :id it)) item-id) new-item it))
           (resurface--drill-block-items block-id))))

(defun resurface--drill-all-pairs ()
  "All (block-id . item-alist) pairs across every drill block."
  (let (result)
    (maphash (lambda (bid b)
               (dolist (item (cdr (assq :items b)))
                 (push (cons bid item) result)))
             (resurface--drill-blocks-ht))
    result))

(defun resurface--drill-item-retired-p (item)
  "Return non-nil when ITEM has been retired (fully mastered)."
  (cdr (assq :retired item)))

(defun resurface--drill-item-interval-secs (item)
  "Effective resurfacing interval in seconds for ITEM, from its :mode."
  (* 86400 (or (cdr (assq (cdr (assq :mode item)) resurface-drill-intervals)) 1)))

(defun resurface--drill-item-due-p (item)
  "Return non-nil when ITEM is due for a drill pass.  Retired items never are."
  (and (not (resurface--drill-item-retired-p item))
       (let ((lr (cdr (assq :last-drilled item))))
         (or (= lr 0)
             (>= (- (resurface--now) lr) (resurface--drill-item-interval-secs item))))))

(defun resurface--drill-item-days-until-due (item)
  "Days until ITEM is next due.  Negative = overdue, 0 = never drilled."
  (let ((lr (cdr (assq :last-drilled item))))
    (if (= lr 0) 0
      (/ (- (resurface--drill-item-interval-secs item) (- (resurface--now) lr))
         86400.0))))

(defun resurface--drill-item-overdue-amount (item)
  "How overdue ITEM is, in days.  Used only for session-cap trimming."
  (max 0.0 (- (resurface--drill-item-days-until-due item))))

(defun resurface--drill-top-overdue-pairs (pairs n)
  "Return the N most overdue of PAIRS (as from `resurface--drill-due-pairs')."
  (seq-take
   (sort (copy-sequence pairs)
         (lambda (a b) (> (resurface--drill-item-overdue-amount (cdr a))
                           (resurface--drill-item-overdue-amount (cdr b)))))
   n))

(defun resurface--drill-due-pairs (&optional block-name)
  "Due (block-id . item-alist) pairs, optionally filtered to BLOCK-NAME."
  (let ((bid (and block-name (cdr (assq :id (resurface--drill-get-block-by-name block-name))))))
    (seq-filter
     (lambda (pair)
       (and (or (null block-name) (equal (car pair) bid))
            (resurface--drill-item-due-p (cdr pair))))
     (resurface--drill-all-pairs))))

(defun resurface--drill-retired-pairs (&optional block-name)
  "Retired (block-id . item-alist) pairs, optionally filtered to BLOCK-NAME."
  (let ((bid (and block-name (cdr (assq :id (resurface--drill-get-block-by-name block-name))))))
    (seq-filter
     (lambda (pair)
       (and (or (null block-name) (equal (car pair) bid))
            (resurface--drill-item-retired-p (cdr pair))))
     (resurface--drill-all-pairs))))

(defun resurface--drill-mode-label (mode)
  "Human-readable label for drill MODE."
  (pcase mode
    ('active      "Active")
    ('stabilizing "Stabilizing")
    ('maintenance "Maintenance")
    (_            "?")))

(defun resurface--drill-next-mode (mode)
  "The mode after MODE on the promotion ladder, or nil at the top (retire)."
  (pcase mode
    ('active      'stabilizing)
    ('stabilizing 'maintenance)
    ('maintenance nil)))

(defun resurface--drill-truncate (text n)
  "Truncate TEXT to at most N characters, appending an ellipsis if cut."
  (if (> (length text) n) (concat (substring text 0 (max 0 (1- n))) "...") text))

;; ---------------------------------------------------------------------------
;;  Drill Item Lifecycle

(defun resurface--drill-make-item (text &optional note)
  "Return a fresh drill item-alist for TEXT (and optional NOTE), Active mode."
  (let ((now (resurface--now)))
    (list (cons :id             (resurface--drill-new-id))
          (cons :text            (string-trim text))
          (cons :note             (and note (not (string-empty-p note)) note))
          (cons :mode              'active)
          (cons :exposures         0)
          (cons :total-exposures   0)
          (cons :mode-entered      now)
          (cons :last-drilled      0)
          (cons :added             now)
          (cons :min-sessions      resurface-drill-promote-exposures)
          (cons :retired           nil))))

(defun resurface--drill-item-reactivate (item)
  "Return a copy of ITEM reset to `active' mode, cleared of :retired.
Used both when manually reactivating a retired sentence, and as the
demotion target when a `maintenance' item comes back `opaque'."
  (list (cons :id               (cdr (assq :id item)))
        (cons :text              (cdr (assq :text item)))
        (cons :note              (cdr (assq :note item)))
        (cons :mode               'active)
        (cons :exposures          0)
        (cons :total-exposures    (cdr (assq :total-exposures item)))
        (cons :mode-entered       (resurface--now))
        (cons :last-drilled       0)
        (cons :added              (cdr (assq :added item)))
        (cons :min-sessions       (cdr (assq :min-sessions item)))
        (cons :retired            nil)))

(defun resurface--drill-item-rate (item outcome)
  "Return a NEW drill item-alist for ITEM rated with OUTCOME.
OUTCOME is one of `clear' (this pass felt smooth), `opaque' (still
needs work), `retire' (force-retire right now), or `skip' (leave the
item untouched)."
  (let* ((mode             (cdr (assq :mode            item)))
         (exposures        (cdr (assq :exposures        item)))
         (total            (cdr (assq :total-exposures  item)))
         (mode-entered     (cdr (assq :mode-entered      item)))
         (min-sessions     (cdr (assq :min-sessions      item)))
         (now              (resurface--now))
         (new-mode         mode)
         (new-exposures    exposures)
         (new-mode-entered mode-entered)
         (new-last-drilled (cdr (assq :last-drilled item)))
         (new-retired      (cdr (assq :retired item)))
         (bump-total       (not (memq outcome '(skip retire)))))
    (pcase outcome
      ('retire (setq new-retired now))
      ('opaque
       (let* ((demote        (eq mode 'maintenance))
              (m2            (if demote 'active mode))
              (interval-secs (* 86400 (or (cdr (assq m2 resurface-drill-intervals)) 1))))
         (setq new-mode         m2
               new-exposures    0
               new-mode-entered (if demote now mode-entered)
               ;; always resurface tomorrow, regardless of the mode
               new-last-drilled (max 0 (- now (max 0 (- interval-secs 86400)))))))
      ('clear
       (let* ((tentative    (1+ exposures))
              (days-in-mode (/ (- now mode-entered) 86400.0))
              (ready        (and (>= tentative min-sessions)
                                  (>= days-in-mode resurface-drill-min-days-per-mode)))
              (next         (and ready (resurface--drill-next-mode mode))))
         (setq new-last-drilled now)
         (cond
          ((and ready (eq mode 'maintenance)) (setq new-retired now))
          (next (setq new-mode next new-exposures 0 new-mode-entered now))
          (t    (setq new-exposures tentative)))))
      ('skip nil))
    (list (cons :id               (cdr (assq :id item)))
          (cons :text              (cdr (assq :text item)))
          (cons :note              (cdr (assq :note item)))
          (cons :mode              new-mode)
          (cons :exposures         new-exposures)
          (cons :total-exposures   (if bump-total (1+ total) total))
          (cons :mode-entered      new-mode-entered)
          (cons :last-drilled      new-last-drilled)
          (cons :added             (cdr (assq :added item)))
          (cons :min-sessions      min-sessions)
          (cons :retired           new-retired))))

;; ---------------------------------------------------------------------------
;;  Drill Persistence (folded into the main resurface--data JSON blob)

(defun resurface--drill-blocks->json-sexp ()
  "Convert the :drill-blocks hash-table to a JSON-encodable sexp."
  (let (blocks-list)
    (maphash
     (lambda (bid b)
       (let* ((items (cdr (assq :items b)))
              (encoded
               (vconcat
                (mapcar
                 (lambda (item)
                   (list (cons 'id              (cdr (assq :id item)))
                         (cons 'text             (cdr (assq :text item)))
                         (cons 'note             (or (cdr (assq :note item)) :json-false))
                         (cons 'mode             (symbol-name (cdr (assq :mode item))))
                         (cons 'exposures        (cdr (assq :exposures item)))
                         (cons 'total_exposures  (cdr (assq :total-exposures item)))
                         (cons 'mode_entered     (cdr (assq :mode-entered item)))
                         (cons 'last_drilled     (cdr (assq :last-drilled item)))
                         (cons 'added            (cdr (assq :added item)))
                         (cons 'min_sessions     (cdr (assq :min-sessions item)))
                         (cons 'retired          (or (cdr (assq :retired item)) :json-false))))
                 items))))
         (push (cons bid (list (cons 'id bid) (cons 'name (cdr (assq :name b)))
                               (cons 'items encoded)))
               blocks-list)))
     (resurface--drill-blocks-ht))
    blocks-list))

(defun resurface--json-sexp->drill-blocks-ht (sexp)
  "Parse the drill_blocks portion of the full index SEXP into a hash-table.
Missing or absent \"drill_blocks\" (older index files) yields an empty
table, so loading a pre-drill-mode index just starts with zero blocks."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (block-pair (cdr (assoc "drill_blocks" sexp)))
      (let* ((bid        (car block-pair))
             (bdata      (cdr block-pair))
             (name       (cdr (assoc "name" bdata)))
             (raw-items  (cdr (assoc "items" bdata)))
             (items-list (if (vectorp raw-items) (append raw-items nil) nil))
             (items
              (mapcar
               (lambda (raw)
                 (let ((note    (cdr (assoc "note" raw)))
                       (retired (cdr (assoc "retired" raw))))
                   (list (cons :id               (cdr (assoc "id" raw)))
                         (cons :text              (cdr (assoc "text" raw)))
                         (cons :note              (if (or (null note) (eq note :json-false)) nil note))
                         (cons :mode              (intern (cdr (assoc "mode" raw))))
                         (cons :exposures         (cdr (assoc "exposures" raw)))
                         (cons :total-exposures   (or (cdr (assoc "total_exposures" raw)) 0))
                         (cons :mode-entered      (cdr (assoc "mode_entered" raw)))
                         (cons :last-drilled      (cdr (assoc "last_drilled" raw)))
                         (cons :added             (cdr (assoc "added" raw)))
                         (cons :min-sessions      (or (cdr (assoc "min_sessions" raw))
                                                       resurface-drill-promote-exposures))
                         (cons :retired           (if (or (null retired) (eq retired :json-false))
                                                       nil retired)))))
               items-list)))
        (puthash bid (list (cons :id bid) (cons :name name) (cons :items items)) ht)))
    ht))

;; ---------------------------------------------------------------------------
;;  Adding / Removing Blocks & Sentences

(defun resurface--drill-read-block-name (&optional prompt)
  "PROMPT for a drill block name with completion."
  (let* ((names   (resurface--drill-block-names))
         (default resurface-drill-default-block)
         (pr      (or prompt (format "Drill block (default %s): " default))))
    (completing-read pr names nil nil nil nil default)))

;;;###autoload
(defun resurface-drill-add-block (name)
  "Create a new empty drill block called NAME."
  (interactive "sNew drill block name: ")
  (resurface--ensure-data)
  (if (resurface--drill-get-block-by-name name)
      (message "Drill: block '%s' already exists." name)
    (resurface--drill-get-or-create-block name)
    (resurface--persist)
    (message "Drill: block '%s' created." name)
    (resurface--maybe-refresh-drill-dashboard)))

;;;###autoload
(defun resurface-drill-add-sentence (&optional text block note)
  "Add TEXT as a new drill item to BLOCK (prompted if omitted).
With NOTE, attach an optional gloss/translation shown alongside the
sentence during review.  The item starts in Active mode."
  (interactive)
  (resurface--ensure-data)
  (let* ((txt (string-trim (or text (read-string "Sentence / chunk to drill: ")))))
    (when (string-empty-p txt)
      (user-error "Drill: sentence is empty"))
    (let* ((grp (or block (resurface--drill-read-block-name)))
           (nte (or note
                    (when (called-interactively-p 'any)
                      (let ((s (read-string "Note / gloss (RET to skip): ")))
                        (unless (string-empty-p s) s)))))
           (b   (resurface--drill-get-or-create-block grp)))
      (resurface--drill-prepend-item (cdr (assq :id b)) (resurface--drill-make-item txt nte))
      (resurface--persist)
      (message "Drill: added sentence to block '%s' (active)." grp)
      (resurface--maybe-refresh-drill-dashboard))))

;;;###autoload
(defun resurface-drill-add-lines-from-region (start end &optional block)
  "Split the region between START and END on newlines.
add each non-blank line as its own drill item in BLOCK (prompted if omitted)."
  (interactive "r")
  (resurface--ensure-data)
  (let* ((grp   (or block (resurface--drill-read-block-name)))
         (b     (resurface--drill-get-or-create-block grp))
         (bid   (cdr (assq :id b)))
         (lines (seq-remove #'string-empty-p
                            (mapcar #'string-trim
                                    (split-string (buffer-substring-no-properties start end) "\n"))))
         (n 0))
    (dolist (line lines)
      (resurface--drill-prepend-item bid (resurface--drill-make-item line))
      (cl-incf n))
    (when (> n 0)
      (resurface--persist)
      (resurface--maybe-refresh-drill-dashboard))
    (message "Drill: added %d sentence(s) to block '%s'." n grp)))

;;;###autoload
(defun resurface-drill-remove-block (name)
  "Delete drill block NAME and all its sentences."
  (interactive (list (completing-read "Delete drill block: " (resurface--drill-block-names) nil t)))
  (resurface--ensure-data)
  (let ((b (resurface--drill-get-block-by-name name)))
    (when (and b (yes-or-no-p (format "Delete drill block '%s' and all its entries? " name)))
      (remhash (cdr (assq :id b)) (resurface--drill-blocks-ht))
      (resurface--persist)
      (message "Drill: block '%s' deleted." name)
      (resurface--maybe-refresh-drill-dashboard))))

;; ---------------------------------------------------------------------------
;;  Drill Session
;;
;; Unlike a file review, there is no "reveal" step: the sentence is shown
;; directly, since drilling is about rereading it, not testing recall.

(defvar resurface--drill-session nil
  "Active drill session state, or nil when idle.
Shape mirrors `resurface--leitner-session': :queue (block-id . item-alist) pairs
left to drill, :reviewed count, :total count, :block-filter
string-or-nil, :capped bool.")

;;;###autoload
(defun resurface-drill-start-session (&optional block-name)
  "Start a drill session for all currently due sentences.
With an optional prefix argument, prompt to limit the session to one
BLOCK-NAME instead of every block."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Limit session to block: " (resurface--drill-block-names) nil t))))
  (resurface--ensure-data)
  (when (and resurface--drill-session
             (not (yes-or-no-p "A drill session is already running.  Start a new one? ")))
    (user-error "Drill session aborted"))
  (let* ((due       (resurface--drill-due-pairs block-name))
         (due-count (length due))
         (bsuffix   (if block-name (format " in '%s'" block-name) ""))
         (capped    (and (integerp resurface-drill-session-max-items)
                          (> resurface-drill-session-max-items 0)
                          (> due-count resurface-drill-session-max-items)))
         (due       (if capped
                        (resurface--drill-top-overdue-pairs due resurface-drill-session-max-items)
                      due)))
    (if (null due)
        (message "Drill: nothing due%s, all caught up." bsuffix)
      (setq resurface--drill-session
            (list (cons :queue        (resurface--shuffle due))
                  (cons :reviewed     0)
                  (cons :total        (length due))
                  (cons :block-filter block-name)
                  (cons :capped       capped)))
      (if capped
          (message "Drill: %d due%s, queuing today's top %d most overdue.  Session starting..."
                   due-count bsuffix (length due))
        (message "Drill: %d sentence%s due%s.  Session starting..."
                 (length due) (if (= (length due) 1) "" "s") bsuffix))
      (resurface--drill-session-advance))))

(defun resurface--drill-session-advance ()
  "Show the next due sentence, or finish the session."
  (let ((queue (cdr (assq :queue resurface--drill-session))))
    (if (null queue)
        (resurface--drill-session-finish)
      (resurface--drill-show-item (car queue)))))

(defun resurface--drill-session-record (outcome)
  "Rate the sentence currently on screen with OUTCOME and advance."
  (unless resurface--drill-session
    (user-error "Drill: no active session"))
  (let* ((queue    (cdr (assq :queue resurface--drill-session)))
         (pair     (car queue))
         (bid      (car pair))
         (item     (cdr pair))
         (item-id  (cdr (assq :id item)))
         (new-item (resurface--drill-item-rate item outcome)))
    (resurface--drill-replace-item bid item-id new-item)
    (cl-incf (cdr (assq :reviewed resurface--drill-session)))
    (setcdr (assq :queue resurface--drill-session) (cdr queue))
    (let* ((retired-p (cdr (assq :retired new-item)))
           (label (pcase outcome
                    ('clear  (if retired-p
                                 (propertize "Retired! Fully saturated, out of the drill pool." 'face 'success)
                               (format "Clear -> %s" (resurface--drill-mode-label (cdr (assq :mode new-item))))))
                    ('opaque (propertize (format "Still opaque, back tomorrow (%s)"
                                                 (resurface--drill-mode-label (cdr (assq :mode new-item))))
                                         'face 'font-lock-doc-face))
                    ('retire (propertize "Retired manually." 'face 'success))
                    ('skip   "Skipped"))))
      (message "Drill: %s  (%d / %d done)" label
               (cdr (assq :reviewed resurface--drill-session))
               (cdr (assq :total    resurface--drill-session))))
    (resurface--drill-session-advance)))

(defun resurface--drill-session-finish ()
  "Clean up and save after every due sentence has been drilled."
  (let* ((n         (cdr (assq :reviewed resurface--drill-session)))
         (capped    (cdr (assq :capped       resurface--drill-session)))
         (bfilter   (cdr (assq :block-filter resurface--drill-session)))
         (remaining (and capped (length (resurface--drill-due-pairs bfilter)))))
    (setq resurface--drill-session nil)
    (resurface-save)
    (resurface--maybe-refresh-drill-dashboard)
    (when (get-buffer resurface--drill-buf) (kill-buffer resurface--drill-buf))
    (if (and capped (> remaining 0))
        (message "Drill: session complete, %d sentence%s drilled.  %d more due, run `resurface-drill-start-session' again when ready.  Index saved."
                 n (if (= n 1) "" "s") remaining)
      (message "Drill: session complete, %d sentence%s drilled.  Index saved."
                n (if (= n 1) "" "s")))))

;; ---------------------------------------------------------------------------
;;  Drill Session Buffer

(defconst resurface--drill-buf "*Resurface: Drill Session*"
  "Name of the buffer used to display the sentence currently under drill.")

(defvar-local resurface--drill-item  nil "Item currently shown in the drill buffer.")
(defvar-local resurface--drill-block nil "Block id of the item currently shown.")

(defvar resurface-drill-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'resurface-drill-mark-clear)
    (define-key map (kbd "o") #'resurface-drill-mark-opaque)
    (define-key map (kbd "R") #'resurface-drill-mark-retire)
    (define-key map (kbd "s") #'resurface-drill-skip)
    (define-key map (kbd "q") #'resurface-drill-quit-session)
    (define-key map (kbd "?") #'resurface-drill-help)
    map)
  "Keymap for `resurface-drill-session-mode'.")

(define-derived-mode resurface-drill-session-mode special-mode "Resurface-Drill"
  "Read-only buffer showing the sentence currently under drill.
Unlike a normal Leitner review there is nothing to reveal: reread the
sentence until it stops feeling like work, then rate it."
  :interactive nil)

(defun resurface--drill-show-item (pair)
  "Display PAIR (block-id . item-alist) in the drill session buffer."
  (let* ((bid       (car pair))
         (item      (cdr pair))
         (bname     (cdr (assq :name (resurface--drill-get-block bid))))
         (mode      (cdr (assq :mode item)))
         (text      (cdr (assq :text item)))
         (note      (cdr (assq :note item)))
         (exposures (cdr (assq :exposures item)))
         (min-sess  (cdr (assq :min-sessions item)))
         (lr        (cdr (assq :last-drilled item)))
         (reviewed  (cdr (assq :reviewed resurface--drill-session)))
         (total     (cdr (assq :total    resurface--drill-session)))
         (buf       (get-buffer-create resurface--drill-buf)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (resurface-drill-session-mode)
        (setq resurface--drill-item  item
              resurface--drill-block bid)
        (cl-flet ((ins (str &optional face)
                    (insert (if face (propertize str 'face face) str))))
          (let* ((width (max 50 (- (window-width) 4)))
                 (rule  (concat "  " (make-string width ?-) "\n")))
            (ins "\n")
            (ins (format "  DRILL  %d / %d\n" (1+ reviewed) total) '(:weight bold))
            (ins rule 'shadow)
            (ins "\n")
            (ins (format "  Block:          %s\n" bname))
            (ins (format "  Mode:           %s  (%d/%d exposures this mode)\n"
                         (resurface--drill-mode-label mode) exposures min-sess))
            (ins (format "  Last drilled:   %s\n" (resurface--format-ts lr)))
            (ins "\n\n")
            (ins (concat "  " text) '(:weight bold :height 1.2))
            (ins "\n")
            (when (and note (not (string-empty-p note)))
              (ins "\n")
              (ins (format "  %s\n" note) '(:slant italic :height 0.9)))
            (ins "\n\n")
            (ins "  Reread it, slowly, then at speed, until it stops feeling like work.\n"
                 '(:slant italic))
            (ins "\n")
            (ins rule 'shadow)
            (ins "  [c] Clear     [o] Still opaque     [R] Retire now     [s] Skip     [q] Quit\n"
                 'shadow)))
        (goto-char (point-min))))
    (switch-to-buffer buf)))

;;;###autoload
(defun resurface-drill-mark-clear ()
  "Rate the current drill sentence CLEAR (this pass felt smooth)."
  (interactive)
  (resurface--drill-session-record 'clear))

;;;###autoload
(defun resurface-drill-mark-opaque ()
  "Rate the current drill sentence OPAQUE (still needs more exposure)."
  (interactive)
  (resurface--drill-session-record 'opaque))

;;;###autoload
(defun resurface-drill-mark-retire ()
  "Force-retire the current drill sentence right now.
Regardless of its mode or exposure thresholds."
  (interactive)
  (resurface--drill-session-record 'retire))

;;;###autoload
(defun resurface-drill-skip ()
  "Skip the current drill sentence without rating it."
  (interactive)
  (resurface--drill-session-record 'skip))

;;;###autoload
(defun resurface-drill-quit-session ()
  "End the current drill session early and save progress."
  (interactive)
  (when (yes-or-no-p "Quit this drill session (progress so far is saved)? ")
    (setq resurface--drill-session nil)
    (resurface-save)
    (when (get-buffer resurface--drill-buf) (kill-buffer resurface--drill-buf))
    (message "Drill: session ended.  Index saved.")))

(defun resurface-drill-help ()
  "Echo drill session keybindings in the echo area."
  (interactive)
  (message "Drill: [c] Clear   [o] Opaque   [R] Retire now   [s] Skip   [q] Quit"))

;; ---------------------------------------------------------------------------
;;  Drill Dashboard

(defvar resurface-drill-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'resurface-drill-menu-view-block)
    (define-key map (kbd "r")   #'resurface-drill-menu-start-session)
    (define-key map (kbd "a")   #'resurface-drill-add-sentence)
    (define-key map (kbd "A")   #'resurface-drill-add-block)
    (define-key map (kbd "d")   #'resurface-drill-menu-delete-block)
    (define-key map (kbd "s")   #'resurface-drill-start-session)
    (define-key map (kbd "S")   #'resurface-save)
    (define-key map (kbd "g")   #'revert-buffer)
    (define-key map (kbd "G")   #'resurface-drill-review-retired)
    (define-key map (kbd "?")   #'resurface-drill-menu-help)
    map)
  "Keymap for the drill block dashboard.")

(defun resurface--drill-menu-format ()
  "Column format for the drill block dashboard."
  [("Block"       22 t)
   ("Items"        7 t)
   ("Due"          5 t)
   ("Active"       8 t)
   ("Stabilizing" 12 t)
   ("Maintenance" 12 t)
   ("Retired"      8 t)])

(define-derived-mode resurface-drill-menu-mode tabulated-list-mode "Resurface-Drill"
  "Drill block overview: due counts and mode distribution for each block."
  (setq tabulated-list-format  (resurface--drill-menu-format))
  (setq tabulated-list-entries #'resurface--drill-menu-entries)
  (setq header-line-format
        (propertize "  Drill: rereading sentences, press ? for keybindings"
                    'face '(:inherit shadow :slant italic)))
  (setq-local revert-buffer-function
              (lambda (_auto _noconfirm) (resurface--ensure-data) (tabulated-list-print t)))
  (tabulated-list-init-header))

(defun resurface--drill-menu-entries ()
  "Compute tabulated-list entries for the drill dashboard."
  (resurface--ensure-data)
  (mapcar
   (lambda (bname)
     (let* ((b            (resurface--drill-get-block-by-name bname))
            (bid          (cdr (assq :id b)))
            (items        (resurface--drill-block-items bid))
            (n            (length items))
            (active-items (seq-remove #'resurface--drill-item-retired-p items))
            (due          (length (seq-filter #'resurface--drill-item-due-p active-items)))
            (nactive      (length (seq-filter (lambda (it) (eq (cdr (assq :mode it)) 'active)) active-items)))
            (nstab        (length (seq-filter (lambda (it) (eq (cdr (assq :mode it)) 'stabilizing)) active-items)))
            (nmaint       (length (seq-filter (lambda (it) (eq (cdr (assq :mode it)) 'maintenance)) active-items)))
            (nret         (- n (length active-items))))
       (list bid
             (vector bname
                     (number-to-string n)
                     (if (> due 0) (propertize (number-to-string due) 'face 'warning) "0")
                     (number-to-string nactive)
                     (number-to-string nstab)
                     (number-to-string nmaint)
                     (if (> nret 0) (propertize (number-to-string nret) 'face 'success) "0")))))
   (resurface--drill-block-names)))

;;;###autoload
(defun resurface-drill ()
  "Open the drill block dashboard."
  (interactive)
  (resurface--ensure-data)
  (let ((buf (get-buffer-create "*Resurface: Drill*")))
    (with-current-buffer buf
      (resurface-drill-menu-mode)
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(defun resurface-drill-menu-view-block ()
  "Open the block detail view for the block on the current dashboard line."
  (interactive)
  (let ((bid (tabulated-list-get-id)))
    (when bid (resurface-drill-view-block (cdr (assq :name (resurface--drill-get-block bid)))))))

(defun resurface-drill-menu-start-session ()
  "Start a drill session for the block on the current dashboard line."
  (interactive)
  (let ((bid (tabulated-list-get-id)))
    (when bid (resurface-drill-start-session (cdr (assq :name (resurface--drill-get-block bid)))))))

(defun resurface-drill-menu-delete-block ()
  "Delete the drill block on the current line, with confirmation."
  (interactive)
  (let ((bid (tabulated-list-get-id)))
    (when (and bid
               (yes-or-no-p (format "Delete drill block '%s' and all its entries? "
                                    (cdr (assq :name (resurface--drill-get-block bid))))))
      (remhash bid (resurface--drill-blocks-ht))
      (resurface--persist)
      (tabulated-list-print t))))

(defun resurface-drill-menu-help ()
  "Echo drill dashboard keybindings to minibuffer."
  (interactive)
  (message
   "Drill: RET view  r review  s review-all  G retired  a add-sentence  A new-block  d delete  S save  g refresh"))

(defun resurface--maybe-refresh-drill-dashboard ()
  "Silently refresh the drill dashboard buffer if it is alive."
  (let ((buf (get-buffer "*Resurface: Drill*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (tabulated-list-print t)))))

;; ---------------------------------------------------------------------------
;;  Drill Block Detail View  (sentence list + status for one block)

(defvar resurface-drill-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "e") #'resurface-drill-view-edit-item)
    (define-key map (kbd "x") #'resurface-drill-view-retire-item)
    (define-key map (kbd "a") #'resurface-drill-view-reactivate-item)
    (define-key map (kbd "d") #'resurface-drill-view-delete-item)
    (define-key map (kbd "R") #'resurface-drill-view-retire-block)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "?") #'resurface-drill-view-help)
    map)
  "Keymap for the drill block detail view.")

(defvar-local resurface--drill-view-block nil "Block id this detail view is showing.")

(define-derived-mode resurface-drill-view-mode tabulated-list-mode "Resurface-Drill-Block"
  "Detail view for one drill block: every sentence and its status."
  (setq tabulated-list-format (resurface--drill-view-column-format))
  (tabulated-list-init-header))

(defun resurface--drill-view-column-format ()
  "Return the column format vector for the drill block detail view."
  [("Sentence"     40 t)
   ("Mode"         12 t)
   ("Exposures"     9 t)
   ("Last drilled" 14 t)
   ("Due in"       10 nil)
   ("Status"        8 nil)])

(defun resurface--drill-view-build-header (block-id)
  "Build the header-line string for the BLOCK-ID detail view."
  (let* ((items   (resurface--drill-block-items block-id))
         (n       (length items))
         (retired (length (seq-filter #'resurface--drill-item-retired-p items)))
         (active  (- n retired))
         (due     (length (seq-filter #'resurface--drill-item-due-p items))))
    (propertize
     (format "  %s   |   %d active  %d retired   |   %d due   |   e edit  x retire  a reactivate  d delete  R retire-block  q close"
             (cdr (assq :name (resurface--drill-get-block block-id))) active retired due)
     'face 'mode-line)))

(defun resurface--drill-view-refresh (block-id)
  "Refresh this block-view buffer's header and table for BLOCK-ID."
  (setq header-line-format (resurface--drill-view-build-header block-id))
  (tabulated-list-print t))

(defun resurface--drill-view-entries (block-id)
  "Build tabulated-list entries for BLOCK-ID in block view."
  (mapcar
   (lambda (item)
     (let* ((id      (cdr (assq :id item)))
            (mode    (cdr (assq :mode item)))
            (retired (resurface--drill-item-retired-p item))
            (exp     (cdr (assq :exposures item)))
            (minsess (cdr (assq :min-sessions item)))
            (lr      (cdr (assq :last-drilled item)))
            (due-p   (resurface--drill-item-due-p item))
            (days    (resurface--drill-item-days-until-due item))
            (due-str (cond (retired  (propertize "-"   'face 'shadow))
                            ((= lr 0) (propertize "new" 'face 'warning))
                            (due-p    (propertize "due" 'face 'warning))
                            (t        (format "%.0fd" days))))
            (status  (cond (retired (propertize "Retired" 'face 'success))
                            (due-p   (propertize "Due" 'face 'warning))
                            (t       ""))))
       (list id
             (vector (resurface--drill-truncate (cdr (assq :text item)) 40)
                     (if retired (propertize (resurface--drill-mode-label mode) 'face 'shadow)
                       (resurface--drill-mode-label mode))
                     (format "%d/%d" exp minsess)
                     (resurface--format-ts lr)
                     due-str
                     status))))
   (resurface--drill-block-items block-id)))

;;;###autoload
(defun resurface-drill-view-block (block-name)
  "Open the detail view listing every sentence in drill block BLOCK-NAME."
  (interactive (list (completing-read "View drill block: " (resurface--drill-block-names) nil t)))
  (resurface--ensure-data)
  (let* ((b   (resurface--drill-get-block-by-name block-name))
         (bid (cdr (assq :id b)))
         (buf (get-buffer-create (format "*Resurface: Drill: %s*" block-name))))
    (with-current-buffer buf
      (resurface-drill-view-mode)
      (setq resurface--drill-view-block bid
            header-line-format         (resurface--drill-view-build-header bid)
            tabulated-list-entries     (lambda () (resurface--drill-view-entries bid)))
      (setq-local revert-buffer-function
                  (lambda (_a _n) (resurface--drill-view-refresh bid)))
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(defun resurface-drill-view-edit-item ()
  "Edit the text of the sentence on the current line."
  (interactive)
  (let* ((id   (tabulated-list-get-id))
         (bid  resurface--drill-view-block)
         (item (and id (resurface--drill-find-item bid id))))
    (when item
      (let ((new-text (read-string "Sentence: " (cdr (assq :text item)))))
        (unless (string-empty-p (string-trim new-text))
          (resurface--drill-replace-item
           bid id
           (mapcar (lambda (kv) (if (eq (car kv) :text) (cons :text (string-trim new-text)) kv)) item))
          (resurface--persist)
          (resurface--drill-view-refresh bid))))))

(defun resurface-drill-view-retire-item ()
  "Force-retire the sentence on the current line."
  (interactive)
  (let* ((id   (tabulated-list-get-id))
         (bid  resurface--drill-view-block)
         (item (and id (resurface--drill-find-item bid id))))
    (when (and item (yes-or-no-p "Retire this sentence? "))
      (resurface--drill-replace-item bid id (resurface--drill-item-rate item 'retire))
      (resurface--persist)
      (resurface--drill-view-refresh bid)
      (resurface--maybe-refresh-drill-dashboard))))

(defun resurface-drill-view-reactivate-item ()
  "Bring a retired sentence on the current line back to `active' mode."
  (interactive)
  (let* ((id   (tabulated-list-get-id))
         (bid  resurface--drill-view-block)
         (item (and id (resurface--drill-find-item bid id))))
    (when (and item (resurface--drill-item-retired-p item)
               (yes-or-no-p "Reactivate this sentence at Active? "))
      (resurface--drill-replace-item bid id (resurface--drill-item-reactivate item))
      (resurface--persist)
      (resurface--drill-view-refresh bid)
      (resurface--maybe-refresh-drill-dashboard))))

(defun resurface-drill-view-delete-item ()
  "Permanently delete the sentence on the current line from the index."
  (interactive)
  (let* ((id  (tabulated-list-get-id))
         (bid resurface--drill-view-block))
    (when (and id (yes-or-no-p "Delete this sentence from the drill block? "))
      (resurface--drill-set-block-items
       bid (seq-remove (lambda (it) (equal (cdr (assq :id it)) id))
                        (resurface--drill-block-items bid)))
      (resurface--persist)
      (resurface--drill-view-refresh bid)
      (resurface--maybe-refresh-drill-dashboard))))

(defun resurface-drill-view-retire-block ()
  "Batch-retire every sentence in this block at once.
Matches the \"batch retirement, not item-level graduation\" design: once
a whole lesson's pattern is absorbed, retire the block together instead
of picking items off one at a time."
  (interactive)
  (let ((bid resurface--drill-view-block))
    (when (yes-or-no-p "Retire EVERY sentence in this block? ")
      (resurface--drill-set-block-items
       bid
       (mapcar (lambda (it) (if (resurface--drill-item-retired-p it) it
                              (resurface--drill-item-rate it 'retire)))
               (resurface--drill-block-items bid)))
      (resurface--persist)
      (resurface--drill-view-refresh bid)
      (resurface--maybe-refresh-drill-dashboard)
      (message "Drill: block retired."))))

(defun resurface-drill-view-help ()
  "Echo drill block detail keybindings."
  (interactive)
  (message "Drill block: e edit  x retire  a reactivate  d delete  R retire-block  q close"))

;; ---------------------------------------------------------------------------
;;  Drill Retired Browser
;;
;; Mirrors `resurface-leitner-review-graduated': lists every retired sentence
;; (across all blocks, or just one) so you can bring anything back that
;; started feeling rusty.  Reuses `resurface--age-str' and
;; `resurface--format-ts', which are generic timestamp helpers.

(defun resurface--drill-retired-column-format ()
  "Column format for the drill retired browser."
  [("Block"     16 t)
   ("Sentence"  36 t)
   ("Retired"   12 t)
   ("Age"        6 t)])

(defun resurface--drill-retired-entries (&optional block-name)
  "Build tabulated-list entries for the drill retired browser."
  (mapcar
   (lambda (pair)
     (let* ((bid   (car pair))
            (item  (cdr pair))
            (bname (cdr (assq :name (resurface--drill-get-block bid))))
            (ret   (cdr (assq :retired item))))
       (list (cons bid (cdr (assq :id item)))
             (vector bname (resurface--drill-truncate (cdr (assq :text item)) 36)
                     (resurface--format-ts ret) (resurface--age-str ret)))))
   (resurface--drill-retired-pairs block-name)))

(defun resurface--drill-retired-build-header (block-name)
  "Build the header-line string for the drill retired browser."
  (let ((n (length (resurface--drill-retired-pairs block-name))))
    (propertize
     (format "  %d retired sentence%s%s   |   r bring back   g refresh   q close"
             n (if (= n 1) "" "s")
             (if block-name (format "  (block: %s)" block-name) ""))
     'face '(:inherit shadow :slant italic))))

(defvar-local resurface--drill-retired-block nil
  "The block this retired-browser buffer is filtered to, or nil for all.")

(defvar resurface-drill-retired-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "r") #'resurface-drill-retired-bring-back)
    (define-key map (kbd "g") #'revert-buffer)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "?") #'resurface-drill-retired-help)
    map)
  "Keymap for the drill retired-sentence browser.")

(define-derived-mode resurface-drill-retired-mode tabulated-list-mode "Resurface-Drill-Retired"
  "Browse every retired drill sentence and decide whether it stays retired."
  (setq tabulated-list-format (resurface--drill-retired-column-format))
  (tabulated-list-init-header))

;;;###autoload
(defun resurface-drill-review-retired (&optional block-name)
  "Browse every retired drill sentence and decide whether it stays retired.
With a prefix argument, limit the browser to one BLOCK-NAME instead of
every block.  Press r on a sentence to reactivate it at `active'."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Limit to block: " (resurface--drill-block-names) nil t))))
  (resurface--ensure-data)
  (let ((buf (get-buffer-create
              (if block-name
                  (format "*Resurface: Drill Retired (%s)*" block-name)
                "*Resurface: Drill Retired*"))))
    (with-current-buffer buf
      (resurface-drill-retired-mode)
      (setq resurface--drill-retired-block block-name
            tabulated-list-entries       (lambda () (resurface--drill-retired-entries block-name))
            header-line-format           (resurface--drill-retired-build-header block-name))
      (setq-local revert-buffer-function
                  (lambda (_auto _noconfirm)
                    (setq header-line-format (resurface--drill-retired-build-header block-name))
                    (tabulated-list-print t)))
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(defun resurface-drill-retired-bring-back ()
  "Reactivate the retired sentence under point back to `active' mode."
  (interactive)
  (let* ((id   (tabulated-list-get-id))
         (bid  (car id))
         (iid  (cdr id))
         (item (resurface--drill-find-item bid iid)))
    (unless item (user-error "Drill: no sentence on current line"))
    (resurface--drill-replace-item bid iid (resurface--drill-item-reactivate item))
    (resurface--persist)
    (setq header-line-format (resurface--drill-retired-build-header resurface--drill-retired-block))
    (tabulated-list-print t)
    (resurface--maybe-refresh-drill-dashboard)
    (message "Drill: sentence back in active rotation.")))

(defun resurface-drill-retired-help ()
  "Echo drill retired-browser keybindings."
  (interactive)
  (message "Drill retired: r bring back   g refresh   q close"))

;; ===========================================================================
;;  Evil-mode Compatibility
;;
;; Keys `g', `?', `s', `a', `d', `A', `S' are intercepted by evil by default;
;; `evil-set-initial-state' MODE 'emacs tells evil to leave each buffer alone
;; so our own keymaps are consulted.  Tabulated-list navigation (TAB, arrows,
;; n/p) still works since those live in `tabulated-list-mode-map'.  We then
;; add back `j'/`k' as plain evil motion on top.
(with-eval-after-load 'evil
  (evil-set-initial-state 'resurface-leitner-menu-mode 'emacs)
  (evil-set-initial-state 'resurface-leitner-group-view-mode 'emacs)
  (evil-set-initial-state 'resurface-leitner-graduated-mode 'emacs)
  (evil-set-initial-state 'resurface-leitner-front-mode 'emacs)  ; SPC/s also live in evil-motion-state-map
  (evil-set-initial-state 'resurface-drill-menu-mode 'emacs)
  (evil-set-initial-state 'resurface-drill-view-mode 'emacs)
  (evil-set-initial-state 'resurface-drill-retired-mode 'emacs)
  (evil-set-initial-state 'resurface-drill-session-mode 'emacs) ; c/o/s/R keys, same idea as front-mode
  (dolist (map (list resurface-leitner-menu-mode-map
                     resurface-leitner-group-view-mode-map
                     resurface-leitner-graduated-mode-map
                     resurface-drill-menu-mode-map
                     resurface-drill-view-mode-map
                     resurface-drill-retired-mode-map))
    (define-key map (kbd "j") #'evil-next-line)
    (define-key map (kbd "k") #'evil-previous-line))
  ;; Review minor mode uses C-c l <key>, which evil doesn't shadow in normal
  ;; state, but evil-make-overriding-map + normalize is a safety net so the
  ;; minor-mode map is always consulted first regardless of buffer state.
  (evil-make-overriding-map resurface-leitner-review-minor-mode-map 'normal)
  (add-hook 'resurface-leitner-review-minor-mode-hook #'evil-normalize-keymaps))


(provide 'resurface)
;;; resurface.el ends here
