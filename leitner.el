;;; leitner.el --- Leitner spaced repetition for note files  -*- lexical-binding: t; -*-
;; Author: vmargb
;; Version: 0.3.4
;; Package-Requires: ((emacs "27.1"))
;; Keywords: notes, spaced-repetition, org, feynman
;; URL: https://github.com/vmargb/leitner.el

;;; Commentary:
;;
;; leitner.el runs the Leitner box system over WHOLE NOTE FILES instead of
;; individual flashcards: write notes as usual, and this package surfaces
;; them for review on a Leitner schedule.  FILES ARE NEVER MODIFIED -- all
;; scheduling metadata lives in an external JSON index file.
;;
;; Quick start:
;;   M-x leitner                  Open the group dashboard
;;   M-x leitner-add-group        Add a new group
;;   M-x leitner-add-file         Add current buffer's file to a group
;;   M-x leitner-start-session    Review all due files (C-u: one group)
;;   M-x leitner-review-graduated Browse graduated files (C-u: one group)
;;
;; Every mode has a `?' binding for its keymap.  See README.md for the full
;; review workflow and customisation reference.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'json)
(require 'tabulated-list)


;; ===========================================================================
;;  Customisation

(defgroup leitner nil
  "Leitner spaced repetition for note files."
  :group 'applications
  :prefix "leitner-")

(defcustom leitner-index-file
  (expand-file-name "var/leitner.json" user-emacs-directory)
  "Path to the JSON file storing all review metadata.
Your note files are never touched, so all state lives here."
  :type 'file
  :group 'leitner)

(defcustom leitner-box-intervals [1 3 7 14 30 60 90]
  "Review intervals (days) for each Leitner box.
Index 0 = Box 1 (reviewed most frequently)."
  :type '(vector integer)
  :group 'leitner)

(defcustom leitner-default-group "General"
  "Default group name when none is specified."
  :type 'string
  :group 'leitner)

(defcustom leitner-session-max-items nil
  "Maximum number of files to queue in a single review session.
Nil disables the cap every due file is queued"
  :type '(choice (const :tag "No limit" nil)
                  (integer :tag "Max items per session"))
  :group 'leitner)

(defcustom leitner-before-review-hook nil
  "Hook run right after a note file is revealed in a review session.")

(defcustom leitner-after-session-hook nil
  "Hook run immediately after a review session is fully completed.")


;; ===========================================================================
;;  Internal State
;;
;; leitner--data  =  ((:groups . HASH-TABLE)  (:dirty . BOOL))
;;   HASH-TABLE  : group-name (string) -> group-alist
;;   group-alist : ((:name . STRING) (:items . LIST-OF-ITEM-ALISTS))
;;   item-alist  : ((:path . STRING) (:box . INT)
;;                  (:last-reviewed . INT) (:added . INT)
;;                  (:graduated . INT-OR-NIL)   ; Unix ts when graduated
;;                  (:paused . BOOL))           ; t after a Partial rating
;;
(defvar leitner--data nil
  "In-memory Leitner index.  Nil until first initialisation.")

;; :queue (group-name . item-alist) pairs left to review, :reviewed count,
;; :total count, :group-filter string-or-nil.
(defvar leitner--session nil
  "Active review session state, or nil when idle.")


;; ===========================================================================
;;  Small Utilities

(defun leitner--now ()
  "Return the current time as a Unix timestamp (integer)."
  (floor (float-time)))

(defun leitner--num-boxes ()
  "Return the number of Leitner boxes."
  (length leitner-box-intervals))

(defun leitner--box-days (box)
  "Review interval in days for BOX (1-indexed)."
  (aref leitner-box-intervals (1- box)))

(defun leitner--box-secs (box)
  "Review interval in seconds for BOX (1-indexed)."
  (* (leitner--box-days box) 86400))

(defun leitner--item-interval-secs (item)
  "Effective review interval in seconds for ITEM."
  (leitner--box-secs (cdr (assq :box item))))

(defun leitner--item-due-p (item)
  "Return non-nil when ITEM is due for review.
Graduated items are never due."
  (and (not (cdr (assq :graduated item)))
       (let ((lr (cdr (assq :last-reviewed item))))
         (or (= lr 0)
             (>= (- (leitner--now) lr) (leitner--item-interval-secs item))))))

(defun leitner--item-days-until-due (item)
  "Days until ITEM is next due.  Negative = overdue, 0 = never reviewed."
  (let ((lr (cdr (assq :last-reviewed item))))
    (if (= lr 0) 0
      (/ (- (leitner--item-interval-secs item)
            (- (leitner--now) lr))
         86400.0))))

(defun leitner--make-item (path)
  "Return a fresh item-alist for PATH placed in Box 1."
  (list (cons :path          (expand-file-name path))
        (cons :box           1)
        (cons :last-reviewed 0)
        (cons :added         (leitner--now))
        (cons :graduated     nil)
        (cons :paused        nil)))

(defun leitner--item-graduated-p (item)
  "Return non-nil when ITEM has been graduated (fully mastered)."
  (cdr (assq :graduated item)))

(defun leitner--item-paused-p (item)
  "Return non-nil when ITEM was last rated Partial.
Cleared the next time the item is rated with any other outcome."
  (cdr (assq :paused item)))

(defun leitner--item-rate (item outcome)
  "Return a NEW item-alist for ITEM rated with OUTCOME.
OUTCOME is one of `good', `reset' (Bad), `skip', `partial', `revised'.
`reset' always sends the item back to Box 1."
  (let* ((old-box (cdr (assq :box item)))
         (last-box (leitner--num-boxes))
         (graduating (and (eq outcome 'good) (= old-box last-box)))
         (new-box (pcase outcome
                    ('good (if graduating last-box (1+ old-box)))
                    ('reset 1)
                    ((or 'skip 'partial 'revised) old-box)))
         ;; partial backdates :last-reviewed so the effective interval is
         ;; satisfied exactly one day from now
         (interval (leitner--item-interval-secs item)))
    (list (cons :path          (cdr (assq :path item)))
          (cons :box           new-box)
          (cons :last-reviewed (cond
                                 ((eq outcome 'skip)
                                  (cdr (assq :last-reviewed item)))
                                 ((eq outcome 'partial)
                                  (max 0 (- (leitner--now)
                                            (max 0 (- interval 86400)))))
                                 (t (leitner--now))))
          (cons :added         (cdr (assq :added item)))
          (cons :graduated     (if graduating (leitner--now) nil))
          (cons :paused        (eq outcome 'partial)))))

(defun leitner--format-ts (ts)
  "Format Unix timestamp TS as YYYY-MM-DD, or \"Never\" for 0."
  (if (= ts 0) "Never"
    (format-time-string "%Y-%m-%d" (seconds-to-time ts))))

(defun leitner--shuffle (seq)
  "Return a shuffled copy of SEQ using fisher-yates."
  (let ((v (vconcat seq)))
    (dotimes (i (length v))
      (let ((j (+ i (random (- (length v) i)))))
        (cl-rotatef (aref v i) (aref v j))))
    (append v nil)))

(defun leitner--extract-prompt (path)
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

(defun leitner--insert-prompt-keyword (path prompt)
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

(defun leitner--ensure-data ()
  "Initialise `leitner--data', loading from disk when available."
  (unless leitner--data
    (if (file-exists-p (expand-file-name leitner-index-file))
        (leitner-load)
      (setq leitner--data
            (list (cons :groups (make-hash-table :test #'equal))
                  (cons :dirty  nil))))))

(defun leitner--groups-ht ()
  "Return the groups hash-table."
  (cdr (assq :groups leitner--data)))

(defun leitner--group-names ()
  "Return a sorted list of all group names."
  (sort (hash-table-keys (leitner--groups-ht)) #'string<))

(defun leitner--get-group (name)
  "Return the group alist for NAME, or nil."
  (gethash name (leitner--groups-ht)))

(defun leitner--get-or-create-group (name)
  "Return the group alist for NAME, creating it if it does not exist."
  (or (leitner--get-group name)
      (let ((g (list (cons :name name) (cons :items nil))))
        (puthash name g (leitner--groups-ht))
        g)))

(defun leitner--group-items (name)
  "Return the items list for group NAME (may be nil)."
  (cdr (assq :items (leitner--get-group name))))

(defun leitner--find-item (group-name path)
  "Return the item in GROUP-NAME with :path PATH, or nil."
  (seq-find (lambda (it) (equal (cdr (assq :path it)) path))
            (leitner--group-items group-name)))

;; setcdr+assq mutates in-place: gethash returns the actual cons-cell list
;; stored in the hash table, so setcdr on one of its cells updates it too

(defun leitner--set-group-items (group-name items)
  "Replace the items list of GROUP-NAME with ITEMS (mutates in-place)."
  (let ((g (gethash group-name (leitner--groups-ht))))
    (when g
      (setcdr (assq :items g) items))))

(defun leitner--prepend-item (group-name item)
  "Add ITEM to the front of GROUP-NAME's items list."
  (leitner--get-or-create-group group-name)
  (leitner--set-group-items
   group-name
   (cons item (leitner--group-items group-name))))

(defun leitner--replace-item (group-name path new-item)
  "Replace the item with :path = PATH in GROUP-NAME with NEW-ITEM."
  (leitner--set-group-items
   group-name
   (mapcar (lambda (it)
             (if (equal (cdr (assq :path it)) path) new-item it))
           (leitner--group-items group-name))))

(defun leitner--mark-dirty ()
  "Mark the index as having an unsaved change."
  (leitner--ensure-data)
  (setcdr (assq :dirty leitner--data) t))

(defun leitner--persist ()
  "Mark the index dirty and write it to disk."
  (leitner--mark-dirty)
  (leitner-save))

(defun leitner--all-pairs ()
  "All (group-name . item-alist) pairs across every group."
  (let (result)
    (maphash (lambda (gname g)
               (dolist (item (cdr (assq :items g)))
                 (push (cons gname item) result)))
             (leitner--groups-ht))
    result))

(defun leitner--due-pairs (&optional group-name)
  "Due (group-name . item-alist) pairs, optionally filtered to GROUP-NAME."
  (seq-filter
   (lambda (pair)
     (and (or (null group-name) (equal (car pair) group-name))
          (leitner--item-due-p (cdr pair))))
   (leitner--all-pairs)))

;; Ranks items only for `leitner-session-max-items' trimming, it has no
;; effect on whether something counts as due (`leitner--item-due-p' alone)
(defun leitner--item-overdue-amount (group-name item)
  "Return how overdue ITEM (in GROUP-NAME) is, in days.
Never-reviewed items rank lowest (0.0) here, behind files that sat
past their interval, they're due, but not yet neglected backlog."
  (max 0.0 (- (leitner--item-days-until-due item))))

(defun leitner--top-overdue-pairs (pairs n)
  "Return the N most overdue of PAIRS (as from `leitner--due-pairs')."
  (seq-take
   (sort (copy-sequence pairs)
         (lambda (a b)
           (> (leitner--item-overdue-amount (car a) (cdr a))
              (leitner--item-overdue-amount (car b) (cdr b)))))
   n))

(defun leitner--graduated-pairs (&optional group-name)
  "Graduated (group-name . item-alist) pairs, optionally filtered to GROUP-NAME."
  (seq-filter
   (lambda (pair)
     (and (or (null group-name) (equal (car pair) group-name))
          (leitner--item-graduated-p (cdr pair))))
   (leitner--all-pairs)))

(defun leitner--group-next-due-str (active)
  "Return a string for when the next item in ACTIVE becomes due.
ACTIVE is any non-graduated items for a group."
  (let ((min-days nil))
    (dolist (item active)
      (unless (leitner--item-due-p item)
        (let ((d (leitner--item-days-until-due item)))
          (when (> d 0)
            (setq min-days (if min-days (min min-days d) d))))))
    (cond
     (min-days (propertize (if (< min-days 1) "<1d" (format "%dd" (floor min-days)))
                            'face 'font-lock-keyword-face))
     (active   (propertize "now" 'face 'warning))
     (t        "—"))))

(defun leitner--path-registered-p (path &optional group-name)
  "Return non-nil if PATH is already registered (optionally in GROUP-NAME)."
  (let ((abs (expand-file-name path)))
    (seq-find
     (lambda (pair)
       (and (or (null group-name) (equal (car pair) group-name))
            (equal (cdr (assq :path (cdr pair))) abs)))
     (leitner--all-pairs))))

;; ===========================================================================
;;  Persistence
;;
;; read with json-key-type 'string so object keys come back as plain
;; strings.  On write, json-encode calls symbol-name on symbol keys, so a
;; symbol-keyed alist serialises fine as-is

(defun leitner--empty-data ()
  "Return a fresh, empty index data structure."
  (list (cons :groups (make-hash-table :test #'equal)) (cons :dirty nil)))

(defun leitner--data->json-sexp ()
  "Convert `leitner--data' to a JSON-encodable sexp."
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
     (leitner--groups-ht))
    (list (cons 'version       1)
          (cons 'box_intervals leitner-box-intervals)
          (cons 'groups        groups-list))))

(defun leitner--json-sexp->data (sexp)
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
                   ;; is intentionally ignored, prompts are read live via `leitner--extract-prompt'
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
    (list (cons :groups ht) (cons :dirty nil))))

;;;###autoload
(defun leitner-save ()
  "Save the Leitner index to `leitner-index-file'."
  (interactive)
  (leitner--ensure-data)
  (let* ((full (expand-file-name leitner-index-file))
         (dir  (file-name-directory full)))
    (when dir (make-directory dir t))
    (let ((json-encoding-pretty-print t))
      (with-temp-file full
        (insert (json-encode (leitner--data->json-sexp)))))
    (setcdr (assq :dirty leitner--data) nil)
    (message "Leitner: saved to %s" (abbreviate-file-name full))))

;;;###autoload
(defun leitner-load ()
  "Load the Leitner index from `leitner-index-file'."
  (interactive)
  (let ((full (expand-file-name leitner-index-file)))
    (if (not (file-exists-p full))
        (progn
          (setq leitner--data (leitner--empty-data))
          (message "Leitner: no index found, starting fresh."))
      (condition-case err
          (let ((json-object-type 'alist)
                (json-array-type  'vector)
                (json-key-type    'string))
            (setq leitner--data
                  (leitner--json-sexp->data (json-read-file full)))
            (message "Leitner: loaded %d group(s)."
                     (hash-table-count (leitner--groups-ht))))
        (error
         (message "Leitner: failed to load -- %s" (error-message-string err))
         (setq leitner--data (leitner--empty-data)))))))

(add-hook 'kill-emacs-hook
          (lambda ()
            (when (and leitner--data (cdr (assq :dirty leitner--data)))
              (leitner-save))))

(defun leitner--all-tracked-paths ()
  "Return a list of all absolute paths currently in the index."
  (let (paths)
    (maphash (lambda (gname _g)
               (dolist (item (leitner--group-items gname))
                 (push (cdr (assq :path item)) paths)))
             (leitner--groups-ht))
    paths))

;; for each missing file: [r]emap prompts for its new location (keeps all
;; SR history), [p]rune removes the entry, [s]kip leaves it stale for now
;; nothing is written until the interactive pass is complete
;;;###autoload
(defun leitner-healthcheck ()
  "Check all files, interactively remap moved/renamed ones, prune gone ones."
  (interactive)
  (leitner--ensure-data)
  (let (missing)
    (maphash (lambda (gname _g)
               (dolist (item (leitner--group-items gname))
                 (unless (file-exists-p (cdr (assq :path item)))
                   (push (cons gname item) missing))))
             (leitner--groups-ht))
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
              (?p (leitner--set-group-items
                   gname (seq-remove (lambda (it) (equal it item))
                                      (leitner--group-items gname)))
                  (cl-incf pruned))
              (?s (cl-incf skipped)))))
        (when (> (+ remapped pruned) 0)
          (leitner--persist))
        (message "Leitner healthcheck: %d remapped, %d pruned, %d skipped."
                 remapped pruned skipped)))))

;; ===========================================================================
;;  Adding / Removing / Resetting Files

(defun leitner--read-group-name (&optional prompt)
  "PROMPT for a group name with completion."
  (let* ((names   (leitner--group-names))
         (default leitner-default-group)
         (pr      (or prompt (format "Group (default %s): " default))))
    (completing-read pr names nil nil nil nil default)))

;; #+LEITNER_PROMPT isn't asked for in batch mode (no sane way to prompt
;; per file across a whole Dired selection), run `leitner-add-file' again
;; from the file's own buffer afterwards, or add the keyword by hand.
;;;###autoload
(defun leitner-add-file (&optional file group)
  "Add FILE to GROUP for spaced repetition.
Called interactively from a `dired' buffer, bulk-adds all marked files to
a single group; folders and already-registered files are skipped."
  (interactive)
  (leitner--ensure-data)
  (if (and (called-interactively-p 'any) (derived-mode-p 'dired-mode))
      (let* ((files (seq-filter (lambda (f) (not (file-directory-p f)))
                                (dired-get-marked-files)))
             (_ (unless files (user-error "Leitner: no files marked in Dired")))
             (grp     (leitner--read-group-name))
             (added   0)
             (skipped 0))
        (dolist (f files)
          (let ((abs (expand-file-name f)))
            (if (leitner--path-registered-p abs grp)
                (cl-incf skipped)
              (leitner--prepend-item grp (leitner--make-item abs))
              (cl-incf added))))
        (when (> added 0)
          (leitner--persist)
          (leitner--maybe-refresh-dashboard))
        (message "Leitner: added %d file(s) to group '%s'%s."
                 added grp
                 (if (> skipped 0)
                     (format ", skipped %d already registered" skipped)
                   "")))
    (let* ((target (expand-file-name
                    (or file
                        (buffer-file-name)
                        (read-file-name "File to add: " nil nil t))))
           (grp (or group (leitner--read-group-name)))
           (prompt
            (when (called-interactively-p 'any)
              (let ((s (read-string
                        (format "#+LEITNER_PROMPT for '%s' (RET to skip): "
                                (file-name-nondirectory target)))))
                (unless (string-empty-p s) s)))))
      (if (leitner--path-registered-p target grp)
          (message "Leitner: '%s' is already in group '%s'."
                   (file-name-nondirectory target) grp)
        (when prompt
          (leitner--insert-prompt-keyword target prompt))
        (leitner--prepend-item grp (leitner--make-item target))
        (leitner--persist)
        (message "Leitner: added '%s' to group '%s' (Box 1)%s."
                 (file-name-nondirectory target) grp
                 (if prompt " with #+LEITNER_PROMPT" ""))
        (leitner--maybe-refresh-dashboard)))))

;;;###autoload
(defun leitner-remove-file (&optional file)
  "Remove FILE from the Leitner index; defaults to the current buffer's file."
  (interactive)
  (leitner--ensure-data)
  (let ((abs (expand-file-name
              (or file (buffer-file-name) (read-file-name "File to remove: ")))))
    (maphash (lambda (gname _g)
               (leitner--set-group-items
                gname
                (seq-remove (lambda (it) (equal (cdr (assq :path it)) abs))
                            (leitner--group-items gname))))
             (leitner--groups-ht))
    (leitner--persist)
    (message "Leitner: removed '%s'." (file-name-nondirectory abs))
    (leitner--maybe-refresh-dashboard)))

;;;###autoload
(defun leitner-add-group (name)
  "Create a new empty group called NAME."
  (interactive "sNew group name: ")
  (leitner--ensure-data)
  (if (leitner--get-group name)
      (message "Leitner: group '%s' already exists." name)
    (leitner--get-or-create-group name)
    (leitner--persist)
    (message "Leitner: group '%s' created." name)
    (leitner--maybe-refresh-dashboard)))

;; ===========================================================================
;;  Review Session
;;
;; review order: the due list is shuffled by `leitner--shuffle', then stable
;; sorted by box number, groups items by box while randomising within it
;;;###autoload
(defun leitner-start-session (&optional group-name)
  "Start a Leitner review session for all currently due files.
With an optional prefix argument, prompt to limit review to one GROUP-NAME."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Limit session to group: "
                            (leitner--group-names) nil t))))
  (leitner--ensure-data)
  (when (and leitner--session
             (not (yes-or-no-p "A session is already running.  Start a new one? ")))
    (user-error "Session aborted"))
  (let* ((due       (leitner--due-pairs group-name))
         (due-count (length due))
         (gsuffix   (if group-name (format " in '%s'" group-name) ""))
         (capped    (and (integerp leitner-session-max-items)
                          (> leitner-session-max-items 0)
                          (> due-count leitner-session-max-items)))
         (due       (if capped
                        (leitner--top-overdue-pairs due leitner-session-max-items)
                      due)))
    (if (null due)
        (message "Leitner: nothing due%s, great work!" gsuffix)
      (let ((queue (sort (leitner--shuffle due)
                          (lambda (a b)
                            (< (cdr (assq :box (cdr a))) (cdr (assq :box (cdr b))))))))
        (setq leitner--session
              (list (cons :queue        queue)
                    (cons :reviewed     0)
                    (cons :total        (length queue))
                    (cons :group-filter group-name)
                    (cons :capped       capped)))
        (if capped
            (message "Leitner: %d due%s -- queuing today's top %d most overdue.  Session starting..."
                     due-count gsuffix (length queue))
          (message "Leitner: %d file%s due%s.  Session starting..."
                   (length queue) (if (= (length queue) 1) "" "s") gsuffix))
        (leitner--session-advance)))))

(defun leitner--session-advance ()
  "Show the front card for the next queue item, or finish the session."
  (let ((queue (cdr (assq :queue leitner--session))))
    (if (null queue)
        (leitner--session-finish)
      (let* ((pair (car queue))
             (path (cdr (assq :path (cdr pair)))))
        (if (not (file-exists-p path))
            (progn
              (message "Leitner: file missing, skipping -- %s"
                       (file-name-nondirectory path))
              (setcdr (assq :queue leitner--session) (cdr queue))
              (leitner--session-advance))
          (leitner--show-front-card pair))))))

(defun leitner--session-record (outcome)
  "Record OUTCOME (good/reset/partial/skip/revised) for the current item and advance."
  (unless leitner--session
    (user-error "Leitner: no active session"))
  (let* ((queue    (cdr (assq :queue leitner--session)))
         (pair     (car queue))
         (gname    (car pair))
         (item     (cdr pair))
         (path     (cdr (assq :path item)))
         (new-item (leitner--item-rate item outcome)))
    (leitner--replace-item gname path new-item)
    (cl-incf (cdr (assq :reviewed leitner--session)))
    (setcdr (assq :queue leitner--session) (cdr queue))
    (when leitner-review-minor-mode
      (leitner-review-minor-mode -1))
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
               (cdr (assq :reviewed leitner--session))
               (cdr (assq :total    leitner--session))))
    (leitner--session-advance)))

(defun leitner--session-finish ()
  "Clean up and save after all items have been reviewed.
When the session was trimmed by `leitner-session-max-items', also reports
how many files are still due so the remaining backlog stays visible."
  (let* ((n            (cdr (assq :reviewed leitner--session)))
         (capped       (cdr (assq :capped       leitner--session)))
         (group-filter (cdr (assq :group-filter leitner--session)))
         (remaining    (and capped (length (leitner--due-pairs group-filter)))))
    (setq leitner--session nil)
    (leitner-save)
    (leitner--maybe-refresh-dashboard)
    (if (and capped (> remaining 0))
        (message "Leitner: session complete, %d file%s reviewed.  %d more due file%s waiting, run `leitner-start-session' again when you're ready. Index saved."
                 n (if (= n 1) "" "s")
                 remaining (if (= remaining 1) "" "s"))
      (message "Leitner: session complete, %d file%s reviewed. Index saved."
                n (if (= n 1) "" "s")))))

;; ===========================================================================
;;  Front Card: recall before reveal

(defconst leitner--front-buf "*Leitner: Review*"
  "Name of the front-card buffer shown before revealing the file.")

(defvar-local leitner--front-item  nil "Item being previewed.")
(defvar-local leitner--front-group nil "Group name of the item being previewed.")

(defvar leitner-front-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'leitner-front-reveal)
    (define-key map (kbd "s")   #'leitner-front-skip)
    (define-key map (kbd "q")   #'leitner-front-quit)
    (define-key map (kbd "?")   #'leitner-front-help)
    map)
  "Keymap for `leitner-front-mode'.")

(define-derived-mode leitner-front-mode special-mode "Leitner"
  "Read-only buffer shown before revealing a note file."
  :interactive nil)

(defun leitner--show-front-card (pair)
  "Display the front card for PAIR (group-name . item-alist)."
  (let* ((gname    (car pair))
         (item     (cdr pair))
         (path     (cdr (assq :path item)))
         (box      (cdr (assq :box  item)))
         (lr       (cdr (assq :last-reviewed item)))
         (reviewed (cdr (assq :reviewed leitner--session)))
         (total    (cdr (assq :total    leitner--session)))
         (fname    (file-name-sans-extension (file-name-nondirectory path)))
         (interval (leitner--box-days box))
         (buf      (get-buffer-create leitner--front-buf)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (leitner-front-mode)
        (setq leitner--front-item  item
              leitner--front-group gname)
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
            (ins (format "  Last reviewed:  %s\n" (leitner--format-ts lr)))
            (ins "\n\n")
            (ins (concat "  " fname) '(:weight bold :height 1.2))
            (ins "\n\n\n")
            ;; The prompt line only appears when one is found in the file.
            (let ((prompt (leitner--extract-prompt path)))
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

(defun leitner-front-reveal ()
  "Reveal the note file for the current front card."
  (interactive)
  (unless leitner--front-item
    (user-error "Leitner: no front card active"))
  (let* ((item  leitner--front-item)
         (gname leitner--front-group)
         (path  (cdr (assq :path item)))
         (fc    (current-buffer)))
    (find-file path)
    (setq-local leitner--review-item  item)
    (setq-local leitner--review-group gname)
    (leitner-review-minor-mode 1)
    (kill-buffer fc)))

(defun leitner-front-skip ()
  "Skip the current front card without revealing."
  (interactive)
  (unless leitner--session (user-error "Leitner: no active session"))
  (let* ((queue (cdr (assq :queue leitner--session)))
         (pair  (car queue))
         (gname (car pair))
         (item  (cdr pair))
         (path  (cdr (assq :path item))))
    (leitner--replace-item gname path
                           (leitner--item-rate item 'skip))
    (cl-incf (cdr (assq :reviewed leitner--session)))
    (setcdr (assq :queue leitner--session) (cdr queue))
    (message "Leitner: skipped '%s'." (file-name-nondirectory path))
    (leitner--session-advance)))

(defun leitner-front-quit ()
  "Quit the session from the front card."
  (interactive)
  (when (yes-or-no-p "Quit this Leitner session (Progress so far is saved.)?")
    (setq leitner--session nil)
    (leitner-save)
    (kill-buffer (current-buffer))
    (message "Leitner: session ended.  Index saved.")))

(defun leitner-front-help ()
  "Show front-card keybindings in the echo area."
  (interactive)
  (message "Leitner front: [SPC] Reveal   [s] Skip   [q] Quit"))

;; ===========================================================================
;;  Review Minor Mode: rating from within the note file

(defvar-local leitner--review-item  nil "Item under review (buffer-local).")
(defvar-local leitner--review-group nil "Group of the item under review (buffer-local).")

(defvar leitner-review-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c l g") #'leitner-rate-good)
    (define-key map (kbd "C-c l b") #'leitner-rate-bad)
    (define-key map (kbd "C-c l p") #'leitner-rate-partial)
    (define-key map (kbd "C-c l r") #'leitner-rate-revised)
    (define-key map (kbd "C-c l s") #'leitner-rate-skip)
    (define-key map (kbd "C-c l q") #'leitner-quit-session)
    (define-key map (kbd "C-c l ?") #'leitner-review-help)
    map)
  "Keymap active while reviewing a revealed note file.")

(define-minor-mode leitner-review-minor-mode
  "Active while a note file is open for review.
The file is fully editable, so you can revise freely and rate your
recall when done."
  :lighter " Leitner"
  :keymap leitner-review-minor-mode-map
  (if leitner-review-minor-mode
      (progn
        (setq header-line-format (leitner--build-review-header))
        (add-hook 'kill-buffer-hook #'leitner--on-review-buffer-kill nil t))
    (kill-local-variable 'header-line-format)
    (remove-hook 'kill-buffer-hook #'leitner--on-review-buffer-kill t)))

(defun leitner--build-review-header ()
  "Construct the header-line string for the file currently under review."
  (when (and leitner--review-item leitner--session)
    (let ((box      (cdr (assq :box leitner--review-item)))
          (reviewed (cdr (assq :reviewed leitner--session)))
          (total    (cdr (assq :total    leitner--session))))
      (concat
       (propertize (format " LEITNER  %d/%d " (1+ reviewed) total) 'face '(:weight bold))
       (propertize (format "  %s" leitner--review-group) 'face 'mode-line)
       (propertize (format "  Box %d " box)
                   'face '(:slant italic))
       (propertize "    C-c l g Good   C-c l b Bad   C-c l p Partial   C-c l r Revised   C-c l s Skip   C-c l q Quit"
                   'face '(:inherit shadow))))))

(defun leitner--on-review-buffer-kill ()
  "Warn when a review buffer is killed mid-session."
  (when (and leitner-review-minor-mode leitner--session)
    (message "Leitner: review buffer killed -- use M-x leitner-start-session to resume.")))

;;;###autoload
(defun leitner-rate-good ()
  "Rate current review item GOOD (move up one box)."
  (interactive)
  (leitner--session-record 'good))

;;;###autoload
(defun leitner-rate-bad ()
  "Rate current review item BAD (reset straight to Box 1)."
  (interactive)
  (leitner--session-record 'reset))

;;;###autoload
(defun leitner-rate-partial ()
  "Rate current review item PARTIAL: only read part of it, or ran out of time.
Box untouched, the file marked paused and scheduled to come due again tomorrow."
  (interactive)
  (leitner--session-record 'partial))

;;;###autoload
(defun leitner-rate-revised ()
  "Rate current review item REVISED: you revised or added to your notes.
Unlike `leitner-rate-partial' the review counts as complete, the file
returns on its normal box interval, new content and all."
  (interactive)
  (leitner--session-record 'revised))

;;;###autoload
(defun leitner-rate-skip ()
  "Skip current review item (keep its box)."
  (interactive)
  (leitner--session-record 'skip))

;;;###autoload
(defun leitner-quit-session ()
  "End the current review session early and save progress."
  (interactive)
  (when (yes-or-no-p "Quit this Leitner session  (Progress so far is saved.)?")
    (when leitner-review-minor-mode
      (leitner-review-minor-mode -1))
    (setq leitner--session nil)
    (leitner-save)
    (message "Leitner: session ended.  Index saved.")))

(defun leitner-review-help ()
  "Echo review keybindings in minibuffer."
  (interactive)
  (message "Leitner: C-c l g Good   C-c l b Bad   C-c l p Partial   C-c l r Revised   C-c l s Skip   C-c l q Quit   C-c l ? Help"))

;; ===========================================================================
;;  Dashboard, main entry point showing each group

(defvar leitner-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'leitner-menu-view-group)    ; open group detail
    (define-key map (kbd "r")   #'leitner-menu-start-session) ; review this group
    (define-key map (kbd "a")   #'leitner-add-file)
    (define-key map (kbd "A")   #'leitner-add-group)
    (define-key map (kbd "d")   #'leitner-menu-delete-group)
    (define-key map (kbd "R")   #'leitner-menu-rename-group)
    (define-key map (kbd "s")   #'leitner-start-session)      ; review ALL groups
    (define-key map (kbd "S")   #'leitner-save)
    (define-key map (kbd "g")   #'revert-buffer)
    (define-key map (kbd "G")   #'leitner-review-graduated)   ; browse graduated files
    (define-key map (kbd "?")   #'leitner-menu-help)
    map)
  "Keymap for the Leitner group dashboard.")

(defun leitner--menu-format ()
  "Build the tabulated-list column format from `leitner-box-intervals'."
  (vconcat
   (list (list "Group"  22 t)
         (list "Files"   7 t)
         (list "Due"     5 t)
         (list "Pause"   6 t)
         (list "Next"    6 t))
   (cl-loop for i from 1 to (leitner--num-boxes)
            collect (list (format "B%d" i) 5 t))
   (list (list "Grad" 5 t))))

(define-derived-mode leitner-menu-mode tabulated-list-mode "Leitner"
  "Group overview: due counts and box distribution for each group."
  (setq tabulated-list-format   (leitner--menu-format))
  (setq tabulated-list-entries  #'leitner--menu-entries)
  (setq header-line-format
        (propertize "  Leitner Notes: press ? for keybindings"
                    'face '(:inherit shadow :slant italic)))
  (setq-local revert-buffer-function
              (lambda (_auto _noconfirm)
                (leitner--ensure-data)
                (setq tabulated-list-format (leitner--menu-format)) ; box count may have changed
                (tabulated-list-init-header)
                (tabulated-list-print t)))
  (tabulated-list-init-header))

(defun leitner--menu-entries ()
  "Compute tabulated-list entries for the dashboard."
  (leitner--ensure-data)
  (let ((nb (leitner--num-boxes)))
    (mapcar
     (lambda (gname)
       (let* ((items  (leitner--group-items gname))
              (n      (length items))
              (active (seq-filter (lambda (it) (not (leitner--item-graduated-p it))) items))
              (due    (length (seq-filter #'leitner--item-due-p active)))
              (paused (length (seq-filter #'leitner--item-paused-p active)))
              (next   (leitner--group-next-due-str active))
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
     (leitner--group-names))))

;;;###autoload
(defun leitner ()
  "Open the Leitner group dashboard."
  (interactive)
  (leitner--ensure-data)
  (let ((buf (get-buffer-create "*Leitner*")))
    (with-current-buffer buf
      (leitner-menu-mode)
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(defun leitner-menu-view-group ()
  "Open the group detail view for the group on the current dashboard line."
  (interactive)
  (let ((gname (tabulated-list-get-id)))
    (when gname (leitner-view-group gname))))

(defun leitner-menu-start-session ()
  "Start a review session for the group on the current dashboard line."
  (interactive)
  (let ((gname (tabulated-list-get-id)))
    (when gname (leitner-start-session gname))))

(defun leitner-menu-delete-group ()
  "Delete the group on the current line, with confirmation."
  (interactive)
  (let ((gname (tabulated-list-get-id)))
    (when (and gname
               (yes-or-no-p (format "Delete group '%s' and all its entries? " gname)))
      (remhash gname (leitner--groups-ht))
      (leitner--persist)
      (tabulated-list-print t))))

(defun leitner-menu-rename-group ()
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
       ((leitner--get-group new-name)
        (user-error "Leitner: group '%s' already exists" new-name))
       (t
        (let ((group-alist (leitner--get-group old-name)))
          (setcdr (assq :name group-alist) new-name)
          (puthash new-name group-alist (leitner--groups-ht)) ; move to new hash key
          (remhash old-name (leitner--groups-ht))
          (let ((detail-buf (get-buffer (format "*Leitner: %s*" old-name))))
            (when detail-buf
              (with-current-buffer detail-buf
                (setq leitner--gv-group new-name)
                (rename-buffer (format "*Leitner: %s*" new-name) t)
                (setq header-line-format (leitner--gv-build-header new-name))
                (tabulated-list-print t))))
          (leitner--persist)
          (tabulated-list-print t)
          (message "Leitner: group renamed to '%s'." new-name)))))))

(defun leitner-menu-help ()
  "Echo dashboard keybindings to minibuffer."
  (interactive)
  (message
   "Leitner: RET view  r review  s review-all  G graduated  a add-file  A new-group  R rename  d delete  S save  g refresh"))

(defun leitner--maybe-refresh-dashboard ()
  "Silently refresh the dashboard buffer if it is alive."
  (let ((buf (get-buffer "*Leitner*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (tabulated-list-print t)))))

;; ===========================================================================
;;  Group Detail View  (file list + status for one group)

(defvar leitner-group-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'leitner-gv-open-file)
    (define-key map (kbd "r")   #'leitner-gv-reset-file)
    (define-key map (kbd "d")   #'leitner-gv-remove-file)
    (define-key map (kbd "a")   #'leitner-gv-add-file)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "?")   #'leitner-gv-help)
    map)
  "Keymap for the group detail view.")

(defvar-local leitner--gv-group nil "Group name this detail view is showing.")

(define-derived-mode leitner-group-view-mode tabulated-list-mode "Leitner-Group"
  "Detail view for one Leitner group: all files and their review status."
  (setq tabulated-list-format (leitner--gv-column-format))
  (tabulated-list-init-header))

(defun leitner--gv-column-format ()
  "Return the column format vector for the group detail view."
  [("File"          36 t)
   ("Box"            5 t)
   ("Last Reviewed" 14 t)
   ("Due in"        10 nil)
   ("Due?"           5 nil)])

(defun leitner--gv-build-header (group-name)
  "Build the header-line string for the GROUP-NAME detail view."
  (let* ((items  (leitner--group-items group-name))
         (n      (length items))
         (grad   (length (seq-filter #'leitner--item-graduated-p items)))
         (active (- n grad))
         (due    (length (seq-filter #'leitner--item-due-p items)))
         (paused (length (seq-filter #'leitner--item-paused-p items))))
    (propertize
     (format "  %s   |   %d active  %d graduated   |   %d due%s   |   RET open  r reset  d remove  a add  q close"
             group-name active grad due
             (if (> paused 0) (format "   |   %d paused" paused) ""))
     'face 'mode-line)))

(defun leitner--gv-refresh (group-name)
  "Refresh this group-view buffer's header and table for GROUP-NAME."
  (setq header-line-format (leitner--gv-build-header group-name))
  (tabulated-list-print t))

(defun leitner--gv-entries (group-name)
  "Build tabulated-list entries for GROUP-NAME in group view."
  (mapcar
   (lambda (item)
     (let* ((path   (cdr (assq :path item)))
            (box    (cdr (assq :box  item)))
            (lr     (cdr (assq :last-reviewed item)))
            (grad   (cdr (assq :graduated item)))
            (paused (leitner--item-paused-p item))
            (due-p  (leitner--item-due-p item))
            (days   (leitner--item-days-until-due item))
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
              (leitner--format-ts lr)
              due-str
              status)))
       (list path base-vec)))
   (leitner--group-items group-name)))

;;;###autoload
(defun leitner-view-group (group-name)
  "Open the detail view listing all files in GROUP-NAME."
  (interactive
   (list (completing-read "View group: " (leitner--group-names) nil t)))
  (leitner--ensure-data)
  (let ((buf (get-buffer-create (format "*Leitner: %s*" group-name))))
    (with-current-buffer buf
      (leitner-group-view-mode)
      (setq tabulated-list-format (leitner--gv-column-format))
      (tabulated-list-init-header)
      (setq leitner--gv-group      group-name
            header-line-format     (leitner--gv-build-header group-name)
            tabulated-list-entries (lambda () (leitner--gv-entries group-name)))
      (setq-local revert-buffer-function
                  (lambda (_a _n)
                    (setq tabulated-list-format (leitner--gv-column-format))
                    (tabulated-list-init-header)
                    (leitner--gv-refresh group-name)))
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(defun leitner-gv-open-file ()
  "Open the file on the current detail-view line."
  (interactive)
  (let ((path (tabulated-list-get-id)))
    (when path (find-file path))))

(defun leitner-gv-remove-file ()
  "Remove the file on the current line from the Leitner index."
  (interactive)
  (let* ((path  (tabulated-list-get-id))
         (gname leitner--gv-group))
    (when (and path
               (yes-or-no-p (format "Remove '%s' from Leitner? "
                                    (file-name-nondirectory path))))
      (leitner--set-group-items
       gname
       (seq-remove (lambda (it) (equal (cdr (assq :path it)) path))
                   (leitner--group-items gname)))
      (leitner--persist)
      (leitner--gv-refresh gname)
      (leitner--maybe-refresh-dashboard)
      (message "Leitner: removed '%s'." (file-name-nondirectory path)))))

(defun leitner-gv-reset-file ()
  "Reset the file on the current line to Box 1, reactivating it if graduated."
  (interactive)
  (let* ((path  (tabulated-list-get-id))
         (gname leitner--gv-group)
         (item  (leitner--find-item gname path)))
    (when (and item
               (yes-or-no-p
                (format (if (cdr (assq :graduated item))
                            "Reactivate '%s' and reset to Box 1? "
                          "Reset '%s' to Box 1? ")
                        (file-name-nondirectory path))))
      ;; 'reset is the only outcome that both forces Box 1 *and* clears
      ;; :graduated, there is no separate 'bad outcome.
      (leitner--replace-item gname path (leitner--item-rate item 'reset))
      (leitner--persist)
      (leitner--gv-refresh gname)
      (leitner--maybe-refresh-dashboard)
      (message "Leitner: '%s' reset to Box 1 (active)." (file-name-nondirectory path)))))

(defun leitner-gv-add-file ()
  "Add a file to the group shown in this detail view."
  (interactive)
  (leitner-add-file nil leitner--gv-group)
  (leitner--gv-refresh leitner--gv-group))

(defun leitner-gv-help ()
  "Echo group-detail keybindings."
  (interactive)
  (message "Leitner group: RET open   r reset   d remove   a add   q close"))

;; ===========================================================================
;;  Graduated Browser
;;
;; Lists every graduated file (across all groups, or just one) so you can
;; check them manually and send anything shaky back to Box 1.  Files you
;; don't touch here simply stay graduated.

(defun leitner--grad-age-str (ts)
  "Return a short \"how long ago\" string for Unix timestamp TS."
  (let ((days (/ (- (leitner--now) ts) 86400.0)))
    (if (< days 1) "<1d" (format "%dd" (floor days)))))

(defun leitner--grad-column-format ()
  "Column format for the graduated browser."
  [("Group"     16 t)
   ("File"      36 t)
   ("Graduated" 12 t)
   ("Age"        6 t)])

(defun leitner--grad-entries (&optional group-name)
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
                     (leitner--format-ts grad) (leitner--grad-age-str grad)))))
   (leitner--graduated-pairs group-name)))

(defun leitner--grad-build-header (group-name)
  "Build the header-line string for the graduated browser for GROUP-NAME."
  (let ((n (length (leitner--graduated-pairs group-name))))
    (propertize
     (format "  %d graduated file%s%s   |   RET open   r bring back   g refresh   q close"
             n (if (= n 1) "" "s")
             (if group-name (format "  (group: %s)" group-name) ""))
     'face '(:inherit shadow :slant italic))))

(defvar-local leitner--grad-group nil
  "The group this graduated-browser buffer is filtered to, or nil for all groups.")

(defvar leitner-graduated-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'leitner-grad-open-file)
    (define-key map (kbd "r")   #'leitner-grad-bring-back)
    (define-key map (kbd "g")   #'revert-buffer)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "?")   #'leitner-grad-help)
    map)
  "Keymap for the graduated browser.")

(define-derived-mode leitner-graduated-mode tabulated-list-mode "Leitner-Grad"
  "Browse graduated files and decide whether each one stays retired."
  (setq tabulated-list-format (leitner--grad-column-format))
  (tabulated-list-init-header))

;;;###autoload
(defun leitner-review-graduated (&optional group-name)
  "Browse every graduated file and decide whether it stays retired.
With a prefix argument, limit the browser to one GROUP-NAME instead of
every group.  Press r on a file to send it back to Box 1."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Limit to group: " (leitner--group-names) nil t))))
  (leitner--ensure-data)
  (let ((buf (get-buffer-create
              (if group-name
                  (format "*Leitner: Graduated (%s)*" group-name)
                "*Leitner: Graduated*"))))
    (with-current-buffer buf
      (leitner-graduated-mode)
      (setq leitner--grad-group      group-name
            tabulated-list-entries   (lambda () (leitner--grad-entries group-name))
            header-line-format       (leitner--grad-build-header group-name))
      (setq-local revert-buffer-function
                  (lambda (_auto _noconfirm)
                    (setq header-line-format (leitner--grad-build-header group-name))
                    (tabulated-list-print t)))
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(defun leitner-grad-open-file ()
  "Open the file under point in the graduated browser."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id (find-file (cdr id)))))

(defun leitner-grad-bring-back ()
  "Send the graduated file under point back to Box 1.
Same as rating it Reset from a normal review: box resets to 1, the
graduated flag clears."
  (interactive)
  (let* ((id    (tabulated-list-get-id))
         (gname (car id))
         (path  (cdr id))
         (item  (leitner--find-item gname path)))
    (unless item (user-error "Leitner: no file on current line"))
    (leitner--replace-item gname path (leitner--item-rate item 'reset))
    (leitner--persist)
    (setq header-line-format (leitner--grad-build-header leitner--grad-group))
    (tabulated-list-print t)
    (leitner--maybe-refresh-dashboard)
    (message "Leitner: '%s' is back in the rotation at Box 1." (file-name-nondirectory path))))

(defun leitner-grad-help ()
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
(with-eval-after-load 'evil
  (evil-set-initial-state 'leitner-menu-mode 'emacs)
  (evil-set-initial-state 'leitner-group-view-mode 'emacs)
  (evil-set-initial-state 'leitner-graduated-mode 'emacs)
  (evil-set-initial-state 'leitner-front-mode 'emacs)  ; SPC/s also live in evil-motion-state-map
  (dolist (map (list leitner-menu-mode-map
                     leitner-group-view-mode-map
                     leitner-graduated-mode-map))
    (define-key map (kbd "j") #'evil-next-line)
    (define-key map (kbd "k") #'evil-previous-line))
  ;; Review minor mode uses C-c l <key>, which evil doesn't shadow in normal
  ;; state, but evil-make-overriding-map + normalize is a safety net so the
  ;; minor-mode map is always consulted first regardless of buffer state.
  (evil-make-overriding-map leitner-review-minor-mode-map 'normal)
  (add-hook 'leitner-review-minor-mode-hook #'evil-normalize-keymaps))


(provide 'leitner)
;;; leitner.el ends here
