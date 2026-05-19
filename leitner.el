;;; leitner.el --- Leitner spaced repetition for note files  -*- lexical-binding: t; -*-
;; Author: vmargb
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: notes, spaced-repetition, org, feynman
;; URL: https://github.com/vmargb/leitner.el

;;; Commentary:
;;
;; leitner.el implements the Leitner box system for WHOLE NOTE FILES
;; rather than individual flashcards.  It is designed to complement your
;; existing note-taking workflow you write notes of concepts in org (or any)
;; files, and this package surfaces them for review according to a Leitner schedule.
;;
;; FILES ARE NEVER MODIFIED.  All scheduling metadata lives in an external
;; JSON index file.  The package is agnostic to the note format (org, markdown,
;; plain text, Denote, etc.).
;;
;; ~~ Quick Start ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
;;   M-x leitner               Open the group dashboard
;;   M-x leitner-add-group     Add new group to add files to
;;   M-x leitner-add-file      Add current buffer's file to a group
;;   M-x leitner-start-session Review all due files (or C-u for one group)
;;
;; ~~ REVIEW WORKFLOW ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
;;   Phase 1 FRONT CARD
;;     A buffer shows only the buffer name, group, and box.
;;     Recall and explain the contents from memory before looking
;;
;;       [SPC]  Reveal the note file
;;       [s]    Skip this item
;;       [q]    Quit the session
;;
;;   Phase 2 REVEALED FILE
;;     Your note file opens normally fully editable.
;;     This is the Feynman step: read, compare with what you recalled, and
;;     update your notes where your understanding was shaky before moving on
;;
;;       C-c l g   Good  confident recall     (advance one box)
;;       C-c l b   Bad   struggled or blank   (reset to Box 1)
;;       C-c l s   Skip
;;       C-c l q   Quit session
;;       C-c l ?   Show keybindings
;;
;; ~~ CUSTOMISATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
;;   leitner-index-file      where the JSON index lives
;;   leitner-box-intervals   vector of per-box intervals in days
;;   leitner-default-group   group used when none is specified
;;
;;   DASHBOARD KEYS
;;
;;   RET   Open the file list for that group (group detail view)
;;   r     Start a review session for the group under point
;;   a     Add a file (defaults to current buffer)
;;   A     Create a new empty group
;;   d     Delete the group under point
;;   s     Start a session across ALL groups
;;   S     Save the index manually
;;   g     Refresh
;;
;;   GROUP DETAIL KEYS
;;
;;   RET   Open the file under point
;;   r     Reset that file to Box 1
;;   d     Remove that file from the index
;;   a     Add a file to this group
;;   q     Close
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'json)
(require 'tabulated-list)


;; ===========================================================================
;;  Customisation
;; ===========================================================================

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


;; ===========================================================================
;;  Internal State
;; ===========================================================================

;; leitner--data  =  ((:groups . HASH-TABLE)  (:dirty . BOOL))
;;
;; HASH-TABLE  :  group-name (string)  ->  group-alist
;; group-alist :  ((:name . STRING)  (:items . LIST-OF-ITEM-ALISTS))
;; item-alist  :  ((:path . STRING)  (:box . INT)
;;                 (:last-reviewed . INT)  (:added . INT)
;;                 (:graduated . INT-OR-NIL))   ; Unix ts when graduated, nil if active
;;
;; All mutations go through setcdr+assq to modify the shared cons cells
;; in-place.  Never use alist-get as a setf target with a gethash argument
;; -- that pattern causes "Symbol's value as variable is void: %s" errors
;; because of how the setf macro expands in some Emacs versions.

(defvar leitner--data nil
  "In-memory Leitner index.  Nil until first initialisation.")

;; leitner--session holds the state of an active review session:
;;   :queue        – list of (group-name . item-alist) pairs still to review
;;   :reviewed     – integer count of items rated so far
;;   :total        – integer total items in this session
;;   :group-filter – string or nil

(defvar leitner--session nil
  "Active review session state, or nil when idle.")


;; ===========================================================================
;;  Small Utilities
;; ===========================================================================

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

(defun leitner--item-due-p (item)
  "Return non-nil when ITEM is due for review.
Graduated items are never due, they have left the active queue."
  (and (not (cdr (assq :graduated item)))
       (let ((lr  (cdr (assq :last-reviewed item)))
             (box (cdr (assq :box item))))
         (or (= lr 0)
             (>= (- (leitner--now) lr) (leitner--box-secs box))))))

(defun leitner--item-days-until-due (item)
  "Days until ITEM is next due; negative = overdue; 0 = never reviewed."
  (let ((lr (cdr (assq :last-reviewed item))))
    (if (= lr 0) 0
      (/ (- (leitner--box-secs (cdr (assq :box item)))
            (- (leitner--now) lr))
         86400.0))))

(defun leitner--make-item (path)
  "Return a fresh item-alist for PATH placed in Box 1."
  (list (cons :path          (expand-file-name path))
        (cons :box           1)
        (cons :last-reviewed 0)
        (cons :added         (leitner--now))
        (cons :graduated     nil)))

(defun leitner--item-graduated-p (item)
  "Return non-nil when ITEM has been graduated (fully mastered)."
  (cdr (assq :graduated item)))

(defun leitner--item-rate (item outcome)
  "Return a NEW item-alist for ITEM rated with OUTCOME (good / bad / skip)."
  (let* ((old-box (cdr (assq :box item)))
         (last-box (leitner--num-boxes))
         ;; graduating: good rating on the final box.
         (graduating (and (eq outcome 'good) (= old-box last-box)))
         (new-box (pcase outcome
                    ('good (if graduating last-box (1+ old-box)))
                    ('bad  1)
                    ('skip old-box))))
    (list (cons :path          (cdr (assq :path item)))
          (cons :box           new-box)
          (cons :last-reviewed (if (eq outcome 'skip)
                                   (cdr (assq :last-reviewed item))
                                 (leitner--now)))
          (cons :added         (cdr (assq :added item)))
          (cons :graduated     (if graduating (leitner--now) nil)))))

(defun leitner--format-ts (ts)
  "Format Unix timestamp TS as YYYY-MM-DD, or \"Never\" for 0."
  (if (= ts 0) "Never"
    (format-time-string "%Y-%m-%d" (seconds-to-time ts))))

;; ===========================================================================
;;  Data Access
;; ===========================================================================

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
  "Return the items list for group NAME (might be nil)."
  (let ((g (leitner--get-group name)))
    (when g (cdr (assq :items g)))))

;; Direct in-place mutation using setcdr+assq.
;; Because gethash returns the actual cons-cell list stored in the hash
;; table, setcdr on one of its cells modifies the hash-table value too.

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
  "Mark the index if it has an unsaved change."
  (leitner--ensure-data)
  (setcdr (assq :dirty leitner--data) t))

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

(defun leitner--path-registered-p (path &optional group-name)
  "Return non-nil if PATH is already registered (optionally in GROUP-NAME)."
  (let ((abs (expand-file-name path)))
    (seq-find
     (lambda (pair)
       (and (or (null group-name) (equal (car pair) group-name))
            (equal (cdr (assq :path (cdr pair))) abs)))
     (leitner--all-pairs))))


;; =========================================================================
;;  Persistence
;; =========================================================================

;; use json-key-type 'string when reading so that JSON object keys
;; come back as plain strings, no interning, no symbol quirks
;; on the write side, json-encode calls symbol-name on symbol keys, so
;; symbol-keyed alists serialise cleanly

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
                   ;; Symbol keys -> json-encode uses symbol-name -> strings.
                   (list (cons 'path          (cdr (assq :path item)))
                         (cons 'box           (cdr (assq :box  item)))
                         (cons 'last_reviewed (cdr (assq :last-reviewed item)))
                         (cons 'added         (cdr (assq :added item)))
                         (cons 'graduated     (or (cdr (assq :graduated item))
                                                  :json-false))))
                 items))))
         ;; gname is a string; json-encode-key handles strings directly.
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
                 (let ((grad (cdr (assoc "graduated" raw))))
                   (list (cons :path          (cdr (assoc "path"          raw)))
                         (cons :box           (cdr (assoc "box"           raw)))
                         (cons :last-reviewed (cdr (assoc "last_reviewed" raw)))
                         (cons :added         (cdr (assoc "added"         raw)))
                         ;; JSON false/null both come back as nil in Emacs;
                         ;; a real timestamp is an integer -- keep it as-is.
                         (cons :graduated     (if (or (null grad)
                                                      (eq grad :json-false))
                                                  nil grad)))))
               items-list)))
        (puthash gname
                 (list (cons :name gname) (cons :items items))
                 ht)))
    (list (cons :groups ht) (cons :dirty nil))))

;;;###autoload
(defun leitner-save ()
  "Save the Leitner index to `leitner-index-file'."
  (interactive)
  (leitner--ensure-data)
  (let* ((full  (expand-file-name leitner-index-file))
         (dir   (file-name-directory full)))
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
          (setq leitner--data
                (list (cons :groups (make-hash-table :test #'equal))
                      (cons :dirty  nil)))
          (message "Leitner: no index found -- starting fresh."))
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
         (setq leitner--data
               (list (cons :groups (make-hash-table :test #'equal))
                     (cons :dirty  nil))))))))

(add-hook 'kill-emacs-hook
          (lambda ()
            (when (and leitner--data
                       (cdr (assq :dirty leitner--data)))
              (leitner-save))))



(provide 'leitner)
;;; leitner.el ends here
