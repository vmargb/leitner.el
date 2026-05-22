;;; leitner.el --- Leitner spaced repetition for note files  -*- lexical-binding: t; -*-
;; Author: vmargb
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: notes, spaced-repetition, org, feynman
;; URL: https://github.com/vmargb/leitner.el

;;; Commentary:
;;
;; leitner.el implements the Leitner box system for WHOLE NOTE FILES
;; rather than individual flashcards.  It is designed to complement your
;; existing note-taking workflow you write notes of concepts in org (or any format)
;; files, and this package surfaces them for review according to a Leitner schedule.
;;
;; FILES ARE NEVER MODIFIED.  All scheduling metadata lives in an external
;; JSON index file.
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
;;       C-c l b   Bad   struggled or blank   (prompts: move back one, or reset to box 1)
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
Graduated items are never due -- they have left the active queue."
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
  "Return a NEW item-alist for ITEM rated with OUTCOME (good / bad / reset / skip)."
  (let* ((old-box (cdr (assq :box item)))
         (last-box (leitner--num-boxes))
         ;; graduating: good rating on the final box
         (graduating (and (eq outcome 'good) (= old-box last-box)))
         (new-box (pcase outcome
                    ('good (if graduating last-box (1+ old-box)))
                    ('reset  1)
                    ('hard (max 1 (1- old-box)))
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
  "Return the items list for group NAME (may be nil)."
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
;; come back as plain strings
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
                         ;; JSON false/null both come back as nil in Emacs
                         ;; a real timestamp is an integer, keep it as-is
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


;; =========================================================================
;;  Adding / Removing / Resetting Files
;; =========================================================================

(defun leitner--read-group-name (&optional prompt)
  "PROMPT for a group name with completion."
  (let* ((names   (leitner--group-names))
         (default leitner-default-group)
         (pr      (or prompt (format "Group (default %s): " default))))
    (completing-read pr names nil nil nil nil default)))

;;;###autoload
(defun leitner-add-file (&optional file group)
  "Add FILE to GROUP for spaced repetition.
prompts for a group and defaults to the current buffers file."
  (interactive)
  (leitner--ensure-data)
  (let* ((target (expand-file-name
                  (or file
                      (buffer-file-name)
                      (read-file-name "File to add: " nil nil t))))
         (grp    (or group (leitner--read-group-name))))
    (if (leitner--path-registered-p target grp)
        (message "Leitner: '%s' is already in group '%s'."
                 (file-name-nondirectory target) grp)
      (leitner--prepend-item grp (leitner--make-item target))
      (leitner--mark-dirty)
      (leitner-save)
      (message "Leitner: added '%s' to group '%s' (Box 1)."
               (file-name-nondirectory target) grp)
      (leitner--maybe-refresh-dashboard))))

;;;###autoload
(defun leitner-remove-file (&optional file)
  "Remove FILE from the Leitner index and defaults to the current buffers file."
  (interactive)
  (leitner--ensure-data)
  (let ((abs (expand-file-name
              (or file
                  (buffer-file-name)
                  (read-file-name "File to remove: ")))))
    (maphash (lambda (gname _g)
               (leitner--set-group-items
                gname
                (seq-remove (lambda (it)
                              (equal (cdr (assq :path it)) abs))
                            (leitner--group-items gname))))
             (leitner--groups-ht))
    (leitner--mark-dirty)
    (leitner-save)
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
    (leitner--mark-dirty)
    (leitner-save)
    (message "Leitner: group '%s' created." name)
    (leitner--maybe-refresh-dashboard)))


;; ===========================================================================
;;  Review Session
;; ===========================================================================

;;;###autoload
(defun leitner-start-session (&optional group-name)
  "Start a Leitner review session for all currently due files.
With a optional prefix argument, prompt to limit review to one GROUP-NAME."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Limit session to group: "
                            (leitner--group-names) nil t))))
  (leitner--ensure-data)
  (when (and leitner--session
             (not (yes-or-no-p "A session is already running.  Start a new one? ")))
    (user-error "Session aborted"))
  (let ((due (leitner--due-pairs group-name)))
    (if (null due)
        (message "Leitner: nothing due%s, great work!"
                 (if group-name (format " in '%s'" group-name) ""))
      (let ((queue
             (sort (copy-sequence due)
                   (lambda (a b)
                     (let ((ba (cdr (assq :box (cdr a))))
                           (bb (cdr (assq :box (cdr b)))))
                       (if (/= ba bb) (< ba bb)
                         (< (cdr (assq :last-reviewed (cdr a)))
                            (cdr (assq :last-reviewed (cdr b))))))))))
        (setq leitner--session
              (list (cons :queue        queue)
                    (cons :reviewed     0)
                    (cons :total        (length queue))
                    (cons :group-filter group-name)))
        (message "Leitner: %d file%s due%s.  Session starting..."
                 (length queue)
                 (if (= (length queue) 1) "" "s")
                 (if group-name (format " in '%s'" group-name) ""))
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
  "Record the OUTCOME (good/bad:{hard,reset}/skip) for the current item and advance."
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
                     ('reset  "Reset -> Box 1")
                     ('hard (if (= (cdr (assq :box new-item)) 1)
                                    (propertize "Already in Box 1" 'face 'warning)
                                  (format "Down -> Box %d" (cdr (assq :box new-item)))))
                     ('skip "Skipped"))))
      (message "Leitner: %s  (%d / %d done)"
               label
               (cdr (assq :reviewed leitner--session))
               (cdr (assq :total    leitner--session))))
    (leitner--session-advance)))

(defun leitner--session-finish ()
  "Clean up and save after all items have been reviewed."
  (let ((n (cdr (assq :reviewed leitner--session))))
    (setq leitner--session nil)
    (leitner-save)
    (leitner--maybe-refresh-dashboard)
    (message "Leitner: session complete, %d file%s reviewed.  Index saved."
             n (if (= n 1) "" "s"))))


;; =========================================================================
;;  Front Card: recall before reveal
;; =========================================================================

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
        ;;
        ;; Layout of front-card:
        ;;   header bar
        ;;   meta line
        ;;   (blank)
        ;;   concept name  <-- big, centred
        ;;   (blank)
        ;;   prompt lines
        ;;   key hints
        ;;
        (let* ((width   (max 50 (- (window-width) 4)))
               (rule    (make-string width ?-))
               (padname (concat "  " fname)))
          (insert "\n")
          (insert (propertize
                   (format "  LEITNER  %d / %d\n" (1+ reviewed) total)
                   'face '(:weight bold)))
          (insert (propertize (concat "  " rule "\n") 'face 'shadow))
          (insert "\n")
          (insert (format "  Group:          %s\n" gname))
          (insert (format "  Box:            %d  (every %d day%s)\n"
                          box interval
                          (if (= interval 1) "" "s")))
          (insert (format "  Last reviewed:  %s\n"
                          (leitner--format-ts lr)))
          (insert "\n\n")
          (insert (propertize padname
                              'face '(:weight bold :height 1.2)))
          (insert "\n\n\n")
          (insert (propertize
                   "  Recall from memory before revealing.\n"
                   'face '(:slant italic)))
          (insert (propertize
                   "  When ready, press SPC to open your notes.\n"
                   'face '(:slant italic)))
          (insert "\n")
          (insert (propertize (concat "  " rule "\n") 'face 'shadow))
          (insert (propertize
                   "  [SPC] Reveal     [s] Skip     [q] Quit\n"
                   'face 'shadow)))
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
    ;; inject session context into the file buffer, then activate review mode
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
    (leitner--replace-item gname path (leitner--item-rate item 'skip))
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


;; =========================================================================
;;  Review Minor Mode: rating from within the note file
;; =========================================================================

(defvar-local leitner--review-item  nil "Item under review (buffer-local).")
(defvar-local leitner--review-group nil "Group of the item under review (buffer-local).")

(defvar leitner-review-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c l g") #'leitner-rate-good)
    (define-key map (kbd "C-c l b") #'leitner-rate-bad)
    (define-key map (kbd "C-c l s") #'leitner-rate-skip)
    (define-key map (kbd "C-c l q") #'leitner-quit-session)
    (define-key map (kbd "C-c l ?") #'leitner-review-help)
    map)
  "Keymap active while reviewing a revealed note file.")


;; [leitner-rate-good]   Good  (advance one box)
;; [leitner-rate-bad]    Bad   (prompts: back one box or reset to box 1)
;; [leitner-rate-skip]   Skip
;; [leitner-quit-session]   Quit session
;; [leitner-review-help]    Show keybindings"
(define-minor-mode leitner-review-minor-mode
  "Active while a note file is open for review.
The file is fully editable, which allows you to revise freely
and rate your recall when done"
  :lighter " Leitner"
  :keymap leitner-review-minor-mode-map
  (if leitner-review-minor-mode
      (progn
        (setq header-line-format (leitner--build-review-header))
        (add-hook 'kill-buffer-hook #'leitner--on-review-buffer-kill nil t))
    (kill-local-variable 'header-line-format)
    (remove-hook 'kill-buffer-hook #'leitner--on-review-buffer-kill t)))

(defun leitner--build-review-header ()
  "Constructs the header-line string for the current file being reviewed."
  (when (and leitner--review-item leitner--session)
    (let ((box      (cdr (assq :box leitner--review-item)))
          (reviewed (cdr (assq :reviewed leitner--session)))
          (total    (cdr (assq :total    leitner--session)))
          (gname    leitner--review-group))
      (concat
       (propertize (format " LEITNER  %d/%d " (1+ reviewed) total)
                   'face '(:weight bold))
       (propertize (format "  %s" gname) 'face 'mode-line)
       (propertize (format "  Box %d " box) 'face '(:slant italic))
       (propertize "    C-c l g Good   C-c l b Bad   C-c l s Skip   C-c l q Quit"
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
;;;###autoload
(defun leitner-rate-bad ()
  "Prompt whether to move the item back one box or reset to Box 1."
  (interactive)
  (let ((choice (read-char-choice
                 "Bad rating: [b]ack one box(hard), [r]eset to Box 1(complete blank), [q]uit? "
                 '(?b ?r ?q))))
    (pcase choice
      (?b (leitner--session-record 'back))
      (?r (leitner--session-record 'reset))
      (?q (message "Leitner: rating cancelled.")))))

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
  (message "Leitner: C-c l g Good   C-c l b Bad (prompts)   C-c l s Skip   C-c l q Quit   C-c l ? Help"))

;; =========================================================================
;;  Dashboard: the main entry point showing each group
;; =========================================================================

(defvar leitner-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'leitner-menu-view-group)    ; open group detail
    (define-key map (kbd "r")   #'leitner-menu-start-session) ; review this group
    (define-key map (kbd "a")   #'leitner-add-file)
    (define-key map (kbd "A")   #'leitner-add-group)
    (define-key map (kbd "d")   #'leitner-menu-delete-group)
    (define-key map (kbd "s")   #'leitner-start-session)      ; review ALL groups
    (define-key map (kbd "S")   #'leitner-save)
    (define-key map (kbd "g")   #'revert-buffer)
    (define-key map (kbd "?")   #'leitner-menu-help)
    map)
  "Keymap for the Leitner group dashboard.")

(defun leitner--menu-format ()
  "Build the tabulated-list column format from `leitner-box-intervals'."
  (vconcat
   (list (list "Group"  26 t)
         (list "Files"   7 t)
         (list "Due"     5 t))
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
                ;; rebuild column format in case leitner-box-intervals changed
                (setq tabulated-list-format (leitner--menu-format))
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
              (grad   (- n (length active)))
              (boxes  (make-vector nb 0)))
         (dolist (item active)
           (let ((b (1- (min nb (cdr (assq :box item))))))
             (aset boxes b (1+ (aref boxes b)))))
         (list gname
               (vconcat
                (list gname
                      (number-to-string n)
                      (if (> due 0)
                          (propertize (number-to-string due) 'face 'warning)
                        "0"))
                (cl-loop for i from 0 below nb
                         collect (number-to-string (aref boxes i)))
                (list (if (> grad 0)
                          (propertize (number-to-string grad) 'face 'success)
                        "0"))))))
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
  "Start a review session for the group currently on the current dashboard line."
  (interactive)
  (let ((gname (tabulated-list-get-id)))
    (when gname (leitner-start-session gname))))

(defun leitner-menu-delete-group ()
  "Delete the group on the current line, with confirmation."
  (interactive)
  (let ((gname (tabulated-list-get-id)))
    (when (and gname
               (yes-or-no-p (format "Delete group '%s' and all its entries? "
                                    gname)))
      (remhash gname (leitner--groups-ht))
      (leitner--mark-dirty)
      (leitner-save)
      (tabulated-list-print t))))

(defun leitner-menu-help ()
  "Echo dashboard keybindings to minibuffer."
  (interactive)
  (message
   "Leitner: RET view-group  r review-group  s review-all  a add-file  A new-group  d delete  S save  g refresh"))

(defun leitner--maybe-refresh-dashboard ()
  "Silently refresh the dashboard buffer if it is alive."
  (let ((buf (get-buffer "*Leitner*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (tabulated-list-print t)))))


;; =========================================================================
;;  Group Detail View  (file list + status for one group)
;; =========================================================================
;; detail view for the current group, shows the file list with scheduling info

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
  (setq tabulated-list-format
        [("File"          36 t)
         ("Box"            5 t)
         ("Last Reviewed" 14 t)
         ("Due in"        10 nil)
         ("Due?"           5 nil)])
  (tabulated-list-init-header))

(defun leitner--gv-build-header (group-name)
  "Build the header-line string for the GROUP-NAME detail view."
  (let* ((items  (leitner--group-items group-name))
         (n      (length items))
         (grad   (length (seq-filter #'leitner--item-graduated-p items)))
         (active (- n grad))
         (due    (length (seq-filter #'leitner--item-due-p items))))
    (propertize
     (format "  %s   |   %d active  %d graduated   |   %d due   |   RET open  r reset/reactivate  d remove  a add  q close"
             group-name active grad due)
     'face 'mode-line)))

(defun leitner--gv-entries (group-name)
  "Build tabulated-list entries for GROUP-NAME in group view."
  (mapcar
   (lambda (item)
     (let* ((path  (cdr (assq :path item)))
            (box   (cdr (assq :box  item)))
            (lr    (cdr (assq :last-reviewed item)))
            (grad  (cdr (assq :graduated item)))
            (due-p (leitner--item-due-p item))
            (days  (leitner--item-days-until-due item))
            (due-str
             (cond (grad       (propertize "—"       'face 'shadow))
                   ((= lr 0)   (propertize "new"     'face 'warning))
                   (due-p      (propertize "overdue" 'face 'warning))
                   (t          (format "%.0fd" days))))
            (status
             (cond (grad   (propertize "Grad" 'face 'success))
                   (due-p  (propertize "Yes"  'face 'warning))
                   (t      ""))))
       (list path
             (vector
              (file-name-nondirectory path)
              (if grad (propertize (number-to-string box) 'face 'shadow)
                (number-to-string box))
              (leitner--format-ts lr)
              due-str
              status))))
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
      (setq leitner--gv-group      group-name
            header-line-format     (leitner--gv-build-header group-name)
            tabulated-list-entries (lambda () (leitner--gv-entries group-name)))
      (setq-local revert-buffer-function
                  (lambda (_a _n)
                    (setq header-line-format
                          (leitner--gv-build-header group-name))
                    (tabulated-list-print t)))
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
      (leitner--mark-dirty)
      (leitner-save)
      (setq header-line-format (leitner--gv-build-header gname))
      (tabulated-list-print t)
      (leitner--maybe-refresh-dashboard)
      (message "Leitner: removed '%s'." (file-name-nondirectory path)))))

(defun leitner-gv-reset-file ()
  "Reset the file on the current line to Box 1, reactivating it if it is graduated."
  (interactive)
  (let* ((path  (tabulated-list-get-id))
         (gname leitner--gv-group)
         (item  (seq-find (lambda (it)
                            (equal (cdr (assq :path it)) path))
                          (leitner--group-items gname))))
    (when (and item
               (yes-or-no-p
                (format (if (cdr (assq :graduated item))
                            "Reactivate '%s' and reset to Box 1? "
                          "Reset '%s' to Box 1? ")
                        (file-name-nondirectory path))))
      ;; leitner--item-rate 'bad clears :graduated as well and sets it to nil
      (leitner--replace-item gname path (leitner--item-rate item 'bad))
      (leitner--mark-dirty)
      (leitner-save)
      (setq header-line-format (leitner--gv-build-header gname))
      (tabulated-list-print t)
      (leitner--maybe-refresh-dashboard)
      (message "Leitner: '%s' reset to Box 1 (active)."
               (file-name-nondirectory path)))))

(defun leitner-gv-add-file ()
  "Add a file to the group shown in this detail view."
  (interactive)
  (leitner-add-file nil leitner--gv-group)
  (setq header-line-format (leitner--gv-build-header leitner--gv-group))
  (tabulated-list-print t))

(defun leitner-gv-help ()
  "Echo group-detail keybindings."
  (interactive)
  (message "Leitner group: RET open   r reset   d remove   a add   q close"))


;; =========================================================================
;;  Evil-mode Compatibility
;; =========================================================================
;;
;; keys `g', `?', `s', `a', `d', `A', `S' are intercepted by evil
;; `evil-set-initial-state' MODE 'emacs' tells evil to leave the buffer alone
(with-eval-after-load 'evil
  ;; dashboard (*Leitner*)
  ;; Emacs state lets RET/r/a/A/d/s/S/g/? all reach leitner-menu-mode-map
  ;; without evil interference.  Tabulated-list navigation (TAB, arrow keys, n/p)
  ;; works because those are in tabulated-list-mode-map
  (evil-set-initial-state 'leitner-menu-mode 'emacs)
  ;; group view (*Leitner: <group>*)
  (evil-set-initial-state 'leitner-group-view-mode 'emacs)
  ;; front card (*Leitner: Review*)
  ;; SPC, s are in evil-motion-state-map too, Emacs state bypasses fixes that
  (evil-set-initial-state 'leitner-front-mode 'emacs)
  ;; allow basic evil motion keys only
  (define-key leitner-menu-mode-map (kbd "j") #'evil-next-line)
  (define-key leitner-menu-mode-map (kbd "k") #'evil-previous-line)
  (define-key leitner-group-view-mode-map (kbd "j") #'evil-next-line)
  (define-key leitner-group-view-mode-map (kbd "k") #'evil-previous-line)
  ;; review minor mode (inside a normal note file buffer)
  ;; the bindings are all C-c l <key>  Evil does not shadow C-c prefixes in
  ;; normal state, but we use evil-make-overriding-map + normalize as a safety
  ;; net so the minor-mode map is always consulted first regardless of what
  ;; state the note buffer is in when the session starts
  (evil-make-overriding-map leitner-review-minor-mode-map 'normal)
  (add-hook 'leitner-review-minor-mode-hook #'evil-normalize-keymaps))


(provide 'leitner)
;;; leitner.el ends here
