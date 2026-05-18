;;; leitner.el --- Leitner spaced repetition for note files  -*- lexical-binding: t; -*-
;; Author: vmargb
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: notes, spaced-repetition, org, feynman
;; URL: https://github.com/vmargb/leitner.el

;;; Commentary:
;;
;; leitner.el implements the Leitner box system for WHOLE NOTE FILES
;; rather than individual flashcards.  It is designed around the "iterative
;; note revision" workflow: you write explanations of concepts in org (or any)
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
;;   M-x leitner-status        Summary of all files and their boxes
;;
;; ~~ REVIEW WORKFLOW ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
;;   Phase 1 FRONT CARD
;;     A buffer shows only the concept name, group, and box.
;;     Recall and explain the concept from memory before looking at your notes.
;;
;;       [SPC]  Reveal the note file
;;       [s]    Skip this item
;;       [q]    Quit the session
;;
;;   Phase 2 REVEALED FILE
;;     Your note file opens normally fully editable.
;;     This is the Feynman step: read, compare with what you recalled, and
;;     update your notes where your understanding was shaky.
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
;;   SETUP (use-package example)
;;
;;   (use-package leitner-notes
;;     :load-path "~/src/leitner-notes"
;;     :custom
;;     (leitner-index-file "~/notes/.leitner-index.json")
;;     (leitner-box-intervals [1 3 7 14 30])
;;     :bind ("C-c r l" . leitner))

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
;;
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


;; =========================================================================
;;  Provide
;; =========================================================================

(provide 'leitner)
;;; leitner.el ends here
