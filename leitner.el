;;; leitner.el --- Leitner spaced repetition for note files  -*- lexical-binding: t; -*-

;; Author: vmargb
;; Version: 0.1
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
;; When a session is running, each due file opens normally so you can read and
;; edit it freely.  A header line shows session context.
;; When you're done with a file, rate it:
;;
;;   C-c l g   Good  – you recalled the concept confidently  (advance one box)
;;   C-c l b   Bad   – you struggled or got confused         (reset to Box 1)
;;   C-c l s   Skip  – defer this file, keep its box
;;   C-c l q   Quit  – end the session (progress is auto-saved)
;;   C-c l ?   Show this help in the echo area
;;
;; ~~ BOX SCHEDULE (default, all customisable) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
;;   Box 1 →  review every  1 day   (new / difficult material)
;;   Box 2 →  review every  3 days
;;   Box 3 →  review every  7 days
;;   Box 4 →  review every 14 days
;;   Box 5 →  review every 30 days  (thoroughly mastered)
;;
;; ~~ CUSTOMISATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
;;   leitner-index-file      where the JSON index lives
;;   leitner-box-intervals   vector of per-box intervals in days
;;   leitner-default-group   group used when none is specified
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

(defcustom leitner-box-intervals [1 3 7 14 30]
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

;; leitner--data is the root in-memory store.
;; It is an alist with these keys:
;;   :groups  – hash-table  group-name (string) -> group-alist
;;   :dirty   – bool        t when there are unsaved changes
;;
;; A group-alist has:
;;   :name    – string
;;   :items   – list of item-alists
;;
;; An item-alist has:
;;   :path           – string  (absolute path)
;;   :box            – integer (1-indexed)
;;   :last-reviewed  – integer (Unix timestamp; 0 = never reviewed)
;;   :added          – integer (Unix timestamp)

(defvar leitner--data nil
  "In-memory representation of the Leitner index.  Is nil until first load.")

;; leitner--session holds the state of an active review session:
;;
;;   :queue        – list of (group-name . item-alist) pairs still to review
;;   :reviewed     – integer count of items rated so far
;;   :total        – integer total items in this session
;;   :group-filter – string or nil

(defvar leitner--session nil
  "Active review session state, or nil when idle.")


(provide 'leitner)
;;; leitner.el ends here
