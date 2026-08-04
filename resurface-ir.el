;;; resurface-ir.el --- Incremental reading for resurface.el  -*- lexical-binding: t; -*-
;; Author: vmargb
;; Package-Requires: ((emacs "27.1") (resurface "0.2.0"))
;; Keywords: notes, spaced-repetition, org, reading
;; URL: https://github.com/vmargb/resurface.el

;;; Commentary:
;;
;; resurface-ir.el is the optional Incremental Reading companion to
;; resurface.el.  It manages a PRIORITY QUEUE of whole sources,
;; PDFs, EPUBs, web pages, anything and decides which of them you see
;; today instead of leaving that to whichever tab happens to be open.
;;
;; Any command that can display a file or URL and leave point/region
;; normally can be plugged in via `resurface-ir-open-function', the
;; default opens local files with `find-file' (which picks up
;; `reader-mode' automatically once that package is on your `auto-mode-alist'
;;
;; -----------------------------------------
;; The two things this file actually owns
;;
;;   1. WHICH sources you see today and in what order: a priority queue
;;      (`resurface-ir-add-source' sets a 0-100 priority) filtered by a
;;      per-source "next eligible" date and shuffled for a little noise
;;      so low-priority sources still surface occasionally.  The next-
;;      eligible date is chosen by the exact same workload-balanced
;;      day-picker resurfac-leitner has (`resurface--choose-next-review'
;;      in resurface.el), just driven by a continuous priority instead
;;      of a box number, `resurface--ir-ideal-gap-days'.
;;
;;   2. TURNING a selection into a card: while a source is open,
;;      `resurface-ir-capture' (`C-c i c') takes the active region,
;;      drops it into a dedicated buffer to reword into your own words
;;      (`C-c C-c' to file it, `C-c C-k' to discard, then hands
;;      the result to `resurface-leitner-add-file' under that source's
;;      own Leitner group, and returns you to exactly where you were
;;      reading.  From that point on the card is a completely ordinary
;;      Leitner item, reviewed by the same Familiarity Scheduler.
;;      `resurface-ir-capture-update' (`C-c i u') is the other half of
;;      this: when a passage adds to something you already wrote a card
;;      for, rather than deserving a new one, it lets you search that
;;      source's own group, edit the chosen card's file directly, and
;;      files the edit as a `revised' review (box stays put) instead of
;;      creating a duplicate.
;;
;; Quick start:
;;   M-x resurface-ir                 Open the source-queue dashboard
;;   M-x resurface-ir-add-source      Register a PDF/EPUB/URL, set its priority
;;   M-x resurface-ir-start-session   Open today's sources by priority, one at a time
;;   M-x resurface-ir-capture         Reword the active selection into a Leitner card
;;   M-x resurface-ir-capture-update  Fold new material into an existing card instead
;;   M-x resurface-ir-review-finished Browse finished sources, reactivate any of them
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'tabulated-list)
(require 'resurface)

;; ===========================================================================
;;  Customisation

(defgroup resurface-ir nil
  "Enable incremental reading, a priority queue of whole sources for resurface.el."
  :group 'resurface
  :prefix "resurface-ir-")

(defcustom resurface-ir-capture-directory
  (expand-file-name "resurface-ir-cards/" user-emacs-directory)
  "Directory new incremental-reading cards are written into."
  :type 'directory
  :group 'resurface-ir)

(defcustom resurface-ir-default-priority 50
  "Priority (0-100, higher = seen sooner/more often) offered when adding a source."
  :type 'integer
  :group 'resurface-ir)

(defcustom resurface-ir-session-max-items nil
  "Maximum number of sources to queue in a single IR session, nil for no cap."
  :type '(choice (const :tag "No limit" nil)
                  (integer :tag "Max sources per session"))
  :group 'resurface-ir)

(defcustom resurface-ir-max-gap-days 14
  "Ideal days between appearances for a source at priority 0."
  :type 'integer
  :group 'resurface-ir)

(defcustom resurface-ir-min-gap-days 1
  "Ideal days between appearances for a source at priority 100."
  :type 'integer
  :group 'resurface-ir)

(defcustom resurface-ir-interval-tolerance 0.3
  "Fractional slack allowed around a source's ideal gap, each way."
  :type 'float
  :group 'resurface-ir)

(defcustom resurface-ir-workload-weight 0.3
  "Weight of same-day load against deviation from the ideal gap."
  :type 'float
  :group 'resurface-ir)

(defcustom resurface-ir-open-function #'resurface--ir-default-open
  "Function called with a source item-alist to open it for reading.
Must leave the opened buffer current when it returns,
`resurface--ir-default-open' for the default behaviour."
  :type 'function
  :group 'resurface-ir)

(defcustom resurface-ir-external-extensions '("pdf")
  "File extensions (no leading dot, case-insensitive) opened externally.
A stopgap until in-Emacs PDF/EPUB selection is solid,
`resurface--ir-default-open'."
  :type '(repeat string)
  :group 'resurface-ir)

(defcustom resurface-ir-external-open-command
  (if (eq system-type 'darwin) "open" "xdg-open")
  "Shell command used to open a file with the OS default application.
Ignored on Windows/Cygwin, which use `w32-shell-execute' instead."
  :type 'string
  :group 'resurface-ir)

(defcustom resurface-ir-before-read-hook nil
  "Hook run right after a source is opened for reading."
  :type 'hook
  :group 'resurface-ir)

(defcustom resurface-ir-after-capture-hook nil
  "Hook run immediately after a captured card is filed as a Leitner item."
  :type 'hook
  :group 'resurface-ir)


;; ===========================================================================
;;  Internal State
;;
;; A source item-alist:
;;   ((:path . STRING) (:type . 'file-OR-'url) (:title . STRING)
;;    (:group . STRING)          ; the Leitner group its captured cards go into
;;    (:priority . INT-0-100)
;;    (:added . INT) (:last-read . INT) (:next-eligible . INT)
;;    (:finished . INT-OR-NIL))  ; Unix ts when marked finished
;;
;; Stored as a flat list under resurface--data's :ir-sources key (see the
;; comment above resurface--empty-data in resurface.el); no sub-grouping
;; the way Leitner groups/drill blocks have, a source's identity is just
;; its :path.

;; :queue (source-alist...) left to open, :reviewed count, :total count, :capped bool
(defvar resurface--ir-session nil
  "Active incremental-reading session state, or nil when idle.")

(defvar-local resurface--ir-review-source nil
  "The IR source being read in this buffer, or nil.  Buffer-local.")

(defvar-local resurface--ir-capture-source nil
  "The IR source a capture buffer's card will be filed under.  Buffer-local.")

(defvar-local resurface--ir-capture-return nil
  "Window configuration to restore when a capture buffer is closed.")

(defvar-local resurface--ir-capture-update-path nil
  "Path of the existing card this capture buffer will overwrite, or nil.
Nil means submit files a new card, a path (set by
`resurface-ir-capture-update') means submit overwrites that card instead.")


;; ===========================================================================
;;  Scheduling: a third instantiation of resurface.el's day-picker
;;
;; IR has no confidence engine (there is no pass/fail rating for
;; \"reading a bit of a book\"), only the domain-agnostic day-picker half
;; of the Scheduling Engine, driven by a continuous priority rather than
;; a discrete box number.

(defun resurface--ir-ideal-gap-days (priority)
  "Ideal days before a source at PRIORITY (0-100) becomes eligible again."
  (let ((frac (/ (float (- 100 (max 0 (min 100 priority)))) 100)))
    (max 1 (round (+ resurface-ir-min-gap-days
                      (* frac (- resurface-ir-max-gap-days resurface-ir-min-gap-days)))))))

(defun resurface--ir-day-loads (exclude-path)
  "Day-loads across every active source's :next-eligible, excluding EXCLUDE-PATH."
  (resurface--day-loads
   (mapcar (lambda (s) (cons (cdr (assq :path s)) s)) (resurface--ir-all-sources))
   (lambda (s) (cdr (assq :next-eligible s)))
   (lambda (pair) (or (equal (car pair) exclude-path)
                       (cdr (assq :finished (cdr pair)))))))

(defun resurface--ir-choose-next-eligible (priority exclude-path)
  "Pick the next-eligible ts for a source at PRIORITY, workload-balanced."
  (resurface--choose-next-review
   (resurface--ir-ideal-gap-days priority)
   resurface-ir-interval-tolerance
   resurface-ir-workload-weight
   (resurface--ir-day-loads exclude-path)))


;; ===========================================================================
;;  Source Lifecycle

(defun resurface--ir-source-finished-p (source)
  "Non-nil when SOURCE has been marked finished."
  (cdr (assq :finished source)))

(defun resurface--ir-source-due-p (source)
  "Non-nil when SOURCE is due: not finished, and its next-eligible date has passed."
  (resurface--item-due-p (resurface--ir-source-finished-p source)
                          (cdr (assq :next-eligible source))))

(defun resurface--ir-source-days-until-due (source)
  "Days until SOURCE is next eligible, `resurface--item-days-until-due'."
  (resurface--item-days-until-due (cdr (assq :next-eligible source))))

(defun resurface--ir-touch-source (source outcome)
  "Update SOURCE for OUTCOME (`read' or `finished'), persist, return the new item."
  (let* ((path (cdr (assq :path source)))
         (now  (resurface--now))
         (new
          (pcase outcome
            ('finished (resurface--build-item source :finished now :last-read now))
            ('read     (resurface--build-item
                        source
                        :last-read now
                        :next-eligible (resurface--ir-choose-next-eligible
                                        (cdr (assq :priority source)) path))))))
    (resurface--ir-replace-source path new)
    (resurface--persist)
    (resurface--ir-maybe-refresh-dashboard)
    new))


;; ===========================================================================
;;  Index Accessors (flat list, keyed by :path)

(defun resurface--ir-all-sources ()
  "Return the full list of registered IR source item-alists."
  (resurface--ensure-data)
  (cdr (assq :ir-sources resurface--data)))

(defun resurface--ir-set-sources (sources)
  "Replace the stored source list with SOURCES (mutates in-place)."
  (setcdr (assq :ir-sources resurface--data) sources))

(defun resurface--ir-find-source (path)
  "Return the source with :path PATH, or nil."
  (seq-find (lambda (s) (equal (cdr (assq :path s)) path)) (resurface--ir-all-sources)))

(defun resurface--ir-replace-source (path new-source)
  "Replace the source with :path PATH with NEW-SOURCE."
  (resurface--ir-set-sources
   (mapcar (lambda (s) (if (equal (cdr (assq :path s)) path) new-source s))
           (resurface--ir-all-sources))))

(defun resurface--ir-prepend-source (source)
  "Add SOURCE to the front of the source list."
  (resurface--ir-set-sources (cons source (resurface--ir-all-sources))))

(defun resurface--ir-remove-source-internal (path)
  "Remove the source with :path PATH from the source list."
  (resurface--ir-set-sources
   (seq-remove (lambda (s) (equal (cdr (assq :path s)) path)) (resurface--ir-all-sources))))

(defun resurface--ir-group-names ()
  "All distinct Leitner group names among registered IR sources."
  (delete-dups (mapcar (lambda (s) (cdr (assq :group s))) (resurface--ir-all-sources))))


;; ===========================================================================
;;  Persistence dispatch
;;
;; Plugged into resurface.el core via the fboundp checks in
;; `resurface--ir-sources-json-sexp-for-save' and the :ir-sources branch
;; of `resurface--json-sexp->data' -- same passthrough pattern Drill mode
;; uses for :drill-blocks, see the comment above resurface--empty-data.

(defun resurface--ir-sources->json-sexp ()
  "Convert the live IR source list to a JSON-encodable sexp."
  (vconcat
   (mapcar
    (lambda (s)
      (list (cons 'path          (cdr (assq :path s)))
            (cons 'type          (symbol-name (cdr (assq :type s))))
            (cons 'title         (cdr (assq :title s)))
            (cons 'group         (cdr (assq :group s)))
            (cons 'priority      (cdr (assq :priority s)))
            (cons 'added         (cdr (assq :added s)))
            (cons 'last_read     (or (cdr (assq :last-read s)) 0))
            (cons 'next_eligible (or (cdr (assq :next-eligible s)) 0))
            (cons 'finished      (or (cdr (assq :finished s)) :json-false))))
    (resurface--ir-all-sources))))

(defun resurface--json-sexp->ir-sources (sexp)
  "Parse the \"ir_sources\" key of SEXP (from `json-read', string keys)."
  (let ((raw (cdr (assoc "ir_sources" sexp))))
    (mapcar
     (lambda (r)
       (let ((finished (cdr (assoc "finished" r))))
         (list (cons :path          (cdr (assoc "path" r)))
               (cons :type          (intern (or (cdr (assoc "type" r)) "file")))
               (cons :title         (cdr (assoc "title" r)))
               (cons :group         (cdr (assoc "group" r)))
               (cons :priority      (or (cdr (assoc "priority" r)) resurface-ir-default-priority))
               (cons :added         (cdr (assoc "added" r)))
               (cons :last-read     (or (cdr (assoc "last_read" r)) 0))
               (cons :next-eligible (or (cdr (assoc "next_eligible" r)) 0))
               (cons :finished      (if (or (null finished) (eq finished :json-false)) nil finished)))))
     (if (vectorp raw) (append raw nil) nil))))


;; ===========================================================================
;;  Adding / Removing Sources

(defun resurface--ir-detect-type (path-or-url)
  "Return `url' for PATH-OR-URL starting with a URL scheme, else `file'."
  (if (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*://" path-or-url) 'url 'file))

(defun resurface--ir-clamp-priority (n)
  "Clamp N to the 0-100 priority range."
  (max 0 (min 100 (round n))))

(defun resurface--ir-slugify (s)
  "Turn S into a filesystem-safe slug."
  (string-trim (downcase (replace-regexp-in-string "[^[:alnum:]]+" "-" s)) "-+" "-+"))

(defun resurface--ir-register-source (path type title group priority)
  "Create and store a fresh source item, creating its Leitner GROUP too."
  (resurface--leitner-get-or-create-group group)
  (resurface--ir-prepend-source
   (list (cons :path path) (cons :type type) (cons :title title) (cons :group group)
         (cons :priority (resurface--ir-clamp-priority priority))
         (cons :added (resurface--now))
         (cons :last-read 0) (cons :next-eligible 0) (cons :finished nil)))
  (resurface--persist))

;;;###autoload
(defun resurface-ir-add-source (&optional path priority title group)
  "Register PATH (a file path or URL) as a new incremental-reading source.
Bulk-adds marked files from a `dired' buffer."
  (interactive)
  (resurface--ensure-data)
  (if (and (called-interactively-p 'any) (derived-mode-p 'dired-mode))
      (let* ((files (seq-filter (lambda (f) (not (file-directory-p f))) (dired-get-marked-files)))
             (_ (unless files (user-error "Resurface-IR: no files marked in Dired")))
             (prio (resurface--ir-clamp-priority
                    (read-number "Priority for all marked (0-100): " resurface-ir-default-priority)))
             (added 0) (skipped 0))
        (dolist (f files)
          (let* ((abs (expand-file-name f))
                 (ttl (file-name-sans-extension (file-name-nondirectory abs))))
            (if (resurface--ir-find-source abs)
                (cl-incf skipped)
              (resurface--ir-register-source abs 'file ttl ttl prio)
              (cl-incf added))))
        (when (> added 0) (resurface--ir-maybe-refresh-dashboard))
        (message "Resurface-IR: added %d source(s)%s." added
                 (if (> skipped 0) (format ", skipped %d already registered" skipped) "")))
    (let* ((raw    (or path
                        (if (y-or-n-p "Is this a URL (rather than a local file)? ")
                            (read-string "Source URL: ")
                          (read-file-name "Source file (TAB to browse/complete): "))))
           (type   (resurface--ir-detect-type raw))
           (norm   (if (eq type 'file) (expand-file-name raw) raw))
           (dtitle (if (eq type 'file) (file-name-sans-extension (file-name-nondirectory norm)) norm))
           (ttl    (or title (read-string (format "Title (default %s): " dtitle) nil nil dtitle)))
           (grp    (or group (read-string (format "Leitner group for its cards (default %s): " ttl)
                                           nil nil ttl)))
           (prio   (resurface--ir-clamp-priority
                    (or priority (read-number "Priority (0-100): " resurface-ir-default-priority)))))
      (when (and (eq type 'file) (not (file-exists-p norm))
                 (called-interactively-p 'any)
                 (not (yes-or-no-p (format "'%s' does not exist yet, add it anyway? " norm))))
        (user-error "Resurface-IR: aborted"))
      (if (resurface--ir-find-source norm)
          (message "Resurface-IR: '%s' is already registered." norm)
        (resurface--ir-register-source norm type ttl grp prio)
        (message "Resurface-IR: added '%s' (priority %d) -> group '%s'." ttl prio grp)
        (resurface--ir-maybe-refresh-dashboard)))))

;;;###autoload
(defun resurface-ir-remove-source (&optional path)
  "Remove PATH from the IR index.  Its Leitner group and cards are untouched."
  (interactive)
  (resurface--ensure-data)
  (let* ((path (or path (read-string "Source path/URL to remove: ")))
         (abs  (if (and (eq (resurface--ir-detect-type path) 'file) (file-exists-p path))
                   (expand-file-name path)
                 path)))
    (resurface--ir-remove-source-internal abs)
    (resurface--persist)
    (message "Resurface-IR: removed '%s'." abs)
    (resurface--ir-maybe-refresh-dashboard)))


;; ===========================================================================
;;  Opening a Source & the Reading Minor Mode

(defun resurface--ir-open-externally (path)
  "Open PATH with the OS default application, asynchronously."
  (let ((path (expand-file-name path)))
    (unless (file-exists-p path)
      (user-error "Resurface-IR: file not found: %s" path))
    (when (zerop (file-attribute-size (file-attributes path)))
      (user-error
       "Resurface-IR: '%s' is 0 bytes on disk, if it's on iCloud Drive it may just be an undownloaded placeholder, try `brctl download %s' or File > Download Now in Finder"
       (file-name-nondirectory path) (shell-quote-argument path)))
    (if (memq system-type '(ms-dos windows-nt cygwin))
        (w32-shell-execute "open" path)
      (unless (executable-find resurface-ir-external-open-command)
        (user-error "Resurface-IR: '%s' not found, customize `resurface-ir-external-open-command'"
                    resurface-ir-external-open-command))
      (start-process "resurface-ir-external-open" nil
                      resurface-ir-external-open-command path))))

(defun resurface--ir-companion-buffer-name (source)
  "Name of the companion buffer standing in for SOURCE's own buffer."
  (format "*Resurface-IR: %s*" (cdr (assq :title source))))

(defun resurface--ir-open-companion-buffer (source)
  "Create/switch to a companion buffer standing in for SOURCE's own buffer."
  (let ((buf (get-buffer-create (resurface--ir-companion-buffer-name source))))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (unless (derived-mode-p 'org-mode) (org-mode))
      (insert (format "#+TITLE: %s\n\n" (cdr (assq :title source))))
      (insert (format "%s is open in your system's default application.\n\n"
                       (file-name-nondirectory (cdr (assq :path source)))))
      (insert "Select a passage there, copy it, then come back here and:\n\n")
      (insert "  C-c i c   capture (C-y to paste once inside)\n")
      (insert "  C-c i u   update an existing card under this source\n")
      (insert "  C-c i d   done for now\n")
      (insert "  C-c i f   mark finished\n")
      (insert "  C-c i s   skip\n")
      (insert "  C-c i q   quit session\n")
      (goto-char (point-min))
      (read-only-mode 1))
    (switch-to-buffer buf)))

(defun resurface--ir-default-open (source)
  "Default `resurface-ir-open-function': URLs to `eww', files to OS app or `find-file'."
  (pcase (cdr (assq :type source))
    ('url (eww (cdr (assq :path source))))
    (_ (let ((path (cdr (assq :path source))))
         (if (member (downcase (or (file-name-extension path) ""))
                     resurface-ir-external-extensions)
             (progn (resurface--ir-open-externally path)
                    (resurface--ir-open-companion-buffer source))
           (find-file path))))))

(defun resurface--ir-open-and-track (source)
  "Open SOURCE via `resurface-ir-open-function' and enable the reading minor mode."
  (funcall resurface-ir-open-function source)
  (setq-local resurface--ir-review-source source)
  (resurface-ir-reading-minor-mode 1)
  (run-hooks 'resurface-ir-before-read-hook))

(defvar resurface-ir-reading-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i c") #'resurface-ir-capture)
    (define-key map (kbd "C-c i u") #'resurface-ir-capture-update)
    (define-key map (kbd "C-c i d") #'resurface-ir-mark-read)
    (define-key map (kbd "C-c i f") #'resurface-ir-mark-finished)
    (define-key map (kbd "C-c i s") #'resurface-ir-skip)
    (define-key map (kbd "C-c i q") #'resurface-ir-quit-session)
    (define-key map (kbd "C-c i ?") #'resurface-ir-reading-help)
    map)
  "Keymap active while a source is open for incremental reading.")

(define-minor-mode resurface-ir-reading-minor-mode
  "Active while a source is open for incremental reading."
  :lighter " Resurface-IR"
  :keymap resurface-ir-reading-minor-mode-map
  (if resurface-ir-reading-minor-mode
      (setq header-line-format (resurface--ir-build-reading-header))
    (kill-local-variable 'header-line-format)))

(defun resurface--ir-build-reading-header ()
  "Construct the header-line string for the source currently being read."
  (when resurface--ir-review-source
    (let* ((title (cdr (assq :title resurface--ir-review-source)))
           (prio  (cdr (assq :priority resurface--ir-review-source)))
           (group (cdr (assq :group resurface--ir-review-source)))
           (pos   (when resurface--ir-session
                    (format " %d/%d" (1+ (cdr (assq :reviewed resurface--ir-session)))
                            (cdr (assq :total resurface--ir-session))))))
      (concat
       (propertize (format " IR%s " (or pos "")) 'face '(:weight bold))
       (propertize (format "  %s" title) 'face 'mode-line)
       (propertize (format "  priority %d -> %s" prio group) 'face '(:slant italic))
       (propertize
        "    C-c i c capture   C-c i u update-existing   C-c i d done-for-now   C-c i f finished   C-c i s skip   C-c i q quit"
        'face '(:inherit shadow))))))

(defun resurface--ir-advance-or-stop ()
  "If a session is running, pop its queue and continue, else just stop here."
  (when resurface--ir-session
    (let ((queue (cdr (assq :queue resurface--ir-session))))
      (cl-incf (cdr (assq :reviewed resurface--ir-session)))
      (setcdr (assq :queue resurface--ir-session) (cdr queue))
      (resurface--ir-session-advance))))

;;;###autoload
(defun resurface-ir-mark-read ()
  "Record today's reading of the current source and move on."
  (interactive)
  (unless resurface--ir-review-source
    (user-error "Resurface-IR: no source is being tracked in this buffer"))
  (let ((title (cdr (assq :title resurface--ir-review-source))))
    (resurface--ir-touch-source resurface--ir-review-source 'read)
    (resurface-ir-reading-minor-mode -1)
    (message "Resurface-IR: '%s' -- next appearance rescheduled." title))
  (resurface--ir-advance-or-stop))

;;;###autoload
(defun resurface-ir-mark-finished ()
  "Mark the current source as finished, it drops out of the reading rotation.
Use `resurface-ir-review-finished' to bring it back later."
  (interactive)
  (unless resurface--ir-review-source
    (user-error "Resurface-IR: no source is being tracked in this buffer"))
  (when (yes-or-no-p (format "Mark '%s' finished? "
                              (cdr (assq :title resurface--ir-review-source))))
    (let ((title (cdr (assq :title resurface--ir-review-source))))
      (resurface--ir-touch-source resurface--ir-review-source 'finished)
      (resurface-ir-reading-minor-mode -1)
      (message "Resurface-IR: '%s' finished." title))
    (resurface--ir-advance-or-stop)))

;;;###autoload
(defun resurface-ir-skip ()
  "Move on without updating this source's read time or schedule."
  (interactive)
  (unless resurface--ir-review-source
    (user-error "Resurface-IR: no source is being tracked in this buffer"))
  (resurface-ir-reading-minor-mode -1)
  (resurface--ir-advance-or-stop))

;;;###autoload
(defun resurface-ir-quit-session ()
  "End the current IR session early.  Progress so far is saved."
  (interactive)
  (when (yes-or-no-p "Quit this Resurface-IR session (progress so far is saved)? ")
    (when resurface-ir-reading-minor-mode
      (resurface-ir-reading-minor-mode -1))
    (setq resurface--ir-session nil)
    (resurface-save)
    (resurface-ir)
    (message "Resurface-IR: session ended.  Index saved.")))

(defun resurface-ir-reading-help ()
  "Echo reading-minor-mode keybindings in the echo area."
  (interactive)
  (message
   "Resurface-IR: C-c i c capture   C-c i u update-existing   C-c i d done-for-now   C-c i f finished   C-c i s skip   C-c i q quit"))


;; ===========================================================================
;;  Session: build today's queue from the priority list

(defun resurface--ir-due-sources ()
  "All currently-due, unfinished source item-alists."
  (seq-filter #'resurface--ir-source-due-p (resurface--ir-all-sources)))

(defun resurface--ir-build-queue (due)
  "Order DUE sources: shuffle first for noise, then stable-sort by priority."
  (sort (resurface--shuffle due)
        (lambda (a b) (> (cdr (assq :priority a)) (cdr (assq :priority b))))))

;;;###autoload
(defun resurface-ir-start-session ()
  "Start an incremental-reading session: queue today's due sources by priority."
  (interactive)
  (resurface--ensure-data)
  (when (and resurface--ir-session
             (not (yes-or-no-p "An IR session is already running.  Start a new one? ")))
    (user-error "Session aborted"))
  (let* ((due       (resurface--ir-due-sources))
         (due-count (length due))
         (capped    (and (integerp resurface-ir-session-max-items)
                          (> resurface-ir-session-max-items 0)
                          (> due-count resurface-ir-session-max-items)))
         (queue     (resurface--ir-build-queue due))
         (queue     (if capped (seq-take queue resurface-ir-session-max-items) queue)))
    (if (null queue)
        (message "Resurface-IR: nothing due, nicely caught up.")
      (setq resurface--ir-session
            (list (cons :queue queue) (cons :reviewed 0) (cons :total (length queue))
                  (cons :capped capped)))
      (if capped
          (message "Resurface-IR: %d source(s) due, queuing today's top %d by priority.  Starting..."
                   due-count (length queue))
        (message "Resurface-IR: %d source(s) due.  Starting..." (length queue)))
      (resurface--ir-session-advance))))

(defun resurface--ir-session-advance ()
  "Open the next queued source, or finish the session."
  (let ((queue (cdr (assq :queue resurface--ir-session))))
    (if (null queue)
        (resurface--ir-session-finish)
      (let* ((source (car queue))
             (path   (cdr (assq :path source))))
        (if (and (eq (cdr (assq :type source)) 'file) (not (file-exists-p path)))
            (progn
              (message "Resurface-IR: file missing, skipping, %s" (file-name-nondirectory path))
              (setcdr (assq :queue resurface--ir-session) (cdr queue))
              (resurface--ir-session-advance))
          (resurface--ir-open-and-track source))))))

(defun resurface--ir-session-finish ()
  "Clean up and save after every queued source has been visited."
  (let* ((n         (cdr (assq :reviewed resurface--ir-session)))
         (capped    (cdr (assq :capped resurface--ir-session)))
         (remaining (and capped (length (resurface--ir-due-sources)))))
    (setq resurface--ir-session nil)
    (resurface-save)
    (resurface-ir)
    (if (and capped (> remaining 0))
        (message
         "Resurface-IR: session complete, %d source(s) touched.  %d more due, run `resurface-ir-start-session' again when ready.  Index saved."
         n remaining)
      (message "Resurface-IR: session complete, %d source(s) touched.  Index saved." n))))


;; ===========================================================================
;;  Capture: selection -> reworded card -> filed as a Leitner item

(defconst resurface--ir-capture-buf "*Resurface-IR: Capture*"
  "Name of the scratch buffer used to reword a capture into a card.")

(define-derived-mode resurface-ir-capture-mode org-mode "Resurface-IR-Capture"
  "Major mode for rewording a captured passage into your own words.
`C-c C-c' files/saves it, `C-c C-k' discards it."
  :interactive nil)

(define-key resurface-ir-capture-mode-map (kbd "C-c C-c") #'resurface-ir-capture-submit)
(define-key resurface-ir-capture-mode-map (kbd "C-c C-k") #'resurface-ir-capture-discard)

;;;###autoload
(defun resurface-ir-capture ()
  "Turn the active region into a Resurface-IR card.
`C-c C-c' files it as a fresh Leitner item, `C-c C-k' discards it."
  (interactive)
  (unless resurface--ir-review-source
    (user-error
     "Resurface-IR: no source is being tracked in this buffer -- open one via `resurface-ir-start-session' or the dashboard first"))
  (let* ((source    resurface--ir-review-source)
         (had-region (use-region-p))
         (text      (when had-region (buffer-substring-no-properties (region-beginning) (region-end))))
         (return-wc (current-window-configuration))
         (buf       (get-buffer-create resurface--ir-capture-buf)))
    (when had-region (deactivate-mark))
    (with-current-buffer buf
      (erase-buffer)
      (resurface-ir-capture-mode)
      (setq resurface--ir-capture-source source
            resurface--ir-capture-return return-wc)
      (when text (insert (string-trim text)))
      (setq header-line-format
            (propertize
             (format "  Reword into your own words, filed under '%s' -> group '%s'   |   C-c C-c submit   C-c C-k discard"
                     (cdr (assq :title source)) (cdr (assq :group source)))
             'face '(:inherit shadow :slant italic)))
      (goto-char (point-max)))
    (pop-to-buffer buf)
    (unless had-region
      (message "Resurface-IR: no active selection, type or yank the passage, then reword it."))))

(defun resurface--ir-card-label (path)
  "Human-readable label for the card file at PATH, for a completion prompt."
  (let ((prompt (resurface--leitner-extract-prompt path))
        (fname  (file-name-nondirectory path)))
    (if prompt (format "%s  (%s)" prompt fname) fname)))

;;;###autoload
(defun resurface-ir-capture-update ()
  "Edit an existing card from the current source's Leitner group.
`C-c C-c' overwrites the file and rates the review `revised' (box unchanged)."
  (interactive)
  (unless resurface--ir-review-source
    (user-error
     "Resurface-IR: no source is being tracked in this buffer, open one via `resurface-ir-start-session' or the dashboard first"))
  (let* ((source resurface--ir-review-source)
         (group  (cdr (assq :group source)))
         (items  (resurface--leitner-group-items group)))
    (unless items
      (user-error "Resurface-IR: group '%s' has no cards yet, nothing to update" group))
    (let* ((choices (mapcar (lambda (it)
                              (let ((path (cdr (assq :path it))))
                                (cons (resurface--ir-card-label path) path)))
                            items))
           (pick    (completing-read (format "Update which card in '%s': " group)
                                      choices nil t))
           (path    (cdr (assoc pick choices)))
           (return-wc (current-window-configuration))
           (buf     (get-buffer-create resurface--ir-capture-buf)))
      (unless (and path (file-exists-p path))
        (user-error "Resurface-IR: '%s' no longer exists on disk, try `resurface-leitner-gv-remove-file' to clean up the index"
                    (file-name-nondirectory (or path ""))))
      (with-current-buffer buf
        (erase-buffer)
        (resurface-ir-capture-mode)
        (insert-file-contents path)
        (setq resurface--ir-capture-source source
              resurface--ir-capture-return return-wc
              resurface--ir-capture-update-path path)
        (setq header-line-format
              (propertize
               (format "  Editing '%s', updates card in group '%s'   |   C-c C-c save   C-c C-k discard"
                       (file-name-nondirectory path) group)
               'face '(:inherit shadow :slant italic)))
        (goto-char (point-max)))
      (pop-to-buffer buf))))

(defun resurface--ir-capture-return-to-reading (wc)
  "Restore window configuration WC, saved when a capture buffer opened."
  (if (window-configuration-p wc)
      (set-window-configuration wc)
    (message "Resurface-IR: couldn't restore the previous window layout.")))

(defun resurface--ir-write-card-file (source title body)
  "Write a new org card for TITLE/BODY under SOURCE, return its path."
  (let* ((dir   (expand-file-name resurface-ir-capture-directory))
         (fname (format "%s--%s--%s.org"
                        (resurface--ir-slugify (cdr (assq :title source)))
                        (resurface--ir-slugify title)
                        (format-time-string "%Y%m%d%H%M%S")))
         (path  (expand-file-name fname dir)))
    (make-directory dir t)
    (with-temp-file path
      (insert (format "#+TITLE: %s\n\n" title))
      (insert body)
      (insert "\n"))
    path))

(defun resurface--ir-capture-submit-new (source body)
  "File BODY as a brand-new Leitner card under SOURCE's group."
  (let* ((first-line    (car (split-string body "\n" t)))
         (default-title (truncate-string-to-width (string-trim first-line) 60 nil nil t))
         (title         (string-trim
                          (read-string (format "Card title (default: %s): " default-title)
                                       nil nil default-title)))
         (group         (cdr (assq :group source)))
         (path          (resurface--ir-write-card-file source title body)))
    (resurface-leitner-add-file path group)
    (message "Resurface-IR: card '%s' filed under group '%s' (Box 1)." title group)))

(defun resurface--ir-capture-submit-update (source path body)
  "Overwrite the existing card at PATH with BODY, rating the review `revised'."
  (let* ((group (cdr (assq :group source)))
         (item  (resurface--leitner-find-item group path)))
    (unless item
      (user-error "Resurface-IR: '%s' is no longer in group '%s' (removed since you picked it?)"
                  (file-name-nondirectory path) group))
    (with-temp-file path
      (insert body)
      (insert "\n"))
    (resurface--leitner-replace-item group path (resurface--leitner-item-rate item 'revised))
    (resurface--persist)
    (resurface--leitner-maybe-refresh-dashboard)
    (message "Resurface-IR: '%s' updated in group '%s' (Revised, box unchanged)."
             (file-name-nondirectory path) group)))

;;;###autoload
(defun resurface-ir-capture-submit ()
  "Accept the current capture buffer, filing a new card or updating one."
  (interactive)
  (unless (derived-mode-p 'resurface-ir-capture-mode)
    (user-error "Resurface-IR: not in a capture buffer"))
  (let* ((source      resurface--ir-capture-source)
         (return      resurface--ir-capture-return)
         (update-path resurface--ir-capture-update-path)
         (body        (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-empty-p body)
      (user-error "Resurface-IR: card is empty, nothing to submit (C-c C-k to discard instead)"))
    (if update-path
        (resurface--ir-capture-submit-update source update-path body)
      (resurface--ir-capture-submit-new source body))
    (run-hooks 'resurface-ir-after-capture-hook)
    (kill-buffer (current-buffer))
    (resurface--ir-capture-return-to-reading return)))

;;;###autoload
(defun resurface-ir-capture-discard ()
  "Discard the current capture buffer without creating or changing a card."
  (interactive)
  (unless (derived-mode-p 'resurface-ir-capture-mode)
    (user-error "Resurface-IR: not in a capture buffer"))
  (let ((return      resurface--ir-capture-return)
        (update-path resurface--ir-capture-update-path))
    (kill-buffer (current-buffer))
    (resurface--ir-capture-return-to-reading return)
    (if update-path
        (message "Resurface-IR: update to '%s' discarded, card unchanged."
                  (file-name-nondirectory update-path))
      (message "Resurface-IR: capture discarded."))))


;; ===========================================================================
;;  Dashboard, main entry point showing every registered source

(defun resurface--ir-menu-format ()
  "Column format vector for the IR dashboard."
  (vector '("Title" 28 t) '("Type" 5 t) '("Pri" 4 t) '("Group" 18 t)
          '("Cards" 6 t) '("Last read" 12 t) '("Status" 12 t)))

(defun resurface--ir-menu-entries ()
  "Tabulated-list entries for the IR dashboard."
  (resurface--ensure-data)
  (mapcar
   (lambda (s)
     (let* ((path     (cdr (assq :path s)))
            (finished (resurface--ir-source-finished-p s))
            (due-p    (resurface--ir-source-due-p s))
            (days     (resurface--ir-source-days-until-due s))
            (cards    (length (resurface--leitner-group-items (cdr (assq :group s)))))
            (status   (cond (finished (propertize "Finished" 'face 'success))
                             (due-p    (propertize "Due"      'face 'warning))
                             (t        (format "in %.0fd" days)))))
       (list path
             (vector (cdr (assq :title s))
                     (symbol-name (cdr (assq :type s)))
                     (number-to-string (cdr (assq :priority s)))
                     (cdr (assq :group s))
                     (number-to-string cards)
                     (resurface--format-ts (cdr (assq :last-read s)))
                     status))))
   (resurface--ir-all-sources)))

(resurface--define-list-mode
 :mode              resurface-ir-menu-mode
 :mode-map          resurface-ir-menu-mode-map
 :mode-lighter      "Resurface-IR"
 :column-format-fn  #'resurface--ir-menu-format
 :entries           #'resurface--ir-menu-entries
 :header-text       "  Incremental Reading: press ? for keybindings"
 :dynamic-columns   t
 :help-command      resurface-ir-menu-help
 :keys (("RET" resurface-ir-menu-open          "RET open")
        ("s"   resurface-ir-start-session      "s session")
        ("a"   resurface-ir-add-source         "a add")
        ("F"   resurface-ir-review-finished    "F finished")
        ("+"   resurface-ir-menu-bump-up       "+ priority")
        ("-"   resurface-ir-menu-bump-down     "- priority")
        ("f"   resurface-ir-menu-mark-finished "f mark-finished")
        ("x"   resurface-ir-menu-remove        "x remove")
        ("S"   resurface-save                  "S save")
        ("g"   revert-buffer                   "g refresh")
        ("?"   resurface-ir-menu-help          nil)))

;;;###autoload
(defun resurface-ir ()
  "Open the Incremental Reading dashboard."
  (interactive)
  (resurface--ensure-data)
  (resurface--open-list-buffer "*Resurface: IR*" #'resurface-ir-menu-mode))

(defun resurface-ir-menu-open ()
  "Open the source on this dashboard line for reading, ad hoc (no session)."
  (interactive)
  (let* ((path (tabulated-list-get-id))
         (src  (and path (resurface--ir-find-source path))))
    (when src (resurface--ir-open-and-track src))))

(defun resurface--ir-adjust-priority (path delta)
  "Adjust the priority of the source at PATH by DELTA, clamped to 0-100."
  (let ((src (resurface--ir-find-source path)))
    (when src
      (resurface--ir-replace-source
       path (resurface--build-item
             src :priority (resurface--ir-clamp-priority (+ delta (cdr (assq :priority src))))))
      (resurface--persist))))

(defun resurface-ir-menu-bump-up ()
  "Raise the priority of the source on this dashboard line by 5."
  (interactive)
  (let ((path (tabulated-list-get-id)))
    (when path (resurface--ir-adjust-priority path 5) (tabulated-list-print t))))

(defun resurface-ir-menu-bump-down ()
  "Lower the priority of the source on this dashboard line by 5."
  (interactive)
  (let ((path (tabulated-list-get-id)))
    (when path (resurface--ir-adjust-priority path -5) (tabulated-list-print t))))

(defun resurface-ir-menu-mark-finished ()
  "Mark the source on this dashboard line as finished."
  (interactive)
  (let* ((path (tabulated-list-get-id))
         (src  (and path (resurface--ir-find-source path))))
    (when (and src (yes-or-no-p (format "Mark '%s' finished? " (cdr (assq :title src)))))
      (resurface--ir-touch-source src 'finished)
      (tabulated-list-print t)
      (message "Resurface-IR: '%s' finished." (cdr (assq :title src))))))

(defun resurface-ir-menu-remove ()
  "Remove the source on this dashboard line from the IR index."
  (interactive)
  (let* ((path (tabulated-list-get-id))
         (src  (and path (resurface--ir-find-source path))))
    (when (and src
               (yes-or-no-p (format "Remove '%s' from Resurface-IR? (Leitner group/cards untouched) "
                                     (cdr (assq :title src)))))
      (resurface--ir-remove-source-internal path)
      (resurface--persist)
      (tabulated-list-print t)
      (message "Resurface-IR: removed '%s'." (cdr (assq :title src))))))

(defun resurface--ir-maybe-refresh-dashboard ()
  "Silently refresh the IR dashboard buffer if it is alive."
  (resurface--refresh-buffer-if-live "*Resurface: IR*"))


;; ===========================================================================
;;  Finished Browser
;;
;; An instantiation of `resurface--define-retirement-browser' (see the
;; sibling Leitner-graduated and Drill-retired browsers), grouped by
;; each source's Leitner group.

(defun resurface--ir-finished-pairs (&optional group-name)
  "Finished (group . source) pairs, optionally filtered to GROUP-NAME."
  (seq-filter
   (lambda (pair) (or (null group-name) (equal (car pair) group-name)))
   (mapcar (lambda (s) (cons (cdr (assq :group s)) s))
           (seq-filter #'resurface--ir-source-finished-p (resurface--ir-all-sources)))))

(defun resurface--ir-finished-find (_group-name path) (resurface--ir-find-source path))
(defun resurface--ir-finished-replace (_group-name path new) (resurface--ir-replace-source path new))
(defun resurface--ir-finished-bring-back (src) (resurface--build-item src :finished nil :next-eligible 0))
(defun resurface--ir-finished-buffer-title (_name) "*Resurface: IR Finished*")
(defun resurface--ir-finished-success-message (src)
  (format "Resurface-IR: '%s' is back in the reading rotation." (cdr (assq :title src))))
(defun resurface--ir-finished-open (_group-name path)
  (let ((src (resurface--ir-find-source path))) (when src (resurface--ir-open-and-track src))))

(resurface--define-retirement-browser
 :mode                 resurface-ir-finished-mode
 :mode-map             resurface-ir-finished-mode-map
 :mode-lighter         "Resurface-IR-Finished"
 :filter-var           resurface--ir-finished-filter
 :browser-command      resurface-ir-review-finished
 :bring-back-command   resurface-ir-reactivate
 :help-command         resurface-ir-finished-help
 :help-prefix          "IR finished"
 :row-open-command     resurface-ir-finished-open-cmd
 :row-open-fn          #'resurface--ir-finished-open
 :buffer-title-fn      #'resurface--ir-finished-buffer-title
 :collection-label     "Group"
 :item-label           "Source"
 :state-label          "Finished"
 :noun                 "source"
 :state-verb           "finished"
 :pairs-fn             #'resurface--ir-finished-pairs
 :collection-names-fn  #'resurface--ir-group-names
 :collection-name-fn   #'identity
 :item-display-fn      (lambda (s) (cdr (assq :title s)))
 :item-key-fn          (lambda (s) (cdr (assq :path s)))
 :timestamp-fn         (lambda (s) (cdr (assq :finished s)))
 :find-item-fn         #'resurface--ir-finished-find
 :replace-item-fn      #'resurface--ir-finished-replace
 :bring-back-fn        #'resurface--ir-finished-bring-back
 :success-message-fn   #'resurface--ir-finished-success-message
 :refresh-dashboard-fn #'resurface--ir-maybe-refresh-dashboard)


;; ===========================================================================
;;  Evil-mode Compatibility
;;
;; Same reasoning and same shared helper as resurface.el's own block; see
;; the comment there.

(with-eval-after-load 'evil
  (resurface--evil-tabulated-compat
   '(resurface-ir-menu-mode resurface-ir-finished-mode)
   (list resurface-ir-menu-mode-map resurface-ir-finished-mode-map))
  (evil-make-overriding-map resurface-ir-reading-minor-mode-map 'normal)
  (add-hook 'resurface-ir-reading-minor-mode-hook #'evil-normalize-keymaps))


(provide 'resurface-ir)
;;; resurface-ir.el ends here
