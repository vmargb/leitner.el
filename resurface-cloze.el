;;; resurface-cloze.el --- Cloze-deletion overlays for resurface.el  -*- lexical-binding: t; -*-
;; Author: vmargb
;; Version: 0.2.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: notes, spaced-repetition, org, feynman
;; URL: https://github.com/vmargb/resurface.el

;;; Commentary:
;;
;; resurface-cloze.el is an optional companion to resurface.el.  It marks
;; spans of text as CLOZES, e.g. "The capital of France is {{Paris}}."
;; and hides the marked content behind a placeholder until point moves
;; into it, at which point the raw text is revealed again.  Leaving the
;; span hides it once more.
;;
;; This is a display-layer feature only, the buffers actual contents are
;; never modified by hiding or revealing, only by the explicit
;; `resurface-cloze-dwim' / `resurface-cloze-remove-at-point' commands
;; Nothing here talks to `resurface--data' or the JSON index.
;;
;; Load automatically alongside Leitner via:
;;   (require 'resurface-cloze)
;;
;; Quick start:
;;   M-x resurface-cloze-mode              Toggle hiding/revealing in this buffer
;;   M-x resurface-cloze-dwim              Wrap region (or point) in a cloze
;;   M-x resurface-cloze-remove-at-point   Strip the cloze around point
;;   M-x resurface-cloze-hide-all          Force every cloze in the buffer shut
;;   M-x resurface-cloze-reveal-all        Force every cloze in the buffer open
;;
;;; Code:

(require 'cl-lib)
(require 'seq)


;; =========================================================
;;  Customisation

(defgroup resurface-cloze nil
  "Cloze-deletion overlays, hide text until point enters it."
  :group 'resurface
  :prefix "resurface-cloze-")

(defcustom resurface-cloze-open "{{"
  "Opening delimiter marking the start of a cloze."
  :type 'string
  :group 'resurface-cloze)

(defcustom resurface-cloze-close "}}"
  "Closing delimiter marking the end of a cloze."
  :type 'string
  :group 'resurface-cloze)

(defcustom resurface-cloze-style 'mask
  "How hidden cloze text is displayed.
`mask' replaces the hidden content with `resurface-cloze-mask-char',
repeated to roughly match its length, so you can still see how much
text is there.  `fixed' always shows `resurface-cloze-placeholder',
regardless of the hidden content's length."
  :type '(choice (const :tag "Mask, matching length" mask)
                  (const :tag "Fixed placeholder" fixed))
  :group 'resurface-cloze)

(defcustom resurface-cloze-mask-char ?█
  "Character used to mask hidden text when `resurface-cloze-style' is `mask'."
  :type 'character
  :group 'resurface-cloze)

(defcustom resurface-cloze-placeholder "[...]"
  "Placeholder shown for hidden clozes when `resurface-cloze-style' is `fixed'."
  :type 'string
  :group 'resurface-cloze)

(defcustom resurface-cloze-hide-delimiters t
  "Whether `resurface-cloze-open'/-close' are hidden along with the content."
  :type 'boolean
  :group 'resurface-cloze)


;; ===========================================================================
;;  Parsing

(defun resurface--cloze-regexp ()
  "Return a regexp matching one cloze, with group 1 as its inner text.
Restricted to a single line, clozes spanning a paragraph break not supported."
  (concat (regexp-quote resurface-cloze-open)
          "\\(.*?\\)"
          (regexp-quote resurface-cloze-close)))

(defun resurface--cloze-build-placeholder (inner-text)
  "Return the display string used to mask INNER-TEXT, per `resurface-cloze-style'."
  (pcase resurface-cloze-style
    ('mask  (make-string (max 1 (length inner-text)) resurface-cloze-mask-char))
    (_      resurface-cloze-placeholder)))

(defun resurface--cloze-make-overlay (beg end inner-text)
  "Create a hidden cloze overlay from BEG to END, masking INNER-TEXT."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'resurface-cloze t)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'resurface-cloze-placeholder
                 (resurface--cloze-build-placeholder inner-text))
    (overlay-put ov 'display (overlay-get ov 'resurface-cloze-placeholder))
    ov))

(defun resurface--cloze-clear-overlays (beg end)
  "Remove all resurface-cloze overlays touching BEG..END."
  (remove-overlays beg end 'resurface-cloze t))

(defun resurface--cloze-scan-region (beg end)
  "Create cloze overlays for every match of `resurface--cloze-regexp' in BEG..END."
  (save-excursion
    (save-match-data
      (goto-char beg)
      (let ((re (resurface--cloze-regexp))
            ;; re-search-forward needs a fixed bound capture it once so
            ;; edits made while scanning can't move it
            (bound (max end beg)))
        (while (re-search-forward re bound t)
          (let* ((full-beg  (match-beginning 0))
                 (full-end  (match-end 0))
                 (inner-beg (match-beginning 1))
                 (inner-end (match-end 1))
                 (inner     (match-string-no-properties 1)))
            (resurface--cloze-make-overlay
             (if resurface-cloze-hide-delimiters full-beg inner-beg)
             (if resurface-cloze-hide-delimiters full-end inner-end)
             inner)))))))

(defun resurface--cloze-rescan-lines (beg end)
  "Clear and rebuild cloze overlays for every line touched by BEG..END."
  (let ((b (save-excursion (goto-char beg) (line-beginning-position)))
        (e (save-excursion (goto-char end) (line-end-position))))
    (resurface--cloze-clear-overlays b e)
    (resurface--cloze-scan-region b e)))

(defun resurface--cloze-after-change (beg end _len)
  "`after-change-functions' member, reparse the lines touched by an edit."
  (resurface--cloze-rescan-lines beg end))

(defun resurface--cloze-overlays ()
  "Return every resurface-cloze overlay in the current buffer."
  (seq-filter (lambda (ov) (overlay-get ov 'resurface-cloze))
              (overlays-in (point-min) (point-max))))


;; ===========================================================================
;;  Reveal engine
;;
;; Every command checks whether point sits inside a cloze overlay.  If it
;; does and that overlay isn't already the revealed one, the previously
;; revealed overlay (if any) is hidden again and this one is revealed.  If
;; point isn't in any cloze, whatever was revealed goes back to hidden.

(defvar-local resurface--cloze-revealed nil
  "The currently revealed cloze overlay in this buffer, or nil.")

(defun resurface--cloze-overlay-at-point ()
  "Return the cloze overlay at point, or nil."
  (cl-find-if (lambda (ov) (overlay-get ov 'resurface-cloze))
              (overlays-at (point))))

(defun resurface--cloze-hide (ov)
  "Put OV back into its hidden, masked state."
  (when (overlay-buffer ov)
    (overlay-put ov 'display (overlay-get ov 'resurface-cloze-placeholder))))

(defun resurface--cloze-reveal (ov)
  "Show OV's raw underlying text instead of its placeholder."
  (overlay-put ov 'display nil))

(defun resurface--cloze-post-command ()
  "`post-command-hook' member: reveal the cloze at point, hide the rest."
  (let ((ov (resurface--cloze-overlay-at-point)))
    (unless (eq ov resurface--cloze-revealed)
      (when resurface--cloze-revealed
        (resurface--cloze-hide resurface--cloze-revealed))
      (when ov
        (resurface--cloze-reveal ov))
      (setq resurface--cloze-revealed ov))))


;; ===========================================================================
;;  Minor mode

;;;###autoload
(define-minor-mode resurface-cloze-mode
  "Hide cloze-deleted text until point moves into it.
Hidden clozes show a placeholder controlled by `resurface-cloze-style';
moving point into one reveals its real text, moving out hides it again."
  :lighter " Cloze"
  :group 'resurface-cloze
  (if resurface-cloze-mode
      (progn
        (resurface--cloze-scan-region (point-min) (point-max))
        (add-hook 'post-command-hook #'resurface--cloze-post-command nil t)
        (add-hook 'after-change-functions #'resurface--cloze-after-change nil t))
    (resurface--cloze-clear-overlays (point-min) (point-max))
    (setq resurface--cloze-revealed nil)
    (remove-hook 'post-command-hook #'resurface--cloze-post-command t)
    (remove-hook 'after-change-functions #'resurface--cloze-after-change t)))


;; ===========================================================================
;;  Authoring commands

;;;###autoload
(defun resurface-cloze-dwim (beg end)
  "Turn the active region into a cloze, or insert an empty one at point.
With a region active, wraps BEG..END in `resurface-cloze-open' and
`resurface-cloze-close'."
  (interactive (if (use-region-p)
                    (list (region-beginning) (region-end))
                  (list nil nil)))
  (let (rescan-beg rescan-end)
    (if (and beg end)
        (progn
          (setq rescan-beg beg)
          (save-excursion
            (goto-char end) (insert resurface-cloze-close)
            (goto-char beg) (insert resurface-cloze-open))
          (setq rescan-end (+ end (length resurface-cloze-open) (length resurface-cloze-close))))
      (setq rescan-beg (point))
      (insert resurface-cloze-open resurface-cloze-close)
      (backward-char (length resurface-cloze-close))
      (setq rescan-end (point)))
    (when resurface-cloze-mode
      (resurface--cloze-rescan-lines rescan-beg rescan-end))))

;;;###autoload
(defun resurface-cloze-remove-at-point ()
  "Strip the cloze delimiters around point, leaving its plain text behind."
  (interactive)
  (let ((pt (point))
        (bol (line-beginning-position))
        (eol (line-end-position))
        (re (resurface--cloze-regexp))
        match)
    (save-excursion
      (save-match-data
        (goto-char bol)
        (while (and (not match) (re-search-forward re eol t))
          (when (and (<= (match-beginning 0) pt) (<= pt (match-end 0)))
            (setq match (list (match-beginning 0) (match-end 0)
                               (match-string-no-properties 1)))))))
    (if (not match)
        (message "resurface-cloze: no cloze at point.")
      (cl-destructuring-bind (mb me inner) match
        (resurface--cloze-clear-overlays mb me)
        (delete-region mb me)
        (goto-char mb)
        (insert inner)
        (when resurface-cloze-mode
          (resurface--cloze-rescan-lines mb (point)))))))


;; =======================================================
;;  Whole-buffer reveal / hide

;;;###autoload
(defun resurface-cloze-reveal-all ()
  "Reveal every cloze in the current buffer."
  (interactive)
  (dolist (ov (resurface--cloze-overlays))
    (resurface--cloze-reveal ov))
  (setq resurface--cloze-revealed nil))

;;;###autoload
(defun resurface-cloze-hide-all ()
  "Hide every cloze in the current buffer."
  (interactive)
  (dolist (ov (resurface--cloze-overlays))
    (resurface--cloze-hide ov))
  (setq resurface--cloze-revealed nil))


(provide 'resurface-cloze)
;;; resurface-cloze.el ends here
