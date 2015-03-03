;;; avy-jump.el --- jump to things tree-style

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; Version: 0.1.0
;; Keywords: point

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package offers various commands for navigating to things using `avy'.
;; They are in the "Commands" outline.

;;; Code:
;;* Requires
(require 'avy)
(require 'ace-window)

;;* Customization
(defgroup avy-jump nil
  "Jump to things tree-style."
  :group 'convenience
  :prefix "avi-")

(defcustom avi-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
  "Keys for jumping.")

(defcustom avi-background nil
  "When non-nil, a gray background will be added during the selection."
  :type 'boolean)

(defface avi-lead-face
  '((t (:foreground "white" :background "#e52b50")))
  "Face used for the leading chars.")

;;* Internals
(defun avi--goto (x)
  "Goto X.
X is (POS . WND)
POS is either a position or (BEG . END)."
  (if (null x)
      (message "zero candidates")
    (select-window (cdr x))
    (let ((pt (car x)))
      (when (consp pt)
        (setq pt (car pt)))
      (goto-char pt))))

(defun avi--process (candidates overlay-fn)
  "Select one of CANDIDATES using `avy-read'."
  (unwind-protect
       (let ((aw-background avi-background))
         (cl-case (length candidates)
           (0
            nil)
           (1
            (car candidates))
           (t
            (aw--make-backgrounds (list (selected-window)))
            (avy-read (avy-tree candidates avi-keys)
                      overlay-fn
                      #'aw--remove-leading-chars))))
    (aw--done)))

(defun avi--regex-candidates (regex &optional wnd beg end)
  "Return all elements that match REGEX in WND.
Each element of the list is ((BEG . END) . WND)."
  (setq wnd (or wnd (selected-window)))
  (let ((we (or end (window-end (selected-window) t)))
        candidates)
    (save-window-excursion
      (select-window wnd)
      (save-excursion
        (goto-char (or beg (window-start)))
        (while (re-search-forward regex we t)
          (push (cons (cons (match-beginning 0)
                            (match-end 0))
                      wnd) candidates)))
      (nreverse candidates))))

(defun avi--overlay (str pt wnd)
  "Create an overlay with STR at PT in WND."
  (let ((ol (make-overlay pt (1+ pt) (window-buffer wnd)))
        (old-str (with-selected-window wnd
                   (buffer-substring pt (1+ pt)))))
    (when avi-background
      (setq old-str (propertize
                     old-str 'face 'aw-background-face)))
    (overlay-put ol 'window wnd)
    (overlay-put ol 'display (concat str old-str))
    (push ol aw-overlays-lead)))

(defun avi--overlay-pre (path leaf)
  "Create an overlay with STR at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is ((BEG . END) . WND)."
  (avi--overlay
   (propertize (apply #'string (reverse path))
               'face 'avi-lead-face)
   (if (consp (car leaf))
       (caar leaf)
     (car leaf))
   (cdr leaf)))

(defun avi--overlay-post (path leaf)
  "Create an overlay with STR at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is ((BEG . END) . WND)."
  (avi--overlay
   (propertize (apply #'string (reverse path))
               'face 'avi-lead-face)
   (if (consp (car leaf))
       (cdar leaf)
     (car leaf))
   (cdr leaf)))

;;* Commands
;;;###autoload
(defun avi-goto-char ()
  "Read one char and jump to it in current window."
  (interactive)
  (avi--goto
   (avi--process
    (avi--regex-candidates
     (string (read-char "char: "))
     (selected-window))
    #'avi--overlay-post)))

;;;###autoload
(defun avi-goto-char-2 ()
  "Read two chars and jump to them in current window."
  (interactive)
  (avi--goto
   (avi--process
    (avi--regex-candidates
     (string
      (read-char "char 1: ")
      (read-char "char 2: "))
     (selected-window))
    #'avi--overlay-post)))

;;;###autoload
(defun avi-isearch ()
  "Jump to one of the current isearch candidates."
  (interactive)
  (let* ((candidates
          (avi--regex-candidates isearch-string))
         (avi-background nil)
         (candidate
          (avi--process candidates #'avi--overlay-post)))
    (isearch-done)
    (avi--goto candidate)))

;;;###autoload
(defun avi-goto-word-0 ()
  "Jump to a word start in current window."
  (interactive)
  (let* ((avi-keys (number-sequence ?a ?z))
         (candidates (avi--regex-candidates "\\b\\sw")))
    (avi--goto
     (avi--process candidates #'avi--overlay-pre))))

;;;###autoload
(defun avi-goto-word-1 ()
  "Jump to a word start in current window.
Read one char with which the word should start."
  (interactive)
  (let ((candidates (avi--regex-candidates
                     (concat
                      "\\b"
                      (string (read-char "char: "))))))
    (avi--goto
     (avi--process candidates #'avi--overlay-pre))))

(defun avi--line ()
  "Select line in current window."
  (let ((avi-background nil)
        candidates)
    (save-excursion
      (save-restriction
        (narrow-to-region (window-start) (window-end (selected-window) t))
        (goto-char (point-min))
        (while (< (point) (point-max))
          (push (cons (point) (selected-window))
                candidates)
          (forward-line 1))))
    (avi--process (nreverse candidates) #'avi--overlay-pre)))

;;;###autoload
(defun avi-goto-line ()
  "Jump to a line start in current buffer."
  (interactive)
  (avi--goto (avi--line)))

;;;###autoload
(defun avi-copy-line (arg)
  "Copy a selected line above the current line.
ARG lines can be used."
  (interactive "p")
  (let ((start (car (avi--line))))
    (move-beginning-of-line nil)
    (save-excursion
      (insert
       (buffer-substring-no-properties
        start
        (save-excursion
          (goto-char start)
          (move-end-of-line arg)
          (point)))
       "\n"))))

;;;###autoload
(defun avi-move-line (arg)
  "Move a selected line above the current line.
ARG lines can be used."
  (interactive "p")
  (let ((start (car (avi--line))))
    (move-beginning-of-line nil)
    (save-excursion
      (save-excursion
        (goto-char start)
        (move-end-of-line arg)
        (kill-region start (point)))
      (insert
       (current-kill 0)))))

;;;###autoload
(defun avi-copy-region ()
  "Select two lines and copy the text between them here."
  (interactive)
  (let ((beg (car (avi--line)))
        (end (car (avi--line)))
        (pad (if (bolp) "" "\n")))
    (move-beginning-of-line nil)
    (save-excursion
      (insert
       (buffer-substring-no-properties
        beg
        (save-excursion
          (goto-char end)
          (line-end-position)))
       pad))))

(provide 'avy-jump)

;;; avy-jump.el ends here
