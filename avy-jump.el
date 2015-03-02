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
;; This package offers various commands for navigating to things using `avy', such as
;; `avy-jump-char', `avy-double-char', and `avy-jump-line'.

;;; Code:
(require 'avy)
(require 'ace-window)

(defgroup avy-jump nil
  "Jump to things tree-style."
  :group 'convenience
  :prefix "avy-")

(defcustom avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
  "Keys for jumping.")

(defun avy--goto (x)
  "Goto X.
X is (POSITION . WINDOW)."
  (select-window (cdr x))
  (goto-char (car x)))

(defun avy--process (candidates action &optional nobackground)
  "Select one of CANDIDATES using `avy-read'.
Call ACTION on that candidate."
  (unwind-protect
       (let ((aw-background (not nobackground)))
         (aw--make-backgrounds (list (selected-window)))
         (cl-case (length candidates)
           (0
            (message "zero candidates"))
           (1
            (funcall action (car candidates)))
           (t
            (funcall
             action
             (avy-read (avy-tree candidates avy-keys)
                       #'aw--lead-overlay
                       #'aw--remove-leading-chars)))))
    (aw--done)))

(defun avy--regex-candidates (regex &optional wnd)
  "Return all elements that match REGEX in WND.
Each element of the list is (POSITION . WND)."
  (let ((we (window-end))
        candidates)
    (setq wnd (or wnd (selected-window)))
    (save-window-excursion
      (select-window wnd)
      (save-excursion
        (goto-char (window-start))
        (while (re-search-forward regex we t)
          (push (cons (1- (point)) wnd) candidates)))
      (nreverse candidates))))

(defun avy-jump-char ()
  "Read one char and jump to it in current window."
  (interactive)
  (avy--process (avy--regex-candidates
                 (string (read-char "char: "))
                 (selected-window))
                #'avy--goto))

(defun avy-jump-double-char ()
  "Read two chars and jump to them in current window."
  (interactive)
  (avy--process (avy--regex-candidates
                 (string
                  (read-char "char 1: ")
                  (read-char "char 2: "))
                 (selected-window))
                #'avy--goto))

(defun avy-jump-isearch ()
  "Jump to one of the current isearch candidates."
  (interactive)
  (let ((candidates
         (mapcar (lambda (x) (cons (1+ (car x))
                              (cdr x)))
                 (avy--regex-candidates isearch-string))))
    (avy--process candidates #'avy--goto t)
    (isearch-done)))

(defun avy-jump-zero-word ()
  "Jump to a word start in current buffer"
  (interactive)
  (let ((we (window-end))
        candidates)
    (save-excursion
      (goto-char (window-start))
      (while (< (point) we)
        (forward-word 2)
        (forward-word -1)
        (push (cons (point) (selected-window))
              candidates)))
    (avy--process (nreverse candidates)
                  #'avy--goto)))

(defun avy-jump-one-word ()
  "Jump to a word start in current buffer.
Read one char with which the word should start."
  (interactive)
  (let ((candidates (avy--regex-candidates
                     (string (read-char "char: "))
                     (selected-window))))
    (save-excursion
      (setq candidates (cl-remove-if-not
                        (lambda (x)
                          (goto-char (car x))
                          (looking-at "\\b"))
                        (nreverse candidates))))
    (avy--process candidates #'avy--goto)))

(defun avy-jump-line ()
  "Jump to a line start in current buffer."
  (interactive)
  (let ((we (window-end))
        candidates)
    (save-excursion
      (goto-char (window-start))
      (while (< (point) we)
        (push (cons (point) (selected-window))
              candidates)
        (forward-line 1)))
    (avy--process (nreverse candidates)
                  #'avy--goto
                  t)))

(provide 'avy-jump)

;;; avy-jump.el ends here
