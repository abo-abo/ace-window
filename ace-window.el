;;; ace-window.el --- Quickly switch windows. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/ace-window
;; Version: 0.8.0
;; Keywords: window, location

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
;; The main function, `ace-window' is meant to replace `other-window'.
;; If fact, when there are only two windows present, `other-window' is
;; called.  If there are more, each window will have its first
;; character highlighted.  Pressing that character will switch to that
;; window.
;;
;; To setup this package, just add to your .emacs:
;;
;;    (global-set-key (kbd "M-p") 'ace-window)
;;
;; replacing "M-p"  with an appropriate shortcut.
;;
;; Depending on your window usage patterns, you might want to set
;;
;;    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;;
;; This way they're all on the home row, although the intuitive
;; ordering is lost.
;;
;; If you don't want the gray background that makes the red selection
;; characters stand out more, set this:
;;
;;    (setq aw-background nil)
;;
;; When prefixed with one `universal-argument', instead of switching
;; to selected window, the selected window is swapped with current one.
;;
;; When prefixed with two `universal-argument', the selected window is
;; deleted instead.

;;; Code:
(require 'avy)

;;* Customization
(defgroup ace-window nil
  "Quickly switch current window."
  :group 'convenience
  :prefix "aw-")

(defcustom aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
  "Keys for selecting window.")

(defcustom aw-scope 'global
  "The scope used by `ace-window'."
  :type '(choice
          (const :tag "global" global)
          (const :tag "frame" frame)))

(defcustom aw-ignored-buffers '("*Calc Trail*" "*LV*")
  "List of buffers to ignore when selecting window."
  :type '(repeat string))

(defcustom aw-ignore-on t
  "When t, `ace-window' will ignore `aw-ignored-buffers'.
Use M-0 `ace-window' to toggle this value."
  :type 'boolean)

(defcustom aw-background t
  "When t, `ace-window' will dim out all buffers temporarily when used.'."
  :type 'boolean)

(defcustom aw-leading-char-style 'char
  "Style of the leading char overlay."
  :type '(choice
          (const :tag "single char" 'char)
          (const :tag "full path" 'path)))

(defface aw-leading-char-face
    '((((class color)) (:foreground "red"))
      (((background dark)) (:foreground "gray100"))
      (((background light)) (:foreground "gray0"))
      (t (:foreground "gray100" :underline nil)))
  "Face for each window's leading char.")

(defface aw-background-face
  '((t (:foreground "gray40")))
  "Face for whole window background during selection.")

(defcustom aw-mode-line-key-display-on t
  "Toggles Workgroups' mode-line display."
  :type 'boolean
  :set (lambda (sym val)
         (custom-set-default sym val)
         (force-mode-line-update)))

(defface aw-mode-line-face
  '((t (:foreground "red")))
  "Face used for ace window key in the mode-line.")

(defcustom ace-window-mode-line-format "#%c" "Ace window mode-line format.")

(defcustom ace-window-mode-line-position 1
  "The position in the mode-line `ace-window-mode' displays the number.")

(defvar aw--table nil "Window and key hash map.")

(defvar aw--mode-line-show-key nil "Ace window mode line show key mode.")

;;* Implementation
(defun aw-ignored-p (window)
  "Return t if WINDOW should be ignored."
  (and aw-ignore-on
       (member (buffer-name (window-buffer window))
               aw-ignored-buffers)))

(defun aw-window-list ()
  "Return the list of interesting windows."
  (sort
   (cl-remove-if
    (lambda (w)
      (let ((f (window-frame w))
            (b (window-buffer w)))
        (or (not (and (frame-live-p f)
                      (frame-visible-p f)))
            (string= "initial_terminal" (terminal-name f))
            (aw-ignored-p w)
            (with-current-buffer b
              (and buffer-read-only
                   (= 0 (buffer-size b)))))))
    (cl-case aw-scope
      (global
       (cl-mapcan #'window-list (frame-list)))
      (frame
       (window-list))
      (t
       (error "Invalid `aw-scope': %S" aw-scope))))
   'aw-window<))

(defvar aw-overlays-lead nil
  "Hold overlays for leading chars.")

(defvar aw-overlays-back nil
  "Hold overlays for when `aw-background' is t.")

(defvar ace-window-mode nil
  "Minor mode during the selection process.")

;; register minor mode
(or (assq 'ace-window-mode minor-mode-alist)
    (nconc minor-mode-alist
           (list '(ace-window-mode ace-window-mode))))

(defun aw--done ()
  "Clean up mode line and overlays."
  ;; mode line
  (setq ace-window-mode nil)
  (force-mode-line-update)
  ;; background
  (mapc #'delete-overlay aw-overlays-back)
  (setq aw-overlays-back nil)
  (aw--remove-leading-chars))

(defun aw--lead-overlay (path leaf)
  "Create an overlay using PATH at LEAF.
LEAF is (PT . WND)."
  (let* ((pt (car leaf))
         (wnd (cdr leaf))
         (ol (make-overlay pt (1+ pt) (window-buffer wnd)))
         (old-str (with-selected-window wnd
                    (buffer-substring pt (1+ pt))))
         (new-str
          (concat
           (cl-case aw-leading-char-style
             (char
              (apply #'string (last path)))
             (path
              (apply #'string (reverse path)))
             (t
              (error "Bad `aw-leading-char-style': %S"
                     aw-leading-char-style)))
           (cond ((string-equal old-str "\t")
                  (make-string (1- tab-width) ?\ ))
                 ((string-equal old-str "\n")
                  "\n")
                 (t
                  (make-string
                   (max 0 (1- (string-width old-str)))
                   ?\ ))))))
    (overlay-put ol 'face 'aw-leading-char-face)
    (overlay-put ol 'window wnd)
    (overlay-put ol 'display new-str)
    (push ol aw-overlays-lead)))

(defun aw--remove-leading-chars ()
  "Remove leading char overlays."
  (mapc #'delete-overlay aw-overlays-lead)
  (setq aw-overlays-lead nil))

(defun aw--make-backgrounds (wnd-list)
  "Create a dim background overlay for each window on WND-LIST."
  (when aw-background
    (setq aw-overlays-back
          (mapcar (lambda (w)
                    (let ((ol (make-overlay
                               (window-start w)
                               (window-end w)
                               (window-buffer w))))
                      (overlay-put ol 'face 'aw-background-face)
                      ol))
                  wnd-list))))

(defvar aw--flip-keys nil
  "Pre-processed `aw-flip-keys'.")

(defcustom aw-flip-keys '("n")
  "Keys which should select the last window."
  :set (lambda (sym val)
         (set sym val)
         (setq aw--flip-keys
               (mapcar (lambda (x) (aref (kbd x) 0)) val))))

(defun aw-select (mode-line)
  "Return a selected other window.
Amend MODE-LINE to the mode line for the duration of the selection."
  (let ((start-window (selected-window))
        (next-window-scope (cl-case aw-scope
                             ('global 'visible)
                             ('frame 'frame)))
        (wnd-list (aw-window-list))
        final-window)
    (cl-case (length wnd-list)
      (0
       start-window)
      (1
       (car wnd-list))
      (2
       (setq final-window (next-window nil nil next-window-scope))
       (while (and (aw-ignored-p final-window)
                   (not (equal final-window start-window)))
         (setq final-window (next-window final-window nil next-window-scope)))
       final-window)
      (t
       (let ((candidate-list
              (mapcar (lambda (wnd)
                        ;; can't jump if the buffer is empty
                        (with-current-buffer (window-buffer wnd)
                          (when (= 0 (buffer-size))
                            (insert " ")))
                        (cons (aw-offset wnd) wnd))
                      wnd-list)))
         (aw--make-backgrounds wnd-list)
         (setq ace-window-mode mode-line)
         (force-mode-line-update)
         ;; turn off helm transient map
         (remove-hook 'post-command-hook 'helm--maybe-update-keymap)
         (unwind-protect
              (condition-case err
                  (or (cdr (avy-read (avy-tree candidate-list aw-keys)
                                     #'aw--lead-overlay
                                     #'aw--remove-leading-chars))
                      start-window)
                (error
                 (if (memq (caddr err) aw--flip-keys)
                     (aw--pop-window)
                   (signal (car err) (cdr err)))))
           (aw--done)))))))

;;* Interactive
;;;###autoload
(defun ace-select-window ()
  "Ace select window."
  (interactive)
  (aw-switch-to-window
   (aw-select " Ace - Window")))

;;;###autoload
(defun ace-delete-window ()
  "Ace delete window."
  (interactive)
  (aw-delete-window
   (aw-select " Ace - Delete Window")))

;;;###autoload
(defun ace-swap-window ()
  "Ace swap window."
  (interactive)
  (aw-swap-window
   (aw-select " Ace - Swap Window")))

;;;###autoload
(defun ace-maximize-window ()
  "Ace maximize window."
  (interactive)
  (select-window
   (aw-select " Ace - Maximize Window"))
  (delete-other-windows))

;;;###autoload
(defun ace-window (arg)
  "Select a window.
Perform an action based on ARG described below.

By default, behaves like extended `other-window'.

Prefixed with one \\[universal-argument], does a swap between the
selected window and the current window, so that the selected
buffer moves to current window (and current buffer moves to
selected window).

Prefixed with two \\[universal-argument]'s, deletes the selected
window."
  (interactive "p")
  (cl-case arg
    (0
     (setq aw-ignore-on
           (not aw-ignore-on))
     (ace-select-window))
    (4 (ace-swap-window))
    (16 (ace-delete-window))
    (t (ace-select-window))))

;;;###autoload
(defun ace-window-show-on-mode-line (&optional arg)
  "Show ace window key on mode-line on and off.
ARG is nil - toggle
ARG >= 1   - turn on
ARG == 0   - turn off
ARG is anything else, turn on `workgroups-mode'."
  (interactive (list current-prefix-arg))
  (setq aw--mode-line-show-key
        (cond ((not arg) (not aw--mode-line-show-key))
              ((integerp arg) (if (> arg 0) t nil))
              (t)))
  (cond (aw--mode-line-show-key
         (unless aw--table
           (save-excursion
             (setq aw--table (make-hash-table :size (length aw-keys)))
             (aw--install-mode-line)
             (add-hook 'window-configuration-change-hook 'aw-update))))
        (t
         (aw--clear-mode-line)
         (remove-hook 'window-configuration-change-hook 'aw-update)
         (setq aw--table nil)))
  (message (concat "Ace window show on mode line: "
                   (if aw--mode-line-show-key "on" "off")))
  aw--mode-line-show-key)

;;* Utility
(defun aw-window< (wnd1 wnd2)
  "Return true if WND1 is less than WND2.
This is determined by their respective window coordinates.
Windows are numbered top down, left to right."
  (let ((f1 (window-frame wnd1))
        (f2 (window-frame wnd2))
        (e1 (window-edges wnd1))
        (e2 (window-edges wnd2)))
    (cond ((string< (frame-parameter f1 'window-id)
                    (frame-parameter f2 'window-id))
           t)
          ((< (car e1) (car e2))
           t)
          ((> (car e1) (car e2))
           nil)
          ((< (cadr e1) (cadr e2))
           t))))

(defvar aw--window-ring (make-ring 10)
  "Hold the window switching history.")

(defun aw--push-window (window)
  "Store WINDOW to `aw--window-ring'."
  (when (or (zerop (ring-length aw--window-ring))
            (not (equal
                  (ring-ref aw--window-ring 0)
                  window)))
    (ring-insert aw--window-ring (selected-window))))

(defun aw--pop-window ()
  "Return the removed top of `aw--window-ring'."
  (let (res)
    (condition-case nil
        (while (not (window-live-p
                     (setq res (ring-remove aw--window-ring 0)))))
      (error
       (error "No previous windows stored")))
    res))

(defun aw-switch-to-window (window)
  "Switch to the window WINDOW."
  (let ((frame (window-frame window)))
    (when (and (frame-live-p frame)
               (not (eq frame (selected-frame))))
      (select-frame-set-input-focus frame))
    (if (window-live-p window)
        (progn
          (aw--push-window (selected-window))
          (select-window window))
      (error "Got a dead window %S" window))))

(defun aw-flip-window ()
  "Switch to the window you were previously in."
  (interactive)
  (aw-switch-to-window (aw--pop-window)))

(defun aw-delete-window (window)
  "Delete window WINDOW."
  (let ((frame (window-frame window)))
    (when (and (frame-live-p frame)
               (not (eq frame (selected-frame))))
      (select-frame-set-input-focus (window-frame window)))
    (if (= 1 (length (window-list)))
        (delete-frame frame)
      (if (window-live-p window)
          (delete-window window)
        (error "Got a dead window %S" window)))))

(defun aw-swap-window (window)
  "Swap buffers of current window and WINDOW."
  (cl-labels ((swap-windows (window1 window2)
                "Swap the buffers of WINDOW1 and WINDOW2."
                (let ((buffer1 (window-buffer window1))
                      (buffer2 (window-buffer window2)))
                  (set-window-buffer window1 buffer2)
                  (set-window-buffer window2 buffer1)
                  (select-window window2))))
    (let ((frame (window-frame window))
          (this-window (selected-window)))
      (when (and (frame-live-p frame)
                 (not (eq frame (selected-frame))))
        (select-frame-set-input-focus (window-frame window)))
      (when (and (window-live-p window)
                 (not (eq window this-window)))
        (aw--push-window this-window)
        (swap-windows this-window window)))))

(defun aw-offset (window)
  "Return point in WINDOW that's closest to top left corner.
The point is writable, i.e. it's not part of space after newline."
  (let ((h (window-hscroll window))
        (beg (window-start window))
        (end (window-end window))
        (inhibit-field-text-motion t))
    (with-current-buffer
        (window-buffer window)
      (save-excursion
        (goto-char beg)
        (while (and (< (point) end)
                    (< (- (line-end-position)
                          (line-beginning-position))
                       h))
          (forward-line))
        (+ (point) h)))))

;;* Mode line
(defun aw-update ()
  "Update the key showed on mode-line."
  (let* ((key-list aw-keys)
         (wnd-list (aw-window-list)))
    (clrhash aw--table)
    (dolist (win wnd-list)
      (let ((key (car key-list)))
        (when key
          (setq key-list (cdr key-list))
          (puthash win key aw--table))))))

(defun aw--mode-line-key-string (&optional window)
  "Format the WINDOW to string."
  (let* ((k (gethash (or window (selected-window)) aw--table))
         (s (format ace-window-mode-line-format k)))
    (propertize s 'face 'aw-mode-line-face)))

(defun aw--install-mode-line (&optional position)
  "Install the window number from `ace-window-mode' to the mode-line at POSITION."
  (let ((mode-line (default-value 'mode-line-format))
        (res))
    (dotimes (i (min (or position ace-window-mode-line-position)
                     (length mode-line)))
      (push (car mode-line) res)
      (pop mode-line))
    (push '(aw-mode-line-key-display-on (:eval (aw--mode-line-key-string))) res)
    (while mode-line
      (push (car mode-line) res)
      (pop mode-line))
    (set-default 'mode-line-format (nreverse res)))
  (force-mode-line-update t))

(defun aw--clear-mode-line ()
  "Remove the window number of `ace-window-mode' from the mode-line."
  (let ((mode-line (default-value 'mode-line-format))
        (res))
    (while mode-line
      (let ((item (car mode-line)))
        (unless (equal item '(aw-mode-line-key-display-on
                              (:eval (aw--mode-line-key-string))) )
          (push item res)))
      (pop mode-line))
    (set-default 'mode-line-format (nreverse res)))
  (force-mode-line-update t))

(provide 'ace-window)

;;; ace-window.el ends here
