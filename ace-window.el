;;; ace-window.el --- Quickly switch windows using `ace-jump-mode'. -*- lexical-binding: t -*-

;; Copyright (C) 2014 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/ace-window
;; Version: 0.7.0
;; Package-Requires: ((ace-jump-mode "2.0"))
;; Keywords: cursor, window, location

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
;; This package uses `ace-jump-mode' machinery to switch between
;; windows.
;;
;; The main function, `ace-window' is meant to replace `other-window'.
;; If fact, when there are only two windows present, `other-window' is
;; called.  If there are more, each window will have its first
;; character highlighted.  Pressing that character will switch to that
;; window.  Note that unlike `ace-jump-mode', the point position will
;; not be changed: only current window focus changes.
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
(require 'ace-jump-mode)

;; ——— Customization ———————————————————————————————————————————————————————————
(defgroup ace-window nil
  "Quickly switch current window."
  :group 'convenience
  :prefix "aw-")

(defcustom aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
  "Keys for selecting window."
  :group 'ace-window)

(defcustom aw-scope 'global
  "The scope used by `ace-window'."
  :group 'ace-window
  :type '(choice
          (const :tag "global" global)
          (const :tag "frame" frame)))

(defcustom aw-ignored-buffers '("*Calc Trail*")
  "List of buffers to ignore when selecting window."
  :group 'ace-window)

(defcustom aw-ignore-on t
  "When t, `ace-window' will ignore `aw-ignored-buffers'.
Use M-0 `ace-window' to toggle this value."
  :type 'boolean
  :group 'ace-window)

(defcustom aw-background t
  "When t, `ace-window' will dim out all buffers temporarily when used.'."
  :type 'boolean
  :group 'ace-window)

(defvar ace-window-end-hook nil
  "Function(s) to call after `ace-window' is done.")
(make-obsolete-variable
 'ace-window-end-hook
 "Don't use `ace-window-end-hook', just call what you need right after `ace-window'" "0.7.0")

(defvar ace-window-end-once-hook nil
  "Function(s) to call once after `ace-window' is done.
This hook is set to nil with each call to `ace-window'.")
(make-obsolete-variable
 'ace-window-end-once-hook
 "Don't use `ace-window-end-once-hook', just call what you need right after `ace-window'" "0.7.0")

(defun aw-ignored-p (window)
  "Return t if WINDOW should be ignored."
  (and aw-ignore-on
       (member (buffer-name (window-buffer window))
               aw-ignored-buffers)))

(defun aw-list-visual-area ()
  "Forward to `ace-jump-list-visual-area', removing invisible frames."
  (cl-remove-if
   (lambda (x)
     (let ((f (aj-visual-area-frame x)))
       (or (not (and (frame-live-p f)
                     (frame-visible-p f)))
           (string= "initial_terminal" (terminal-name f))
           (aw-ignored-p (aj-visual-area-window x)))))
   (ace-jump-list-visual-area)))

(defun aw--done ()
  "Clean up ace-jump overlays."
  ;; clean up mode line
  (setq ace-jump-current-mode nil)
  (setq ace-jump-mode nil)
  (force-mode-line-update)

  ;; delete background overlay
  (loop for ol in ace-jump-background-overlay-list
     do (delete-overlay ol))
  (setq ace-jump-background-overlay-list nil)

  ;; delete overlays in search tree
  (when ace-jump-search-tree
    (ace-jump-delete-overlay-in-search-tree ace-jump-search-tree)
    (setq ace-jump-search-tree nil)))

(defun aw-select (mode-line)
  "Return a selected other window.
Amend MODE-LINE to the mode line for the duration of the selection."
  (let* ((start-window (selected-window))
         (ace-jump-mode-scope aw-scope)
         (next-window-scope
          (cl-case aw-scope
            ('global 'visible)
            ('frame 'frame)))
         (visual-area-list
          (cl-remove-if
           (lambda (va)
             (let ((b (aj-visual-area-buffer va))
                   (w (aj-visual-area-window va)))
               (or (with-current-buffer b
                     (and buffer-read-only
                          (= 0 (buffer-size b))))
                   (aw-ignored-p w))))
           (sort (aw-list-visual-area) 'aw-visual-area<))))
    (cl-case (length visual-area-list)
      (0)
      (1
       (select-window (aj-visual-area-window (car visual-area-list))))
      (2
       (select-window
        (next-window nil nil next-window-scope)))
      (t
       (let ((candidate-list
              (mapcar (lambda (va)
                        (let ((b (aj-visual-area-buffer va)))
                          ;; ace-jump-mode can't jump if the buffer is empty
                          (when (= 0 (buffer-size b))
                            (with-current-buffer b
                              (insert " "))))
                        (make-aj-position
                         :offset
                         (aw-offset (aj-visual-area-window va))
                         :visual-area va))
                      visual-area-list)))
         ;; create background for each visual area
         (if aw-background
             (setq ace-jump-background-overlay-list
                   (loop for va in visual-area-list
                      collect (let* ((w (aj-visual-area-window va))
                                     (b (aj-visual-area-buffer va))
                                     (ol (make-overlay (window-start w)
                                                       (window-end w)
                                                       b)))
                                (overlay-put ol 'face 'ace-jump-face-background)
                                ol))))
         ;; construct search tree and populate overlay into tree
         (setq ace-jump-search-tree
               (ace-jump-tree-breadth-first-construct
                (length candidate-list)
                (length aw-keys)))
         (ace-jump-populate-overlay-to-search-tree
          ace-jump-search-tree candidate-list)
         (ace-jump-update-overlay-in-search-tree
          ace-jump-search-tree aw-keys)
         (setq ace-jump-mode mode-line)
         (force-mode-line-update)
         ;; turn off helm transient map
         (remove-hook 'post-command-hook 'helm--maybe-update-keymap)
         (unwind-protect
              (let (node)
                (catch 'done
                  (while t
                    (setq node (cl-position (read-char) aw-keys))
                    (when node
                      (setq node (nth node (cdr ace-jump-search-tree))))
                    (cond ((null node)
                           (message "No such position candidate.")
                           (throw 'done nil))

                          ((eq (car node) 'branch)
                           (let ((old-tree ace-jump-search-tree))
                             (setq ace-jump-search-tree
                                   (cons 'branch (cdr node)))
                             (ace-jump-update-overlay-in-search-tree
                              ace-jump-search-tree aw-keys)
                             (setf (cdr node) nil)
                             (ace-jump-delete-overlay-in-search-tree old-tree)))

                          ((eq (car node) 'leaf)
                           (let ((aj-data (overlay-get (cdr node) 'aj-data)))
                             (select-window (aj-position-window aj-data)))
                           (throw 'done t))

                          (t
                           (error "[AceJump] Internal error: tree node type is invalid"))))))
           (aw--done)))))
    (prog1 (selected-window)
      (select-window start-window))))

;; ——— Interactive —————————————————————————————————————————————————————————————
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
  "Select a window with function `ace-jump-mode'.
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

;; ——— Utility —————————————————————————————————————————————————————————————————
(defun aw-visual-area< (va1 va2)
  "Return true if visual area VA1 is less than VA2.
This is determined by their respective window coordinates.
Windows are numbered top down, left to right."
  (let ((f1 (aj-visual-area-frame va1))
        (f2 (aj-visual-area-frame va2))
        (e1 (window-edges (aj-visual-area-window va1)))
        (e2 (window-edges (aj-visual-area-window va2))))
    (cond ((string< (frame-parameter f1 'window-id)
                    (frame-parameter f2 'window-id))
           t)
          ((< (car e1) (car e2))
           t)
          ((> (car e1) (car e2))
           nil)
          ((< (cadr e1) (cadr e2))
           t))))

(defun aw-switch-to-window (window)
  "Switch to the window WINDOW."
  (let ((frame (window-frame window)))
    (when (and (frame-live-p frame)
               (not (eq frame (selected-frame))))
      (select-frame-set-input-focus frame))
    (if (window-live-p window)
        (select-window window)
      (error "Got a dead window %S" window))))

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

(provide 'ace-window)

;;; ace-window.el ends here
