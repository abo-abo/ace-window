;;; ace-window.el --- Quickly switch windows. -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; Maintainer: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/ace-window
;; Version: 0.10.0
;; Package-Requires: ((avy "0.5.0"))
;; Keywords: window, location

;; This file is part of GNU Emacs.

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
;; The main function, `ace-window' is meant to replace `other-window'
;; by assigning each window a short, unique label.  When there are only
;; two windows present, `other-window' is called (unless
;; aw-dispatch-always is set non-nil).  If there are more, each
;; window will have its first label character highlighted.  Once a
;; unique label is typed, ace-window will switch to that window.
;;
;; To setup this package, just add to your .emacs:
;;
;;    (global-set-key (kbd "M-o") 'ace-window)
;;
;; replacing "M-o"  with an appropriate shortcut.
;;
;; By default, ace-window uses numbers for window labels so the window
;; labeling is intuitively ordered.  But if you prefer to type keys on
;; your home row for quicker access, use this setting:
;;
;;    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;;
;; Whenever ace-window prompts for a window selection, it grays out
;; all the window characters, highlighting window labels in red.  To
;; disable this behavior, set this:
;;
;;    (setq aw-background nil)
;;
;; If you want to know the selection characters ahead of time, turn on
;; `ace-window-display-mode'.
;;
;; When prefixed with one `universal-argument', instead of switching
;; to the selected window, the selected window is swapped with the
;; current one.
;;
;; When prefixed with two `universal-argument', the selected window is
;; deleted instead.

;;; Code:
(require 'avy)
(require 'ring)
(require 'subr-x)

;;* Customization
(defgroup ace-window nil
  "Quickly switch current window."
  :group 'convenience
  :prefix "aw-")

(defcustom aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
  "Keys for selecting window."
  :type '(repeat character))

(defcustom aw-scope 'global
  "The scope used by `ace-window'."
  :type '(choice
          (const :tag "visible frames" visible)
          (const :tag "global" global)
          (const :tag "frame" frame)))

(defcustom aw-translate-char-function #'identity
  "Function to translate user input key into another key.
For example, to make SPC do the same as ?a, use
\(lambda (c) (if (= c 32) ?a c))."
  :type '(choice
          (const :tag "Off" #'identity)
          (const :tag "Ignore Case" #'downcase)
          (function :tag "Custom")))

(defcustom aw-minibuffer-flag nil
  "When non-nil, also display `ace-window-mode' string in the minibuffer when ace-window is active."
  :type 'boolean)

(defcustom aw-ignored-buffers '("*Calc Trail*" " *LV*")
  "List of buffers and major-modes to ignore when choosing a window from the window list.
Active only when `aw-ignore-on' is non-nil."
  :type '(repeat string))

(defcustom aw-ignore-on t
  "When t, `ace-window' will ignore buffers and major-modes in `aw-ignored-buffers'.
Use M-0 `ace-window' to toggle this value."
  :type 'boolean)

(defcustom aw-ignore-current nil
  "When t, `ace-window' will ignore `selected-window'."
  :type 'boolean)

(defcustom aw-background t
  "When t, `ace-window' will dim out all buffers temporarily when used."
  :type 'boolean)

(defcustom aw-leading-char-style 'char
  "Style of the leading char overlay."
  :type '(choice
          (const :tag "single char" 'char)
          (const :tag "full path" 'path)))

(defcustom aw-dispatch-always nil
  "When non-nil, `ace-window' will issue a `read-char' even for one window.
This will make `ace-window' act different from `other-window' for
  one or two windows."
  :type 'boolean)

(defcustom aw-dispatch-when-more-than 2
  "If the number of windows is more than this, activate ace-window-ness."
  :type 'integer)

(defcustom aw-reverse-frame-list nil
  "When non-nil `ace-window' will order frames for selection in
the reverse of `frame-list'"
  :type 'boolean)

(defcustom aw-frame-offset '(13 . 23)
  "Increase in pixel offset for new ace-window frames relative to the selected frame.
Its value is an (x-offset . y-offset) pair in pixels."
  :type '(cons integer integer))

(defcustom aw-frame-size nil
  "Frame size to make new ace-window frames.
Its value is a (width . height) pair in pixels or nil for the default frame size.
(0 . 0) is special and means make the frame size the same as the last selected frame size."
  :type '(cons integer integer))

(defcustom aw-char-position 'top-left
  "Window positions of the character overlay.
Consider changing this if the overlay tends to overlap with other things."
  :type '(choice
          (const :tag "top left corner only" 'top-left)
          (const :tag "both left corners" 'left)))

;; Must be defined before `aw-make-frame-char' since its :set function references this.
(defvar aw-dispatch-alist
  '((?x aw-delete-window "Delete Window")
    (?m aw-swap-window "Swap Windows")
    (?M aw-move-window "Move Window")
    (?c aw-copy-window "Copy Window")
    (?j aw-switch-buffer-in-window "Select Buffer")
    (?n aw-flip-window)
    (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
    (?e aw-execute-command-other-window "Execute Command Other Window")
    (?F aw-split-window-fair "Split Fair Window")
    (?v aw-split-window-vert "Split Vert Window")
    (?b aw-split-window-horz "Split Horz Window")
    (?o delete-other-windows "Delete Other Windows")
    (?T aw-transpose-frame "Transpose Frame")
    ;; ?i ?r ?t are used by hyperbole.el
    (?? aw-show-dispatch-help))
  "List of actions for `aw-dispatch-default'.
Each action is a list of either:
  (char function description) where function takes a single window argument
or
  (char function) where function takes no argument and the description is omitted.")

(defun aw-set-make-frame-char (option value)
  ;; Signal an error if `aw-make-frame-char' is ever set to an invalid
  ;; or conflicting value.
  (when value
    (cond ((not (characterp value))
           (user-error "`aw-make-frame-char' must be a character, not `%s'" value))
          ((memq value aw-keys)
           (user-error "`aw-make-frame-char' is `%c'; this conflicts with the same character in `aw-keys'" value))
          ((assq value aw-dispatch-alist)
           (user-error "`aw-make-frame-char' is `%c'; this conflicts with the same character in `aw-dispatch-alist'" value))))
  (set option value))

(defcustom aw-make-frame-char ?z
  "Non-existing ace window label character that triggers creation of a new single-window frame for display."
  :set 'aw-set-make-frame-char
  :type 'character)

(defface aw-leading-char-face
  '((((class color)) (:foreground "red"))
    (((background dark)) (:foreground "gray100"))
    (((background light)) (:foreground "gray0"))
    (t (:foreground "gray100" :underline nil)))
  "Face for each window's leading char.")

(defface aw-minibuffer-leading-char-face
  '((t :inherit aw-leading-char-face))
  "Face for minibuffer leading char.")

(defface aw-background-face
  '((t (:foreground "gray40")))
  "Face for whole window background during selection.")

(defface aw-mode-line-face
  '((t (:inherit mode-line-buffer-id)))
  "Face used for displaying the ace window key in the mode-line.")

(defface aw-key-face
  '((t :inherit font-lock-builtin-face))
  "Face used by `aw-show-dispatch-help'.")

;;* Implementation
(defun aw-ignored-p (window)
  "Return t if WINDOW should be ignored when choosing from the window list."
  (or (and aw-ignore-on
           ;; Ignore major-modes and buffer-names in `aw-ignored-buffers'.
           (or (memq (buffer-local-value 'major-mode (window-buffer window))
                     aw-ignored-buffers)
               (member (buffer-name (window-buffer window)) aw-ignored-buffers)))
      ;; ignore child frames
      (and (fboundp 'frame-parent) (frame-parent (window-frame window)))
      ;; Ignore selected window if `aw-ignore-current' is non-nil.
      (and aw-ignore-current
           (equal window (selected-window)))
      ;; When `ignore-window-parameters' is nil, ignore windows whose
      ;; `no-other-window’ or `no-delete-other-windows' parameter is non-nil.
      (unless ignore-window-parameters
        (cl-case this-command
          (ace-select-window (window-parameter window 'no-other-window))
          (ace-delete-window (window-parameter window 'no-delete-other-windows))
          (ace-delete-other-windows (window-parameter
                                     window 'no-delete-other-windows))))))

(defun aw-window-list ()
  "Return the list of interesting windows."
  (sort
   (cl-remove-if
    (lambda (w)
      (let ((f (window-frame w)))
        (or (not (and (frame-live-p f)
                      (frame-visible-p f)))
            (string= "initial_terminal" (terminal-name f))
            (aw-ignored-p w))))
    (cl-case aw-scope
      (visible
       (cl-mapcan #'window-list (visible-frame-list)))
      (global
       (cl-mapcan #'window-list (frame-list)))
      (frame
       (window-list))
      (t
       (error "Invalid `aw-scope': %S" aw-scope))))
   'aw-window<))

(defvar aw-overlays-back nil
  "Hold overlays for when `aw-background' is t.")

(defvar ace-window-mode nil
  "Minor mode during the selection process.")

;; register minor mode
(or (assq 'ace-window-mode minor-mode-alist)
    (nconc minor-mode-alist
           (list '(ace-window-mode ace-window-mode))))

(defvar aw-empty-buffers-list nil
  "Store the read-only empty buffers which had to be modified.
Modify them back eventually.")

(defvar aw--windows-hscroll nil
  "List of (window . hscroll-columns) items, each listing a window whose
  horizontal scroll will be restored upon ace-window action completion.")

(defvar aw--windows-points nil
  "List of (window . point) items. The point position had to be
  moved in order to display the overlay.")

(defun aw--done ()
  "Clean up mode line and overlays."
  ;; mode line
  (aw-set-mode-line nil)
  ;; background
  (mapc #'delete-overlay aw-overlays-back)
  (setq aw-overlays-back nil)
  (avy--remove-leading-chars)
  (dolist (b aw-empty-buffers-list)
    (with-current-buffer b
      (when (string= (buffer-string) " ")
        (let ((inhibit-read-only t))
          (delete-region (point-min) (point-max))))))
  (setq aw-empty-buffers-list nil)
  (aw--restore-windows-hscroll)
  (let (c)
    (while (setq c (pop aw--windows-points))
      (with-selected-window (car c)
        (goto-char (cdr c))))))

(defun aw--restore-windows-hscroll ()
  "Restore horizontal scroll of windows from `aw--windows-hscroll' list."
  (let (wnd hscroll)
    (mapc (lambda (wnd-and-hscroll)
            (setq wnd (car wnd-and-hscroll)
                  hscroll (cdr wnd-and-hscroll))
            (when (window-live-p wnd)
              (set-window-hscroll wnd hscroll)))
          aw--windows-hscroll))
  (setq aw--windows-hscroll nil))

(defun aw--overlay-str (wnd pos path)
  "Return the replacement text for an overlay in WND at POS,
accessible by typing PATH."
  (let ((old-str (or
                  (ignore-errors
                    (with-selected-window wnd
                      (buffer-substring pos (1+ pos))))
                  "")))
    (concat
     (cl-case aw-leading-char-style
       (char
        (string (avy--key-to-char (car (last path)))))
       (path
        (mapconcat
         (lambda (x) (string (avy--key-to-char x)))
         (reverse path)
         ""))
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

(defun aw--point-visible-p ()
  "Return non-nil if point is visible in the selected window.
Return nil when horizontal scrolling has moved it off screen."
  (and (>= (- (current-column) (window-hscroll)) 0)
       (< (- (current-column) (window-hscroll))
          (window-width))))

(defun aw--lead-overlay (path leaf)
  "Create an overlay using PATH at LEAF.
LEAF is (PT . WND)."
  ;; Properly adds overlay in visible region of most windows except for any one
  ;; receiving output while this function is executing, since that moves point,
  ;; potentially shifting the added overlay outside the window's visible region.
  (let ((wnd (cdr leaf))
        ;; Prevent temporary movement of point from scrolling any window.
        (scroll-margin 0))
    (with-selected-window wnd
      (when (= 0 (buffer-size))
        (push (current-buffer) aw-empty-buffers-list)
        (let ((inhibit-read-only t))
          (insert " ")))
      ;; If point is not visible due to horizontal scrolling of the
      ;; window, this next expression temporarily scrolls the window
      ;; right until point is visible, so that the leading-char can be
      ;; seen when it is inserted.  When ace-window's action finishes,
      ;; the horizontal scroll is restored by (aw--done).
      (while (and (not (aw--point-visible-p))
                  (not (zerop (window-hscroll)))
                  (progn (push (cons (selected-window) (window-hscroll)) aw--windows-hscroll) t)
                  (not (zerop (scroll-right)))))
      (let* ((ws (window-start))
             (prev nil)
             (vertical-pos (if (eq aw-char-position 'left) -1 0))
             (horizontal-pos (if (zerop (window-hscroll)) 0 (1+ (window-hscroll))))
             (old-pt (point))
             (pt
              (progn
                ;; If leading-char is to be displayed at the top-left, move
                ;; to the first visible line in the window, otherwise, move
                ;; to the last visible line.
                (move-to-window-line vertical-pos)
                (move-to-column horizontal-pos)
                ;; Find a nearby point that is not at the end-of-line but
                ;; is visible so have space for the overlay.
                (setq prev (1- (point)))
                (while (and (>= prev ws) (/= prev (point)) (eolp))
                  (setq prev (point))
                  (unless (bobp)
                    (line-move -1 t)
                    (move-to-column horizontal-pos)))
                (recenter vertical-pos)
                (point)))
             (ol (make-overlay pt (1+ pt) (window-buffer wnd))))
        (if (= (aw--face-rel-height) 1)
            (goto-char old-pt)
          (when (/= pt old-pt)
            (goto-char (+ pt 1))
            (push (cons wnd old-pt) aw--windows-points)))
        (overlay-put ol 'display (aw--overlay-str wnd pt path))
        (if (window-minibuffer-p wnd)
            (overlay-put ol 'face 'aw-minibuffer-leading-char-face)
          (overlay-put ol 'face 'aw-leading-char-face))
        (overlay-put ol 'window wnd)
        (push ol avy--overlays-lead)))))

(defvar aw--lead-overlay-fn #'aw--lead-overlay
  "Function used to display the lead chars.")

(defun aw--remove-leading-chars ()
  (avy--remove-leading-chars))

(defvar aw--remove-leading-chars-fn #'aw--remove-leading-chars
  "Function used to cleanup lead chars.")

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

(defvar aw-dispatch-function 'aw-dispatch-default
  "Function to call when a character not in `aw-keys' is pressed.")

(defvar aw-action nil
  "Function to call at the end of `aw-select'.")

(defun aw-set-mode-line (str)
  "Set mode line indicator to STR."
  (setq ace-window-mode str)
  (when (and aw-minibuffer-flag ace-window-mode)
    (message "%s" (string-trim-left str)))
  (force-mode-line-update))

(defun aw--dispatch-action (char)
  "Return item from `aw-dispatch-alist' matching CHAR."
  (assoc char aw-dispatch-alist))

(defun aw-make-frame ()
  "Make a new Emacs frame using the values of `aw-frame-size' and `aw-frame-offset'."
  (make-frame
   (delq nil
         (list
          ;; This first parameter is important because an
          ;; aw-dispatch-alist command may not want to leave this
          ;; frame with input focus.  If it is given focus, the
          ;; command may not be able to return focus to a different
          ;; frame since this is done asynchronously by the window
          ;; manager.
          '(no-focus-on-map . t)
          (when aw-frame-size
            (cons 'width
                  (if (zerop (car aw-frame-size))
                      (frame-width)
                    (car aw-frame-size))))
          (when aw-frame-size
            (cons 'height
                  (if (zerop (cdr aw-frame-size))
                      (frame-height)
                    (car aw-frame-size))))
          (cons 'left (+ (car aw-frame-offset)
                         (car (frame-position))))
          (cons 'top (+ (cdr aw-frame-offset)
                        (cdr (frame-position))))))))

(defun aw-use-frame (window)
  "Create a new frame using the contents of WINDOW.

The new frame is set to the same size as the previous frame, offset by
`aw-frame-offset' (x . y) pixels."
  (aw-switch-to-window window)
  (aw-make-frame))

(defun aw-clean-up-avy-current-path ()
  "Edit `avy-current-path' so only window label characters remain."
  ;; Remove any possible ace-window command char that may
  ;; precede the last specified window label, so
  ;; functions can use `avy-current-path' as the chosen
  ;; window label.
  (when (and (> (length avy-current-path) 0)
             (assq (aref avy-current-path 0) aw-dispatch-alist))
    (setq avy-current-path (substring avy-current-path 1))))

(defun aw-dispatch-default (char)
  "Perform an action depending on CHAR."
  (cond ((and (fboundp 'avy-mouse-event-window)
              (avy-mouse-event-window char)))
        ((= char (aref (kbd "C-g") 0))
         (throw 'done 'exit))
        ((and aw-make-frame-char (= char aw-make-frame-char))
         ;; Make a new frame and perform any action on its window.
         (let ((start-win (selected-window))
               (end-win (frame-selected-window (aw-make-frame))))
           (if aw-action
               ;; Action must be called from the start-win.  The action
               ;; determines which window to leave selected.
               (progn (select-frame-set-input-focus (window-frame start-win))
                      (funcall aw-action end-win))
             ;; Select end-win when no action
             (aw-switch-to-window end-win)))
         (throw 'done 'exit))
        (t
         (let ((action (aw--dispatch-action char)))
           (if action
               (cl-destructuring-bind (_key fn &optional description) action
                 (if (and fn description)
                     (prog1 (setq aw-action fn)
                       (aw-set-mode-line (format " Ace - %s" description)))
                   (if (commandp fn)
                       (call-interactively fn)
                     (funcall fn))
                   (throw 'done 'exit)))
             (aw-clean-up-avy-current-path)
             ;; Prevent any char from triggering an avy dispatch command.
             (let ((avy-dispatch-alist))
               (avy-handler-default char)))))))

(defcustom aw-display-mode-overlay t
  "When nil, don't display overlays. Rely on the mode line instead."
  :type 'boolean)

(defvar ace-window-display-mode)

(defun aw-select (mode-line &optional action)
  "Return a selected other window.
Amend MODE-LINE to the mode line for the duration of the selection."
  (setq aw-action action)
  (let ((start-window (selected-window))
        (next-window-scope (cl-case aw-scope
                             ('visible 'visible)
                             ('global 'visible)
                             ('frame 'frame)))
        (wnd-list (aw-window-list))
        window)
    (setq window
          (cond ((<= (length wnd-list) 1)
                 (when aw-dispatch-always
                   (setq aw-action
                         (unwind-protect
                              (catch 'done
                                (funcall aw-dispatch-function (read-char)))
                           (aw--done)))
                   (when (eq aw-action 'exit)
                     (setq aw-action nil)))
                 (or (car wnd-list) start-window))
                ((and (<= (+ (length wnd-list) (if (aw-ignored-p start-window) 1 0))
                          aw-dispatch-when-more-than)
                      (not aw-dispatch-always)
                      (not aw-ignore-current))
                 (let ((wnd (next-window nil nil next-window-scope)))
                   (while (and (or (not (memq wnd wnd-list))
                                   (aw-ignored-p wnd))
                               (not (equal wnd start-window)))
                     (setq wnd (next-window wnd nil next-window-scope)))
                   wnd))
                (t
                 (let ((candidate-list
                        (mapcar (lambda (wnd)
                                  (cons (aw-offset wnd) wnd))
                                wnd-list)))
                   (aw--make-backgrounds wnd-list)
                   (aw-set-mode-line mode-line)
                   ;; turn off helm transient map
                   (remove-hook 'post-command-hook 'helm--maybe-update-keymap)
                   (unwind-protect
                        (let* ((avy-handler-function aw-dispatch-function)
                               (avy-translate-char-function aw-translate-char-function)
                               (transient-mark-mode nil)
                               (res (avy-read (avy-tree candidate-list aw-keys)
                                              (if (and ace-window-display-mode
                                                       (null aw-display-mode-overlay))
                                                  (lambda (_path _leaf))
                                                aw--lead-overlay-fn)
                                              aw--remove-leading-chars-fn)))
                          (if (eq res 'exit)
                              (setq aw-action nil)
                            (or (cdr res)
                                start-window)))
                     (aw--done))))))
    (if aw-action
        (funcall aw-action window)
      window)))

;;* Interactive
;;;###autoload
(defun ace-select-window ()
  "Ace select window."
  (interactive)
  (aw-select " Ace - Window"
             #'aw-switch-to-window))

;;;###autoload
(defun ace-delete-window ()
  "Ace delete window."
  (interactive)
  (aw-select " Ace - Delete Window"
             #'aw-delete-window))

;;;###autoload
(defun ace-swap-window ()
  "Ace swap window."
  (interactive)
  (aw-select " Ace - Swap Window"
             #'aw-swap-window))

;;;###autoload
(defun ace-delete-other-windows ()
  "Ace delete other windows."
  (interactive)
  (aw-select " Ace - Delete Other Windows"
             #'delete-other-windows))

;;;###autoload
(defun ace-display-buffer (buffer alist)
  "Make `display-buffer' and `pop-to-buffer' select using `ace-window'.
See sample config for `display-buffer-base-action' and `display-buffer-alist':
https://github.com/abo-abo/ace-window/wiki/display-buffer."
  (let* ((aw-ignore-current (cdr (assq 'inhibit-same-window alist)))
         (rf (cdr (assq 'reusable-frames alist)))
         (aw-scope (cl-case rf
                     ((nil) 'frame)
                     (visible 'visible)
                     ((0 t) 'global))))
    (unless (or (<= (length (aw-window-list)) 1)
                (not aw-scope))
      (window--display-buffer
       buffer (aw-select "Ace - Display Buffer") 'reuse))))

(declare-function transpose-frame "ext:transpose-frame")
(defun aw-transpose-frame (w)
  "Select any window on frame and `tranpose-frame'."
  (transpose-frame (window-frame w)))

;;;###autoload
(defun ace-window (arg)
  "Select a window.
Perform an action based on ARG described below.

By default, behaves like extended `other-window'.
See `aw-scope' which extends it to work with frames.

Prefixed with one \\[universal-argument], does a swap between the
selected window and the current window, so that the selected
buffer moves to current window (and current buffer moves to
selected window).

Prefixed with two \\[universal-argument]'s, deletes the selected
window."
  (interactive "p")
  (setq avy-current-path "")
  (cl-case arg
    (0
     (let ((aw-ignore-on (not aw-ignore-on)))
       (ace-select-window)))
    (4 (ace-swap-window))
    (16 (ace-delete-window))
    (t (ace-select-window))))

;;* Utility
(unless (fboundp 'frame-position)
  (defun frame-position (&optional frame)
    (let ((pl (frame-parameter frame 'left))
          (pt (frame-parameter frame 'top)))
      (when (consp pl)
        (setq pl (eval pl)))
      (when (consp pt)
        (setq pt (eval pt)))
      (cons pl pt))))

(defun aw-window< (wnd1 wnd2)
  "Return true if WND1 is less than WND2.
This is determined by their respective window coordinates.
Windows are numbered top down, left to right."
  (let* ((f1 (window-frame wnd1))
         (f2 (window-frame wnd2))
         (e1 (window-edges wnd1))
         (e2 (window-edges wnd2))
         (p1 (frame-position f1))
         (p2 (frame-position f2))
         (nl (or (null (car p1)) (null (car p2)))))
    (cond ((and (not nl) (< (car p1) (car p2)))
           (not aw-reverse-frame-list))
          ((and (not nl) (> (car p1) (car p2)))
           aw-reverse-frame-list)
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
        (while (or (not (window-live-p
                         (setq res (ring-remove aw--window-ring 0))))
                   (equal res (selected-window))))
      (error
       (if (= (length (aw-window-list)) 2)
           (progn
             (other-window 1)
             (setq res (selected-window)))
         (error "No previous windows stored"))))
    res))

(defun aw-switch-to-window (window)
  "Switch to the window WINDOW."
  (let ((frame (window-frame window)))
    (aw--push-window (selected-window))
    (when (and (frame-live-p frame)
               (not (eq frame (selected-frame))))
      (select-frame-set-input-focus frame))
    (if (window-live-p window)
        (select-window window)
      (error "Got a dead window %S" window))))

(defun aw-flip-window ()
  "Switch to the window you were previously in."
  (interactive)
  (aw-switch-to-window (aw--pop-window)))

(defun aw-show-dispatch-help ()
  "Display action shortucts in echo area."
  (interactive)
  (message "%s" (mapconcat
                 (lambda (action)
                   (cl-destructuring-bind (key fn &optional description) action
                     (format "%s: %s"
                             (propertize
                              (char-to-string key)
                              'face 'aw-key-face)
                             (or description fn))))
                 aw-dispatch-alist
                 "\n"))
  ;; Prevent this from replacing any help display
  ;; in the minibuffer.
  (let (aw-minibuffer-flag)
    (mapc #'delete-overlay aw-overlays-back)
    (call-interactively 'ace-window)))

(defun aw-delete-window (window &optional kill-buffer)
  "Delete window WINDOW.
When KILL-BUFFER is non-nil, also kill the buffer."
  (let ((frame (window-frame window)))
    (when (and (frame-live-p frame)
               (not (eq frame (selected-frame))))
      (select-frame-set-input-focus (window-frame window)))
    (if (= 1 (length (window-list)))
        (delete-frame frame)
      (if (window-live-p window)
          (let ((buffer (window-buffer window)))
            (delete-window window)
            (when kill-buffer
              (kill-buffer buffer)))
        (error "Got a dead window %S" window)))))

(defun aw-switch-buffer-in-window (window)
  "Select buffer in WINDOW."
  (aw-switch-to-window window)
  (aw--switch-buffer))

(declare-function ivy-switch-buffer "ext:ivy")

(defun aw--switch-buffer ()
  (cond ((bound-and-true-p ivy-mode)
         (ivy-switch-buffer))
        ((bound-and-true-p ido-mode)
         (ido-switch-buffer))
        (t
         (call-interactively 'switch-to-buffer))))

(defcustom aw-swap-invert nil
  "When non-nil, the other of the two swapped windows gets the point."
  :type 'boolean)

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
        (if aw-swap-invert
            (swap-windows window this-window)
          (swap-windows this-window window))))))

(defun aw-move-window (window)
  "Move the current buffer to WINDOW.
Switch the current window to the previous buffer."
  (let ((buffer (current-buffer)))
    (switch-to-buffer (other-buffer))
    (aw-switch-to-window window)
    (switch-to-buffer buffer)))

(defun aw-copy-window (window)
  "Copy the current buffer to WINDOW - including window-start and point."
  (let ((buffer (current-buffer))
        (window-start (window-start))
        (point (point)))
    (aw-switch-to-window window)
    (switch-to-buffer buffer)
    (set-window-start (frame-selected-window) window-start)
    (goto-char point)))

(defun aw-split-window-vert (window)
  "Split WINDOW vertically."
  (select-window window)
  (split-window-vertically))

(defun aw-split-window-horz (window)
  "Split WINDOW horizontally."
  (select-window window)
  (split-window-horizontally))

(defcustom aw-fair-aspect-ratio 2
  "The aspect ratio to aim for when splitting windows.
Sizes are based on the number of characters, not pixels.
Increase to prefer wider windows, or decrease for taller windows."
  :type 'number)

(defun aw-split-window-fair (window)
  "Split WINDOW vertically or horizontally, based on its current dimensions.
Modify `aw-fair-aspect-ratio' to tweak behavior."
  (let ((w (window-body-width window))
        (h (window-body-height window)))
    (if (< (* h aw-fair-aspect-ratio) w)
        (aw-split-window-horz window)
      (aw-split-window-vert window))))

(defun aw-switch-buffer-other-window (window)
  "Switch buffer in WINDOW."
  (aw-switch-to-window window)
  (unwind-protect
      (aw--switch-buffer)
    (aw-flip-window)))

(defun aw-execute-command-other-window (window)
  "Execute a command in WINDOW."
  (aw-switch-to-window window)
  (unwind-protect
      (funcall
       (key-binding
        (read-key-sequence
         "Enter key sequence: ")))
    (aw-flip-window)))

(defun aw--face-rel-height ()
  (let ((h (face-attribute 'aw-leading-char-face :height)))
    (cond
      ((eq h 'unspecified)
       1)
      ((floatp h)
       (max (floor h) 1))
      ((integerp h)
       1)
      (t
       (error "unexpected: %s" h)))))

(defun aw-offset (window)
  "Return point in WINDOW that's closest to top left corner.
The point is writable, i.e. it's not part of space after newline."
  (let ((h (window-hscroll window))
        (beg (window-start window))
        (end (window-end window))
        (inhibit-field-text-motion t))
    (with-current-buffer (window-buffer window)
      (save-excursion
        (goto-char beg)
        (forward-line (1-
                       (min
                        (count-lines
                         (point)
                         (point-max))
                        (aw--face-rel-height))))
        (while (and (< (point) end)
                    (< (- (line-end-position)
                          (line-beginning-position))
                       h))
          (forward-line))
        (+ (point) h)))))

(defun aw--after-make-frame (f)
  (aw-update)
  (make-frame-visible f))

;;* Mode line
;;;###autoload
(define-minor-mode ace-window-display-mode
  "Minor mode for showing the ace window key in the mode line."
  :global t
  (if ace-window-display-mode
      (progn
        (aw-update)
        (set-default
         'mode-line-format
         `((ace-window-display-mode
            (:eval (window-parameter (selected-window) 'ace-window-path)))
           ,@(assq-delete-all
              'ace-window-display-mode
              (default-value 'mode-line-format))))
        (force-mode-line-update t)
        (add-hook 'window-configuration-change-hook 'aw-update)
        ;; Add at the end so does not precede select-frame call.
        (add-hook 'after-make-frame-functions #'aw--after-make-frame t))
    (set-default
     'mode-line-format
     (assq-delete-all
      'ace-window-display-mode
      (default-value 'mode-line-format)))
    (remove-hook 'window-configuration-change-hook 'aw-update)
    (remove-hook 'after-make-frame-functions 'aw--after-make-frame)))

(defun aw-update ()
  "Update ace-window-path window parameter for all windows.

Ensure all windows are labeled so the user can select a specific
one, even from the set of windows typically ignored when making a
window list."
  (let ((aw-ignore-on)
        (aw-ignore-current)
        (ignore-window-parameters t))
    (avy-traverse
     (avy-tree (aw-window-list) aw-keys)
     (lambda (path leaf)
       (set-window-parameter
        leaf 'ace-window-path
        (propertize
         (apply #'string (reverse path))
         'face 'aw-mode-line-face))))))

(provide 'ace-window)

;;; ace-window.el ends here
