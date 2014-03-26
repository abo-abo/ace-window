;;; ace-window.el --- Quickly switch windows using `ace-jump-mode'. -*- lexical-binding: t -*-

;; Copyright (C) 2014 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/ace-window
;; Version: 0.1.0
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
;; To setup this package, just add to your ~.emacs:
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

;;; Code:
(require 'ace-jump-mode)

(defgroup ace-window nil
  "Quickly switch current window."
  :group 'convenience
  :prefix "aw-")

(defcustom aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
  "Keys for selecting window.")

;;;###autoload
(defun ace-window ()
  "Use variable `ace-jump-mode' to switch windows."
  (interactive)
  (let ((visual-area-list
         (sort (ace-jump-list-visual-area)
               'aw-visual-area<)))
    (cl-case (length visual-area-list)
      (0)
      (1)
      (2
       (other-window 1))
      (t
       (let ((candidate-list
              (mapcar (lambda (va)
                        (make-aj-position
                         :offset (window-start (aj-visual-area-window va))
                         :visual-area va))
                      visual-area-list)))
         ;; make indirect buffer for those windows that show the same buffer
         (setq ace-jump-recover-visual-area-list
               (ace-jump-mode-make-indirect-buffer visual-area-list))
         ;; create background for each visual area
         (if ace-jump-mode-gray-background
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
         (ace-jump-populate-overlay-to-search-tree ace-jump-search-tree
                                                   candidate-list)
         (ace-jump-update-overlay-in-search-tree ace-jump-search-tree
                                                 aw-keys)
         (setq ace-jump-mode " Ace - Window")
         (force-mode-line-update)
         ;; override the local key map
         (setq overriding-local-map
               (let ((map (make-keymap)))
                 (dolist (key-code aw-keys)
                   (define-key map (make-string 1 key-code) 'aw-move-window))
                 (define-key map [t] 'ace-jump-done)
                 map))
         (add-hook 'mouse-leave-buffer-hook 'ace-jump-done)
         (add-hook 'kbd-macro-termination-hook 'ace-jump-done))))))

(defun aw-move-window ()
  "Switch window based on user input."
  (interactive)
  (let* ((index (let ((ret (position (aref (this-command-keys) 0)
                                     aw-keys)))
                  (if ret ret (length aw-keys))))
         (node (nth index (cdr ace-jump-search-tree))))
    (cond
      ;; we do not find key in search tree. This can happen, for
      ;; example, when there is only three selections in screen
      ;; (totally five move-keys), but user press the forth move key
      ((null node)
       (message "No such position candidate.")
       (ace-jump-done))
      ;; this is a branch node, which means there need further
      ;; selection
      ((eq (car node) 'branch)
       (let ((old-tree ace-jump-search-tree))
         ;; we use sub tree in next move, create a new root node
         ;; whose child is the sub tree nodes
         (setq ace-jump-search-tree (cons 'branch (cdr node)))
         (ace-jump-update-overlay-in-search-tree ace-jump-search-tree
                                                 aw-keys)
         ;; this is important, we need remove the subtree first before
         ;; do delete, we set the child nodes to nil
         (setf (cdr node) nil)
         (ace-jump-delete-overlay-in-search-tree old-tree)))
      ;; if the node is leaf node, this is the final one
      ((eq (car node) 'leaf)
       ;; need to save aj data, as `ace-jump-done' will clean it
       (let ((aj-data (overlay-get (cdr node) 'aj-data)))
         (ace-jump-done)
         (ace-jump-push-mark)
         (run-hooks 'ace-jump-mode-before-jump-hook)
         (aw-switch-to-window aj-data))
       (run-hooks 'ace-jump-mode-end-hook))
      (t
       (ace-jump-done)
       (error "[AceJump] Internal error: tree node type is invalid")))))

(defun aw-visual-area< (va1 va2)
  "Return true if visual area VA1 is less than VA2.
This is determined by their respective window coordinates.
Windows are numbered top down, left to right."
  (let ((e1 (window-edges (aj-visual-area-window va1)))
        (e2 (window-edges (aj-visual-area-window va2))))
    (cond ((< (car e1) (car e2))
           t)
          ((> (car e1) (car e2))
           nil)
          ((< (cadr e1) (cadr e2))
           t))))

(defun aw-switch-to-window (position)
  "Switch to window of `aj-position' structure POSITION."
  (let ((frame (aj-position-frame position))
        (window (aj-position-window position)))
    (if (and (frame-live-p frame)
             (not (eq frame (selected-frame))))
        (select-frame-set-input-focus (window-frame window)))
    (if (and (window-live-p window)
             (not (eq window (selected-window))))
        (select-window window))))

(provide 'ace-window)

;;; ace-window.el ends here
