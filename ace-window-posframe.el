;;; ace-window-posframe.el --- posframe support for ace-window -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022  Free Software Foundation, Inc.

(require 'ace-window)

;; Suppress warnings
(declare-function posframe-poshandler-window-center "ext:posframe")
(declare-function posframe-show "ext:posframe")
(declare-function posframe-hide "ext:posframe")
(declare-function posframe-workable-p "ext:posframe")

(defvar aw--posframe-frames '())

(defvar aw-posframe-position-handler #'posframe-poshandler-window-center)

(defun aw--lead-overlay-posframe (path leaf)
  (let* ((wnd (cdr leaf))
         (str (format "%s" (apply #'string path)))
         ;; It's important that buffer names are not unique across
         ;; multiple invocations: posframe becomes very slow when
         ;; creating new frames, and so being able to reuse old ones
         ;; makes a huge difference. What defines "able to reuse" is
         ;; something like: a frame exists which hasn't been deleted
         ;; (with posframe-delete) and has the same configuration as
         ;; the requested new frame.
         (bufname (format " *aw-posframe-buffer-%s*" path)))
    (with-selected-window wnd
      (push bufname aw--posframe-frames)
      (posframe-show bufname
                     :string str
                     :poshandler aw-posframe-position-handler
                     :font (face-font 'aw-leading-char-face)
                     :foreground-color (face-foreground 'aw-leading-char-face nil t)
                     :background-color (face-background 'aw-leading-char-face nil t)))))

(defun aw--remove-leading-chars-posframe ()
  ;; Hide rather than delete. See aw--lead-overlay-posframe for why.
  (mapc #'posframe-hide aw--posframe-frames)
  (setq aw--posframe-frames nil))

(defun ace-window-posframe-enable ()
  (unless (and (require 'posframe nil t) (posframe-workable-p))
    (error "Posframe is not workable"))

  (setq aw--lead-overlay-fn #'aw--lead-overlay-posframe)
  (setq aw--remove-leading-chars-fn #'aw--remove-leading-chars-posframe))

(defun ace-window-posframe-disable ()
  (setq aw--lead-overlay-fn #'aw--lead-overlay)
  (setq aw--remove-leading-chars-fn #'aw--remove-leading-chars))

;;;###autoload
(define-minor-mode ace-window-posframe-mode
  "Minor mode for showing the ace window key with child frames."
  :global t
  :require 'ace-window
  :group 'ace-window
  :init-value nil
  (if ace-window-posframe-mode
      (ace-window-posframe-enable)
    (ace-window-posframe-disable)))

(provide 'ace-window-posframe)
