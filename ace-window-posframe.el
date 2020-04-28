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
         (bufname (format "*aw-posframe-buffer-%s*" path)))
    (with-selected-window wnd
      (push bufname aw--posframe-frames)
      (posframe-show bufname
                     :string str
                     :poshandler aw-posframe-position-handler
                     :font (face-font 'aw-leading-char-face)
                     :foreground-color (face-foreground 'aw-leading-char-face)
                     :background-color (face-background 'aw-leading-char-face)))))

(defun aw--remove-leading-chars-posframe ()
  ;; Hide rather than delete. See aw--lead-overlay-posframe for why.
  (map nil #'posframe-hide aw--posframe-frames)
  (setq aw--posframe-frames '()))

(defun ace-window-posframe-enable ()
  (setq aw--lead-overlay-fn #'aw--lead-overlay-posframe
        aw--remove-leading-chars-fn #'aw--remove-leading-chars-posframe))

(defun ace-window-posframe-disable ()
  (setq aw--lead-overlay-fn #'aw--lead-overlay
        aw--remove-leading-chars-fn #'aw--remove-leading))

;;;###autoload
(define-minor-mode ace-window-posframe-mode
  ""
  :global t
  :require 'ace-window
  :init-value nil
  (if ace-window-posframe-mode
      (ace-window-posframe-enable)
    (ace-window-posframe-disable)))

(provide 'ace-window-posframe)
