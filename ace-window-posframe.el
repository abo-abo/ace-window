(defvar aw--posframe-frames '())

(defun aw--lead-overlay-posframe (path leaf)
  (let* ((wnd (cdr leaf))
         (str (apply #'string path))
         (bufname (format "*aw-posframe-buffer-%s*" (gensym))))
    (with-selected-window wnd
      (push bufname aw--posframe-frames)
      (posframe-show bufname
                     :string str
                     :poshandler 'posframe-poshandler-window-center
                     :font (face-font 'aw-leading-char-face)
                     :foreground-color (face-foreground 'aw-leading-char-face)
                     :background-color (face-background 'aw-leading-char-face)))))

(defun aw--remove-leading-chars-posframe ()
  (map nil #'posframe-delete aw--posframe-frames))

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
