(with-eval-after-load 'comint
  (key comint-mode-map
       "C-e" 'comint-previous-matching-input-from-input
       "C-n" 'comint-next-matching-input-from-input
       ))

(defun send-cr (&optional process &rest args)
  "Send a CR control char to PROCESS or the process of the current buffer."
  (process-send-string (or process
                           (get-buffer-process (current-buffer)))
                       "\r"))

(advice-add 'comint-send-input :after 'send-cr)
