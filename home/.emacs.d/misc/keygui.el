
(defun desc-maps (map)
  (insert (format "-- Keymaps --\n"))
  (dolist (m map)
    (insert (format "Description: %s\n" (or (cadr m) 'none)))
    ;; (dolist (x (cddr m))
    ;;   (insert (format "%s" x)))
    ))

(defun setup ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Keys*")
    (erase-buffer)

    (insert "[ Click ]\n")
    (make-button 1 10 :type 'custom-button)

    (desc-maps (current-active-maps))
    ))

(defun button-pressed (button)
  (message (format "Press a key ..."))
  (message "Key = %s" (help--read-key-sequence))
  )

(define-button-type 'custom-button
  'action 'button-pressed
  'follow-link t
  'help-echo "Click Button"
  'help-args "test")


(setup)
(global-set-key (kbd "<f7>") 'setup)
