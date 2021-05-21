(use selectrum
  :c
  (selectrum-mode +1)
  (define-key selectrum-minibuffer-map (kbd "C-w") 'backward-kill-word)
  (use selectrum-prescient
    :c
    (selectrum-prescient-mode +1)
    (prescient-persist-mode +1)
    (setq prescient-filter-method 'fuzzy)
    (recentf-mode))
  (use consult)
  (use ctrlf:m))

(defun selectrum-switch-buffer+ ()
  (interactive)
  (let* ((selectrum-should-sort   nil)
         (selectrum-should-sort-p nil) ;; old
         (candidates
          (lexical-let*
              ((all-bufs       (remove (current-buffer) (buffer-list)))
               (all-buf-names  (mapcar 'buffer-name all-bufs))
               (bufs           (remove-if (lambda (b)
                                            (string-prefix-p
                                             " " (buffer-name b)))
                                          all-bufs))
               (buf-names      (mapcar 'buffer-name bufs))
               (buf-files      (mapcar 'buffer-file-name bufs))
               (recent-files   (mapcar 'abbreviate-file-name
                                       (set-difference recentf-list
                                                       buf-files
                                                       :test 'string=)))
               (lis            (append buf-names recent-files)))
            (lambda (input)
              `((candidates . ,(if (string-prefix-p " " input) all-buf-names lis))
                (input      . ,input)))))
         (cand (selectrum--read "Switch to: " candidates)))

    (funcall (if (get-buffer cand) 'switch-to-buffer 'find-file)
             cand)))

(defun consult-line-nofuzzy ()
  (interactive)
  (let ((prescient-filter-method 'literal))
    (call-interactively 'consult-line)))

