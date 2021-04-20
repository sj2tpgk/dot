(use-package selectrum
  :ensure t
  ;; (execute-extended-command describe-function describe-variable describe-key switch-to-buffer selectrum-switch-buffer+ find-file)
  ;; :defer t
  ;; :init
  ;; (setq selectrum-commands
  ;;       '(execute-extended-command describe-function describe-variable describe-key switch-to-buffer selectrum-switch-buffer+ find-file))
  ;; (defun load-selectrum (&rest args)
  ;;   (selectrum-mode 1)
  ;;   (selectrum-prescient-mode 1)
  ;;   (dolist (cmd selectrum-commands) (advice-remove cmd 'load-selectrum)))
  ;; (dolist (cmd selectrum-commands) (advice-add cmd :before 'load-selectrum))
  :config
  (selectrum-mode +1)

  (define-key selectrum-minibuffer-map (kbd "C-w") 'backward-kill-word)

  (defun selectrum-switch-buffer+ ()
    (interactive)
    (let* ((selectrum-should-sort-p nil)
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
  )

(use-package selectrum-prescient
  :ensure t
  :after selectrum
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1)
  (setq prescient-filter-method 'fuzzy)
  (recentf-mode))

(use consult)
