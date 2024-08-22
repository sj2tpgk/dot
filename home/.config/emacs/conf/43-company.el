(use-package company
  :ensure t
  :commands (global-company-mode)
  :init (add-hook 'evil-normal-state-exit-hook 'global-company-mode)
  :config
  (remove-hook 'evil-normal-state-exit-hook 'global-company-mode)

  (require 'company-keywords-plus)

  ;; small delay
  ;; comp types: file, snippet, keyword
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        completion-styles  '(initials basic partial-completion)
        company-backends   '(
                             ;; company-bbdb company-eclim company-semantic company-clang company-xcode company-cmake
                             ;; company-capf
                             (company-capf :with company-files :with company-yasnippet)
                             ;; (company-dabbrev-code company-gtags company-etags company-keywords)
                             (company-dabbrev-code company-gtags company-etags company-keywords+)
                             company-dabbrev
                             company-oddmuse
                             )
        company-global-modes '(not inferior-python-mode org-mode))

  ;; kakutei kakspace next quit space
  ;; Complete on symbols (,.() etc.)
  (defun company-smart-space ()
    (interactive)
    (if (= company-selection 0) (company-abort) (company-complete-selection))
    (insert " "))

  (eldoc-add-command 'company-smart-space 'company-complete-selection 'company-abort)

  (company-tng-configure-default) ;; tab key for cycle+insert

  )

