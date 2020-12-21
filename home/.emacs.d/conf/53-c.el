(use-package cc-mode
  :defer t

  :commands (c-mode)

  :config
  (defun my/c-config ()

    ;; C language indententation
    (setq-default c-basic-offset 4)
    (setq c-basic-offset 4)

    ;; use line comment "//"
    (c-toggle-comment-style -1)

    (define-key c-mode-map (kbd "TAB") 'smart-tab))


  (add-hook 'c-mode-common-hook 'my/c-config)

  )
