(use-package kotlin-mode
  :ensure t
  :commands (kotlin-mode)
  )

(use-package lua-mode
  :ensure t
  :commands (lua-mode)
  :config
  (defun send-cr-lua (&rest args)
    (send-cr (lua-get-create-process)))
  (advice-add 'lua-send-string :after 'send-cr-lua)
  )

(use-package raku-mode
  :ensure t
  :commands (raku-mode)
  :defer t)

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  )
