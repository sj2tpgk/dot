;; (use-package eglot
;;   :ensure t
;;   :init
;;   ;; (add-hook 'shell-mode-hook 'eglot-ensure)
;;   ;; (add-hook 'python-mode-hook 'eglot-ensure)
;;   ;; (add-hook 'js-mode-hook 'eglot-ensure)
;;   ;; (add-to-list 'eglot-server-programs '(java-mode . ("jdtls")))
;;   )

;; (use-package lsp-java :ensure t)
;; ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")

(use-package lsp-mode :ensure t :commands lsp
  :config
  (setq lsp-idle-delay 3)
  (setq flymake-no-changes-timeout 3)
  (setq flycheck-idle-change-delay 3)
  (setq lsp-keymap-prefix "C-f")
  )

;; ;; optionally
;; (use-package lsp-ui :ensure t :commands lsp-ui-mode)
;; ;; if you are helm user
;; ;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;; (use-package lsp-ivy :ensure t :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)

;; ;; optionally if you want to use debugger
;; (use-package dap-mode :ensure t)
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; ;; optional if you want which-key integration
;; ;; (use-package which-key :config (which-key-mode))
