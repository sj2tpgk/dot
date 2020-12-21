;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(eval-when-compile (require 'use-package))

(setq use-package-verbose nil)

(advice-add 'my-package-refresh ':around 'package-install)
(defun my-package-refresh (&rest as)
  (package-refresh-contents)
  (advice-remove 'my-package-refresh 'package-install))
