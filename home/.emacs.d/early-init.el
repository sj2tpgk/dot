(setq package-enable-at-startup nil)

;; Native comp
(setq warning-suppress-types '((comp) (comp))
      warning-suppress-log-types '((comp)))

;; Disable native comp (until 28.1 is released)
;; (setq native-comp-deferred-compilation-deny-list '("."))

;; To force compile builtin:
;; (native-compile-async (format "/usr/share/emacs/%s/lisp" emacs-version) 'recursively)

;; To force compile pkgs:
;; (setq package-native-compile t)
;; (native-compile-async (format "%s/elpa" user-emacs-directory) 'recursively)

;; Margins (inspired by NANO Emacs)
;; (window-divider-mode 1)
;; (setq-default left-margin-width  1
;;               right-margin-width 1)
