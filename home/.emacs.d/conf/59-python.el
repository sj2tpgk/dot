;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "--simple-prompt --pprint")

(setq python-shell-interpreter "python"
      python-shell-interpreter-args ""
      python-shell-completion-native-disabled-interpreters '("python"))

(use-package python-mode :defer t)

;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable))


;; See `send-cr' function for the comint mode issue where you can't send inputs.
