(add-to-list 'load-path (concat user-emacs-directory "/lisp"))
(require 'essential)
(require 'mylib)
(require 'evil-mapcmd)

(mapc (lambda (x) (load (cadr x) t t))
      '(
        (1 "~/.emacs.d/conf/20-misc2.el")
        (1 "~/.emacs.d/conf/21-package.el")

        (1  "~/.emacs.d/conf/30-misc3.el")
        (1  "~/.emacs.d/conf/31-cmds.el")
        (1  "~/.emacs.d/conf/35-comint.el")
        ;(1  "~/.emacs.d/conf/36-modeline.el")

        ;(1  "~/.emacs.d/conf/40-ivy.el")
        ;(1  "~/.emacs.d/conf/41-lsp.el")
        (1  "~/.emacs.d/conf/43-company.el")
        (1  "~/.emacs.d/conf/44-evil.el")
        (1  "~/.emacs.d/conf/45-selectrum.el")
        (1  "~/.emacs.d/conf/49-otherpacs.el")

        (1  "~/.emacs.d/conf/50-otherlangs.el")
        (1  "~/.emacs.d/conf/51-scheme.el")
        (1  "~/.emacs.d/conf/52-commonlisp.el")
        (1  "~/.emacs.d/conf/57-org.el")
        (1  "~/.emacs.d/conf/59-python.el")

        (1  "~/.emacs.d/conf/82-keys-common.el")
        (1  "~/.emacs.d/conf/84-keys-evil.el")

        (1  "~/.emacs.d/conf/90-kcui.el")
        ))
