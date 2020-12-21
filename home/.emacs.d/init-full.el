(add-to-list 'load-path (concat user-emacs-directory "/lisp"))

(require 'loader)

(setq loader/list
      '(;; (phase file)
        (0 "~/.emacs.d/conf/10-misc1.el")
        (0 "~/.emacs.d/conf/11-theme.el")

        (1 "~/.emacs.d/conf/20-misc2.el")
        (1 "~/.emacs.d/conf/21-package.el")

        (1  "~/.emacs.d/conf/30-misc3.el")
        (1  "~/.emacs.d/conf/31-cmds.el")
        (1  "~/.emacs.d/conf/32-hideshow.el")
        (1  "~/.emacs.d/conf/33-hippie.el")
        ;(1  "~/.emacs.d/conf/34-ido.el")
        (1  "~/.emacs.d/conf/35-comint.el")
        (1  "~/.emacs.d/conf/36-modeline.el")
        (1  "~/.emacs.d/conf/37-evilesc.el")

        (1  "~/.emacs.d/conf/40-ivy.el")
        (1  "~/.emacs.d/conf/41-lsp.el")
        ;(1  "~/.emacs.d/conf/42-skk.el")
        (1  "~/.emacs.d/conf/43-company.el")
        (1  "~/.emacs.d/conf/44-evil.el")
        (1  "~/.emacs.d/conf/45-selectrum.el")
        (1  "~/.emacs.d/conf/49-otherpacs.el")

        (1  "~/.emacs.d/conf/51-scheme.el")
        (1  "~/.emacs.d/conf/52-commonlisp.el")
        (1  "~/.emacs.d/conf/53-c.el")
        ;(1  "~/.emacs.d/conf/55-rust.el")
        (1  "~/.emacs.d/conf/56-nim.el")
        (1  "~/.emacs.d/conf/57-org.el")
        ;(1  "~/.emacs.d/conf/58-povray.el")
        (1  "~/.emacs.d/conf/59-python.el")
        (1  "~/.emacs.d/conf/60-otherlang.el")

        ;; (1  "~/.emacs.d/conf/81-fastedit.el")
        (1  "~/.emacs.d/conf/82-keys-common.el")
        ;; (1  "~/.emacs.d/conf/83-keys-noevil.el")
        (1  "~/.emacs.d/conf/84-keys-evil.el")
        ))

(loader/load)

