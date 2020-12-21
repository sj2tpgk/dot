(add-to-list 'load-path (concat user-emacs-directory "/lisp"))

(require 'loader)

(setq loader/list
      '(;; (file phase regexp)
        (0 "~/.emacs.d/conf/10-misc1.el")
        (0 "~/.emacs.d/conf/11-theme.el")

        (1 "~/.emacs.d/conf/20-misc2.el")
        (1 "~/.emacs.d/conf/21-package.el")

        (1  "~/.emacs.d/conf/30-misc3.el")
        (1  "~/.emacs.d/conf/31-cmds.el")
        (1  "~/.emacs.d/conf/32-hideshow.el")
        (1  "~/.emacs.d/conf/33-hippie.el")
        (1  "~/.emacs.d/conf/34-ido.el")
        (1  "~/.emacs.d/conf/35-comint.el")
        (1  "~/.emacs.d/conf/36-modeline.el")
        (1  "~/.emacs.d/conf/37-evilesc.el")

        (1  "~/.emacs.d/conf/57-org.el")

        (1  "~/.emacs.d/conf/81-fastedit.el")
        (1  "~/.emacs.d/conf/82-keys-common.el")
        ))

(loader/load)
