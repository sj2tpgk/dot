(load           (locate-user-emacs-file "lisp/superlazy.el") nil t)
(superlazy/load (locate-user-emacs-file "init2.el"))
(load           (locate-user-emacs-file "conf/10-misc1.el") nil t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7763c0c73c795a5c6d36c84aaf448be040b15cc9faa29fdee84b6a176915c15b" "924fe2fcc9fa4b81fdbe83f4cf3af92766e501316f910d61a810e8a840cd67fc" "1010a81bf6786b59280673e59fb06b730e8f9d1eef845479a5b778e701f21108" "5e24ce3084aa4aa9052ec530be52b0998c6f765278f8b26a44c0650a8594779d" "c842d79bbde6470d5874e7893484281f7b34b910a35f8ba7ce2967cd4253c839" "28d3fe53bad1b351cae519852e722a53d1abf340ccb009dc150302455fb9dacf" "a58daf5c27b37d655593bb547a4b32dac9a8cefbf2864ee7ca2f27488000e0f1" "70b7352d145ad2b03b9c7ccd0bce5e19493cd996d61771f05d289417125de2da" "536032175147d90e4363883b1694cb36ca995a4f3bb4f7ca32c296920558d6dc" "89bfe15ed98dad8433444d5dbee93271ddcabef088a40d2d4c6960cb4e16c705" "17d68b9ea91c6d97ea7e704e0fc676e35b5c2118445b3b53cbcb7e0fd94f1bc8" default))
 '(package-selected-packages
   '(marginalia embark geiser-gauche geiser php-mode eglot undo-tree emr sr-speedbar electric-operator ctrlf neotree fvwm-mode fvwm swiper ivy web web-mode ialign raku-mode centaur-tabs evil-lispy lispy helpful python-mode lua-mode nodejs-repl inf-ruby elpy kotlin-mode kotlin evil-collection lsp-mode magit htmlize selectrum-prescient selectrum dumb-jump org-plus-contrib company-fuzzy nim-mode lisp-extra-font-lock slime-company slime racket-mode highlight-symbol rainbow-delimiters md4rd diminish key-chord multiple-cursors expand-region avy xclip evil-snipe evil-commentary evil-surround company ivy-rich use-package evil counsel))
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.office365.com")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((((type tty)) :foreground "red") (((type graphic)) :foreground "#f66")))
 '(rainbow-delimiters-depth-2-face ((((type tty)) :foreground "yellow") (((type graphic)) :foreground "yellow")))
 '(rainbow-delimiters-depth-3-face ((((type tty)) :foreground "magenta") (((type graphic)) :foreground "#a363d5")))
 '(rainbow-delimiters-depth-4-face ((((type tty)) :foreground "white") (((type graphic)) :foreground "gray")))
 '(rainbow-delimiters-depth-5-face ((((type tty)) :foreground "cyan") (((type graphic)) :foreground "#4ce")))
 '(slime-repl-output-face ((((type tty)) :background "unspecified-bg" :foreground "blue"))))
(put 'narrow-to-region 'disabled nil)
