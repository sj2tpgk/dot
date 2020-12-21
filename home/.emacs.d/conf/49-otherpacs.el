(defmacro use (pkg commands &rest args)
  (let ((commands-clause (if commands `(:commands ,commands) nil)))
    `(use-package ,pkg :ensure t ,@commands-clause ,@args)))
(put 'use 'lisp-indent-function 2)

(use xclip () :config (xclip-mode 1))
(use avy (avy-goto-word-1))
(use expand-region (er/expand-region))
(use multiple-cursors (mc/mark-next-like-this) :config (global-set-key (kbd "C-n") 'mc/mark-next-like-this))
(use key-chord () :config (setq key-chord-two-keys-delay 0.03) (key-chord-mode 1))
(use emr (emr-show-refactor-menu) :config (setq emr-pop-help-delay 0))

(use diminish ()
  :config
  (dolist (x '((ivy-mode " Iv") (ivy-rich-mode "IvR") (counsel-mode) (company-mode " Cp") (eldoc-mode "Ed") (undo-tree-mode)))
    (diminish (car x) (cadr x))))

(use md4rd (md4rd)
  :config
  (setq md4rd--oauth-access-token
        "298191575661-C6QD4uxejaUzrIlHuUt8XUFhe1k")
  (setq md4rd--oauth-refresh-token
        "298191575661-C6QD4uxejaUzrIlHuUt8XUFhe1k")
  (setq md4rd-subs-active '(jokes emacs)))

(use rainbow-delimiters (rainbow-delimiters-mode)
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  ;; Brighten parenthesis
  (setq rainbow-delimiters-max-face-count 5)

  (defun my-set-rainbow-delim-colors ()
    (let ((colors '(;; tty             gui
                    ("red"         . "#f66"   )
                    ("yellow"      . "yellow" )
                    ("magenta"     . "#a363d5")
                    ("white"       . "gray"   )
                                        ;("green"       . "#7cc844")
                    ("cyan"        . "#4ce"   )
                                        ;("blue"        . "#33b5e1")
                    )))
      (dotimes (i rainbow-delimiters-max-face-count)
        (let* ((face   (intern (format "rainbow-delimiters-depth-%d-face" (1+ i))))
               (pair   (nth (mod i (length colors)) colors))
               (ttycol (car pair))
               (guicol (cdr pair)))
          (custom-set-faces
           `(,face ( (((type tty))     :foreground ,ttycol)
                     (((type graphic)) :foreground ,guicol))))))))

  (my-set-rainbow-delim-colors)

  ;; "brightred" is not available in GUI emacs
  )


(use highlight-symbol ()
  :init
  (setq highlight-symbol-idle-delay 0.6
        highlight-symbol-occurrence-message '(explicit)
        highlight-symbol-highlight-single-occurrence nil)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode) ;; M-n/M-p move around symbols
  (global-set-key (kbd "M-s M-r") 'highlight-symbol-query-replace))

;; (use dumb-jump ()
;;   :config
;;   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use magit (magit-status))

(use lispy ())

(require 'evil-marker-mode)
(evil-marker-mode 1)
