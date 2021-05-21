(use xclip :c (xclip-mode 1))
;; (use avy (avy-goto-word-1))
;; (use expand-region (er/expand-region))
;; (use multiple-cursors (mc/mark-next-like-this) :config (global-set-key (kbd "C-n") 'mc/mark-next-like-this))
(use key-chord :c (setq key-chord-two-keys-delay 0.03) (key-chord-mode 1))
(use emr (emr-show-refactor-menu) :config (setq emr-pop-help-delay 0))

(use diminish
  :i
  (mapc (lambda (x) (diminish (car x) (cadr x)))
        '((ivy-mode " Iv") (ivy-rich-mode "IvR") (counsel-mode) (company-mode " Cp") (eldoc-mode "Ed") (undo-tree-mode))))

(use rainbow-delimiters (rainbow-delimiters-mode)
  :i (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :c
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

(use highlight-symbol
  :i
  (setq highlight-symbol-idle-delay 0.6
        highlight-symbol-occurrence-message '(explicit)
        highlight-symbol-highlight-single-occurrence nil)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode) ;; M-n/M-p move around symbols
  (global-set-key (kbd "M-s M-r") 'highlight-symbol-query-replace))

;; (use dumb-jump :c (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
;; (use magit (magit-status))

(require 'motion-marker-mode) (motion-marker-mode)

(use ialign ($) :c (setq ialign-initial-repeat t)) ;; if nil, only align at first match
;; (use neotree ($))
;; (use lispy ($mode))
(use electric-operator (electric-operator-mode)
  :config
  (let ((derive (lambda (new-mode base-mode nullify rest)
                  (apply 'electric-operator-add-rules-for-mode new-mode
                         (electric-operator-get-rules-for-mode base-mode))
                  (dolist (x nullify) (electric-operator-add-rules-for-mode new-mode (cons x nil)))
                  (while rest
                    (let* ((x (pop rest)) (y (pop rest)))
                      (electric-operator-add-rules-for-mode new-mode (cons x y)))))))
    (dolist (x '(raku-mode raku-repl-mode))
      (funcall derive x 'prog-mode '("%" "/" "<")
               '("=>" " => "
                 "->" " -> ")))))

(use sr-speedbar (sr-speedbar))

;; (use embark ($act) :c (use marginalia:m))

(use eglot ($)
  :c (setq resize-mini-windows t eglot-send-changes-idle-time 2)
  (add-to-list 'eglot-server-programs '(js-mode . ("javascript-typescript-stdio"))))
