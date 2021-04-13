(use-package scheme
  :defer t

  :init
  (add-to-list 'auto-mode-alist '("\\.sld\\'"  . scheme-mode))
  (add-to-list 'auto-mode-alist '("\\.gosh\\'" . scheme-mode))
  (setq scheme-program-name "scheme")

  :config
  (definteractive scheme-send-buffer
    (save-excursion (scheme-send-region (point-min) (point-max))))

  (defun run-scheme-other-window (&optional prog-name)
    (interactive)
    (let ((prog (or prog-name scheme-program-name)))
      (if (get-buffer "*scheme*")
          (switch-to-buffer-other-window "*scheme*")
        (split-window-horizontally (/ (frame-width) 2))
        (other-window 1)
        (scheme-mode))
      (run-scheme prog)))

  (definteractive run-gosh-other-window
    (run-scheme-other-window "gosh"))

  (key scheme-mode-map
       "<escape> e" 'scheme-send-definition
       "<escape> b" 'scheme-send-buffer
       "<escape> r" 'run-scheme-other-window)

  (progn ;; Add scheme keywords (for syntax highlighting and indentation) {{{
    (defun my-scheme-add-keyword (level symbol &optional face)
      (let ((face (or face 'font-lock-keyword-face))
            (reg  (concat "(\\(" (symbol-name symbol) "\\)[ \n]"))
            )

        ;;            highlight group 1 in regexp ---v
        (font-lock-add-keywords 'scheme-mode `((,reg 1 ,face)))
        ;; should avoid "1 rule for 1 keyword";
        ;; (concat "(\\(" (regexp-opt keyword-list) "\\)[ \n]"))

        ;; set indent level
        (put symbol 'scheme-indent-function level)))

    (mapc (apply-partially 'apply 'my-scheme-add-keyword)
          '((1 when)
            (2 if) ;; different indent for then-clause and else-clause
            (2 let1)))
    ) ;; }}}

  (with-eval-after-load 'evil-mapcmd
    (nmap scheme-mode-map
          ",e" 'scheme-send-definition
          ",b" 'scheme-send-buffer
          ",r" 'run-scheme-other-window)
    (vmap scheme-mode-map
          ",e" 'scheme-send-region))

  (setq company-scheme-keywords
        '(define define-syntax list list-tabulate)

        company-scheme-keyword-db
        (mapcar 'symbol-name company-scheme-keywords)
        ;; (regexp-opt (mapcar 'symbol-name company-scheme-keywords))
        )

  (defun company-scheme-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-scheme-backend))
      (prefix      (company-scheme--prefix))
      (candidates  (company-scheme--candidates arg))
      (meta        (format "This value is named %s" arg))))

  (defun company-scheme--candidates (prefix)
    (let* ((regexp (concat "^"
                           (mapconcat 'string prefix "[^-]*")))
           (pred   (lambda (s) (string-match-p regexp s))))
      (remove-if-not pred company-scheme-keyword-db)))

  (defun company-scheme--prefix ()
    (let ((prefix (company-grab-symbol)))
      (or prefix 'stop)))

  (definteractive setup-company-scheme
    (setq-local company-backends '(company-scheme-backend)))
  )

(use-package racket-mode
  :ensure t
  :commands (racket-mode))

(use-package geiser
  :ensure t
  :init
  (with-eval-after-load 'geiser-impl
    (add-to-list 'geiser-active-implementations 'gauche))
  :commands (geiser)
  :config
  (use-package geiser-gauche :ensure t)
  (nmap scheme-mode-map
        ",e" 'geiser-eval-definition
        ",b" 'geiser-eval-buffer)
  )
