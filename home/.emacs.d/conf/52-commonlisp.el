(use-package slime
  :ensure t
  :commands (slime-mode)

  :init

  (setq slime-contribs        '(slime-fancy slime-company slime-autodoc)
        inferior-lisp-program "/usr/bin/sbcl")

  ;; (add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))

  (defun my/common-lisp-conf ()
    (interactive)

    ;; Indent conf
    (put 'if 'common-lisp-indent-function 2)

    ;; Eldoc
    (setq-local eldoc-documentation-function 'slime-autodoc)
    (eldoc-mode 1)
    (setq-local eldoc-documentation-function 'slime-autodoc)

    ;; Color
    (custom-set-faces
     '(slime-repl-output-face ( (((type tty))
                                 :background "unspecified-bg"
                                 :foreground "blue"
                                 ) )) )

    ;; Keymap
    (nmap lisp-mode-map
          "|"  'slime-eval-defun
          ",e" 'slime-eval-defun
          ",b" 'slime-eval-buffer)
    (vmap lisp-mode-map
          "|"  'slime-eval-region
          ",e" 'slime-eval-region)

    )

  (progn ;; syntax
    (let* ((common-lisp-builtins (append
                                  '(cons list append length)
                                  '(car cdr caar cadr cdar cddr cdddr caddr caadr cdadr)
                                  '(not and or)
                                  '(setf incf decf setq push pop elt nth getf values)
                                  ))
           ;; (tmp (mapconcat 'symbol-name common-lisp-builtins "\\|"))
           ;; (re (concat "(\\(" tmp "\\)\\_>"))
           (re (format "[^']\\_<\\(%s\\)\\_>"
                       (regexp-opt (mapcar 'symbol-name common-lisp-builtins))))
           )
      (font-lock-add-keywords 'lisp-mode `((,re 1 font-lock-builtin-face)))
      )

    (let* ((consts '("t" "nil" "[+-]?[0-9]*[./]?[0-9]+"))
           (re (concat "\\_<\\(" (mapconcat 'identity consts "\\|") "\\)\\_>")))
      (font-lock-add-keywords
       'lisp-mode `((,re 0 font-lock-constant-face))))
    )

  (add-hook 'slime-mode-hook 'my/common-lisp-conf)
  ;; (global-set-key (kbd "<f7>") 'common-lisp-mode)
  ;; (global-set-key (kbd "<f7>") 'my/common-lisp-conf)

  :config

  (slime-setup '(slime-fancy slime-company))
  (setq slime-company-completion 'simple
        slime-company-after-completion 'slime-company-just-one-space)

  (defun slime-company--fetch-candidates-simple (prefix)
    (let ((slime-current-thread t))
      (lexical-let ((package (slime-current-package))
                    (prefix prefix)
                    (prefix01 (substring prefix 0 1)))
        (cons :async (lambda (callback)
                       (lexical-let ((callback callback))
                         (slime-eval-async
                          ;; `(swank:simple-completions ,prefix ',package)
                          `(swank:fuzzy-completions ,prefix ',package)
                          (lambda (result)
                            ;; (funcall callback (car result))
                            (funcall callback
                                     (mapcar 'car
                                             (remove-if-not
                                              (lambda (l) (string-prefix-p prefix01 (car l)))
                                              (car result)))))
                          package)))))))

  (defun slime-autodoc-space-1 (n)
    "Like `slime-space' but nicer. `slime-autodoc-space' inserts 2 spaces when `company-tng-configure-default' is used."
    (interactive "p")

    ;; changed
    (unless (eq last-command 'company-complete-selection)
      (self-insert-command 1))

    (let ((doc (slime-autodoc)))
      (when doc
        (eldoc-message doc))))

  (add-hook 'slime-autodoc-mode-hook
            (lambda ()
              (eldoc-add-command 'slime-autodoc-space-1)
              (define-key slime-autodoc-mode-map (kbd "SPC") 'slime-autodoc-space-1)))

  ;; (defmacro add-one-time-hook (hook &rest body)
  ;;   (let ((f (gensym)))
  ;;     `(progn
  ;;        (defun ,f () ,@body (remove-hook ,hook ',f))
  ;;        (add-hook ,hook ',f))))

  )

(use-package slime-company
  :ensure t
  :after slime)

(use-package lisp-extra-font-lock
  :ensure t
  :after slime
  :config
  (lisp-extra-font-lock-global-mode 1)
  ;; Disable highlighting of loop due to a performance issue (長押しがもたつく)
  ;; See github.com/Lindydancer/lisp-extra-font-lock/issues/8
  (setq lisp-extra-font-lock-loop-functions nil
        lisp-extra-font-lock-loop-keywords nil
        lisp-extra-font-lock-loop-keywords-with-var nil)
  (add-to-list 'lisp-extra-font-lock-let-functions "labels")
  )
