;; -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.osh?\\'" . sh-mode))

(use kotlin-mode ($))
(use php-mode ($))
(use rust-mode ($))
(use nim-mode ($)
     :c (add-hook 'nim-mode-hook (lambda () (auto-fill-mode 0) (electric-indent-local-mode 0))))
(use pov-mode ($))

(use js (js-mode javascript-mode) :c (nmap js-mode-map "|" 'nodejs-repl-send-line ",e" 'nodejs-repl-send-line ",b" 'nodejs-repl-send-buffer))
(use nodejs-repl ($))

(use raku-mode ($ run-raku)
     :i (when (fboundp 'electric-operator-mode) ;; featurep not works
          (dolist (x '(raku-mode-hook raku-repl-mode-hook))
            (add-hook x 'electric-operator-mode))))

(use web-mode ($)
     :i (add-to-list 'auto-mode-alist '("\\.html?\\'" . $)) (add-to-list 'auto-mode-alist '("\\.svg\\'" . $))
     (add-hook 'web-mode-hook (lambda () (set (make-local-variable 'company-backends) '((company-css company-web-html company-keywords-web company-capf company-dabbrev-code company-files)))))
     :c (setq web-mode-markup-indent-offset 1 web-mode-code-indent-offset 4))

(use-package cc-mode :defer t :commands (c-mode)
  :config
  (defun my/c-config ()

    ;; C language indententation
    (setq-default c-basic-offset 4)
    (setq c-basic-offset 4)

    ;; use line comment "//"
    (c-toggle-comment-style -1)

    (define-key c-mode-map (kbd "TAB") 'smart-tab))

  (add-hook 'c-mode-common-hook 'my/c-config))

(use lua-mode ($)
     :config
     (defun send-cr-lua (&rest args)
       (send-cr (lua-get-create-process)))
     (advice-add 'lua-send-string :after 'send-cr-lua)
     (setq lua-default-application (or (executable-find "lua")
                                       (executable-find "lua5.2")))
     (defun lua-send-defun-or-line ()
       (interactive)
       (condition-case nil
           (lua-send-defun (point))
         (error
          (lua-send-current-line))))

     (nmap lua-mode-map
           "|"  'lua-send-defun-or-line
           ",e" 'lua-send-defun-or-line
           ",b" 'lua-send-buffer)
     (vmap lua-mode-map
           "|"  'lua-send-region
           ",e" 'lua-send-region))

(use markdown-mode ($))

(use tex-mode)
;; (let ((custom--inhibit-theme-enable nil))
;;   (custom-theme-set-faces
;;    'wombat
;;    '(font-latex-sedate-face      ((t (:inherit font-lock-keyword-face :weight normal))))
;;    '(font-latex-script-char-face ((t (:foreground "Red" :weight normal))))
;;    ))

(defun insert-snippet (snippet)
  "Insert snippet. | denotes the cursor position."
  (let ((l (split-string snippet "|")))
    (insert (car l))
    (save-excursion (insert (cadr l)))))
(define-key tex-mode-map (kbd "$")
  ;; already has space => utilize it
  ;; following char is in equation => only put first dollar
  ;; no additional space in line beg
  ;; double dollar $$ $$
  (lambda () (interactive)
    (cond ((looking-at-p "$$ ") (forward-char 3))
          ((looking-at-p "$ ") (if (looking-back "\\$" nil)
                                   (insert-snippet "$|$")
                                 (forward-char 2)))
          ((looking-back "\\(^\\| \\)") (insert-snippet "$|$ "))
          (t (insert-snippet " $|$ ")))))
