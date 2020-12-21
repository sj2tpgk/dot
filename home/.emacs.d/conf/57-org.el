(use-package org
  :commands (org-mode)

  :init
  (defun orgmode-local-settings ()
    "Set truncate-lines and evil-auto-indent to nil."
    (when (eq major-mode 'org-mode)
      (setq-local truncate-lines nil)
      (org-indent-mode 1)
      (electric-pair-local-mode -1)
      ;; (electric-indent-local-mode -1)
      ;; (setq indent-region-function nil)
      ;; (when (featurep 'evil)
      ;;   (setq-local evil-auto-indent nil))
      ))

  :config

  (setq system-time-locale "C")
  (define-skeleton org-skeleton-block
    "Insert new org-mode block."
    "Block name: "
    '(unless (string-blank-p (thing-at-point 'line t))
       (move-end-of-line 1) (newline))
    "#+begin_" str | "src" " " _ \n
    "#+end_"   str | "src")

  (add-hook 'org-mode-hook 'orgmode-local-settings)
  (orgmode-local-settings)

  (defun org-smart-meta-return (&optional arg)
    "Like `org-meta-return', but don't split current item."
    (interactive "P")
    (org-check-before-invisible-edit 'insert)
    (or (run-hook-with-args-until-success 'org-metareturn-hook)
        (call-interactively
         (cond (arg              'org-insert-heading)
               ((org-at-table-p) 'org-table-wrap-region)
               ((org-in-item-p)  (end-of-line) 'org-insert-item)
               (t                'org-insert-heading)))))

  (define-key org-mode-map (kbd "M-RET") 'org-smart-meta-return)
  (define-key org-mode-map (kbd "RET") (lambda()(interactive)(org-return 1)))

  (defun insert-snippet (snippet)
    "Insert snippet. | denotes the cursor position."
    (let ((l (split-string snippet "|")))
      (insert (car l))
      (save-excursion (insert (cadr l)))))

  (define-key org-mode-map (kbd "$")
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

  (define-prefix-command 'latex-snippet-map)
  (define-key org-mode-map (kbd "C-q") latex-snippet-map)
  (define-key latex-snippet-map (kbd "C-b") 'org-skeleton-block)

  (defmacro dfltxsnp (key snippet)
    "Define a latex snippet."
    `(define-key latex-snippet-map (kbd ,key) (lambda () (interactive) (insert-snippet ,snippet))))

  ;; Snippet definitions
  (mapc (lambda (x)
          (eval `(define-key latex-snippet-map (kbd ,(car x))
                   (lambda () (interactive) (insert-snippet ,(cadr x))))))

        ;; key    snippet
        '(("a"   "\\begin{align*}\n|\n\\end{align*}")
          ("e"   "\\[
  |
  \\]")
          ("p"   "\\begin{pmatrix}\n|\n\\end{pmatrix}")
          ("v"   "\\begin{vmatrix}\n|\n\\end{vmatrix}")
          ("x"   "\\frac{\\partial|}{\\partial}")
          ("h"   "\\hspace{|mm}") ; for inline math preview in org-mode
          ("s"   "\\sqrt{|}")
          ("f"   "\\frac{|}{}")
          ("^"   "^{|}")
          ("."   "\\cdots")
          ("i"   "\\sum_{|}^{}{}")
          ("I"   "\\int_{|}^{}{}")

          ("bf"  "\\mathbf{|}")
          ("t"   "\\textrm{|}")

          ;; Blackboard
          ("n"   "\\mathbb{N}|")
          ("z"   "\\mathbb{Z}|")
          ("q"   "\\mathbb{Q}|")
          ("r"   "\\mathbb{R}|")
          ("c"   "\\mathbb{C}|")
          ;; Bold
          ("bn"  "\\mathrm{N}|")
          ("bz"  "\\mathrm{Z}|")
          ("bq"  "\\mathrm{Q}|")
          ("br"  "\\mathrm{R}|")
          ("bc"  "\\mathrm{C}|")
          ;; Greek letters
          ("ga"  "\\alpha|")
          ("gb"  "\\beta|")
          ("gg"  "\\gamma|")
          ("gd"  "\\delta|")
          ("ge"  "\\varepsilon|")
          ("gt"  "\\theta|")
          ("gl"  "\\lambda|")
          ("gp"  "\\varphi|")
          ("go"  "\\omega|")
          ("gq"  "\\psi|")
          ))

  )

(use-package ox
  :after org
  :config
  (setq org-export-allow-bind-keywords t
        org-confirm-babel-evaluate nil)
  (require 'ox-texinfo)
  )
;; todo no indent
;; todo hydra bindings
'(
  org-insert-heading-respect-content
  org-insert-todo-heading-respect-content
  org-list-insert-item

  )
