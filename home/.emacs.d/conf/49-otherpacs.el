;; -*- lexical-binding: t; -*-

;; (use undo-fu)
;; (setq evil-undo-system 'undo-fu)
;; (nmap "U" 'evil-redo)
;; (use undo-tree :c (global$mode))
;; (setq evil-undo-system 'undo-tree)
;; (require 'undo-redo)

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
    (let ((colors '(;; tty      fullcolor     gui
                    ("red"      "#f66"       "#f66"   )
                    ("yellow"   "yellow"     "yellow" )
                    ("magenta"  "#a363d5"    "#a363d5")
                    ("white"    "gray"       "gray"   )
                    ("cyan"     "#4ce"       "#4ce"   )
                    )))
      (dotimes (i rainbow-delimiters-max-face-count)
        (let* ((face   (intern (format "rainbow-delimiters-depth-%d-face" (1+ i))))
               (entry  (nth (mod i (length colors)) colors))
               (ttycol (nth 0 entry))
               (fulcol (nth 1 entry))
               (guicol (nth 2 entry)))
          (custom-set-faces
           `(,face (;; order is important
                    (((type tty) (min-colors 16777214)) :foreground ,fulcol)
                    (((type tty))                       :foreground ,ttycol)
                    (((type graphic))                   :foreground ,guicol))))))))

  (my-set-rainbow-delim-colors)

  ;; "brightred" is not available in GUI emacs
  )

(use highlight-symbol
    :i
  (setq highlight-symbol-idle-delay 0.6
        highlight-symbol-occurrence-message '(explicit)
        highlight-symbol-highlight-single-occurrence nil)
  (setq list-matching-lines-default-context-lines 1)
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

(use sr-speedbar (sr-speedbar)
  :c (setq $right-side t))

(use eglot ($)
  :i (dolist (h nil) ;; '(js-mode-hook c-mode-hook)
       (add-hook h 'eglot-ensure))
  :c
  (setq resize-mini-windows t eglot-send-changes-idle-time 2)
  (add-to-list 'eglot-server-programs '(js-mode . ("javascript-typescript-stdio"))))

(use embark ($act) :c (use marginalia:m) (setq $prompter '$completing-read-prompter) (define-key $symbol-map (kbd "r") 'emr-show-refactor-menu))
;; (setq embark-prompter 'embark-completing-read-prompter2)
;; (setq embark-prompter 'embark-completing-read-prompter)


;; (defun embark-completing-read-prompter2 (keymap &optional no-default)
;;   "Prompt via completion for a command bound in KEYMAP.
;; If NO-DEFAULT is t, no default value is passed to `completing-read'."
;;   (let* ((commands
;;           (cl-loop for (key . cmd) in (embark--all-bindings keymap)
;;                    for name = (embark--command-name cmd)
;;                    unless (or
;;                            ;; skip which-key pseudo keys and other invalid pairs
;;                            (and (consp cmd) (not (stringp (car cmd))))
;;                            (eq cmd #'embark-keymap-help))
;;                    collect (list name
;;                                  (if (and (consp cmd) (stringp (car cmd)))
;;                                      (cdr cmd)
;;                                    cmd)
;;                                  key
;;                                  (concat (key-description key)))))
;;          (width (cl-loop for (_name _cmd _key desc) in commands
;;                          maximize (length desc)))
;;          (def)
;;          (candidates
;;           (cl-loop for item in commands
;;                    for (name cmd key desc) = item
;;                    for formatted =
;;                    (propertize
;;                     (concat (propertize desc 'face 'embark-keybinding)
;;                             (make-string (- width (length desc) -1) ? )
;;                             name)
;;                     'embark-command cmd)
;;                    when (and (not no-default) (equal key [13]))
;;                    do (setq def formatted)
;;                    collect (cons formatted item)))
;;          (retlist))
;;     (cl-block block1
;;       (pcase
;;           (assoc
;;            (minibuffer-with-setup-hook
;;                (lambda ()
;;                  (when embark-keymap-prompter-key
;;                    (use-local-map
;;                     (make-composed-keymap
;;                      (let ((map (make-sparse-keymap)))
;;                        ;; (with-current-buffer "*scratch*"
;;                        ;;   (insert (format "%s" candidates)))
;;                        (cl-loop for cand in candidates
;;                                 for (_formatted name cmd key desc) in candidates
;;                                 when (string-match-p "refactor" name)
;;                                 do
;;                                 ;; (with-current-buffer "*scratch*"
;;                                 ;;      (insert (format "%s" (list name cmd key desc))))
;;                                 ;; quote and eval!!!!!!!!!!!!!!!
;;                                 (define-key map key
;;                                   (eval
;;                                    `(lambda ()
;;                                       (interactive)
;;                                       ;; (with-current-buffer "*scratch*"
;;                                       ;;   (insert (format "%s" ',(list name cmd key desc))))
;;                                       ;; (setq retlist ',(list cmd key))
;;                                       ;; (abort-recursive-edit )
;;                                       ;; (cl-return-from block1)
;;                                       ;; (insert (format "%s" ',name))
;;                                       ;; (selectrum-set-selected-candidate "embark-insert")
;;                                       ;; (add-hook 'pre-command-hook 'selectrum-select-current-candidate nil t)
;;                                       (selectrum-select-current-candidate)
;;                                       (timer )
;;                                       ;; (setq last-command-event (seq-elt key (1- (length key))))
;;                                       ;; (cl-return-from block1 cmd)
;;                                       ;; (cl-return-from block1 ',(cons 1 item))
;;                                       ))))
;;                        (define-key map embark-keymap-prompter-key
;;                          (lambda ()
;;                            (interactive)
;;                            (let*
;;                                ((desc
;;                                  (let ((overriding-terminal-local-map keymap))
;;                                    (key-description
;;                                     (read-key-sequence "Key:"))))
;;                                 (cand
;;                                  (cl-loop
;;                                   for (cand _n _c _k desc1) in candidates
;;                                   when (equal desc desc1) return cand)))
;;                              (if (null cand)
;;                                  (user-error "Unknown key")
;;                                (delete-minibuffer-contents)
;;                                (insert cand)
;;                                (add-hook 'post-command-hook
;;                                          #'exit-minibuffer nil t)))))
;;                        map)
;;                      (current-local-map)))))
;;              (completing-read
;;               "Command: "
;;               (lambda (string predicate action)
;;                 (if (eq action 'metadata)
;;                     `(metadata (category . embark-keybinding))
;;                   (complete-with-action action candidates string predicate)))
;;               nil 'require-match nil 'embark--prompter-history def))
;;            candidates)
;;         (`(,_formatted ,_name ,cmd ,key ,_desc)
;;          (setq retlist (list cmd key))
;;          ;; (setq last-command-event (seq-elt key (1- (length key))))
;;          ;; cmd
;;          )))
;;     (setq last-command-event (seq-elt (cadr retlist) (1- (length (cadr retlist)))))
;;     (car retlist)
;;     ))
