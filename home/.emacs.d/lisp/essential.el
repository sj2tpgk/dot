;; Notes:
;;
;; (setq var val)   "var=val" in ordinary languages
;; (progn ...)      "do{...}" or "begin...end" in ordinary languages
;; (xxx-mode 1)     enable feature xxx (features are called "minor-modes" in emacs)
;; t, nil           True and False

;; Packages installed on first run: use-package

(progn ;; Misc settings
  (setq inhibit-startup-screen t) ;; No startup screen

  (cua-mode 1) ;; Enable C-z (undo), C-x (cut), C-c (copy), C-v (paste)

  ;; (tool-bar-mode 1) ;; Enable tool bar (gtk icons)
  ;; (menu-bar-mode 1) ;; Enable menu bar (file, edit, etc.)

  (setq use-file-dialog t  ;; Use file opener of OS
        use-dialog-box  t)

  (blink-cursor-mode 0) ;; No cursor blinking

  (setq scroll-margin 3) ;; Scroll screen when cursor reaches top/bottom N lines

  (setq mouse-wheel-progressive-speed nil) ;; Disable mouse scroll acceleration

  ;; Scroll one line at a time (not half screen) (see emacswiki->SmoothScrolling)
  (setq scroll-step           1
        scroll-conservatively 10000)

  ;; Don't truncate lines (= show horizontal overflow = enable wrapping)
  (setq-default truncate-lines nil) ;; nil = wrap
  (setq truncate-partial-width-windows nil)

  ;; Highlight matching parens
  (setq show-paren-delay 0)
  (show-paren-mode 1)

  (column-number-mode 1) ;; Show column-number like in vim

  (global-auto-revert-mode 1) ;; Reload buffers when the file is modified outside Emacs

  (defalias 'yes-or-no-p 'y-or-n-p) ;; y or n instead of yes or no

  (electric-pair-mode 1) ;; Auto insert closing parens

  (add-hook 'prog-mode-hook 'hs-minor-mode) ;; Code folding

  (save-place-mode 1) ;; Remember positions in visited files

  (add-hook 'prog-mode-hook 'display-line-numbers-mode) ;; Show line numbers in the left margin

  ;; (global-hl-line-mode 1) ;; Highlight current line
  (setq indicate-empty-lines t) ;; Buffer end mark

  ;; Show number of matches in search
  (setq isearch-lazy-count       t
        lazy-count-prefix-format "%s/%s ")

  ;; Backup in .emacs.d directory
  (unless backup-directory-alist
    (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                   "backup")))))

  (setq custom-file (concat user-emacs-directory "/custom.el"))

  (setq vc-follow-symlinks t) ;; Avoid "Symbolic link to Git-controlled ..." question

  (setq max-specpdl-size 30000) ;; Number of lisp bindings
  )

(progn ;; CUI specific settings
  (xterm-mouse-mode) ;; Mouse in CUI
  (unless (display-graphic-p) (menu-bar-mode 0)) ;; No menubar in CUI
  )

(progn ;; Show trailing whitespace
  (add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
  (defun minibuffer-hide-trailing-space () (setq show-trailing-whitespace nil))
  (add-hook 'minibuffer-setup-hook 'minibuffer-hide-trailing-space))

(progn ;; Tab/space/indent settings
  ;; Show tabs as N spaces (vim's tabstop)
  (setq-default indent-tabs-mode nil)
  (setq-default default-tab-width 4))

(progn ;; Remember recently visited files (recentf)
  (recentf-mode 1) ;; enable in ivy config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items  50
        recentf-auto-cleanup    'never ;; don't remove non-existent files
        recentf-exclude         '("/recentf")))

(progn ;; Reload config command
  (defun reload-config ()
    (interactive)
    (save-some-buffers t)
    (load-file user-init-file))
  (global-set-key (kbd "<f5>") 'reload-config))

(progn ;; ido

  (ido-mode 1)
  ;; (ido-everywhere)
  (global-set-key (kbd "C-x C-f") 'ido-find-file)
  (global-set-key (kbd "C-x b")   'ido-switch-buffer)

  ;; Flexible matching
  (setq ido-enable-flex-matching t)

  ;; Display ido results vertically, rather than horizontally
  (setq ido-decorations
        '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]"
          " [Not readable]" " [Too big]" " [Confirm]"))

  ;; Use up/down key to cycle through candidates
  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-completion-map (kbd "<up>")   'ido-prev-match)
              (define-key ido-completion-map (kbd "<down>") 'ido-next-match))))

(progn ;; Tab key to complete, indent of hide code
  (define-key prog-mode-map (kbd "<tab>") 'smart-tab)
  (define-key prog-mode-map (kbd "TAB")   'smart-tab)

  ;; Better completion (hippie-expand)
  (global-set-key (kbd "M-/") 'hippie-expand)
  (setq hippie-expand-verbose nil)
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))

  (defun smart-tab (&optional oldfun &rest args)
    "Complete, indent or toggle code hiding"
    (interactive)
    (if (region-active-p)
        (progn (call-interactively 'indent-region)
               (call-interactively 'delete-trailing-whitespace))
      (if (or (= (point) (line-beginning-position))
              (looking-at "\\s(") (looking-back "\\s("))
          (hs-toggle-hiding)
        (if (looking-at "\\b")
            (call-interactively 'hippie-expand)
          (if oldfun
              (call-interactively oldfun)
            (indent-for-tab-command)))))))

(progn ;; Package
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/") t)
  (cond
   ((not (package-installed-p 'use-package))
    (package-refresh-contents)
    (package-install 'use-package))
   (t
    ;; Refresh package database every 3 days
    (let ((elpa-file (concat user-emacs-directory "/elpa/archives/gnu")))
      (when (or (not (file-exists-p elpa-file))
                (< (* 3 24 60 60) ;; 3 days * 24 hours * 60 minutes * 60 seconds
                   (float-time
                    (time-since
                     (file-attribute-modification-time
                      (file-attributes elpa-file))))))
        (package-refresh-contents t)))))
  (require 'use-package))

(use-package xclip :ensure t :config (xclip-mode))

(when nil ;; Keybinding help in header-line
  (let* ((lis '(("C-x C-c"  "Quit"          save-buffers-kill-terminal)
                ("C-x s"    "Save"          save-some-buffers)
                ("C-x C-f"  "Open"          menu-find-file-existing)
                ("C-x u"    "Undo"          undo)
                ("C-s"      "Search"        isearch-forward)
                ("M-x"      "Command"       execute-extended-command)
                ("C-x b"    "Buffers"       ido-switch-buffer)
                ("C-h b"    "Keybinds"      describe-bindings)))
         (fmt
          (cl-loop
           for l in lis
           for map = (let ((m (make-sparse-keymap)))
                       (define-key m [header-line mouse-1] (caddr l))
                       m)
           collect (propertize
                    (concat
                     (propertize (format "%s" (car l))
                                 'face '(:inverse-video t)
                                 'help-echo (format "Press %s" (car l)))
                     (propertize (format " %s " (cadr l))))
                    'mouse-face '()
                    'keymap map))))
    (setq header-line-format fmt)
    (setq-default header-line-format fmt)
    ))

(provide 'essential)
;;; essential.el ends here
