;; Not sure what this is (scroll speedup?)
(setq auto-window-vscroll nil)

;; Vim's scrolloff
(setq scroll-margin 3)

;; Disable mouse scroll acceleration
(setq mouse-wheel-progressive-speed nil)

;; Make mouse scroll speed faster
(setq mouse-wheel-scroll-amount '(6
                                  ((shift)   . hscroll)
                                  ((meta))
                                  ((control) . text-scale)))

;; Scroll one line at a time (not half screen) (see emacswiki->SmoothScrolling)
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; Don't truncate lines (= show horizontal overflow = enable wrapping)
(setq-default truncate-lines nil) ;; nil = wrap
(setq truncate-partial-width-windows nil)

;; Backup files in one directory (like vim's view file)
(let ((backup-directory (concat user-emacs-directory "/swap")))
  (setq backup-directory-alist `((".*" . ,backup-directory))
        auto-save-file-name-transforms `((".*" ,backup-directory t))))

;; Show column-number like vim
(column-number-mode 1)

;; Hilight matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Auto-revert-mode
;; Reload buffers when the file is modified outside Emacs. Dropbox syncing.
(global-auto-revert-mode 1)

;; y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Switch to help window auto (see evil-smart-close-help-window)
(setq help-window-select nil)

(progn ;; Tab/space/indent settings

  ;; Vim's tabstop (show tabs as N spaces)
  ;; (setq tab-width 8) is buffer-local setting.
  (setq-default indent-tabs-mode nil)
  (setq-default default-tab-width 4))

(progn ;; Prevent simultaneous editing
  ;; Enable read-only mode when another emacs instance is editing the file you are about to open.
  (defun my-no-simul-editing ()
    (when (file-locked-p (buffer-file-name))
      (read-only-mode)
      (message "Enabled read-only mode to prevent simultaneous editing.")))
  (add-hook 'find-file-hook 'my-no-simul-editing))

(progn ;; recentf
  ;; (recentf-mode 1) ;; enable in ivy config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items  50
        recentf-auto-cleanup    'never ;; don't remove non-existent files
        recentf-exclude         '("/recentf")))

(progn ;; Tabs (emacs 27)
  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-tab-name-truncated-max 24
        tab-bar-tab-name-ellipsis ".."
        tab-bar-separator " "
        tab-bar-new-tab-choice "*scratch*")
  (fset tab-bar-tab-name-function
        (lambda ()
          (let ((tab-name (tab-bar-tab-name-truncated)))
            (format " %s " tab-name))))
  (defun add-tab-scroll-bindings (lis) ;; lis = (keymap x1 x2 ...)
    `(keymap (mouse-4 . tab-previous)
             (mouse-5 . tab-next)
             ,@(cdr lis)))
  (advice-add 'tab-bar-make-keymap-1 :filter-return 'add-tab-scroll-bindings))

(progn ;; xref TODO grep

  (setq tags-revert-without-query t) ;; no confirm on reloading TAGS

  ;;      tags .git none
  ;; gd   nil  ask  ask
  ;; sav  t    t    nil
  ;; int  ask  ask  ask

  (defun auto-create-tags (&optional will-create?)
    (let* ((tags-default-dir (or (locate-dominating-file
                                  default-directory
                                  (lambda (d)
                                    (find-if (lambda (x)
                                               (file-exists-p (concat d x)))
                                             '("TAGS" ".git"))))
                                 default-directory))
           (tags-dir (funcall will-create?
                              (lambda ()
                                (let* ((d (abbreviate-file-name
                                           (expand-file-name tags-default-dir)))
                                       (res (read-char-from-minibuffer
                                             (format "Create TAGS in %s? (y/n/[o]ther):" d)
                                             '(?y ?n ?o))))
                                  (if (eq res ?o)
                                      (read-directory-name "Directory: " d nil t)
                                    (if (eq res ?y) d nil)))))))
      (when tags-dir
        (let ((tags-path (concat tags-dir "/TAGS"))
              (default-directory tags-dir))

          (message default-directory)
          ;; (when will-create
          ;;   ;; (message default-directory)
          ;;   (call-process "ls" nil 0 nil "-Reo" "TAGS")
          ;;   ;; (shell-command "ctags -Reo TAGS . >/dev/null 2>&1")
          ;;   )

          ;; (when (file-exists-p "TAGS")
          ;;   (visit-tags-table "TAGS"))
          ))))

  (defun tags-default-dir ()
    (or (locate-dominating-file
         default-directory
         (lambda (d)
           (find-if (lambda (x)
                      (file-exists-p (concat d x)))
                    '("TAGS" ".git"))))
        default-directory))

  (defun tags-ask-dir (default-dir)
    (read-directory-name "Create TAGS in: " default-dir nil t))

  (defun tags-create-in (dir filename)
    (let ((default-directory dir))
      ;; (shell-command "ctags -Reo TAGS . >/dev/null 2>&1")
      (call-process "ctags" nil 0 nil "-Reo" filename) ;; silent
      ))

  (defun auto-create-tags-on-xref (&rest args)
    (let ((d (tags-default-dir)))
      (if (file-exists-p (concat d "TAGS"))
          (unless tags-file-name
            (visit-tags-table (concat d "TAGS")))
        (when (file-exists-p (concat d ".git"))
          (when-let (d2 (tags-ask-dir d))
            (tags-create-in d2 "TAGS")
            (visit-tags-table (concat d2 "TAGS")))))))

  (defun auto-create-tags-on-save (&rest args)
    (let ((d (tags-default-dir)))
      (when (file-exists-p (concat d "TAGS"))
        (tags-create-in d "TAGS")
        (visit-tags-table (concat d "TAGS")))))

  (defun auto-create-tags-interactive (&rest args)
    (interactive)
    (when-let (d (tags-ask-dir (tags-default-dir)))
      (tags-create-in d "TAGS")
      (visit-tags-table (concat d "TAGS"))))

  (advice-add 'xref-find-definitions :before 'auto-create-tags-on-xref)
  (add-hook 'write-file-functions 'auto-create-tags-on-save)

  )

(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer, end-of-buffer and
\"End/Beginning of history\" signals; pass the rest to the default handler."
  (unless (or (memq (car data) '(buffer-read-only
                                 beginning-of-buffer
                                 end-of-buffer))
                                        ;(string-prefix-p "End of history"       (cadr data))
                                        ;(string-prefix-p "Beginning of history" (cadr data))
              )
    (command-error-default-function data context caller)))

(setq command-error-function 'my-command-error-function)

(when (and (display-graphic-p)
           (not (and (require 'server)
                     (fboundp 'server-running-p)
                     (server-running-p))))
  (server-start))

;; Don't colorize colors in css
(setq css-fontify-colors nil)

;; Auto insert closing parens
(electric-pair-mode 1)

