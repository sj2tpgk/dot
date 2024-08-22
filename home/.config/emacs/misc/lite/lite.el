(progn ;; Boost startup

  ;; GC threshold
  (setq gc-cons-threshold 256000000) ;; 256MB can be used without GC
  (add-hook 'after-init-hook ;; Restore after startup
            (lambda () (setq gc-cons-threshold 800000)))

  ;; file-name-handler-alist
  (setq lite/file-name-handler-alist-save file-name-handler-alist)
  (setq file-name-handler-alist nil)
  (add-hook 'after-init-hook ;; Restore after startup
            (lambda () (setq file-name-handler-alist
                             lite/file-name-handler-alist-save)))
  )

(defvar lite/enable-external t
  "If non-nil, do package.el initialization and load following packages:
xclip ido-completing-read+ hydra undo-tree")

(defvar lite/enable-modal t
  "If non-nil, enable modal editing mode.")

(when lite/enable-external ;; package
  (require 'package)
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
    ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
    ;; and `package-pinned-packages`. Most users will not need or want to do this.
    ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
    )
  (package-initialize)

  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  )

(defmacro definteractive (name &optional docstring &rest body)
  (if (stringp docstring)
      `(defun ,name ()
         ,docstring
         (interactive)
         ,@body)
    `(defun ,name ()
       (interactive)
       ,@(cons docstring body))))

(progn ;; 1
  ;; "Reload config" command
  (global-set-key (kbd "<f5>") 'reload-conf)
  (definteractive reload-conf
    "Reload configuration."
    (save-some-buffers t)
    (load-file "~/lite/lite.el"))

  ;; No startup screen
  (setq inhibit-startup-screen t)

  ;; No menubar in CUI (for X, see .Xresources)
  (unless (display-graphic-p) (menu-bar-mode 0))

  ;; Tell emacs dark bg color is used (in CUI)
  (setq frame-background-mode 'dark)

  ;; No cursor blink
  (blink-cursor-mode 0)

  ;; Avoid "Symbolic link to Git-controlled ..." question
  (setq vc-follow-symlinks t)

  ;; Must be here
  (xterm-mouse-mode)
  )

(progn ;; 2
  (progn ;; Show trailing whitespaces {{{
    (setq-default show-trailing-whitespace t)
    (custom-set-faces
     '(trailing-whitespace ( (((type tty)) :underline t) )))
    ) ;; }}}
  )

(progn ;; 3
  ;; Vim's scrolloff
  (setq scroll-margin 3)

  ;; Disable mouse scroll acceleration
  (setq mouse-wheel-progressive-speed nil)

  ;; Scroll one line at a time (not half screen) (see emacswiki->SmoothScrolling)
  (setq scroll-step 1)
  (setq scroll-conservatively 10000)

  ;; Truncate lines (= show horizontal overflow = enable wrapping)
  ;; nil = wrap
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
  (custom-set-faces
   '(show-paren-match-expression
     ((t (:inverse-video t)))))


  ;; Auto-revert-mode
  ;; Reload buffers when the file is modified outside Emacs. Dropbox syncing.
  (global-auto-revert-mode 1)

  ;; y or n instead of yes or no
  (defalias 'yes-or-no-p 'y-or-n-p)

  (progn ;; Tab/space/indent settings {{{

    ;; Vim's tabstop (show tabs as N spaces)
    ;; (setq tab-width 8) is buffer-local setting.
    (setq-default indent-tabs-mode nil)
    (setq-default default-tab-width 4)

    ;; The offset uned by > and <
    (setq-default evil-shift-width 4)

    ) ; }}}

  (progn ;; Prevent simultaneous editing
    ;; Enable read-only mode when another emacs instance is editing the file you are about to open.
    (defun my-no-simul-editing ()
      (when (file-locked-p (buffer-file-name))
        (read-only-mode)
        (message "Enabled read-only mode to prevent simultaneous editing.")))
    (add-hook 'find-file-hook 'my-no-simul-editing)
    ) ;;

  )

(when lite/enable-external ;; xclip.el
  (package-install 'xclip)
  (require 'xclip)
  (xclip-mode 1)
  ;; (add-to-list 'load-path "~/light/")
  )

(progn ;; hideshow

  (dolist (hook '(lisp-interaction-mode-hook
                  emacs-lisp-mode-hook
                  common-lisp-mode-hook
                  lisp-mode-hook
                  scheme-mode-hook
                  js-mode-hook))
    (add-hook hook 'lite/hs-enable))

  (defun lite/hideshow-p () (and (boundp 'hs-minor-mode) hs-minor-mode))

  (definteractive lite/hs-enable
    "Enable hideshow and fold all foldings."
    (hs-minor-mode 1)
    (hs-hide-all))

  (definteractive lite/hs-show-and-forward
    (when (and (lite/hideshow-p) (= (point) (line-beginning-position)) (hs-already-hidden-p))
      (condition-case e (hs-show-block) (forward-char)))
    (forward-char))

  (definteractive lite/hs-hide-and-backward
    (when (and (lite/hideshow-p) (= (point) (line-beginning-position)))
      (condition-case e (hs-hide-block) (backward-char)))
    (backward-char))

  (definteractive lite/hs-toggle
    (cond
     ((eq major-mode 'org-mode) (org-cycle))
     ((lite/hideshow-p)         (hs-toggle-hiding))
     (t                         (lite/hs-enable))))

  (setq lite/hs-level     0
        lite/hs-level-max 4)

  (definteractive lite/inc-hs-hide-level
    (unless (lite/hideshow-p) (lite/hs-enable))
    (setq lite/hs-level (mod (1+ lite/hs-level)
                             (1+ lite/hs-level-max)))
    (hs-hide-level lite/hs-level))

  )

(progn ;; hippie-expand

  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          ;; try-expand-list
          ;; try-expand-line
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          ))

  (setq hippie-expand-verbose nil)

  )

(progn ;; ido

  (ido-mode 1)
  (ido-everywhere)

  ;; Flexible matching
  (setq ido-enable-flex-matching t)

  ;; Display ido results vertically, rather than horizontally
  (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
  ;; (with-eval-after-load 'ido
  ;;   (define-key ido-completion-map (kbd "<up>")   'ido-prev-match)
  ;;   (define-key ido-completion-map (kbd "<down>") 'ido-next-match))

  ;; ido-ubiquitous
  (when lite/enable-external
    (package-install 'ido-completing-read+)
    (require 'ido-completing-read+)
    (ido-ubiquitous-mode 1))

  )

(progn ;; org
  )

(progn ;; flymake
  ;; (add-hook 'prog-mode-hook 'flymake-mode-on)
  )

(progn ;; winner mode
  (winner-mode 1)
  )

(progn ;; Keys

  (definteractive lite/smart-tab
    "Complete, toggle folding or indent."
    (cond
     ((minibufferp)      (or (minibuffer-complete) (hippie-expand 1)))
     ((looking-at "\\>") (hippie-expand 1))
     (t                  (indent-for-tab-command) (lite/inc-hs-hide-level))))

  (definteractive lite/beautify-buffer
    "Indent whole buffer and delete trailing whitespaces."
    (delete-trailing-whitespace (point-min) (point-max))
    (indent-region (point-min) (point-max)))

  (global-set-key (kbd "TAB") 'lite/smart-tab)

  )

(when lite/enable-modal
  (unless lite/enable-external
    (error "Enable `lite/enable-external' in addition to `lite/enable-modal' to use modal editing."))
  (load-file "~/lite/modal.el"))

(progn
  (require 'server)
  (unless (server-running-p)
    (server-start)))

;; speedbar, ibuffer
;; snippet
;; winner
;; theme
