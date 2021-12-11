;; No startup screen
(setq inhibit-startup-screen t)

;; No menu-bar, tool-bar, cursor blink
(menu-bar-mode 0) (tool-bar-mode 0) (blink-cursor-mode 0) (setq visible-cursor nil)
(defun contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a
    graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines
                       (if (display-graphic-p frame)
                           1 0)))
(add-hook 'after-make-frame-functions 'contextual-menubar)
(mapc 'contextual-menubar (frame-list))

;; Line numbers
(setq display-line-numbers-width 3
      display-line-numbers-width-start 5)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Avoid "Symbolic link to Git-controlled ..." question
(setq vc-follow-symlinks t)

;; More GC thresh
(setq gc-cons-threshold 8000000)

(progn ;; Set theme
  (setq frame-background-mode 'dark)
  (progn (add-to-list 'custom-theme-load-path "~/.emacs.d/lisp") (load-theme 'simple-dark t))
  ;; (progn (add-to-list 'custom-theme-load-path "~/.emacs.d/lisp") (load-theme 'tejr t))
  ;; (load-theme 'wombat t)
  ;; (load-theme 'modus-vivendi t)
  ;; (custom-set-faces
  ;;  '(cursor  (( t            (:background "#ddd" :foreground "#111")                )))
  ;;  '(default (( ((type tty)) (:background "unspecified-bg")                         )))
  ;;  '(hl-line (( t            (:underline t)                                         )))

  ;;  '(tab-bar              (( ((type tty)) (:background "unspecified-bg" :foreground "#fff" :weight bold) )))
  ;;  '(tab-bar-tab          (( ((type tty)) (:background "#aa4" :foreground "#000" :weight bold) )))
  ;;  '(tab-bar-tab-inactive (( ((type tty)) (:background "unspecified-bg" :foreground "#fff" :weight bold) )))

  ;;  '(font-lock-comment-face (( ((type tty)) (:foreground "blue")          )))

  ;;  '(button (( ((type tty)) (:foreground "blue") )
  ;;            ( ((type gtk)) (:foreground "#6cf") )))

  ;;  '(mode-line          (( ((type tty)) (:background "#505055" :foreground "#f6f3e8") )))
  ;;  '(mode-line-inactive (( ((type tty)) (:background "#4e4e4e" :foreground "#857b6f") )))

  ;;  '(org-code          (( t (:foreground "black" :background "green" :inverse-video nil) )))

  ;;  '(outline-1         (( t (:inherit font-lock-function-name-face :weight bold :underline t) )))
  ;;  '(outline-2         (( t (:inherit font-lock-variable-name-face :weight bold) )))
  ;;  '(outline-3         (( t (:inherit font-lock-keyword-face :weight bold) )))
  ;;  )
  )

;; "Reload config" command
(global-set-key (kbd "<f5>") 'reload-config)
(defun reload-config ()
  "Reload configuration."
  (interactive)
  (save-some-buffers t)
  (load-file user-init-file))

;; Mouse config: Must be here
(when t ;; don't watch mousemove events on console (to avoid tmux problem)
  ;; (require 'xt-mouse)
  (xterm-mouse-mode -1)

  ;; Seems like an issue with tmux + mouse tracking
  ;; If emacs is watching mousemove events on console, wheelup in any pane
  ;; will cause strange behavior (behave as if always dragging) on tmux+urxvt
  ;; (not sure if it persists on other terminal emulators)
  ;; So remove \e[?1003h from xterm-...-tracking-...-sequence so that emacs
  ;; does NOT enable xterm's mouse tracking.

  (defun xterm-mouse--tracking-sequence (suffix)
    "Return a control sequence to enable or disable mouse tracking.
SUFFIX is the last character of each escape sequence (?h to
enable, ?l to disable)."
    (mapcar
     (lambda (code) (format "\e[?%d%c" code suffix))
     `(1000 ,@(when xterm-mouse-utf-8 '(1005)) 1006))) ;; removed 1003

  ;; aur/emacs-gcc-wayland-devel-bin 28.0.50.150165-1
  ;; We must override the following two funcs.
  ;; Old xterm-mouse-tracking-enable-sequence and xterm-mouse-tracking-disable-sequence are now just overridden by the return value of the following func.
  (setq my-xterm-mouse-tracking-disable-sequence "\e[?1006l\e[?1005l\e[?1000l"
        my-xterm-mouse-tracking-enable-sequence  "\e[?1000h\e[?1005h\e[?1006h")
  (defun my-xterm-mouse-tracking-disable-sequence (&rest r)
    my-xterm-mouse-tracking-disable-sequence)
  (defun my-xterm-mouse-tracking-enable-sequence (&rest r)
    my-xterm-mouse-tracking-enable-sequence)
  (advice-add 'xterm-mouse-tracking-disable-sequence :override
              'my-xterm-mouse-tracking-disable-sequence)
  (advice-add 'xterm-mouse-tracking-enable-sequence :override
              'my-xterm-mouse-tracking-enable-sequence)

  (xterm-mouse-mode 1)

  ;; emacs-pgtk-native-comp-git 28.0.50.150006-1
  ;; on CUI wheel events does not work
  ;; (global-set-key (kbd "<mouse-4>") 'scroll-down)
  ;; does not select the window under the mouse.
  ;; Another problem: Left click on emacsclient crashes emacs
  (global-set-key (kbd "<mouse-4>")
                  (lambda ()
                    (interactive)
                    (save-excursion
                      (call-interactively 'mouse-set-point)
                      (scroll-down 10))))
  (global-set-key (kbd "<mouse-5>")
                  (lambda ()
                    (interactive)
                    (save-excursion
                      (call-interactively 'mouse-set-point)
                      (scroll-up 10))))
  )
