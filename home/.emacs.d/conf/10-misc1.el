;; No startup screen
(setq inhibit-startup-screen t)

;; No menu-bar, tool-bar, cursor blink
(menu-bar-mode 0) (tool-bar-mode 0) (blink-cursor-mode 0) (setq visible-cursor nil)

;; Line numbers
(setq display-line-numbers-width 3)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Avoid "Symbolic link to Git-controlled ..." question
(setq vc-follow-symlinks t)

;; More GC thresh
(setq gc-cons-threshold 8000000)

(progn ;; Set theme
  (setq frame-background-mode 'dark)
  (load-theme 'wombat t)
  (custom-set-faces
   '(cursor  (( t            (:background "#ddd" :foreground "#111")                )))
   '(default (( ((type tty)) (:background "unspecified-bg")                         )))
   '(hl-line (( t            (:underline t)                                         )))

   '(tab-bar              (( ((type tty)) (:background "unspecified-bg" :foreground "#fff" :weight bold) )))
   '(tab-bar-tab          (( ((type tty)) (:background "#aa4" :foreground "#000" :weight bold) )))
   '(tab-bar-tab-inactive (( ((type tty)) (:background "unspecified-bg" :foreground "#fff" :weight bold) )))

   '(font-lock-comment-face (( ((type tty)) (:foreground "blue")          )))
   )
  )

;; "Reload config" command
(global-set-key (kbd "<f5>") 'reload-config)
(defun reload-config ()
  "Reload configuration."
  (interactive)
  (save-some-buffers t)
  (load-file user-init-file))

;; Must be here
(progn ;; don't watch mousemove events on console (to avoid tmux problem)
  (require 'xt-mouse)
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
  (setq xterm-mouse-tracking-disable-sequence "\e[?1006l\e[?1005l\e[?1000l"
        xterm-mouse-tracking-enable-sequence  "\e[?1000h\e[?1005h\e[?1006h")
  (xterm-mouse-mode 1))
