;; "Reload config" command
(global-set-key (kbd "<f5>") 'reload-config)
(defun reload-config ()
  "Reload configuration."
  (interactive)
  (save-some-buffers t)
  (load-file user-init-file))

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

;; Wrap line on org mode
(add-hook 'org-mode-hook (lambda()(setq truncate-lines nil)))

;; More GC thresh
(setq gc-cons-threshold 8000000)
