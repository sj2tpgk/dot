;; C- keys: qwfpb;ly arstkneo dv
;; Important: ugxczmh
;; Not recognizable: (':i),./

(definteractive set-mark-line
  (beginning-of-line)
  (set-mark-command nil)
  (next-line)
  (beginning-of-line))

(definteractive set-mark-rectangle
  (rectangle-mark-mode)
  (set-mark-command nil))

(definteractive set-mark-symbol
  (if (not (region-active-p))
      (er/mark-symbol)
    (er/expand-region)))

;; Something like tmux's copy mode
(defvar region-mode-map (make-sparse-keymap))
(define-minor-mode region-mode :init-value nil
  :keymap region-mode-map :lighter nile)
(defun region-mode-on () (unless (minibufferp) (region-mode 1)))
(defun region-mode-off () (region-mode -1))
(add-hook 'activate-mark-hook   'region-mode-on)
(add-hook 'deactivate-mark-hook 'region-mode-off)
;; (dolist (h '(deactivate-mark-hook minibuffer-setup-hook minibuffer-inactive-mode-hook minibuffer-exit-hook))
;;   ;; region-mode in enabled in minibuffer for some reason, so disable on hook.
;;   ;; update: this was solved by ":init-value nil".
;;   (add-hook h 'region-mode-off))

(key region-mode-map "<escape>" 'keyboard-quit
     "k" 'backward-char "n" 'next-line "e" 'previous-line "i" 'forward-char
     "g" 'beginning-of-buffer "G" 'end-of-buffer "m" 'match-paren
     "y" 'kill-ring-save "s" 'kill-region ";" 'comment-lines
     "o" 'exchange-point-and-mark
     "l" 'string-insert-rectangle "c" 'string-rectangle
     "/" 'search-region
     "t" 'er/expand-region "-" 'er/contract-region)

(key "C-SPC" 'set-mark-command ;; default
     "C-r" 'set-mark-rectangle
     "C-v" 'set-mark-line
     "C-t" 'set-mark-symbol)

;; This approach doesn't work well with company-mode, since it shows popup on
;; self-insert-command.

;; (defmacro ifregion (&rest spec)
;;   (let (lis)
;;     (while spec
;;       (let* ((k (pop spec)) (c (eval (pop spec)))
;;              (name (intern (concat "ifreg-" (symbol-name c)))))
;;         (push `(progn
;;                  (definteractive ,name
;;                    (if (not (region-active-p))
;;                        (self-insert-command 1)
;;                      (call-interactively ',c)))
;;                  (key ,k ',name))
;;               lis)))
;;     `(progn ,@lis)))
;;
;;
;; (ifregion "k" 'backward-char "n" 'next-line "e" 'previous-line "i" 'forward-char
;;           "y" 'kill-ring-save "s" 'kill-region "o" 'exchange-point-and-mark
;;           "l" 'string-insert-rectangle "c" 'string-rectangle)
;; (ifregion "t" 'er/expand-region "-" 'er/contract-region)

(key "C-w" 'backward-kill-word
     "M-j" (lambda()(interactive)(join-line 1)) ;; vim's J
     )

(defvar modal-mode-map (make-sparse-keymap))
(define-minor-mode modal-mode "Modal editing" nil " Modal" modal-mode-map)

(key modal-mode-map "C-l" (lambda()(interactive)(modal-mode 0)))
(key "C-l" (lambda()(interactive)(modal-mode 1)))

(key modal-mode-map
     "k" 'rm-backward-char "n" 'next-line "e" 'previous-line "i" 'forward-char
     "l" 'rm-backward-word "u" 'rm-forward-word
     "m" 'rm-match-paren
     "h" 'er/expand-region
     "d" 'kill-ring-save "s" 'delete-forward-char
     )

(defmacro defregmove (&rest cmds)
  (cons 'progn
        (mapcar (lambda (cmd)
                  `(defun ,(intern (concat "rm-" (symbol-name cmd))) ()
                     (interactive)
                     (set-mark-command nil)
                     (,cmd)))
                cmds)))

(defregmove backward-char forward-word backward-word match-paren)

(key "<prior>" (lambda()(interactive)(previous-line(/(window-height)2)))
     "<next>"  (lambda()(interactive)(next-line    (/(window-height)2))))
