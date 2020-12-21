(use-package evil
  :ensure t

  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil) ;; set nil when using evil-collection

  :config
  ;; (evil-set-initial-state 'help-mode 'emacs)
  (setq evil-mode-line-format nil)
  (evil-mode 1)
  (setq-default evil-cross-lines nil)
  (setq evil-split-window-below  t
        evil-vsplit-window-right t
        evil-flash-delay         60 ;; Highilght longer time
        )

  (progn ;; Cursor color {{{

    (defun my/change-cursor (&optional a)
      ;;                 mode     skk     shape     color
      (let* ((alist  '(((normal . nil) . (block . "#e0a618"))
                       ((insert . nil) . (bar   . "#63c205"))
                       ((normal . t)   . (block . nil))
                       ((insert . t)   . (bar   . nil))))

             (mode   (if evil-insert-state-minor-mode 'insert 'normal))
             (skk    (and (boundp 'skk-mode) skk-mode))
             (action (cdr (assoc (cons mode skk) alist)))
             (color  (cdr action))
             (shape  (car action)))

        ;; Change cursor color
        (when color (set-cursor-color color))

        ;; Change cursor shape
        ;; https://github.com/syl20bnr/spacemacs/issues/7112
        (unless (display-graphic-p)
          (send-string-to-terminal (cond ((eq shape 'block) "\033[2 q")
                                         ((eq shape 'bar)   "\033[6 q")
                                         (t ""))))
        ))

    (dolist (h '(evil-insert-state-entry-hook
                 evil-normal-state-entry-hook
                 input-method-activate-hook
                 input-method-deactivate-hook
                 window-selection-change-functions))
      (add-hook h 'my/change-cursor))

    (my/change-cursor)
    ) ;; }}}

  )

;; todo: manually bind keys (little speedup on startup)
(use-package evil-surround
  ;; :commands (evil-surround-edit evil-surround-region)
  :ensure t
  :config
  (global-evil-surround-mode))

(use-package evil-commentary
  ;; :commands (evil-commentary-line)
  :ensure t
  :config
  (evil-commentary-mode))

(use-package evil-snipe
  ;; :commands (evil-snipe-f evil-snipe-F evil-snipe-t evil-snipe-T)
  :ensure t
  :config
  ;; enable f, F, t, and T keys
  (evil-snipe-override-mode 1))

(use-package evil-collection
  :ensure t :after evil
  :config
  (evil-collection-init
   '(help dired xref grep))
  ;; (evil-collection-init)
  (evil-collection-translate-key
   nil 'evil-motion-state-map
   ;; colemak hnei is qwerty hjkl
   "k" "h"
   "n" "j"
   "e" "k"
   "i" "l"
   "h" "k"
   "j" "n"
   "k" "e"
   "l" "i"))

;;
;; (use-package evil-magit
;;   :ensure t
;;   :after (:all evil magit))
