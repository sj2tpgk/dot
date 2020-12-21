(progn ;; Show trailing whitespace
  (setq-default show-trailing-whitespace t)
  (defun minibuffer-hide-trailing-space () (setq show-trailing-whitespace nil))
  (add-hook 'minibuffer-setup-hook 'minibuffer-hide-trailing-space)
  )

(when (display-graphic-p) ;; Font config
  (setq text-scale-mode-step 1.03)

  (global-set-key (kbd "C-<prior>") 'text-scale-increase)
  (global-set-key (kbd "C-<next>")  'text-scale-decrease)

  (defun set-monospace-space-font ()
    (let ((cp #x2002) (tbl (make-display-table)))
      (aset tbl ?  (vector (make-glyph-code cp)))
      (setq-local buffer-display-table tbl)
      (set-fontset-font t (cons cp cp) (font-spec :family "Migu 2M"))))

  (defun set-font-variable ()
    (interactive)
    (set-face-font 'default (font-spec :family "Comic Relief" :size 14))
    (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Droid Sans Japanese"))
    ;; (set-fontset-font t 'japanese-jisx0208 (font-spec :family "azukifontP"))
    (custom-set-faces '(mode-line ((t (:family "Migu 2M" :size 15)))))
    (setq-default line-spacing 0) ;; 0.2 in certain font
    (add-hook 'prog-mode-hook 'set-monospace-space-font)
    (set-monospace-space-font))

  (defun set-font-monospace ()
    (interactive)
    ;; (set-face-font 'default (font-spec :family "Droid Sans Mono Dotted" :size 13))
    ;; (set-face-font 'default (font-spec :family "Migu 2M" :size 14))
    ;; (custom-set-faces '(mode-line ((t (:family "Migu 2M" :size 14)))))
    (set-face-font 'default (font-spec :family "Source Code Pro" :size 13))
    (custom-set-faces '(mode-line ((t (:family "Source Code Pro" :size 13)))))
    (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Droid Sans Japanese" :size 15))
    (setq-default line-spacing 0.2)
    (remove-hook 'prog-mode-hook 'set-monospace-space-font))

  (set-font-monospace)
  ;; (set-font-variable)

  ;; (set-default-font (font-spec :family "Comic Relief" :size 14))
  ;; (set-default-font (font-spec :family "Comic Mono" :size 14))
  ;; (set-fontset-font t nil (font-spec :family "Comic Sans MS"))

  ;; (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Droid Sans Japanese"))
  ;; (setq-default line-spacing 0) ;; 0.2 in certain font
  )
