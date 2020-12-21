(use-package ivy
  :ensure t
  :commands (ivy-switch-buffer ivy-switch-buffer-other-window)

  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format        "(%d/%d) "
        ivy-re-builders-alist   '((read-file-name-internal . ivy--regex-plus)
                                  (swiper                  . ivy--regex-plus) ;; see my-swiper-xxx
                                  (swiper-all              . ivy--regex-plus)
                                  (t                       . ivy--regex-fuzzy)))

  ;; (ivy-rich-reload)

  ;; (ivy-mode 1)
  (recentf-mode)

  (defun ivy-copy-to-buffer-action (x) (with-ivy-window (insert x)))
  (defun ivy-yank-action (x) (kill-new x))

  (ivy-set-actions t '(("i" ivy-copy-to-buffer-action "insert")
                       ("y" ivy-yank-action "yank")))

  )

(use-package counsel
  :disabled t
  :ensure t
  :commands (counsel-M-x counsel-describe-face counsel-describe-function counsel-describe-variable)

  :config
  (ivy-rich-reload)
  (counsel-mode 1)
  )

(use-package swiper
  :ensure t
  :commands (swiper)

  :init
  (definteractive swiper-regplus
    "Run swiper using ivy--regex-plus"
    (require 'ivy)
    (setf (cdr (assoc 'swiper ivy-re-builders-alist)) 'ivy--regex-plus)
    (swiper))

  (definteractive swiper-fuzzy
    "Run swiper using ivy--regex-fuzzy"
    (require 'ivy)
    (setf (cdr (assoc 'swiper ivy-re-builders-alist)) 'ivy--regex-fuzzy)
    (swiper))

  (definteractive swiper-all-regplus
    (require 'ivy)
    (setf (cdr (assq 'swiper-all ivy-re-builders-alist)) 'ivy--regex-plus)
    (swiper-all))

  (definteractive swiper-all-fuzzy
    (require 'ivy)
    (setf (cdr (assq 'swiper-all ivy-re-builders-alist)) 'ivy--regex-fuzzy)
    (swiper-all))
  )

(use-package flx
  :disabled t
  ;; Necessary for sane sorting
  ;; https://www.reddit.com/r/emacs/comments/3xzas3/help_with_ivycounsel_fuzzy_matching_and_sorting/
  :ensure t :after ivy)

(use-package ivy-rich
  :disabled t
  ;; Issue: https://github.com/Yevgnen/ivy-rich/issues/76
  ;; Workaround add (ivy-rich-reload) before enabling ivy-mode and counsel-mode.
  ;; :disabled
  :ensure t
  :after ivy

  :config
  ;; (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  ;; (setq ivy-virtual-abbreviate 'full
  ;;       ivy-rich-switch-buffer-align-virtual-buffer t
  ;;       ivy-rich-path-style 'abbrev)

  (ivy-rich-mode 1)

  )
