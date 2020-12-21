;; Keybinds for "apps" (md4rd, notmuch) go into its own use-package declaration

(key "TAB"     'smart-tab
     "C-x s"   'save-some-buffers!
     "C-x ;"   'comment-lines
     "<home>"  'smart-beg-of-line "C-a" 'smart-beg-of-line
     "C-x C-r" 'recentf-open-files)

(key "<escape> TAB" 'other-window
     "<escape> w" 'save-some-buffers!
     "<escape> q" 'save-buffers-kill-terminal)

(key "<escape> e" 'eval-defun
     "<escape> b" 'eval-buffer)

(key "<escape> u" 'undo
     "<escape> =" 'beautify-buffer)

(key "<escape> s" 'isearch-forward)

(key :chord "md"  'xref-find-definitions)

;; ivy
;; (key "<escape> s" 'swiper
;;      "<f6>"       'ivy-resume
;;      :chord "mx"  'counsel-M-x
;;      :chord "ms"  'counsel-switch-buffer ;; ivy-switch-buffer has no preview
;;      :chord "mf"  'counsel-describe-function
;;      :chord "mv"  'counsel-describe-variable
;;      "C-h F" 'counsel-describe-face)

;; selectrum
(key "<f6>"      'selectrum-repeat
     :chord "mx" 'execute-extended-command
     ;; :chord "ms" 'switch-to-buffer
     :chord "ms" 'selectrum-switch-buffer+
     :chord "mf" 'describe-function
     :chord "mv" 'describe-variable
     "C-h F"     'describe-face)

;; (let ((e "<escape>") (mkq 'minibuffer-keyboard-quit))
;;   (key minibuffer-local-map e mkq)
;;   (key ivy-minibuffer-map :a 'ivy e mkq)
;;   (key swiper-map :a swiper e mkq)
;;   (key iedit-lib-keymap :a iedit-lib e 'iedit-quit)
;;   (key popup-menu-keymap :a 'popup e 'popup-close)
;;   (key mc/keymap :h 'multiple-cursors-mode-hook e 'mc/keyboard-quit))
;;
;; (key company-active-map :a company
;;      "TAB"      'company-complete-common-or-cycle
;;      "RET"      'company-complete-selection
;;      "SPC"      'company-smart-space
;;      "<escape>" 'company-abort)

(with-eval-after-load 'ivy
  (key ivy-minibuffer-map "<escape>" 'minibuffer-keyboard-quit))
(with-eval-after-load 'swiper
  (key swiper-map         "<escape>" 'minibuffer-keyboard-quit))

(key minibuffer-local-map "<escape>" 'abort-recursive-edit)

(with-eval-after-load 'company
  (key company-mode-map   "<escape>" 'evil-smart-esc)
  (key company-active-map   "<escape>" 'evil-smart-esc)
  (key company-search-map "<escape>" 'evil-smart-esc)
  ;; "TAB"      'company-complete-common-or-cycle
  ;; "RET"      'company-complete-selection
  ;; "SPC"      'company-smart-space
  (key company-active-map "RET" 'company-complete-selection)

  (key company-active-map
       "<delete>"     'company-abort
       "<deletechar>" 'company-abort)
  )


(with-eval-after-load 'ivy
  (key ivy-minibuffer-map
       :c "ai" (lambda()(interactive)(setq unread-command-events(listify-key-sequence(kbd"M-o i"))))
       :c "ay" (lambda()(interactive)(setq unread-command-events(listify-key-sequence(kbd"M-o y"))))))

(key "<escape> f" 'find-file)
(with-eval-after-load 'counsel
  (key "<escape> f" 'counsel-find-file "<escape> F" 'counsel-fzf))

(with-eval-after-load 'swiper
  (key "<escape> s" 'swiper-fuzzy     "<escape> C-s" 'swiper-regplus
       "<escape> S" 'swiper-all-fuzzy "<escape> M-s" 'swiper-all-regplus))

(add-hook 'multiple-cursors-mode-hook
          (lambda ()
            (key mc/keymap "<escape>" 'mc/keyboard-quit)))

(with-eval-after-load 'iedit-lib ;; used in emr
  (key iedit-lib-keymap "<escape>" 'iedit-quit))
(with-eval-after-load 'popup
  (key popup-menu-keymap "<escape>" 'popup-close))

(key "<escape> r" 'emr-show-refactor-menu)
