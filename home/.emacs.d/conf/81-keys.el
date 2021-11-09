;; -*- lexical-binding: t; -*-

;; Todo: integrate with evil-mapcmd.el
(defun chord (&rest l) (let (k f) (while (setq k (pop l) f (pop l)) (key-chord-define-global k f))))

(nunmap "SPC") (nunmap "s") (nunmap ",") (vunmap ",") (nunmap "z")

(progn ;; builtin
  (nmap "C-x s" 'save-some-buffers! "C-x C-r" 'recentf-open-files
        "C-x ;" 'command-lines)
  (chord "mw" 'save-some-buffers! "md" 'xref--find-definitions))

(progn ;; escape sequenct etc.
  (global-set-key [Home] 'smart-beg-of-line)
  (global-set-key [End]  'end-of-line)
  (evil-global-set-key 'normal [End] 'evil-end-of-line-or-visual-line)
  )

(progn ;; selectrum, consult
  (nmap "<f6>"  'selectrum-repeat
        "C-h F" 'describe-face
        ;; "s/" 'consult-line "/" 'consult-line-nofuzzy
        "C-f" 'ctrlf-forward-default)
  (chord "mx" 'execute-extended-command "ms" 'selectrum-switch-buffer+ "mf" 'describe-function "mv" 'describe-variable))

(nmap "/" 'evil-search-forward)
(nmap "/" 'consult-line-nofuzzy)

(progn ;; company
  (with-eval-after-load 'company
    (emap company-mode-map   "<escape>" 'evil-smart-esc)
    (emap company-active-map "<escape>" 'evil-smart-esc)
    (emap company-search-map "<escape>" 'evil-smart-esc)
    (emap company-active-map
          "RET" 'company-complete-selection
          "<delete>"     'company-abort
          "<deletechar>" 'company-abort)))

;; mouse
(global-set-key (kbd "<double-mouse-1>") 'smart-toggle-folding-mouse)

(progn ;; misc

  (progn ;; minibuffer
    (emap minibuffer-local-map "<escape>" 'abort-recursive-edit)
    (with-eval-after-load 'iedit-lib ;; used in emr
      (emap iedit-lib-keymap "<escape>" 'iedit-quit))
    (with-eval-after-load 'popup
      (emap popup-menu-keymap "<escape>" 'popup-close)))

  (chord "mr" 'embark-act)
  (chord "mp" 'sr-speedbar-toggle)

  (emap Info-mode-map
        "<S-mouse-4>" 'Info-history-back
        "<S-mouse-5>" 'Info-history-forward
        "<mouse-8>" 'Info-history-back
        "<mouse-9>" 'Info-history-forward)

  (emap occur-mode-map "-" 'other-window))

(progn ;; evil insert
  (imap "<escape>" 'evil-smart-esc
        "<delete>" 'smart-forward-delete-char
        "<deletechar>" 'smart-forward-delete-char
        "TAB" 'smart-tab
        "M-j" 'evil-join)
  (imap python-mode-map "DEL" 'python-indent-dedent-line-backspace))

(progn ;; evil normal, visual (keyboard layout independent bindings)
  ;; editing
  (nmap "D" 'evil-delete-whole-line "Y" 'evil-yank-line)
  (nmap "s=" 'beautify-buffer "M-j" 'evil-join
        "g SPC" 'evil-commentary-yank-line)
  (nvmap ";" 'evil-commentary-yank-line)
  (nvmap "ga" 'ialign)
  (nmap "U" 'my-redo-wrapper)
  ;; movement
  (nmap "0" 'smart-beg-of-line)
  (nvmap "m" 'evil-jump-item
         "C-n" 'evil-jump-forward "C-o" 'evil-jump-backward)
  (nmap "*"  'evil-search-symbol-forward "#"  'evil-search-symbol-backward
        "s*" 'evil-search-word-forward "s#" 'evil-search-word-backward)
  (nmap "<prior>" 'smart-evil-scroll-up "<next>" 'smart-evil-scroll-down)
  (nmap "(" 'beginning-of-defun ")" '(lambda () (interactive) (end-of-defun 2) (beginning-of-defun)))
  (nmap "z" 'recenter-top-bottom)
  (nmap "gh" (lambda () (interactive) (describe-symbol (intern (thing-at-point 'symbol)))))
  ;; window, buffer, tab
  (nmap "sb" 'kill-current-buffer
        "gp" (lambda()(interactive)(find-file(with-temp-buffer(yank)(buffer-string))))
        "-" (lambda()(interactive(other-window -1)))
        "sd" 'delete-window "so" 'delete-other-windows
        "ss" 'smart-split-window-below "sv" 'smart-split-window-right
        "+"  'tab-next "tn" 'tab-new "tk" 'tab-close "to" 'tab-close-other
        "tl" 'tab-line-new-tab "td" 'tab-line-close-tab)
  (nmap "C-e" 'sr-speedbar-toggle)
  (emap speedbar-mode-map "C-e" 'sr-speedbar-toggle)
  (dotimes (i 9)
    (eval `(nmap ,(format "SPC %s" (1+ i))
                 (lambda()(interactive)(tab-bar-select-tab ,(1+ i))))))
  ;; quitting
  (nmap "q" 'evil-smart-close
        "Q" 'evil-smart-quit
        ;; "Q" smart close tab, buffer etc.
        )
  ;; misc
  (nmap "RET" 'highlight-symbol-occur
        "TAB" 'smart-toggle-folding "<tab>" 'smart-toggle-folding)
  (nmap "|" 'eval-defun  ",e" 'eval-defun ",b" 'eval-buffer)
  (vmap "|" 'eval-region ",e" 'eval-region)
  (nimap "C-j" 'eval-print-defun)
  (vmap "a" 'evil-append)
  )

;; TODO dynamic keyboard layout (emacs, ranger)
(let ((i (if (string= "colemakdh" (or (getenv "MYKBD") ""))
             2 1)))
  (when (= i 2) (ounmap "l"))

  (dolist (x '(("i" "l" occur-edit-mode)
               ("j" "n" occur-next)
               ("k" "e" occur-prev)))
    (define-key occur-mode-map (kbd (if (= i 2) (cadr x) (car x))) (caddr x)))

  (mapc (lambda (x) ;; x = (states key-for-layout-1 key-for-layout-2 ... cmd)
          (cond
           ((eq (car x) 'n) (define-key evil-normal-state-map   (kbd (nth i x)) (nth 3 x)))
           ((eq (car x) 'v) (define-key evil-visual-state-map   (kbd (nth i x)) (nth 3 x)))
           ((eq (car x) 'o) (define-key evil-operator-state-map (kbd (nth i x)) (nth 3 x)))
           ((eq (car x) 'nv)
            (define-key evil-normal-state-map (kbd (nth i x)) (nth 3 x))
            (define-key evil-visual-state-map (kbd (nth i x)) (nth 3 x)))))
        '((n  "h" "k" smart-backward-char)
          (n  "l" "i" smart-forward-char)
          (v  "h" "k" evil-backward-char)
          (v  "l" "i" evil-forward-char)

          (nv "j" "n" evil-next-visual-line)
          (nv "k" "e" evil-previous-visual-line)
          (nv "gj" "gn" evil-next-line)
          (nv "gk" "ge" evil-previous-line)

          (n "J" "N" smart-evil-scroll-down)
          (n "K" "E" smart-evil-scroll-up)
          (v "J" "N" evil-scroll-down)
          (v "K" "E" evil-scroll-up)

          (nv "gh" "gk" smart-beg-of-line)
          (nv "gl" "gi" evil-end-of-line)

          (nv "i" "l" evil-insert)
          (n  "I" "L" evil-insert-line)
          (v  "I" "L" evil-insert)
          (nv "si" "sl" evil-substitute)

          (nv "n" "j" evil-search-next)
          (nv "N" "J" evil-search-previous)

          (nv "e" "h" evil-forward-word-end)
          (nv "E" "H" evil-forward-WORD-end)

          (n "sh" "sk" evil-window-left)
          (n "sj" "sn" evil-window-down)
          (n "sk" "se" evil-window-up)
          (n "sl" "si" evil-window-right)
          (n "sH" "sK" evil-window-move-far-left)
          (n "sJ" "sN" evil-window-move-far-down)
          (n "sK" "sE" evil-window-move-far-up)
          (n "sL" "sI" evil-window-move-far-right)

          (n "H" "K" tab-line-switch-to-prev-tab)
          (n "L" "I" tab-line-switch-to-next-tab)

          (o "h" "k" evil-beginning-of-line)
          (o "l" "i" evil-end-of-line)
          (o "iw" "lw" evil-inner-word)
          (o "iW" "lW" evil-inner-WORD)
          (o "e" "h" evil-forward-word-end)
          (o "E" "H" evil-forward-WORD-end))))

