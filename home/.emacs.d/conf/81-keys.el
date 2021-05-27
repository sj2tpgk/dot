;; Todo: integrate with evil-mapcmd.el
(defun chord (&rest l) (let (k f) (while (setq k (pop l) f (pop l)) (key-chord-define-global k f))))

(nunmap "SPC") (nunmap "s") (nunmap ",") (vunmap ",")

(progn ;; builtin
  (nmap "C-x s" 'save-some-buffers! "C-x C-r" 'recentf-open-files
        "C-x ;" 'command-lines "<home>" 'smart-beg-of-line)
  (chord "mw" 'save-some-buffers! "md" 'xref--find-definitions))

(progn ;; selectrum, consult
  (nmap "<f6>"  'selectrum-repeat
        "C-h F" 'describe-face
        "/" 'consult-line "s/" 'consult-line-nofuzzy)
  (chord "mx" 'execute-extended-command "ms" 'selectrum-switch-buffer+ "mf" 'describe-function "mv" 'describe-variable))

(progn ;; company
  (with-eval-after-load 'company
    (emap company-mode-map   "<escape>" 'evil-smart-esc)
    (emap company-active-map "<escape>" 'evil-smart-esc)
    (emap company-search-map "<escape>" 'evil-smart-esc)
    (emap company-active-map
          "RET" 'company-complete-selection
          "<delete>"     'company-abort
          "<deletechar>" 'company-abort)))

(progn ;; misc

  (progn ;; minibuffer
    (emap minibuffer-local-map "<escape>" 'abort-recursive-edit)
    (with-eval-after-load 'iedit-lib ;; used in emr
      (emap iedit-lib-keymap "<escape>" 'iedit-quit))
    (with-eval-after-load 'popup
      (emap popup-menu-keymap "<escape>" 'popup-close)))

  (emap Info-mode-map
        "<S-mouse-4>" 'Info-history-back
        "<S-mouse-5>" 'Info-history-forward
        "<mouse-8>" 'Info-history-back
        "<mouse-9>" 'Info-history-forward))

(progn ;; evil insert
  (imap "<escape>" 'evil-smart-esc
        "<delete>" 'smart-forward-delete-char
        "<deletechar>" 'smart-forward-delete-char
        "TAB" 'smart-tab
        "<home>" 'smart-beg-of-line)
  (imap python-mode-map "DEL" 'python-indent-dedent-line-backspace))

(nmap ";" 'evil-commentary-yank-line)
(nvmap "ga" 'ialign)
(nmap "sb" 'kill-current-buffer
      "gp" (lambda()(interactive)(find-file(with-temp-buffer(yank)(buffer-string))))
      "RET" 'highlight-symbol-occur
      "<prior>" 'evil-scroll-up "<next>" 'evil-scroll-down
      "TAB" 'smart-toggle-folding
      "s=" 'beautify-buffer)
(nmap "|" 'eval-defun)
(nmap "-" (lambda()(interactive(other-window -1)))
      "sd" 'delete-window "so" 'delete-other-windows
      "ss" 'smart-split-window-below "sv" 'smart-split-window-right
      "+"  'tab-next "tn" 'tab-new "tk" 'tab-close "to" 'tab-close-other
      "tl" 'tab-line-new-tab "td" 'tab-line-close-tab)
(nvmap "m" 'evil-jump-item
       "C-e" 'evil-jump-forward "C-o" 'evil-jump-backward)
(nmap "M-j" 'evil-join)

(dotimes (i 9)
  (eval `(nmap ,(format "SPC %s" (1+ i))
               (lambda()(interactive)(tab-bar-select-tab ,(1+ i))))))

;; TODO dynamic keyboard layout (emacs, ranger)
(let ((i (if (string= "colemakdh" (or (getenv "MYKBD") ""))
             2 1)))
  (when (= i 2) (ounmap "l"))
  (mapc (lambda (x)
          (cond
           ((eq (car x) 'n) (define-key evil-normal-state-map (kbd (nth i x)) (nth 3 x)))
           ((eq (car x) 'v) (define-key evil-visual-state-map (kbd (nth i x)) (nth 3 x)))
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
          (nv "J" "N" evil-scroll-down)
          (nv "K" "E" evil-scroll-up)

          (nv "gh" "gk" smart-beg-of-line)
          (nv "gl" "gi" evil-end-of-line)

          (nv "i" "l" evil-insert)
          (nv "I" "L" evil-insert-line)
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
