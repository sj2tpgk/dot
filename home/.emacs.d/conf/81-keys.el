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
      "s=" 'beautify-buffer
      "sl" 'evil-substitute)
(nmap "|" 'eval-defun)
(nmap "-" (lambda()(interactive(other-window -1)))
      "sd" 'delete-window "so" 'delete-other-windows
      "ss" 'smart-split-window-below "sv" 'smart-split-window-right
      "+"  'tab-next "tn" 'tab-new "tk" 'tab-close "to" 'tab-close-other
      "tl" 'tab-line-new-tab "td" 'tab-line-close-tab)
(nmap "sK" 'evil-window-move-far-left "sL" 'evil-window-move-far-right "sN" 'evil-window-move-very-bottom "sE" 'evil-window-move-very-top)

(dotimes (i 9)
  (eval `(nmap ,(format "SPC %s" (1+ i))
               (lambda()(interactive)(tab-bar-select-tab ,(1+ i))))))
