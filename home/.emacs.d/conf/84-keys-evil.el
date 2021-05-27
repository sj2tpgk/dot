(nunmap "SPC") (nunmap "s") (nunmap ",") (vunmap ",")

(progn ;; insert
  (imap "<escape>" 'evil-smart-esc
        "<delete>" 'smart-forward-delete-char
        "<deletechar>" 'smart-forward-delete-char ;; for terminal
        )
  ;; (imap "DEL" 'smart-backward-delete-char)

  (with-eval-after-load 'python
    (imap python-mode-map "DEL" 'python-indent-dedent-line-backspace))
  )

(progn ;; normal, editing
  (nmap "D" 'evil-delete-whole-line
        "Y" 'evil-yank-line
        "U" 'my-redo-wrapper
        "s=" 'beautify-buffer)
  (nmap "g SPC" 'evil-commentary-line)
  (vmap "ga" 'ialign)
  )

(progn ;; normal, misc
  (nmap "SPC w" 'save-some-buffers!
        "SPC q" 'save-buffers-kill-emacs
        "s w" 'save-some-buffers!
        "s q" 'save-buffers-kill-emacs
        "sb"    'kill-current-buffer)
  ;; visit file in clipboard
  (nmap "gp" (lambda()(interactive)(find-file(with-temp-buffer(yank)(buffer-string)))))

  (nmap "<prior>" 'evil-scroll-up
        "<next>"  'evil-scroll-down)

  (nmap "TAB" 'smart-toggle-folding)

  (nmap "RET" 'embark-act)
  ;; (nmap "RET" 'highlight-symbol-occur)

  (nmap "/"     'swiper-fuzzy
        "s/"    'swiper-regplus
        "g/"    'swiper-all-regplus
        "SPC /" 'swiper-all-fuzzy)

  (nmap "*"  'evil-search-symbol-forward
        "#"  'evil-search-symbol-backward
        "s*" 'evil-search-word-forward
        "s#" 'evil-search-word-backward)
  (nmap ",e" 'eval-defun
        ",b" 'eval-buffer)
  (vmap ",e" 'eval-region)

  (nmap "(" 'beginning-of-defun
        ;; ")" '(lambda () (interactive) (end-of-defun 2) (beginning-of-defun))
        ")" 'end-of-defun)

  (nmap "0" 'smart-beg-of-line)

  (nmap "q" 'evil-smart-close
        "Q" 'evil-quit
        ;; "Q" smart close tab, buffer etc.
        )

  ;; (nmap xref--xref-buffer-mode-map "RET" 'xref-goto-xref)

  (nmap "zm" 'evil-smart-close-folds)

  (nmap "zs" (lambda () "Toggle selective display." (interactive) (set-selective-display (mod (1+ (or selective-display 0)) 2))))

  )

(progn ;; window
  (nmap "-" (lambda()(interactive(other-window -1)))
        "sd" 'delete-window
        "so" 'delete-other-windows
        "ss" 'smart-split-window-below
        "sv" 'smart-split-window-right)

  (nmap "+"  'tab-next
        "tn" 'tab-new
        "tk" 'tab-close
        "to" 'tab-close-other)

  (nmap "tl" 'tab-line-new-tab
        "td" 'tab-line-close-tab)

  (dotimes (i 9)
    (eval `(nmap ,(format "SPC %s" (1+ i))
                 (lambda()(interactive)(tab-bar-select-tab ,(1+ i)))))))

;; nano-like insert mode
;; ()

(when (and nil (getenv "MYKBD")
           (string= (getenv "MYKBD") "colemakdh"))

  (dolist (cmd '(smart-backward-char smart-forward-char))
    ;; prevent smart- commands overrides the repeat ring.
    (evil-add-command-properties cmd :repeat 'ignore))
  (nmap  "k" 'smart-backward-char
         "i" 'smart-forward-char)
  (vmap  "k" 'evil-backward-char
         "i" 'evil-forward-char)

  (nvmap "n" 'evil-next-visual-line
         "e" 'evil-previous-visual-line

         "gn"  'evil-next-line
         "ge"  'evil-previous-line

         "N"   'evil-scroll-down
         "E"   'evil-scroll-up

         "gk"  'smart-beg-of-line
         "gi"  'evil-end-of-line

         "m"   'evil-jump-item

         "C-e" 'evil-jump-forward  ;; Can't distinguish C-i and TAB
         "C-o" 'evil-jump-backward

         "l" 'evil-insert
         "L" 'evil-insert-line

         "j" 'evil-search-next
         "J" 'evil-search-previous

         "h" 'evil-forward-word-end
         "H" 'evil-forward-WORD-end

         "sl" 'evil-substitute

         )

  (nmap "l" 'evil-insert)

  (nimap "M-j" 'evil-join)

  (vmap "L" 'evil-insert)
  (ounmap "l")
  (omap "k" 'evil-beginning-of-line
        "i" 'evil-end-of-line
        "lw" 'evil-inner-word
        "lW" 'evil-inner-WORD
        "h" 'evil-forward-word-end
        "H" 'evil-forward-WORD-end
        )

  (nmap "sk" 'evil-window-left
        "sn" 'evil-window-down
        "se" 'evil-window-up
        "si" 'evil-window-right
        "sK" 'evil-window-move-far-left
        "sN" 'evil-window-move-very-bottom
        "sE" 'evil-window-move-very-top
        "sI" 'evil-window-move-far-right)

  (nmap "K" 'tab-line-switch-to-prev-tab
        "I" 'tab-line-switch-to-next-tab)

  )
