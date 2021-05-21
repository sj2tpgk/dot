(definteractive evil-smart-esc
  (if company-selection-changed (company-complete-selection) (company-abort))
  ;; (when company-selection (if (= company-selection 0) (company-abort) (company-complete-selection)))
  (let ((p (point)) (b (line-beginning-position)) (e (line-end-position)))
    (evil-normal-state 1)
    ;; basically same as (evil-forward-char), but no eol error message
    (unless (member p (list b e))
      (forward-char))))

(setq smart-close-temp-window-bufnames '("*Help*" "*Messages*" "*grep*" "*Occur*" "*Backtrace*" "*Warnings*" "*Geiser dbg*"))
(definteractive smart-close-temp-window
  "Quit *Help* etc. if exists, or behave as vim's q command"
  ;; Note: `quit-windows-on' kills window (not just hiding *Help*) thus breaks window layout.
  (let ((ws (remove-if-not (lambda (w) (member (buffer-name (window-buffer w))
                                               smart-close-temp-window-bufnames))
                           (window-list))))
    (when ws (dolist (w ws) (quit-window nil w)) t)))
(definteractive smart-close-buffer
  ;; close buffer with saving
  (if (and (buffer-modified-p) (buffer-file-name (current-buffer)))
      (if (yes-or-no-p (format "Save and kill buffer %s?" (current-buffer)))
          (prog1 t (save-buffer) (kill-buffer-and-window))
        nil)
    (prog1 t (kill-buffer-and-window))
    ;; (if (yes-or-no-p (format "Kill buffer %s?" (current-buffer)))
    ;;     (prog1 t (kill-buffer-and-window))
    ;;   nil)
    ))
(definteractive evil-smart-close
  (or (smart-close-temp-window)
      ;; (smart-close-buffer)
      (call-interactively 'evil-record-macro)))

(definteractive evil-smart-close-folds
  (unless (find (lambda (x) (and (boundp x) x))
                '(outline-mode hs-minor-mode origami-mode hide-ifdef-mode))
    (hs-minor-mode 1))
  (evil-close-folds))

(evil-define-motion evil-search-symbol-forward (count &optional x)
  "Search forward for symbol (not just word) under point."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     evil-symbol-word-search))
  (evil-search-word-forward count t))
(evil-define-motion evil-search-symbol-backward (count &optional x)
  "Search backward for symbol (not just word) under point."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     evil-symbol-word-search))
  (evil-search-word-backward count t))

(definteractive my-redo-wrapper ;; For emacs27 and emacs28 compatibility
  (call-interactively (if (fboundp 'undo-redo) 'undo-redo 'undo-tree-redo)))

;; Treat smart-beg-of-line same as forward-char etc. for evil-repeat
(evil-set-command-property 'smart-beg-of-line :repeat 'motion)
