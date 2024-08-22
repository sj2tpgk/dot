;; -*- lexical-binding: t -*-

(defmacro definteractive (name &optional docstring &rest body)
  "Define an interactive function."
  (if (stringp docstring)
      `(defun ,name ()
         ,docstring
         (interactive)
         ,@body)
    `(defun ,name ()
       (interactive)
       ,@(cons docstring body))))

(defvar key-ignore-error t "Whether (key) ignores errors.")
(defun key (&rest args)
  "Define keys.
ARGS = ([keymap] [:chord] key1 func1 [:chord] key2 func2 ...)

Examples:
(key kmap \"C-s\" 'cmd1)       ==> (define-key (current-global-map) (kbd \"C-s\") 'cmd1)
(key kmap :chord \"jk\" 'cmd2) ==> (key-chord-define kmap \"jk\" 'cmd2)
(key :hook some-hook some-map \"C-s\" 'cmd3) ==> (add-hook some-hook (lambda () (key some-map \"C-s\" 'cmd3)))
(key :after some-feature some-map \"C-s\" 'cmd4) ==> (with-eval-after-load some-feature (key some-map \"C-s\" 'cmd4))

If keymap is omitted, (current-global-map) is used by default."
  ;; TODO: :map keyword (also allow (key kmap "key" 'cmd) without :map keyword)
  ;; TODO: evil integration
  ;; TODO: lazy mapping
  (let ((kmap (if (keymapp (car args)) (pop args) (current-global-map))))
    (while args
      (cond
       ((memq (car args) '(:chord :chor :cho :ch :c))
        (cond ((featurep 'key-chord)
               (pop args)
               (let ((key-string1 (pop args)) (cmd (pop args)))
                 (key-chord-define kmap key-string1 cmd)))
              (t (unless key-ignore-error
                   (error ":chord was used, but key-chord isn't present!"))
                 (pop args) (pop args) (pop args))))
       (t
        (let ((key-string (pop args)) (cmd (pop args)))
          (define-key kmap (kbd key-string) cmd)))))))

(definteractive smart-tab
  "Complete or indent or toggle folding"
  (cond
   ;; ((minibufferp)       (or (minibuffer-complete) (hippie-expand)))
   ;; \\_> = symbol end, \\> = word end
   ((bound-and-true-p evil-insert-state-minor-mode)
    (if (looking-at "\\_>")
        (evil-complete-next)
      (indent-for-tab-command)))
   ((looking-at "\\_>") (or (and (boundp 'company) company-mode
                                 (company-complete-common-or-cycle))
                            (hippie-expand 1)
                            (dabbrev-expand 1)))
   (t                   (smart-toggle-folding))))

(definteractive save-some-buffers!
  "`save-some-buffers' without confirmation."
  (save-some-buffers 1))

(definteractive beautify-buffer
  "Indent buffer and remove trailing spaces."
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace (point-min) (point-max)))

(definteractive comment-lines
  "Toggle-comment a line or region. Linewise."
  (save-excursion
    (if (not (region-active-p))
        (comment-line 1)
      (let ((rb (region-beginning)) (re (region-end)))
        (goto-char rb)
        (let ((b (line-beginning-position)))
          (goto-char re)
          (let ((e (line-end-position)))
            (comment-or-uncomment-region b e)))))))

(definteractive smart-beg-of-line
  "Call `beginning-of-line-text'. If cursor didn't move, call `beginning-of-line'."
  (let ((p (point)))
    (beginning-of-line-text)
    (when (= p (point)) (beginning-of-line))))

(definteractive match-paren
  "Similar to vim's %."
  (cond ((looking-at "\\s(")     (forward-list 1) (backward-char 0))
        ((looking-back "\\s)" 2) (forward-char -0) (backward-list 1))
        (t (search-forward-regexp "\\(\\s(\\|\\s)\\)"))))

(definteractive kill-current-buffer
  (kill-buffer (current-buffer)))

;; (definteractive smart-toggle-folding
;;   "Toggle folding. (Enable hs-minor-mode if not present)"
;;   (cond
;;    ((eq major-mode 'org-mode) (org-cycle))
;;    ((bound-and-true-p hs-minor-mode)
;;     (if (bound-and-true-p evil-mode)
;;         (evil-toggle-fold)
;;       (hs-toggle-hiding)))
;;    (t (hs-minor-mode) (hs-toggle-hiding))))

(setq smart-toggle-folding-last-point nil
      smart-toggle-folding-detected   nil)
(defun smart-toggle-folding-detector (&rest args)
  (setq smart-toggle-folding-detected t))
(defun smart-toggle-folding-web-folded-at-p (pos)
  "In web-mode, check whether code at POS is folded."
  (and pos
       (dolist (elt (overlays-at pos))
         (when (eq (overlay-get elt 'font-lock-face) 'web-mode-folded-face)
           (cl-return t)))))
(definteractive smart-toggle-folding
  "Toggle folding. (Automatically enable hs-minor-mode if not present)"
  (cond

   ;; In org-mode, call org-cycle
   ;; TODO BUG: cant open fold (cursor automatically go to line-end)
   ((eq major-mode 'org-mode) (org-cycle))

   ;; In web-mode
   ((and (eq major-mode 'web-mode)
         (string= "html" (web-mode-language-at-pos)))
    (let ((child (web-mode-element-child-position))
          (children (web-mode-element-children)))
      (if children
          (if (find-if 'smart-toggle-folding-web-folded-at-p children)
              ;; If the child is folded -> close this tag
              ;; If this tag and children are folded -> open this tag
              (web-mode-fold-or-unfold)
            ;; If the child is open -> close children
            (web-mode-element-children-fold-or-unfold))
        ;; If no child -> toggle this tag
        (web-mode-fold-or-unfold))))

   ;; Enable hs-minor-mode and recur (to check following clauses)
   ((not (bound-and-true-p hs-minor-mode))
    (progn (hs-minor-mode) (smart-toggle-folding)))

   ;; Already hidden -> show
   ((hs-already-hidden-p) (hs-toggle-hiding))

   ;; Repeated call -> toggle
   ((equal smart-toggle-folding-last-point (point))
    (progn (hs-toggle-hiding) (backward-char 0)))

   ;; Else: try (hs-hide-level 1) and if nothing happens toggle
   (t (setq smart-toggle-folding-last-point (point))
      (setq smart-toggle-folding-detected   nil)
      (advice-add 'hs-make-overlay :after 'smart-toggle-folding-detector)
      (hs-hide-level 1)
      (advice-remove 'hs-make-overlay 'smart-toggle-folding-detector)
      (unless smart-toggle-folding-detected
        (hs-toggle-hiding) (backward-char)))))

(definteractive smart-toggle-folding-mouse
  "Toggle folding under mouse."
  (call-interactively 'mouse-set-point)
  (smart-toggle-folding)
  (when (and (fboundp 'evil-visual-state-p) (evil-visual-state-p))
    (evil-normal-state)))

(defun smart-forward-delete-char-empty-line-p () (string-match-p "\\`\\s-*$" (thing-at-point 'line)))
(defun smart-forward-delete-line-length-multibyte ()
  ;; (goto-char b) ;; (goto-char (+ b c)) doesn't work well with multibyte
  ;; (forward-char multibyte-column-num)
  (save-excursion (let ((i 0) (b (line-beginning-position)))
                    (while (< b (point))
                      (setq i (+ i 1))
                      (backward-char))
                    i)))
(definteractive smart-forward-delete-char
  "Similar to `delete-forward-char' but when at end of line, joins lines."
  (let ((b (line-beginning-position)) (e (line-end-position)))
    (if (or (not (= (point) e)) (< 256 (- e b)))
        ;; do nothing special if not at beg of line, or if in very long line
        (call-interactively 'delete-forward-char)
      (if (not (smart-forward-delete-char-empty-line-p))
          ;; when non-empty line
          (progn (join-line 1) (indent-region b (line-end-position)))
        ;; when empty line
        (join-line 1)
        (indent-region b (line-end-position))
        (beginning-of-line-text) ;; move to char that was originally beg of next line
        ))))

(dolist (cmd '(backward-delete-char-untabify evil-delete-backward-char-and-join))
  (advice-add cmd ':around 'backward-delete-decorator))
(defun backward-delete-decorator (oldfun &rest args)
  (interactive)
  (let ((p (point)) (b (line-beginning-position)))
    (cond
     ((= b p) (join-line))
     ((and (looking-back "\\s-" (1- p)) ;; just for performance
           (string-match-p "^\\s-*$" (buffer-substring b p)))
      (while (< b (point)) (backward-delete-char 1)))
     (t (call-interactively oldfun)))))
;; (definteractive smart-backward-delete-char
;;   "Similar to `backward-delete-char-untabify' but join to prev line if needed."
;;   ;; two-step? (delete space and next time join line)
;;   (let ((p (point)) (b (line-beginning-position)))
;;     (cond
;;      ((= b p) (join-line))
;;      ((and (looking-back "\\s-" (1- p)) ;; just for performance
;;            (string-match-p "^\\s-*$" (buffer-substring b p)))
;;       (while (< b (point)) (backward-delete-char 1)))
;;      (t (call-interactively 'backward-delete-char-untabify)))))

(definteractive smart-forward-char
  ;; Open folding optionally (= (point) (line-beginning-position))
  (unless (eq last-command 'smart-forward-char) ;; Skip if called consequtively
    (and (bound-and-true-p hs-minor-mode)
         (condition-case e (when (hs-already-hidden-p) (save-excursion (hs-show-block))) (error 'error))))
  (if (bound-and-true-p evil-mode) (evil-forward-char) (forward-char)))

(definteractive smart-backward-char
  ;; Close folding optionally
  ;; (when (= (point) (line-beginning-position))
  ;;   (and (bound-and-true-p hs-minor-mode)
  ;;        (condition-case e (save-excursion (hs-hide-block)) (error 'error))))
  (if (bound-and-true-p evil-mode) (evil-backward-char) (backward-char)))

(eldoc-add-command 'smart-forward-char 'smart-backward-char)
(eldoc-add-command 'smart-forward-delete-char 'smart-backward-delete-char)

(definteractive subst-comma-period ;; for japanese scientific texts
  (save-excursion
    (subst-char-in-region (point-min) (point-max) ?、 ?，)
    (subst-char-in-region (point-min) (point-max) ?。 ?．)))

(definteractive smart-split-window-below "Split below + switch" (select-window (split-window-below)))
(definteractive smart-split-window-right "Split right + switch" (select-window (split-window-right)))

;; Use header line to show occur bindings etc.
(definteractive occur-this
  "Call `occur' for the symbol at point or text in active region."
  (let ((str
         (if (region-active-p)
             (buffer-substring (region-beginning) (region-end))
           (format "%s" (thing-at-point 'symbol)))))
    (occur str)))

(defun make-scratch-of (&optional major-mode-symbol)
  "MAJOR-MODE-SYMBOL is a symbol (e.g. 'sh-mode) or nil (prompt to choose)."
  ;; idea: multiple buf for same mode (ask "use existing buffer?" if necessary)
  (interactive)
  (let* ((mode
          (or major-mode-symbol
              (let ((cands
                     (mapcar (lambda (sym)
                               (string-remove-suffix "-mode"
                                                     (symbol-name sym)))
                             (remove-duplicates
                              (mapcan (lambda (x)
                                        (let ((y (if (symbolp (cdr x)) (cdr x) (cadr x))))
                                          (and y (list y))))
                                      auto-mode-alist)))))
                (intern
                 (format "%s-mode"
                         (completing-read "Choose major mode: " cands))))))
         (get-nth-bufname
          (lambda (n) ;; n = 1,2,3,...
            (format "*scratch%s%s*"
                    (if (eq 'lisp-interaction-mode mode)
                        ""
                      (concat "[" (substring (symbol-name mode) 0 -5) "]"))
                    (if (= n 1) "" n))))
         (n 1))

    ;; (while (get-buffer (funcall get-nth-bufname n)) (cl-incf n))

    (let ((exist (get-buffer (funcall get-nth-bufname n))))
      (switch-to-buffer
       (get-buffer-create
        (funcall get-nth-bufname n)))
      (unless exist (funcall mode)))))
