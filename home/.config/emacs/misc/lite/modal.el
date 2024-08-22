;; help mode proportional font

(package-install 'hydra)
(require 'hydra)

(setq hydra-hint-display-type 'no)
(setq hydra-hint-display-alist '((no (lambda(&rest a)) (lambda(&rest a)))))
(defhydra hydra-modal (global-map
                       "C-q"
                       :pre  (update-cursor "red"   'box)
                       :post (update-cursor "green" 'bar)
                       :hint nil
                       :idle 1000
                             :foreign-keys run)
  "Modal"
  ("k" backward-char)
  ("n" next-line)
  ("e" previous-line)
  ("i" forward-char)

  ("gk" beginning-of-line)
  ("gi" end-of-line)

  ("u" undo-tree-undo)
  ("U" undo-tree-redo)
  ("r" delete-backward-char)
  ("y" copy-region-as-kill)

                                        ;("k" kak-backward-char)
                                        ;("n" kak-next-line)
                                        ;("e" kak-previous-line)
                                        ;("i" kak-forward-char)

                                        ;("K" kak-s-backward-char)
                                        ;("N" kak-s-next-line)
                                        ;("E" kak-s-previous-line)
                                        ;("I" kak-s-forward-char)

                                        ;("gk" kak-beginning-of-line)
                                        ;("gi" kak-end-of-line)

                                        ;("b" kak-backward-to-word)
                                        ;("f" kak-forward-to-word)

                                        ;("B" kak-s-backward-to-word)
                                        ;("F" kak-s-forward-to-word)

                                        ;("w" kak-forward-to-word)
                                        ;("q" kak-backward-to-word)

  ;; ("<escape>" nil "cancel")
  ("RET" nil)
  )

(progn ;; undo-tree
  (package-install 'undo-tree)
  (require 'undo-tree)
  (global-undo-tree-mode 1))

(progn ;; kakoune like commands

  (defmacro defkak2 (base-name &rest body)
    `(progn
       (defkak ,(intern (concat "kak-" (symbol-name base-name))) ,@body)
       (defkak-s ,(intern (concat "kak-s-" (symbol-name base-name))) ,@body)))

  (defmacro defkak (name &rest body)
    `(defun ,name () (interactive) (set-mark-command nil) ,@body))

  (defmacro defkak-s (name &rest body)
    `(defun ,name ()
       (interactive)
       (unless (region-active-p) (set-mark-command nil))
       ,@body))

  (defkak2 beginning-of-line (beginning-of-line))
  (defkak2 end-of-line       (end-of-line))

  (defkak kak-previous-line (previous-line))
  (defkak kak-next-line     (next-line))

  (defkak-s kak-s-previous-line (previous-line))
  (defkak-s kak-s-next-line     (next-line))

  ;; (defun kak-forward-char () (interactive) (forward-char) (set-mark-command nil))
  (defkak kak-forward-char  (forward-char))
  (defkak kak-backward-char (backward-char))

  (defkak-s kak-s-forward-char  (forward-char))
  (defkak-s kak-s-backward-char (backward-char))

  (defkak kak-forward-word (forward-word))
  (defkak kak-backward-word (backward-word))
  (defkak-s kak-s-forward-word (forward-word))
  (defkak-s kak-s-backward-word (backward-word))

  (defkak2 forward-to-word (forward-to-word 1))
  (defkak2 backward-to-word (backward-to-word 1))
  )

(defun update-cursor (color type)
  "Change cursor."

  (set-cursor-color color)

  (if (display-graphic-p)
      (setq cursor-type type)
    (send-string-to-terminal ;; github.com/syl20bnr/spacemacs/issues/7112
     (cond
      ((eq type nil) (error "cursor-type nil on tty not implemented ..."))
      ((eq type t) "")
      ((or (eq type 'box))              "\033[2 q")
      ((or (eq type 'bar)
           (and (consp type)
                (eq (car type) 'bar)))  "\033[6 q")
      ((or (memq type '(hollow hbar))
           (and (consp type)
                (eq (car type) 'hbar))) "\033[4 q")))))

(progn ;; map ESC for hydra activation

  (add-to-list 'load-path "~/lite")
  (require 'evil-esc-mode)
  (evil-esc-mode 1)
  (global-set-key (kbd "<escape>") 'hydra-modal/body)

  ;; Changing escape key behavior is complicated (especially on tty)
  ;; Finally I decided to use scripts from evil.
  ;; See documentation of `evil-esc-mode'

  ;; Some references:
  ;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Searching-Keymaps.html
  ;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Translation-Keymaps.html#Translation-Keymaps
  ;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Keys.html

  ;; (setq overriding-terminal-local-map (make-sparse-keymap))
  ;; (define-key overriding-terminal-local-map (kbd "ESC") 'hydra-modal/body)
  ;; (setq meta-prefix-char 200)

  ;; hook other-window command

  ;; (setq input-decode-map (make-sparse-keymap))
  ;; (define-key key-translation-map (kbd "ESC") (kbd "C-g"))

  ;; (global-set-key [escape] 'hydra-modal/body)

  ;; (keyboard-translate ?\e ?\C-n)

  )

