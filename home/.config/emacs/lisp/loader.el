;;; -*- lexical-binding: t; -*-

;; Usage : set `loader/list' and call `loader/load'
;; (setq package-enable-at-startup nil) in ** early-init.el **

;; TODO: profiling

(defvar loader/list nil
  "Loading specification. ((phase file) ...) where phase is :
  0 = load immediately
  1 = load on first key press")

(defvar loader/enable-package-el t
  "Set to nil if you ure alternative package managers.")

(defvar loader/time-data nil)

(defvar loader/do-profile nil)

(defmacro loader/time (tag &rest body)
  (let ((g-time-start (gensym)))
    `(let ((,g-time-start (current-time)))
       ,@body
       (push (cons ,tag (time-since ,g-time-start)) loader/time-data))))

(defun loader/show-time-data ()
  (let ((sum 0))
    (dolist (entry (reverse loader/time-data))
      (message "%.05s   %s" (float-time (cdr entry)) (car entry))
      (setq sum (+ (float-time (cdr entry)) sum)))
    (message "%.05s   Total" sum)))

(defun loader/load-silently (file)
  (if loader/do-profile
      (loader/time file (load (expand-file-name file) nil t t))
    (load (expand-file-name file) nil t t)))

(defun loader/load-phase (n)
  (dolist (entry loader/list)
    (when (= n (car entry))
      (loader/load-silently (cadr entry)))))

(defun loader/load-1 () ;; load files of category 1

  ;; initialize package.el (optional for straight.el users)
  (when loader/enable-package-el
    (package-initialize))

  (loader/load-phase 1)

  ;; Set major modes in each buffer (for nim etc.)
  (dolist (b (buffer-list)) (with-current-buffer b (normal-mode)))

  ;; Revert GC thresh and an alist
  (setq gc-cons-threshold       800000
        file-name-handler-alist loader/file-name-handler-alist-save)
  )


(defun loader/load ()

  ;; Disable GC and file-name-handler-alist during startup
  (setq gc-cons-threshold                   256000000 ;; 256MB can be used without GC
        loader/file-name-handler-alist-save file-name-handler-alist
        file-name-handler-alist             nil)

  (setq loader/time-data nil)

  ;; Load files [0]
  (loader/load-phase 0)

  (if (and (boundp 'loader/has-run) loader/has-run)
      (loader/load-1)
    (let (trap-key) ;; letrec
      (setq trap-key (lambda ()
                       (remove-hook 'window-setup-hook trap-key)
                       (setq loader/first-event-save (read-key-sequence nil))
                       (loader/load-1)
                       ;; Replay key
                       (setq unread-command-events   (listify-key-sequence loader/first-event-save))
                       ;; Show profile when needed
                       (when loader/do-profile (loader/show-time-data))))
      (add-hook 'window-setup-hook trap-key)))

  (setq loader/has-run t)

  ;; New: saving the result of (read-key-sequence) and assign it to
  ;;      unread-command-events is fine for lazy loading.

  ;; Useful links :
  ;;   - Startup summary : https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html
  ;;   - (read-key-sequence) : https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Sequence-Input.html
  ;;   - (read-event) etc. : https://www.gnu.org/software/emacs/manual/html_node/elisp/Reading-One-Event.html
  ;;   - Simulating keys : https://emacs.stackexchange.com/a/2471
  ;;   - unread-command-events : https://www.gnu.org/software/emacs/manual/html_node/elisp/Event-Input-Misc.html
  ;;   - Command loop : https://www.gnu.org/software/emacs/manual/html_node/elisp/Command-Overview.html
  ;;   - Set major modes automatically : https://www.gnu.org/software/emacs/manual/html_node/elisp/Auto-Major-Mode.html

  ;; after-init-hook   : before deciding *scratch* or startup screen
  ;; post-command-hook : after ready to accept key input
  ;; find-file-hook    : file load; before buffer is rendered
  ;; window-setup-hook : after buffer is rendered
  ;; pre-command-hook  : when first key is pressed

  ;; Loading evil on "pre-command-hook" doesn't work well
  ;;   - Command for the first key event (say "i") is decided before evil is
  ;;   - loaded: "self-insert-command" runs instead of "evil-insert-state"
  ;;   - An attempt: call "(undo)" to cancel "self-insert-command"
  ;;   - But how can we determine corresponding command to be "evil-insert-state"?

  ;; So we want to avoid package manager startup just to load evil.
  ;;   - But "(require 'evil)" on "emacs-startup-hook" is not fast at all.
  ;;   - This also breaks use-package's reproductivity.

  ;; Loading evil (and straight.el) on "post-command-hook" doesn't feel very fast.
  ;;   - It seems evil is loaded BEFORE buffer is fully rendered.
  ;;   - It'd be better if there's  "after-buffer-render-hook".

  )

(provide 'loader)
