;; -*- lexical-binding: t; -*-

;; todo
;;   save modifications
;;   restore default?
;;   evil integration
;;   multicolumn

;; (require 'cl)

(setq ;; kc-managed-maps '(emacs-lisp-mode-map global-map)
 kc-dat-file (concat user-emacs-directory "/kc-file.el"))

;; Key logger
;; todo use hashtable (or once each 10 cmds etc.)
;; todo hidden commands (self-insert, mouse etc..)
(setq keylog-list nil)
(defun kc-keylogger ()
  (push this-command keylog-list))
(defun kc-kl-freq-cmds (list)
  (let ((al nil))
    (dolist (sym list)
      (let ((pair (assoc sym al)))
        (if pair (setcdr pair (1+ (cdr pair))) (push (cons sym 0) al))))
    (sort al (lambda (x y) (> (cdr x) (cdr y))))))
(kc-kl-freq-cmds keylog-list)
(add-hook 'pre-command-hook 'kc-keylogger)

;; Save/load
(defun kc-save ()
  (interactive)
  (with-temp-file kc-dat-file
    (let ((standard-output (current-buffer)))
      (dolist (mapname kc-managed-maps)
        (princ (format ";; Keymap: %s\n" mapname))
        (mapcar-keymap-rec
         (lambda (k b)
           (prin1 `(define-key ,mapname ,k ',b))
           (princ (format " ;; %s" (key-description k)))
           (terpri))
         (eval mapname))
        (terpri)))))
(definteractive kc-load (load-file kc-dat-file))
;; (kc-load)
;; (add-hook 'kill-emacs-hook 'kc-save)

;; Helper
(defun mapcar-keymap-rec (func map)
  "Map thru MAP flattening, calling FUNC."
  (let ((res nil))
    (cl-labels ((rec (v m)
                  (map-keymap
                   (lambda (k b)
                     (let ((v2 (concatenate 'vector v (vector k))))
                       (cond
                        ;; Some keymaps are symbols (e.g. Control-X-prefix)
                        ;; so (keymapp b) must be first.
                        ((keymapp b)
                         (rec v2 b))
                        ((symbolp b)
                         (push (funcall func v2 b) res)))))
                   m)))
      (rec [] map))
    (nreverse res)))
(defun kc-current-active-maps-by-name ()
  (let ((maps (current-active-maps))
        (names nil))
    (mapatoms (lambda (sym)
                (and (boundp sym)
                     (keymapp (eval sym))
                     (memq (eval sym) maps)
                     (push sym names))))
    names))

;; Panel
(defun kc-buffer () (get-buffer-create "*Keys*"))
(defmacro with-kc-buffer (&rest body)
  `(with-current-buffer (kc-buffer) ,@body))
(defun kc-get-bindings (map)
  "Return alist; each entry is (command key1 key2 ...)"
  (let ((al nil))
    (mapcar-keymap-rec
     (lambda (k b)
       (let ((l (assoc b al)))
         (if l
             (setcdr l (cons k (cdr l)))
           (push (list b k) al))))
     map)
    al))
;; (setq kc-panel-state nil)
(defun kc-show-maps (mapnames &optional nofold)
  (with-kc-buffer
   (erase-buffer)
   (let* ((freq (kc-kl-freq-cmds keylog-list)))
     (dolist (mapname mapnames)
       (let* ((map (eval mapname))
              (al1 (kc-get-bindings map))
              (al (sort al1
                        (lambda (x y)
                          ;; todo if same freq, ABC-order.
                          (> (or (cdr (assoc (car x) freq)) 0)
                             (or (cdr (assoc (car y) freq)) 0))
                          ;; (string< (format "%s" (car x)) (format "%s" (car y)))
                          )))
              (i 0)
              (folded (let ((p (assoc mapname nofold))) (not (and p (cdr p)))))
              )
         (insert (format "Keymap: %s " mapname))
         (insert-button "[Add]" 'mouse-action (lambda(a)(kc-add-cmd map)))
         (insert "\n")
         (cl-block loop1
           (dolist (x al)
             (let ((cmd (car x)) (keys (cdr x)))
               (insert-button (substring (format "%-20s" cmd) 0 20)
                              'help-echo (format "%s\n%s" cmd (if (fboundp cmd) (documentation cmd) "Not a function."))
                              'mouse-action (lambda(a)(kc-set-cmd x)))
               (insert "  =  ")
               (dolist (k keys)
                 (insert-button (format "%s" (key-description k))
                                'mouse-action (lambda(a)(kc-remove-key map k)))
                 (insert ","))
               (insert-button "[+]"
                              'mouse-action (lambda(a)(kc-add-key map cmd)))
               (insert "\n"))
             (cl-incf i)
             (when (and folded (< 20 i)) (cl-return-from loop1))
             ))
         (when (< 20 i)
           (insert-button (format "[%s...]\n" (if folded "More" "Hide"))
                          'mouse-action (lambda(a)(save-excursion (kc-show-maps mapnames(cons(cons mapname folded)nofold))))))
         (insert "\n")
         )))))
(defun kc-redraw ()
  (interactive)
  (let ((p (point)))
    ;; (kc-show-maps kc-managed-maps)
    (kc-show-maps (kc-current-active-maps-by-name))
    (goto-char (min p (point-max)))))
(defun kc-panel ()
  (interactive)
  (split-window-sensibly)
  (switch-to-buffer-other-window (kc-buffer))
  (kc-redraw))
(global-set-key [f7] 'kc-redraw)
(global-set-key [f8] 'kc-save)

;; Event actions
(defun kc-set-cmd (arg)
  (message "Called: %s" (list arg)))
(defun kc-remove-key (map key)
  (define-key map key nil)
  (message "Removed %s" (key-description key))
  (kc-redraw))
(defun kc-add-key (map cmd)
  (let* ((key (read-key-sequence "New key = ")))
    (define-key map key cmd)
    (message "Defined %s = %s" cmd (key-description key))
    (kc-redraw)))
(defun kc-add-cmd (map)
  (let ((cmd (read-command "Command = "))
        (key (read-key-sequence "Key = ")))
    (define-key map key cmd)
    (message "Added %s = %s" cmd (key-description key))
    (kc-redraw)))
