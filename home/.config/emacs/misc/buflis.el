;; (defun up (&rest as)
;;   (with-current-buffer "*Ibuffer*"
;;     (message "%s" (point))
;;     (save-excursion
;;       (ibuffer-update nil))))

;; (add-hook 'window-selection-change-functions 'up)
;; (remove-hook 'window-selection-change-functions 'up)

;; (dolist (h '(after-focus-change-function))
;;   (add-function :after h 'up)
;;   (remove-function h 'up)
;;   )

;; (defun sbf ()
;;   (interactive)
;;   (let ((b (ibuffer-current-buffer)))
;;     (with-selected-frame f (switch-to-buffer b))))

;; (define-key ibuffer-mode-map (kbd "a") 'sbf)

;; (setq f (selected-frame))

(server-start)

(defvar status-frame)

(defun setup ()
  (interactive)
  (setq status-frame (make-frame))
  (with-selected-frame status-frame
    (buflis-update)
    (switch-to-buffer "*status*")))

(setq hidden-bufs '("*Messages*" "*scratch*" "NEWS" "*Backtrace*" "*Help*"
                    "*Ibuffer*" "*Shell Command Output*"
                    "*status*"))

(require 'dash) (require 's)
(defun buflis-update ()
  (interactive)
  (let ((lines
         (sort
          (mapcar (lambda (b)
                    (format "%s %-20.20s %-20.20s %s\n"
                            (if (with-current-buffer b buffer-read-only) "R" (if (buffer-modified-p b) "+" "-"))
                            (decor (buffer-name b) "yellow")
                            (substring (symbol-name (with-current-buffer b major-mode)) 0 -5)
                            (with-current-buffer b (shorten default-directory))
                            ))
                  (--remove (or (member (buffer-name it) hidden-bufs)
                                (string-prefix-p " " (buffer-name it)))
                            (buffer-list)))
          'string<)))
    (with-current-buffer (get-buffer-create "*status*")
      (erase-buffer)
      (mapc 'insert lines)
      nil)))

;; (add-hook 'after-save-hook 'buflis-update)

(defun decor (str &optional fg bg bold underline)
  (let ((ps (append (and fg `(:foreground ,fg))
                    (and bg `(:background ,bg))
                    (and bold `(:weight bold))
                    (and underline `(:underline t)))))
    (if ps (propertize str 'face ps) str)))

;; (message (decor "hello" "red" "blue" t t))

(defun shorten (path)
  ;; relative to current dir ?
  (--> (abbreviate-file-name path)
       (replace-regexp-in-string "/$" "" it)
       (replace-regexp-in-string "\\([._]*[^/]\\)[^/]*/" "\\1/" it)))

(--map (replace-regexp-in-string "\\([._]*[^/]\\)[^/]*/" "\\1/"
                                 (replace-regexp-in-string "/$" "" it))
       '("~/.emacs.d"
         "~/.emacs.d/config"
         "~/.emacs.d/config/file.el"
         "/home"
         "/home/user1"
         "/home/user1/"
         "/home/user1/.vimrc"))

(nmap "|" 'buflis-update)


(defvar buftree nil)
;; tree = (obj) | (obj . trees)
(defun bt-show ()
  (interactive)
  (let ((buf (get-buffer-create "*buftree*")))
    (labels ((show (d b)
                   (insert (format "%s%s\n" (make-string (* 2 d) ?\s) b)))
             (rec (depth tree)
                  (show depth (car tree))
                  (--each (cdr tree) (rec (1+ depth) it))))
      (with-current-buffer buf
        (erase-buffer)
        (--each (cdr buftree) (rec 0 it))))))

(defun make-buftree ()
  (cons 'root (mapcar 'list (buffer-list))))
(setq buftree (make-buftree))

(defun add-new-buf (buf)
  (labels ((find-subtree (tree buf)
                         (if (eq (car tree) (current-buffer))
                             tree
                           (--find (find-subtree it buf) (cdr tree)))))
    (let ((tree (find-subtree buftree buf)))
      (setcdr tree (cons (list buf) (cdr tree))))))

(add-new-buf (get-buffer "*scratch*"))

(nmap "|" 'bt-show)

(add-hook 'find-file-hook 'add-new-buf)

