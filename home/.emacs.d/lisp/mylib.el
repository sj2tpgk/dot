(progn
  (put 'use 'lisp-indent-function 2)
  (defmacro use (pkgspec &optional cmds &rest body)
    (let* ((pkgpl (use-analyze-pkgspec pkgspec))
           (pkg   (plist-get pkgpl :pkg))
           (rules (plist-get pkgpl :rules))
           (cfgs2 (plist-get pkgpl :cfgs))
           (cmds2 (if (and (consp cmds) (not (null cmds))) `(:commands ,(use-expand-withs rules cmds))))
           (body2 (use-expand-kwds
                   (use-expand-withs rules
                                     (append (if (listp cmds) nil (list cmds)) body)))))
      `(use-package ,pkg :ensure t ,@cfgs2 ,@cmds2 ,@body2)))
  (defun use-analyze-pkgspec (pkgspec)
    (let* ((str (symbol-name pkgspec))
           (l   (split-string str ":"))
           (pkg (intern (car l)))
           (cfg (let ((l1 (mapcar (lambda (x)
                                    (list
                                     (intern
                                      (cond
                                       ((string= x "")   (format "%s" pkg))
                                       ((string= x "m")  (format "%s-mode" pkg))
                                       ((string= x "gm") (format "global-%s-mode" pkg))
                                       (t                (format "%s-%s" pkg x))))))
                                  (cdr l))))
                  (and l1 `(:config ,@l1))))
           (rls (list "$" (car l))))
      `(:pkg ,pkg :cfgs ,cfg :rules ,rls)))
  (defun use-expand-withs (rules expr)
    (cond
     ((symbolp expr) (use-modify rules expr))
     ((and (listp expr) (eq (car expr) 'use))
      `(use ,(use-expand-withs rules (cadr expr)) ,@(cddr expr)))
     ((consp expr)
      (cons (use-expand-withs rules (car expr))
            (use-expand-withs rules (cdr expr))))
     (t expr)))
  (defun use-modify (rules sym) ;; rules = list of strings
    (let (sf st (ss (symbol-name sym)))
      (while (setq sf (pop rules) st (pop rules))
        (let ((snew
               (replace-regexp-in-string (concat "\\(.?\\)" (regexp-quote sf) "\\(.?\\)")
                                         (lambda (ms)
                                           (let ((s1 (match-string 1 ms))
                                                 (s2 (match-string 2 ms)))
                                             (format "%s%s%s%s%s"
                                                     s1 (if (string-match-p "^[^-+=*/_~!@$%^&:<>{}?]" s1) "-" "")
                                                     st
                                                     (if (string-match-p "^[^-+=*/_~!@$%^&:<>{}?]" s2) "-" "") s2)))
                                         ss)))
          (unless (string= ss snew)
            (setq ss snew))))
      (intern ss)))
  (defun use-expand-kwds (expr)
    (let ((KWDS '(:c :config :i :init)))
      (mapcar (lambda (x) (or (and (keywordp x) (plist-get KWDS x)) x))
              expr)))
  ;; (mapcar (lambda (s) (use-modify '("$" "evil" "%" "company") s)) '($ $mode $-mode $global%mode global-$mode))
  )

(defun add-one-time-hook (hook func &optional skips) ;; Add hook that runs only once.
  "Add hook that runs only once. Optionally, skip first SKIPS calls."
  (setq skips (or skips 0))
  (let (f)
    (setq f (lambda (&rest args)
              (when (<= skips 0)
                (apply func args)
                (remove-hook hook f))
              (setq skips (1- skips))))
    (add-hook hook f)))

(provide 'mylib)
