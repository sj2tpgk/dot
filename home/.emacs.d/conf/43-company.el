(use-package company
  :ensure t
  :commands (global-company-mode)
  :init
  (add-hook 'evil-normal-state-exit-hook 'global-company-mode)
  :config
  (remove-hook 'evil-normal-state-exit-hook 'global-company-mode)
  ;; small delay
  ;; comp types: file, snippet, keyword
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        completion-styles  '(initials basic partial-completion)
        company-backends   '(
                             ;; company-bbdb
                             ;; company-eclim
                             ;; company-semantic
                             ;; company-clang
                             ;; company-xcode
                             ;; company-cmake
                             ;; company-capf
                             (company-capf :with company-files :with company-yasnippet)
                             (company-dabbrev-code company-gtags company-etags company-keywords)
                             company-dabbrev
                             company-oddmuse
                             )
        company-global-modes '(not inferior-python-mode org-mode))


  ;; kakutei kakspace next quit space
  ;; Complete on symbols (,.() etc.)
  (defun company-smart-space ()
    (interactive)
    (if (= company-selection 0) (company-abort) (company-complete-selection))
    (insert " "))

  (eldoc-add-command 'company-smart-space 'company-complete-selection 'company-abort)

  ;; (global-company-mode 1)
  (company-tng-configure-default) ;; tab key for cycle+insert
  ;; (global-company-fuzzy-mode 1)
  )

;; TODO
;;  priority = currentBuf > otherBufOfSameType > Dict > otherBufs
;;  evaluation function
;;  completion from file-type specific dictionary (with different keybinding)

;; (use-package hippie-exp
;;   :commands (hippie-expand my-he/file-comp-cmd)

;;   :config
;;   (setq hippie-expand-verbose nil)

;;   (setq hippie-expand-try-functions-list
;;         '(
;;           my-he/try-expand-hist
;;           my-he/try-expand-fuzzy
;;           ;; my-he/try-expand-test
;;           ))

;;   (defun my-he/ordered-lset-diff (l1 l2 same?)
;;     "Compute set difference L1 - L2 (using SAME? for equality test).
;; Returns newly allocated list."
;;     (let ((res nil))
;;       (dolist (a l1)
;;         (let ((in-l2 nil)
;;               (l l2))
;;           (while l
;;             (if (funcall same? a (car l))
;;                 (setq in-l2 t
;;                       l nil)
;;               (setq l (cdr l))))
;;           (unless in-l2
;;             (push a res))))
;;       (nreverse res)))

;;   (defun my-he/remove (elt lis same?)
;;     "Delete ELT from LIS using SAME? for equality test."
;;     (my-he/ordered-lset-diff lis (list elt) same?))

;;   (defun my-he/remove-dups (lis same?)
;;     "Remove duplicates in LIS using SAME? for equality test."
;;     (let ((res nil) (l lis))
;;       (while l
;;         (push (car l) res)
;;         (setq l (my-he/remove (car l) (cdr l) same?)))
;;       (nreverse res)))

;;   ;; Hist completion with fuzzy matching

;;   (defun my-he/try-expand-hist (has-init)
;;     (unless has-init
;;       ;; init region
;;       (he-init-string (he-lisp-symbol-beg) (point))
;;       ;; add original text to he-tried-table
;;       (add-to-list 'he-tried-table he-search-string)
;;       ;; set he-expand-list to candidate list
;;       ;; ensure no completion duplicates
;;       (let ((l (my-he/matching-hists he-search-string)))
;;         (setq he-expand-list
;;               (my-he/ordered-lset-diff l he-tried-table 'string=))))

;;     ;; do completion or reset, and return nil iff there's no more candidates
;;     (cond
;;      ((null he-expand-list)
;;       ;; reset region if has-init
;;       (when has-init (he-reset-string))
;;       nil)
;;      (t
;;       ;; completion with first candidate
;;       (let ((cand (pop he-expand-list)))
;;         (he-substitute-string cand)
;;         (my-he/add-hist cand))
;;       t)))

;;   (defvar my-he/hist nil)
;;   (defun my-he/add-hist (str)
;;     (setq my-he/hist (my-he/remove str my-he/hist 'string=))
;;     (when (< 100 (length my-he/hist)) (setcdr (nthcdr 100 my-he/hist) nil))
;;     (push str my-he/hist))
;;   (defun my-he/matching-hists (str)
;;     (let ((regexp (my-he/create-fuzzy-regexp str))
;;           (res nil))
;;       (dolist (hist my-he/hist)
;;         (when (string-match regexp hist) (push hist res)))
;;       (nreverse res)))

;;   ;; Fuzzy completion

;;   (defun my-he/try-expand-fuzzy (has-init)
;;     (unless has-init
;;       ;; init region
;;       (he-init-string (he-lisp-symbol-beg) (point))
;;       ;; add original text to he-tried-table
;;       (add-to-list 'he-tried-table he-search-string)
;;       ;; set he-expand-list to candidate list
;;       ;; ensure no completion duplicates
;;       (let ((l (my-he/collect-regmatch (my-he/create-fuzzy-regexp he-search-string) t)))
;;         (setq he-expand-list
;;               (my-he/ordered-lset-diff l he-tried-table 'string=))))

;;     ;; do completion or reset, and return nil iff there's no more candidates
;;     (cond
;;      ((null he-expand-list)
;;       ;; reset region if has-init
;;       (when has-init (he-reset-string))
;;       nil)
;;      (t
;;       ;; completion with first candidate
;;       (let ((cand (pop he-expand-list)))
;;         (he-substitute-string cand)
;;         (my-he/add-hist cand))
;;       t)))

;;   (defun my-he/create-fuzzy-regexp (str &optional left-bndry right-bndry constituent)
;;     ;; TODO properly escape * > \\ etc. in STR
;;     (unless left-bndry (setq left-bndry "\\_<"))
;;     (unless right-bndry (setq right-bndry "\\_>"))
;;     (unless constituent (setq constituent "\\(\\w\\|\\s_\\)"))

;;     (concat left-bndry
;;             (mapconcat (lambda (c)
;;                          (concat (string c)
;;                                  constituent
;;                                  "*"))
;;                        str "")
;;             right-bndry))

;;   (defun my-he/collect-regmatch (re &optional all-buffers)
;;     (let ((collection nil)
;;           (buffers (if all-buffers
;;                        (append (remove (current-buffer) (buffer-list))
;;                                (list (current-buffer)))
;;                      (list (current-buffer)))))
;;       (save-excursion
;;         (dolist (b buffers)
;;           (with-current-buffer b
;;             (goto-char (point-min))
;;             (while (search-forward-regexp re nil t)
;;               ;; or use (thing-at-point 'symbol t) ?
;;               (push (buffer-substring-no-properties (match-beginning 0)
;;                                                     (match-end 0))
;;                     collection)))))
;;       (my-he/remove-dups collection 'string=)))

;;   ;; Filename completion

;;   (fset 'my-he/file-comp-cmd (make-hippie-expand-function '(try-complete-file-name-partially
;;                                                             try-complete-file-name)))
;;   (fset 'my-he/yas (make-hippie-expand-function '(yas-hippie-try-expand)))
;;   )

;; Hippie documentation {{{

;; (he-init-string beg end)
;;   set the region from BEG to END to be expanded. also sets he-search-string

;; he-search-string  (variable)
;;   the original text in the expanded region (set when he-init-string runs)

;; (he-string-member str lst trans-case)
;;   same as (member str lst) unless trans-case is non-nil

;; (he-reset-string)
;;   reset the expanded region (to its initial contents)

;; (he-substitute-string str)
;;   substitute the expanded region with str

;; he-tried-table  (variable)
;;   list containing all tried expansions so far

;; The argument OLD of try-*** functions is nil in the first call or t otherwise


;; Example try-expand function

;; (defun my-he/try-expand-test (has-init)
;;   (unless has-init
;;     ;; init region
;;     (he-init-string (he-lisp-symbol-beg) (point))
;;     ;; add original text to he-tried-table
;;     (add-to-list 'he-tried-table he-search-string)
;;     ;; set he-expand-list to candidate list
;;     ;; ensure no duplicates in he-expand-list
;;     ;; ensure no strings from he-tried-table in he-expand-list
;;     (let ((l '("apple" "banana" "ant" "basketball")))
;;       (setq l (delete-dups (copy-sequence l)))
;;       (dolist (x he-tried-table) (delete x l))
;;       (setq he-expand-list l)))

;;   ;; do completion or reset, and return nil iff there's no more candidates
;;   (cond
;;    ((null he-expand-list)
;;     ;; reset region if has-init
;;     (when has-init (he-reset-string))
;;     nil)
;;    (t
;;     ;; completion with the first candidate
;;     (let ((cand (pop he-expand-list)))
;;       (he-substitute-string cand))
;;     t)))

;; }}}
