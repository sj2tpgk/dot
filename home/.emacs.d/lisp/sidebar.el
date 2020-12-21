(setq mybuf (get-buffer-create "*my*")
      items (mapcar (lambda (b) (cons nil b)) (buffer-list)))

(defun make-leaf (x) (cons 'leaf x))
(defun leaf? (x) (eq (car x) 'leaf))
(defun tree-recurse (proc tree) (tree-recurse1 proc tree -1))
(defun tree-recurse1 (proc tree depth)
  (if (leaf? tree)
      (funcall proc (cdr tree) depth)
    (dolist (sub tree)
      (tree-recurse1 proc sub (1+ depth)))))
(defun tree-nth (n tree)
  (lexical-let ((i n) (result nil))
    (block rec
      (tree-recurse (lambda (item depth)
                      (when (= i 0)
                        (setq result item)
                        (return-from rec))
                      (setq i (1- i)))
                    tree))
    result))

(setq thetree (mapcar (lambda (b) (make-leaf `((buffer . ,b)))) (buffer-list)))

(defun itemset (item attr val)
  (let ((pair (assq attr item)))
    (if pair (setcdr pair val) (setcdr item (list (cons attr val))))))
(defun itemget (item attr) (cdr (assq attr item)))

(defun current-item () (tree-nth (1- (line-number-at-pos)) thetree))

(defun tree-visit ()
  (interactive)
  (switch-to-buffer-other-window (itemget (current-item) 'buffer)))

;; (defun list-move (lis i)
;;   (when (< i (1- (length lis)))
;;     (append (subseq lis 0 i)
;;             (list (nth (1+ i) lis) (nth i lis))
;;             (nthcdr (+ 2 i) lis))))
;;
;; (defun item-down ()
;;   (interactive)
;;   (let ((n (line-number-at-pos)))
;;     (setq bufl (list-move bufl (1- (line-number-at-pos))))
;;     (show)
;;     (goto-char (point-min)) (forward-line n)))
;;
;; (defun visit ()
;;   (interactive)
;;   (switch-to-buffer-other-window (cdr (current-item))))
;;
;; (defun mark ()
;;   (interactive)
;;   (set (current-item) 'mark t))
;;
;; (defun attatch-marked ()
;;   (interactive))

(defun setup ()
  (with-current-buffer mybuf
    (dolist (x '(("n" next-line) ("e" previous-line)
                 ("N" item-down) ("E" item-up)
                 ("SPC" mark) ("r" show) ("a" attatch-marked)
                 ("RET" tree-visit)))
      (local-set-key (kbd (car x)) (cadr x)))
    (erase-buffer)))

(defun show ()
  (interactive)
  (with-current-buffer mybuf
    (tree-recurse (lambda (item depth)
                    (insert (format "%s- %s"
                                    (make-string depth ? )
                                    (buffer-name (itemget item 'buffer))))
                    (newline))
                  thetree)))

(setup)
(show)
