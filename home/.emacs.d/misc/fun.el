(require 'dash)
(defun primitive? (s) (or (not (listp s)) (eq (car s) 'fn)))

(defun reduce1 (s)
  (if (primitive? s)
      s
    (let ((r (reduce1-in-list s)))
      (if (equal r s)
          (reduce-app s)
        r))))

(defun reduce1-in-list (l)
  (if (null l)
      nil
    (let ((r (reduce1 (car l))))
      (if (equal r (car l))
          (cons r (reduce1-in-list (cdr l)))
        (cons r (cdr l))))))

(defun reduce-app (l)
  (cond
   ((and (listp (car l)) (eq (caar l) 'fn)) (reduce-fn (build-table l) l))
   ((eq (car l)  '+)  (reduce '+ (cdr l)))
   (t (error "err %s" l))))

(defun build-table (app)
  (let ((formals (cadr (car app)))
        (vals (cdr app)))
    (-zip formals vals)))

(defun reduce-fn (table app)
  (cl-labels ((rec (s tbl)
                   (cond
                    ((null tbl) s)
                    ((symbolp s) (let ((pair (assq s tbl)))
                                   (if pair (cdr pair) s)))
                    ((numberp s) s)
                    ((eq (car s) 'fn) (list 'fn
                                            (cadr s)
                                            (rec (caddr s)
                                                 (-remove (lambda (pair)
                                                            (memq (car pair) (cadr s)))
                                                          tbl))))
                    (t (let (res)
                         (dolist (x s) (push (rec x tbl) res))
                         (nreverse res))))))
    (rec (caddr (car app)) table)))

;; (reduce1 '((fn (x) (+ ((fn (x) (+ x 1)) 100) x)) 5))
;; (my-show '((fn (x) (+ ((fn (x) (+ x 1)) 100) x)) 5))
;; (my-show '(((fn (f) (fn (x) (f (x x))))
;;             (fn (f) (fn (x) (f (x x)))))))

;; (+ 1 2 3)
;; (short-list) = one-line
;; (long-list no nest) = one-line
;; (long-list has nest) = split by continuous simples
;; must not separate fn and (a)

;; (pp2 '(1 sep 2))
;; (pp2 '(fn (n) sep (a b c)))
;; (pp2 '(+ 1 2 sep (a b c) sep 8 9))

(defun pp2 (lsep)
  (letrec ((sep? (x) (eq x 'sep))
           (pp-rec (x) (if (and (not (null x)) (listp x))
                           (pp-list idt x)
                         (format "%s" x)))
           (pp-list (x)
                    (cond
                     ((sep? (cadr lsep)) (pp-list-do 1 x))
                     ((seq? (caddr lsep)) (pp-list-do 5 x)))))
    ;; (cond ((sep? (cadr lsep)) (format "(%s\n%s)" (car lsep) (caddr lsep)))
    ;;   (t (format "%s" lsep))))
    (pp-rec 0 lsep)))

(defun my-show (e)
  (with-current-buffer "*reduction*"
    (setq print-level nil)
    (erase-buffer)
    ;; (insert (pp e nil))
    (pp e (current-buffer))
    (sit-for 0.5)
    (let ((e2 (reduce1 e)))
      (unless (equal e e2)
        (my-show e2)))
    ))

;; (my-show '(+ (+ 3 1) (+ 4 2)))

(defun reduction (&optional expr)
  (interactive)
  (let ((buf (or (get-buffer "*reduction*")
                 (generate-new-buffer "*reduction*"))))
    (switch-to-buffer buf)
    (setq print-level nil)
    (let ((e expr) (e2 (reduce1 expr)))
      (while (not (equal e e2))
        (erase-buffer)
        (pp e buf)
        (goto-char 0)
        (sit-for 0.5)
        (setq e e2 e2 (reduce1 e2))))))

(reduction '(((fn (le)
                  ((fn (f) (f f))
                   (fn (f)
                       (le (fn (x) ((f f) x))))))
              (fn (le)
                  (fn (n) (+ 1 (le n))))
              ;; +
              )
             1))
