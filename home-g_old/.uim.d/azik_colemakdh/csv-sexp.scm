;; Usage: ./r7srfi csv-sexp.scm input.csv output.scm
;; csv -> csv-sexp (table is expressed as 2-dimensional list)

(define (csvline-to-list str)
  (define (comb-dblquotes str1) ;; \"\" to \"
    (let lo ((i 0) (str2 str1))
      (cond ((>= i (- (string-length str2) 1)) str2)
            ((and (eqv? #\" (string-ref str2 i)) (eqv? #\" (string-ref str2 (+ i 1))))
             (lo (+ i 1) (string-replace str2 "\"" i (+ i 2))))
            (else (lo (+ i 1) str2)))))
  (let lo ((x -1) (result '()))
    (if (<= (string-length str) x)
        (reverse result)
      (receive (s e newx) (read-token str x)
        (lo newx (cons (comb-dblquotes (substring str s e)) result))))))

(define (read-token str x) ;; x = pos of comma
  ;; if str="a,bc,"      and x=1  then return (2,3,3))
  ;; if str="\"\",\"\"," and x=-1 then return (1,4,5))
  (let ((le (string-length str)))
    (cond ((>= x (- le 1)) (values 0 0 le))
          ((not (eqv? #\" (string-ref str (+ x 1))))
           (let ((y (or (string-index str #\, (+ x 1)) le)))
             (values (+ x 1) y y)))
          (else (let lo ((i (+ x 2)))
                  (if (eqv? #\" (string-ref str i))
                      (if (eqv? #\" (string-ref str (+ i 1)))
                          (lo (+ i 2))
                        (values (+ x 2) i (+ i 1)))
                    (lo (+ i 1))))))))

(define (read-csv file)
  (call-with-input-file file
    (lambda (ip)
      (let lo ((line (read-line ip))
               (result '()))
        (if (eof-object? line)
            (reverse result)
          (lo (read-line ip) (cons (csvline-to-list line) result)))))))

(define (test)
  (map (lambda (x) (let* ((y (csvline-to-list (car x))) (e (equal? y (cadr x)))) (if e e (list e y))))
     '(("a,b,c"            ("a" "b" "c"))
       ("a,,b,"            ("a" "" "b" ""))
       ("\"\"\"\",,\" \"," ("\"" "" " " ""))
       )))

(define (main)
  (let ((ifile (cadr  (command-line)))
        (ofile (caddr (command-line))))
    (with-output-to-file ofile
      (lambda ()
        (write (read-csv ifile))))
    ))

(main)
