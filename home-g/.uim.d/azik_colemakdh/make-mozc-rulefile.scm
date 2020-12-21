;; Usage: r7srfi make-rulelist.scm csv-sexp.scm rules.scm rules-uim-utf8.scm
;; csv-sexp -> rules (list of rules as pair) and rules in uim format (utf8)

(define (main)
  (unless (= 3 (length (command-line)))
    (error ""))
  (let* ((ifile      (cadr   (command-line)))
         (ofile-mozc (caddr  (command-line)))
         (rule-list  (with-input-from-file ifile read)))
    (with-output-to-file ofile-mozc
      (lambda ()
        (for-each (lambda (one-rule)
                    (display (car one-rule))
                    (display "\t")
                    (display (cdr one-rule))
                    (newline))
                  rule-list)))))

(main)
