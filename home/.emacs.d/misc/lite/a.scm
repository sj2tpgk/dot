(define (rev l)
  (let loop ((l1 l) (r '()))
    (if (null? l)
        l
        (loop (cdr l1) (cons (car l) r)))))

