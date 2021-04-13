(require-module "skk")
(require-module "anthy")
(require-module "anthy-utf8")
(require "japanese-azik.scm")
(require "japanese.scm")

;; File containing azik rules; must be EUC-JP
(define azikpath "/home/tpa4/.uim.d/azik_colemakdh/rules-uim-eucjp.scm")

;; "abc" -> ("a" "b" "c")
(define (string->string-list str) (map string (string->list str)))

(define (push-azik-rule! rule)
  (let ((a (car ja-azik-rule)) (d (cdr ja-azik-rule)))
    (set-car! ja-azik-rule rule)
    (set-cdr! ja-azik-rule (cons a d))))
(define (make-rule seq hira kata han-kata)
  (let* ((seq-separated (string->string-list seq))
         (new-entry `((,seq-separated) ((,hira ,kata ,han-kata)))))
    new-entry))
(define (add-simple-azik-rule! seq val)
  (push-azik-rule! (make-rule seq val val val)))

;; Load azik rule
;; (set! ja-azik-rule (append (with-input-from-file azikpath read) ja-rk-rule-basic))
(set! ja-azik-rule (with-input-from-file azikpath read))

;; For anthy; 半角スペース
(define ja-space '(" " " " " "))

;; For skk; 半角/全角スペース
;; doesn't work for anthy since it handles space in its own way
(add-simple-azik-rule! " "  " ")
(add-simple-azik-rule! "h " " ")
(add-simple-azik-rule! "z " "　")
