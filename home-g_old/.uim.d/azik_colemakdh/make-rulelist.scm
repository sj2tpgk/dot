;; Usage: r7srfi make-rulelist.scm csv-sexp.scm rules.scm rules-uim-utf8.scm
;; csv-sexp -> rules (list of rules as pair) and rules in uim format (utf8)

(define (csv-cols csv) (length (car csv)))
(define (csv-rows csv) (length csv))
(define (csv-ref csv row col) (list-ref (list-ref csv row) col))

(define (csv-to-rules csv)
  (list-ec (: i (csv-cols csv))
           (: j (csv-rows csv))
           (not (or (= 0 i) (= 0 j)))
           (not (string-null? (csv-ref csv j i)))
           (cons (string-append (csv-ref csv 0 i) (csv-ref csv j 0))
                 (csv-ref csv j i))))

(define (print-rules rule)
  (for-each (lambda (x)
              (display (car x))
              (display " ")
              (display (cdr x))
              (newline))
            rules))

(define (rules-uim-form rules)
  (map (lambda (r)
         (let ((in-sp (map string (string->list (car r))))
               (out-sp-list (map (lambda (s)
                                   (let* ((sk (or (kata s) s))
                                          (shk (or (han-kata s) s)))
                                     (list s sk shk)))
                                 (unfold string-null?
                                         ;; workaround katakana v exists, but
                                         ;; hira and han-kata version doesn't
                                         (lambda (s)
                                           (if (string-prefix? "う゛" s)
                                               "う゛"
                                             (substring s 0 1)))
                                         (lambda (s)
                                           (if (string-prefix? "う゛" s)
                                               (substring s 2 (string-length s))
                                             (substring s 1 (string-length s))))
                                         (cdr r)))))
           ;;        (,out-sp-list) works in skk, but not in anthy
           `((,in-sp) ,out-sp-list)))
       rules))


(define (kata c)     (let ((p (assoc c hira-kata-table)))     (and p (cdr p))))
(define (han-kata c) (let ((p (assoc c hira-han-kata-table))) (and p (cdr p))))

(define hira-kata-table
  '(("あ" . "ア") ("い" . "イ") ("う" . "ウ") ("え" . "エ") ("お" . "オ") ("か" . "カ") ("き" . "キ") ("く" . "ク") ("け" . "ケ") ("こ" . "コ") ("さ" . "サ") ("し" . "シ") ("す" . "ス") ("せ" . "セ") ("そ" . "ソ") ("た" . "タ") ("ち" . "チ") ("つ" . "ツ") ("て" . "テ") ("と" . "ト") ("な" . "ナ") ("に" . "ニ") ("ぬ" . "ヌ") ("ね" . "ネ") ("の" . "ノ") ("は" . "ハ") ("ひ" . "ヒ") ("ふ" . "フ") ("へ" . "ヘ") ("ほ" . "ホ") ("ま" . "マ") ("み" . "ミ") ("む" . "ム") ("め" . "メ") ("も" . "モ") ("や" . "ヤ") ("ゆ" . "ユ") ("よ" . "ヨ") ("ら" . "ラ") ("り" . "リ") ("る" . "ル") ("れ" . "レ") ("ろ" . "ロ") ("わ" . "ワ") ("を" . "ヲ") ("ん" . "ン") ("ぁ" . "ァ") ("ぃ" . "ィ") ("ぅ" . "ゥ") ("ぇ" . "ェ") ("ぉ" . "ォ") ("っ" . "ッ") ("ゃ" . "ャ") ("ゅ" . "ュ") ("ょ" . "ョ") ("ゎ" . "ヮ") ("が" . "ガ") ("ぎ" . "ギ") ("ぐ" . "グ") ("げ" . "ゲ") ("ご" . "ゴ") ("ざ" . "ザ") ("じ" . "ジ") ("ず" . "ズ") ("ぜ" . "ゼ") ("ぞ" . "ゾ") ("だ" . "ダ") ("ぢ" . "ヂ") ("づ" . "ヅ") ("で" . "デ") ("ど" . "ド") ("ば" . "バ") ("び" . "ビ") ("ぶ" . "ブ") ("べ" . "ベ") ("ぼ" . "ボ") ("ぱ" . "パ") ("ぴ" . "ピ") ("ぷ" . "プ") ("ぺ" . "ペ") ("ぽ" . "ポ")
    ("う゛" . "ヴ")))
(define hira-han-kata-table
  '(("あ" . "ｱ") ("い" . "ｲ") ("う" . "ｳ") ("え" . "ｴ") ("お" . "ｵ") ("か" . "ｶ") ("き" . "ｷ") ("く" . "ｸ") ("け" . "ｹ") ("こ" . "ｺ") ("さ" . "ｻ") ("し" . "ｼ") ("す" . "ｽ") ("せ" . "ｾ") ("そ" . "ｿ") ("た" . "ﾀ") ("ち" . "ﾁ") ("つ" . "ﾂ") ("て" . "ﾃ") ("と" . "ﾄ") ("な" . "ﾅ") ("に" . "ﾆ") ("ぬ" . "ﾇ") ("ね" . "ﾈ") ("の" . "ﾉ") ("は" . "ﾊ") ("ひ" . "ﾋ") ("ふ" . "ﾌ") ("へ" . "ﾍ") ("ほ" . "ﾎ") ("ま" . "ﾏ") ("み" . "ﾐ") ("む" . "ﾑ") ("め" . "ﾒ") ("も" . "ﾓ") ("や" . "ﾔ") ("ゆ" . "ﾕ") ("よ" . "ﾖ") ("ら" . "ﾗ") ("り" . "ﾘ") ("る" . "ﾙ") ("れ" . "ﾚ") ("ろ" . "ﾛ") ("わ" . "ﾜ") ("を" . "ｦ") ("ん" . "ﾝ") ("ぁ" . "ｧ") ("ぃ" . "ｨ") ("ぅ" . "ｩ") ("ぇ" . "ｪ") ("ぉ" . "ｫ") ("っ" . "ｯ") ("ゃ" . "ｬ") ("ゅ" . "ｭ") ("ょ" . "ｮ")
    ("が" . "ｶﾞ") ("ぎ" . "ｷﾞ") ("ぐ" . "ｸﾞ") ("げ" . "ｹﾞ") ("ご" . "ｺﾞ") ("ざ" . "ｻﾞ") ("じ" . "ｼﾞ") ("ず" . "ｽﾞ") ("ぜ" . "ｾﾞ") ("ぞ" . "ｿﾞ") ("だ" . "ﾀﾞ") ("ぢ" . "ﾁﾞ") ("づ" . "ﾂﾞ") ("で" . "ﾃﾞ") ("ど" . "ﾄﾞ") ("ば" . "ﾊﾞ") ("び" . "ﾋﾞ") ("ぶ" . "ﾌﾞ") ("べ" . "ﾍﾞ") ("ぼ" . "ﾎﾞ") ("ぱ" . "ﾊﾟ") ("ぴ" . "ﾋﾟ") ("ぷ" . "ﾌﾟ") ("ぺ" . "ﾍﾟ") ("ぽ" . "ﾎﾟ")
    ("う゛" . "ｳﾞ")))

(define (test)
  (equal? '(((("g" "a")) ((("が" "ガ" "ｶﾞ")))) ((("g" ":")) ((("っ" "ッ" "ｯ"))))
            ((("c" "a")) ((("ち" "チ" "ﾁ") ("ゃ" "ャ" "ｬ")))) ((("c" ":")) ((("-"))))
            ((("v" "a")) ((("う゛" "ヴ" "ｳﾞ") ("ぃ" "ィ" "ｨ")))) ((("v" ":")) (())))
          (rules-uim-form
           (csv-to-rules '(("" "g" "c" "v")
                           ("a" "が" "ちゃ" "う゛ぃ")
                           (":" "っ" "-" ""))))))

(define (main)
  (unless (= 4 (length (command-line)))
    (error ""))
  (let ((ifile      (cadr (command-line)))
        (ofile-lisp (caddr (command-line)))
        (ofile-uim  (cadddr (command-line))))
    (with-input-from-file ifile
      (lambda ()
        (let* ((csv (read)) (rules (csv-to-rules csv)))
          (with-output-to-file ofile-lisp
            (cut write rules))
          (with-output-to-file ofile-uim
            (cut write (rules-uim-form rules))))))))

(main)
