(require-module "skk")

;; Keybindings for my own features {{{
;; Note that " " means the space key.
(define latin-conv-space-key '(" "))
(define commit+space-key '("<Shift> "))
(define latin-ignore-key '("j"))

(define latin-conv-space-key? (make-key-predicate latin-conv-space-key))
(define commit+space-key? (make-key-predicate commit+space-key))
;; }}}

;; Check if a key-predicate accepts the key key-source {{{
;; example: (test-key-pred skk-on-key? "zenkaku-hankaku")
;; Note that key-predicates takes "translated key" as arguments.
;; thus this is not correct: (skk-on-key? "zenkaku-hankaku" 0)
;;                  0 means that no modifiers are pressed --^
;; see /usr/share/uim/key.scm for make-key-predicate , key-state-alist
(define (translate-key key-source)
  (apply apply-translators
         (cdr (parse-key-str key-source () -1 0))))
(define (test-key-pred key-pred? key-source)
  (let ((translated (translate-key key-source)))
    (key-pred? (nth 1 translated) (nth 2 translated))))
;; same func: (define (pt p? s) (let ((t (apply apply-translators (cdr (parse-key-str s () -1 0))))) (p? (nth 1 t) (nth 2 t)) ))
;; }}}

;; Ignore keybinding in latin conv mode {{{
;; if given key is alphabetical in latin mode, simply input the key.
(let* ((orig-proc skk-proc-state-kanji)
       (is-l-key? (make-key-predicate latin-ignore-key))
       )
  (set! skk-proc-state-kanji
                                        ; if key==j and (not latin) then toggle kana
                                        ; j seems not to be passed to skk-proc-state-kanji
        (lambda (c key key-state)
          (if (is-l-key? key key-state)
              (let ((sc (skk-find-descendant-context c)))
                (if (not (skk-context-latin-conv sc))
                    (begin
                      (skk-append-residual-kana sc)
                      (if (not (null? (skk-context-head sc)))
                          (begin
                            (skk-commit sc (skk-make-string
                                            (skk-context-head sc)
                                            (skk-opposite-kana
                                             (skk-context-kana-mode sc))))
                            (skk-flush sc)))
                      #f)
                  (orig-proc c key key-state)
                  ))
            (orig-proc c key key-state))
          ))
  )
;; }}}

;; Insert space in latin conversion (abbrev mode) {{{
;; the func skk-proc-state-kanji (spsk) runs on ▽ mode
;; (for both of kanji-conversion and latin-conversion)
(let* ((orig-skk-proc-state-kanji skk-proc-state-kanji)
       (space-key-translated (nth 1 (translate-key " ")))
       (insert-space (let* ((s (charcode->string space-key-translated))
                            (p (cons s (cons s (cons s s)))))
                       (lambda (sc) (skk-append-string sc p))))
       )
  (set! skk-proc-state-kanji
        (lambda (c key key-state)
                                        ; (if (and space-key? latin-conv-mode?) (insert-space) (orig))
          (if (latin-conv-space-key? key key-state)
              (let ((sc (skk-find-descendant-context c)))
                (if (skk-context-latin-conv sc)
                    (insert-space sc)
                  (orig-skk-proc-state-kanji c key key-state)))
            (orig-skk-proc-state-kanji c key key-state))
          )))
;; }}}

;; Mode_switch (ms) が変換中に確定として扱われる問題を修正 {{{
;; 変換中のキー入力は skk-proc-state-converting (spsc) が受け付ける
;; spsc では、ms は「確定」「次候補」といった操作キーに該当しないので、ただの文字入力(a,b,1 などと同じ)として扱われる。従って変換モードが終わる。
;; (これは Ryokou<SPC>suru の s を押した時点で「旅行」が確定され、変換モードが終わるのと同じ)
;; そこで ms が来たら即座に関数を中断し、#f (これは spsc 中の変数 res の初期値) を返すようにする。
;; それ以外の入力では引数をオリジナルの spsc に渡す
;; see /usr/share/uim/skk.scm
(let ((orig-skk-proc-state-converting skk-proc-state-converting)
      (mode-switch-key? (make-key-predicate "Mode_switch"))
      )
  (set! skk-proc-state-converting
        (lambda (c key key-state)
          (cond
           ((mode-switch-key? key key-state) #f)
           (else (orig-skk-proc-state-converting c key key-state))))))
;; }}}

;; Commit + space {{{
;; see /usr/share/uim/skk.scm
(let* ((orig-skk-proc-state-converting skk-proc-state-converting)
       (orig-skk-proc-state-kanji skk-proc-state-kanji)
       (hankaku-space-mapping "h ")
                                        ; simulate pressing `(car skk-commit-key)` usually it's "return"
       (simulate-commit-key
        (let ((key&key-state (cdr (translate-key (car skk-commit-key)))))
          (lambda (c) (apply skk-push-key c key&key-state))))
                                        ; insert space ' '. directly simulating pressing ' ' doesn't work.
                                        ; instead use some mapping for ' '
       (simulate-inserting-space
                                        ; (lambda (c)
                                        ;   (apply skk-push-key c (cdr (translate-key "h")))
                                        ;   (apply skk-push-key c (cdr (translate-key " "))) )
        (let ((key&key-states (map (lambda (s) (cdr (translate-key s)))
                                   (string->string-list hankaku-space-mapping))))
          (lambda (c)
            (let loop ((ls key&key-states) (last-val #f))
              (if (null? ls)
                  last-val
                (loop (cdr ls) (apply skk-push-key c (car ls)))))) ))
       )
  (set! skk-proc-state-kanji
        (lambda (c key key-state)
          (cond
           ((commit+space-key? key key-state)
            (simulate-commit-key c)
            (simulate-inserting-space c))
           (else (orig-skk-proc-state-kanji c key key-state)))
          ))
  (set! skk-proc-state-converting
        (lambda (c key key-state)
          (cond
           ((commit+space-key? key key-state)
                                        ; commiting will be automatically done, because the mapping 'h '
                                        ; starts with non-controlling char
            (simulate-inserting-space c))
           (else (orig-skk-proc-state-converting c key key-state))))))
;; }}}

;; 片仮名語を辞書に自動登録 {{{
(let* ((orig-proc skk-proc-state-kanji)
       )
  (set! skk-proc-state-kanji
        (lambda (c key key-state)
          (if (skk-kana-toggle-key? key key-state)
              (let ((sc (skk-find-descendant-context c)))
                (skk-append-residual-kana sc)
                (begin
                  (if (not (null? (skk-context-head sc)))
                                        ; get current string in `sc` and kana-mode
                      (let ((head      (skk-context-head sc))
                            (kana-mode (skk-context-kana-mode sc))
                            )
                                        ; register to the dict only when Hiragana -> (han)Katakana conv
                        (if (= kana-mode skk-type-hiragana)
                            (let* ((from (skk-make-string head kana-mode))
                                   (to   (skk-make-string head (skk-opposite-kana kana-mode)))
                                   (dict-entry (string-append from " /" to "/"))
                                   )
                              (with-output-to-file "/home/tpa4/.uim.d/newentry"
                                (lambda ()
                                        ; (display dict-entry)
                                  (display from) (newline)
                                  (display to) (newline)
                                  ))
                              (process-with-daemon "/home/tpa4/.uim.d/addentry.sh")
                              ))
                                        ; commit converted string and flush as usual
                        (skk-commit sc (skk-make-string
                                        head
                                        (skk-opposite-kana kana-mode)))
                        (skk-flush sc)))
                  #f))
            (orig-proc c key key-state)
            ))))
                                        ; }}}

                                        ; ;; 動的補完で候補が一つしかないときは積極的な確定をする {{{
                                        ; (define (make-key-simulator key)
                                        ;   (let ((key&key-state (cdr (translate-key (car skk-commit-key)))))
                                        ;     (lambda (c) (apply skk-push-key c key&key-state))))
                                        ; (let* ((orig-proc skk-proc-state-kanji)
                                        ;        (simulate-commit-key (make-key-simulator (car skk-commit-key)))
                                        ;        )
                                        ;   (set! skk-proc-state-kanji
                                        ;     (lambda (c key key-state)
                                        ;       (if (skk-begin-conv-with-completion-key? key key-state)
                                        ;         ; do uim's own way --ekato. see [Anthy-dev: 2646, 2654]
                                        ;         ; comp -> incr -> null? -> decr -> commit or nop
                                        ;         (let ((sc (skk-find-descendant-context c)))
                                        ;           (cond
                                        ;             ; complete 1 or more characters
                                        ;             ((and skk-dcomp-activate?
                                        ;                   (not (skk-rk-pending? sc))
                                        ;                   (not (string=? (skk-context-dcomp-word sc) "")))
                                        ;              (let ((sl (string-to-list (skk-context-dcomp-word sc))))
                                        ;                (skk-string-list-to-context-head sc sl)
                                        ;                (let ((result (skk-begin-conversion sc)))
                                        ;                  ; try next candidate. if it's null, theres only one candidate.
                                        ;                  (skk-incr-candidate-index sc)
                                        ;                  (let ((theres-only-one-candidate (null? (skk-get-current-candidate sc))))
                                        ;                    (skk-decr-candidate-index sc)
                                        ;                    (if theres-only-one-candidate
                                        ;                      (simulate-commit-key c)
                                        ;                      result)
                                        ;                    ))
                                        ;                ))
                                        ;             ; complete no characters
                                        ;             ((and skk-dcomp-activate?
                                        ;                   (skk-rk-pending? sc)
                                        ;                   (not (string=? (skk-context-dcomp-word sc) "")))
                                        ;              (skk-append-residual-kana sc)
                                        ;              (let ((sl (string-to-list
                                        ;                          (skk-lib-get-dcomp-word
                                        ;                            skk-dic
                                        ;                            (skk-make-string
                                        ;                              (skk-context-head sc)
                                        ;                              (skk-context-kana-mode sc))
                                        ;                            skk-use-numeric-conversion?
                                        ;                            skk-use-look?))))
                                        ;                (if (not (null? sl))
                                        ;                  (begin
                                        ;                    (skk-string-list-to-context-head sc sl)
                                        ;                    (skk-begin-conversion sc))
                                        ;                  (begin
                                        ;                    (if (not (null? (skk-context-head sc)))
                                        ;                      (skk-begin-conversion sc)
                                        ;                      (skk-flush sc))))))
                                        ;             (else
                                        ;               (skk-append-residual-kana sc)
                                        ;               (if (not (null? (skk-context-head sc)))
                                        ;                 (skk-begin-conversion sc)
                                        ;                 (skk-flush sc)))
                                        ;             ))
                                        ;         (orig-proc c key key-state)
                                        ;         ))))
                                        ; ; }}}

                                        ; ;; Uim indicator in polybar {{{
                                        ; (let ((orig-skk-context-set-state! skk-context-set-state!)
                                        ;       (skk-mode-file "/home/tpa4/.uim.d/skkmode"))
                                        ;   (set! skk-context-set-state!
                                        ;     (lambda (sc state)
                                        ;       ; (display state)
                                        ;       ; identify kanji-conv and latin-conv
                                        ;       ; identify hiragana and katakana
                                        ;       (call-with-output-file skk-mode-file
                                        ;         (lambda (oport) (display (symbol->string state) oport)))
                                        ;       (orig-skk-context-set-state! sc state))))
                                        ; ;; }}}

;; less keys, consistency

;; insert spc + abbrev mode + insert space
;; enter(commit) + insert space

;; map ja,ji,ju ... onto la,li,lu ...
;; and use j as controlling key

;; vim:ft=scheme:fdm=marker
