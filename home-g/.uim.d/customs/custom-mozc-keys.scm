(define mozc-on-key '("zenkaku-hankaku" "<IgnoreShift><Control>;"))
(define mozc-on-key? (make-key-predicate '("zenkaku-hankaku" "<IgnoreShift><Control>;")))
(define mozc-off-key '("zenkaku-hankaku" "<IgnoreShift><Control>;"))
(define mozc-off-key? (make-key-predicate '("zenkaku-hankaku" "<IgnoreShift><Control>;")))
(define mozc-kana-toggle-key '())
(define mozc-kana-toggle-key? (make-key-predicate '()))
(define mozc-vi-escape-key '("escape" "<Control>["))
(define mozc-vi-escape-key? (make-key-predicate '("escape" "<Control>[")))
