(require "./faster-miniKanren/mk.rkt" )



(define (automato str path result)
  (letrec ((S1 (lambda (str path result)
                (fresh (-str -path)
                       (conde
                        [(== '() str) (== '(1)  path) (== result #t)]
                        [(== `(0 . ,-str) str) (== `(1 . ,-path) path)
                         (S2 -str -path result)]
                        [(== `(1 . ,-str) str) (== `(1 . ,-path) path)
                         (S1 -str -path result)]))))
          (S2 (lambda (str path result)
                (fresh (-str -path)
                       (conde
                        [(== '() str) (== '(2)  path) (== result #f)]
                        [(== `(0 . ,-str) str) (== `(2 . ,-path) path)
                         (S1 -str -path result)]
                        [(== `(1 . ,-str) str) (== `(2 . ,-path) path)
                         (S2 -str -path result)])))))
    (S1 str path result)))
