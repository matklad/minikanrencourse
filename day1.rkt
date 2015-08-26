(define (append l s)
  (if (null? l)
      s
      (cons
       (car l)
       (append (cdr l) s))))


(define (rember x xs)
  (cond
    ((null? xs) null)
    ((equal? x (car xs)) (cdr xs))

    (else
     (cons (car xs)
           (rember x (cdr xs))))))

(define (myappendo l s q)
  (conde
   [(== l '()) (== s q)]

   [(fresh (h t result)
           (== (cons h t) l)
           (== q (cons h result))
           (myappendo t s result))]))

(define (myrembero x xs q)
  (conde
   [(== xs '()) (== q '())]
   [(== xs (cons x q))]
   [(fresh (h t result)
           (== xs (cons h t))
           (=/= h x)
           (== q (cons h result))
           (myrembero x t result))]))
