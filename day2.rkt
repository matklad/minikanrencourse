(define one-item
  (lambda (x s)
    (cond
      [(null? s) '()]
      [else (cons (cons x (car s))
                  (one-item x (cdr s)))])))


(define (one-itemo x s q)
  (conde
   [(== s '()) (== q '())]
   [(fresh (h t acc)
           (== (cons h t) s)
           (== q (cons (cons x h)
                       acc))
           (one-itemo x t acc))]))


(define assq
  (lambda (x ls)
    (cond
      [(null? ls) #f]
      [(eq? (caar ls) x) (car ls)]
      [else (assq x (cdr ls))])))


(define (assqo x ls q)
  (fresh (k v t acc)
         (== `((,k . ,v) . ,t) ls)
         (conde
          [(== k x) (== q `(,x . ,v))]
          [(assqo x t q)])))



(define (peanoo n)
  (conde
   [(== 'z n)]
   [(fresh (n-1)
           (== `(s ,n-1) n)
           (peanoo n-1))]))


(define (pluso n m sum)
  (fresh ()
         (peanoo n)
         (peanoo m)
         (peanoo sum)
         (conde
          [(== 'z n) (== sum m)]
          [(fresh (n-1 sum-1)
                  (== `(s ,n-1) n)
                  (== `(s ,sum-1) sum)
                  (pluso n-1 m sum-1))])))


(define (subo n m difference)
  (pluso m difference m))


(define (lesso n m)
  (fresh (diff)
         (=/= diff 'z)
         (pluso diff n m)))


(define (mullo n m mul)
  (conde
   [(== 'z n) (== mul 'z)]
   [(fresh (n-1 mul-m)
           (== `(s ,n-1) n)
           (pluso m mul-m mul)
           (mullo n-1 m mul-m))]))

(require "./faster-miniKanren/mk.rkt")

(define (lookupo ctx x type)
  (fresh (k v t acc)
         (== `((,k . ,v) . ,t) ctx)
         (conde
          [(== k x) (== type v)]
          [(=/= k x) (lookupo t x type)])))


(define (!-o ctx term type)
  (conde
   [(!-fixo ctx term type)]
   [(!-bolo ctx term type)]
   [(!-into ctx term type)]
   [(!-varo ctx term type)]
   [(!-abso ctx term type)]
   [(!-appo ctx term type)]))

(define (!-varo ctx x type)
  (fresh ()
         (symbolo x)
         (lookupo ctx x type)))

(define (!-abso ctx x type)
  (fresh (var var-t body body-t)
         (== `(lambda (,var) ,body) x)
         (== `(,var-t -> ,body-t) type)
         (!-o `((,var . ,var-t) . ,ctx) body body-t)))

(define (!-appo ctx x type)
  (fresh (f arg arg-t)
         (== `(,f ,arg) x)
         (!-o ctx f `(,arg-t -> ,type))
         (!-o ctx arg arg-t)))


(define (!-bolo ctx x type)
  (fresh (c e1 e2)
         (conde
          [(== x #t) (== type 'bool)]
          [(== x #f) (== type 'bool)]
          [(== x `(if ,c ,e1 ,e2))
           (!-o ctx c 'bool)
           (!-o ctx e1 type)
           (!-o ctx e2 type)]
          [(fresh (op)
                  (conde [(== op 'and)] [ (== op 'or)])
                  (== x `(,op ,e1 ,e2))
                  (!-o ctx e1 'bool)
                  (!-o ctx e2 'bool)
                  (== type 'bool))])))

(define (!-into ctx x type)
  (fresh (e1 e2)
         (conde
          [(numbero x) (== type 'int)]

          [(== x `(sub1 ,e1))
           (!-o ctx e1 'int)
           (== type 'int)]
          [(== x `(add1 ,e1))
           (!-o ctx e1 'int)
           (== type 'int)]

          [(== x `(zero? ,e1))
           (!-o ctx e1 'int)
           (== type 'bool)]

          [(== x `(* ,e1 ,e2))
           (!-o ctx e1 'int)
           (!-o ctx e2 'int)
           (== type 'int)])))

(define (!-fixo ctx x type)
  (fresh (F)
         (== `(fix ,F) x)
         (!-o ctx F `(,type -> ,type))))
