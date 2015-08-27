#lang racket

(define (lookup var env)
  (match env
    ['() (error "unbound var" var)]
    [`((,v . ,val) . ,tail)
     (if (equal? v var)
         val
         (lookup var tail))]))


(define (push var val env)
  `((,var . ,val) . ,env))


(define (eval-expr expr env)
  (match expr
    [(? number?) expr]
    [(? string?) expr]
    [#t #t]
    [#f #f]
    [`(quote ,datum) datum]
    [`(null? ,e) (null? (eval-expr e env))]
    [`(not ,e) (not (eval-expr e env))]
    [`(* ,e1 ,e2) (* (eval-expr e1 env)
                     (eval-expr e2 env))]
    [`(sub1 ,e) (sub1 (eval-expr e env))]
    [`(zero?,e) (zero? (eval-expr e env))]
    [`(if ,c ,e1 ,e2) (if (eval-expr c env)
                          (eval-expr e1 env)
                          (eval-expr e2 env))]
    [(? symbol?) (lookup expr env)]
    [`(lambda (,x) ,e) `(closure ,(lambda (arg)
                                    (eval-expr e (push x arg env))))]

    [`(,e1 ,e2) (match (eval-expr e1 env)
                  [`(closure ,c) (c (eval-expr e2 env))]
                  [_ (error "not a function" e1)])]))


(define (eval-exp expr env)
  (match expr
    [(? symbol?) (env expr)]
    [`(lambda (,x) ,body)
     (lambda (arg)
       (eval-exp body (lambda (y)
                        (if (equal? y x) arg (env y)))))]
    [`(,e1 ,e2) ((eval-exp e1 env)
                 (eval-exp e2 env))]))

(require "./faster-miniKanren/mk.rkt")


(define (lookupo x env result)
  (fresh (k v t)
         (== `((,k . ,v) . ,t) env)
         (conde
          [(== k x) (== result v)]
          [(=/= k x) (lookupo x t result)])))



(define Z
  '(lambda (f)
     ((lambda (x) (f (lambda (n) ((x x) n))))
      (lambda (x) (f (lambda (n) ((x x) n)))))))

(define (evalo expr env result)
  (conde
   [(== #t expr) (== result #t)]
   [(== #f expr) (== result #f)]
   [(== `(quote ,result) expr)
    (absento 'closure result)]
   [(fresh (e1 e2 r1 r2)
           (== `(cons ,e1 ,e2) expr)
           (== result `(,r1 . ,r2))
           (evalo e1 env r1)
           (evalo e2 env r2))]

   [(fresh (e1 t)
           (== `(car ,e1) expr)
           (evalo e1 env `(,result . ,t)))]

   [(fresh (e1 h)
           (== `(cdr ,e1) expr)
           (evalo e1 env `(,h . ,result)))]

   [(fresh (items h t hr tr)
           (== `(list . ,items)  expr)
           (conde
            [(== '() items) (== result '())]
            [(== `(,h . ,t) items)
             (== result `(,hr . ,tr))
             (evalo h env hr)
             (evalo `(list . ,t) env tr)]))]

   [(fresh (e1 er)
           (== `(null? ,e1) expr)
           (conde
            [(== er '()) (== result #t)]
            [(=/= er '()) (== result #f)])
           (evalo e1 env er))]

   [(fresh (c cr e1 e2)
           (== `(if ,c ,e1 ,e2) expr)
           (evalo c env cr)
           (conde
            [(== cr #t) (evalo e1 env result)]
            [(== cr #f) (evalo e2 env result)]))]

   [(fresh (e)
           (== `(fix ,e) expr)
           (evalo `(,Z ,e) env result))]

   [(symbolo expr) (lookupo expr env result)]

   [(fresh (x body)
           (== `(lambda (,x) ,body) expr)
           (=/= x 'null?)
           (=/= x 'cons)
           (=/= x 'cdr)
           (=/= x 'car)
           (=/= x 'fix)
           (== result `(closure ,x ,body ,env)))]

   [(fresh (e1 e2 r1 r2 x body env^)
           (== `(,e1 ,e2) expr)
           (evalo e1 env `(closure ,x ,body ,env^))
           (evalo e2 env r2)
           (evalo body `((,x . ,r2) . ,env^) result))]))
