#lang typed/racket

(require "asgn1.rkt")

(define (works? [s : Sexp] [target : Value]) : Boolean
  (equal? (top-interp s) target))

(define lower-bound 0)
(define upper-bound 3)
(define nums (cast (range lower-bound upper-bound) (Listof Any)))
(define symbols (list '+ '*))

(define (arith-binops) : (Listof (Listof Any))
  (cartesian-product symbols nums nums))

(define progs
  (append
   nums ;; numbers
   (arith-binops) ;; binops with numbers
   (cartesian-product symbols (arith-binops) nums) ;; binops with a left binop with numbers
   (cartesian-product symbols nums (arith-binops)) ;; binops with a right binop with numbers
   (cartesian-product symbols (arith-binops) (arith-binops)))) ;; binops with two children

(filter (lambda ([s : Any]) : Boolean
          (works? (cast s Sexp) 6)) progs)
