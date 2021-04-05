#lang typed/racket

(require "asgn1.rkt")

(define (works? [s : Sexp] [target : Value]) : Boolean
  (equal? (top-interp s) target))

(define lower-bound 0)
(define upper-bound 3)
(define nums (cast (range lower-bound upper-bound) (Listof Any)))
(define arith-symbols (list '+ '*))
(define logic-symbols (list 'and 'or))
(define arith-to-logic-symbols (list '<= '< '== '> '>=))
(define bools (list #t #f))

(define (arith-binops) : (Listof (Listof Any))
  (cartesian-product arith-symbols nums nums))

(define (logic-binops) : (Listof (Listof Any))
  (cartesian-product logic-symbols bools bools))

(define (arith-to-logic-binops) : (Listof (Listof Any))
  (cartesian-product arith-to-logic-symbols nums nums))

(define progs
  (append
   nums ;; numbers
   (arith-binops) ;; binops with numbers
   (cartesian-product arith-symbols (arith-binops) nums) ;; binops with a left binop with numbers
   (cartesian-product arith-symbols nums (arith-binops)) ;; binops with a right binop with numbers
   (cartesian-product arith-symbols (arith-binops) (arith-binops)) ;; binops with two children
   bools
   (logic-binops)
   (cartesian-product logic-symbols (logic-binops) bools)
   (cartesian-product logic-symbols bools (logic-binops))
   (cartesian-product logic-symbols (logic-binops) (logic-binops))
   (arith-to-logic-binops)
   (cartesian-product arith-to-logic-symbols (arith-binops) nums)
   (cartesian-product arith-to-logic-symbols nums (arith-binops))
   (cartesian-product arith-to-logic-symbols (arith-binops) (arith-binops))
   (cartesian-product logic-symbols (arith-to-logic-binops) bools)
   (cartesian-product logic-symbols bools (arith-to-logic-binops))
   (cartesian-product logic-symbols (arith-to-logic-binops) (arith-to-logic-binops))))

(filter (lambda ([s : Any]) : Boolean
          (works? (cast s Sexp) #t)) progs)
