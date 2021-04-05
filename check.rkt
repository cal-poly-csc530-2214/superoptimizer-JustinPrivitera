#lang typed/racket

(require "asgn1.rkt")

(define (works?
         [s : Sexp]
         [args : (Listof Integer)]
         [targets : (Listof Value)]) : Boolean
  (match* (args targets)
    [((cons first-arg rest-args) (cons first-targ rest-targs))
     (and
      (equal? (top-interp s first-arg) first-targ)
      (works? s rest-args rest-targs))]
    [('() '()) #t]
    [(_ _) (error 'works? "arguments and targets mismatch")]))

(define lower-bound -1)
(define upper-bound 2)
(define nums (cast (range lower-bound upper-bound) (Listof Any)))
(define arith-symbols (list '+ '*))
(define logic-symbols (list 'and 'or))
;(define arith-to-logic-symbols (list '<= '< '== '> '>=))
(define arith-to-logic-symbols (list '< '>))
(define bools (list #t #f))

(define (arith-binops) : (Listof (Listof Any))
  (cartesian-product arith-symbols nums nums))

(define (arith-binops-arg) : (Listof (Listof Any))
  (cartesian-product arith-symbols (list 'x) nums))

(define (logic-binops) : (Listof (Listof Any))
  (cartesian-product logic-symbols bools bools))

(define (arith-to-logic-binops) : (Listof (Listof Any))
  (cartesian-product arith-to-logic-symbols nums nums))

(define (arith-to-logic-binops-arg) : (Listof (Listof Any))
  (cartesian-product arith-to-logic-symbols (list 'x) nums))

#;(define useless-progs
    (append
     nums ;; numbers
     (arith-binops) ;; binops with numbers
     (cartesian-product arith-symbols (arith-binops) nums) ;; binops with a left binop with numbers
     ;(cartesian-product arith-symbols nums (arith-binops)) ;; binops with a right binop with numbers
     (cartesian-product arith-symbols (arith-binops) (arith-binops)) ;; binops with two children
     bools
     ;(logic-binops)
     ;(cartesian-product logic-symbols (logic-binops) bools)
     ;(cartesian-product logic-symbols bools (logic-binops))
     ;(cartesian-product logic-symbols (logic-binops) (logic-binops))
     ;(arith-to-logic-binops)
     ;(cartesian-product arith-to-logic-symbols (arith-binops) nums)
     ;(cartesian-product arith-to-logic-symbols nums (arith-binops))
     ;(cartesian-product arith-to-logic-symbols (arith-binops) (arith-binops))
     ;(cartesian-product logic-symbols (arith-to-logic-binops) bools)
     ;(cartesian-product logic-symbols bools (arith-to-logic-binops))
     #;(cartesian-product logic-symbols (arith-to-logic-binops) (arith-to-logic-binops))))

(define arith-binops-2-levels-w/-args
  (append
   nums
   (arith-binops-arg)
   ;(cartesian-product arith-symbols (arith-binops-arg) nums)
   ;(cartesian-product arith-symbols nums (arith-binops-arg))
   #;(cartesian-product arith-symbols (arith-binops-arg) (arith-binops-arg))))

(define arith-to-logic-binops-2-levels-w/-args
  (append
   bools
   (arith-to-logic-binops-arg)
   ;(cartesian-product arith-to-logic-symbols (arith-binops-arg) nums)
   ;(cartesian-product arith-to-logic-symbols nums (arith-binops-arg))
   ;(cartesian-product arith-to-logic-symbols (arith-binops-arg) (arith-binops-arg))
   ;(cartesian-product logic-symbols (arith-to-logic-binops-arg) bools)
   ;(cartesian-product logic-symbols bools (arith-to-logic-binops-arg))
   #;(cartesian-product logic-symbols (arith-to-logic-binops-arg) (arith-to-logic-binops-arg))))

(define progs
  (append
   (list 'x)
   ;useless-progs
   arith-binops-2-levels-w/-args
   ;arith-to-logic-binops-2-levels-w/-args
   (cartesian-product ;; if statements
    (list 'if)
    arith-to-logic-binops-2-levels-w/-args
    arith-binops-2-levels-w/-args
    arith-binops-2-levels-w/-args)
   (cartesian-product ;; if statements
    (list 'if)
    arith-to-logic-binops-2-levels-w/-args
    arith-binops-2-levels-w/-args
    (cartesian-product ;; if statements
     (list 'if)
     arith-to-logic-binops-2-levels-w/-args
     arith-binops-2-levels-w/-args
     arith-binops-2-levels-w/-args))
   (cartesian-product ;; if statements
    (list 'if)
    arith-to-logic-binops-2-levels-w/-args
    (cartesian-product ;; if statements
     (list 'if)
     arith-to-logic-binops-2-levels-w/-args
     arith-binops-2-levels-w/-args
     arith-binops-2-levels-w/-args)
    arith-binops-2-levels-w/-args)))

#;(define progs (list
               '(if (> x 0)
                    1
                    (if (< x 0)
                        -1
                        0))))

(length progs)
(newline)

(define len (length progs))
(define index (box 0.0))

;; all programs of the form f(x) = x + 1
#;(filter (lambda ([s : Any]) : Boolean
          (works? (cast s Sexp) '(0 1 2) '(1 2 3))) progs)

(filter (lambda ([s : Any]) : Boolean
          (display (format "\r~a%        " (* 100.0 (/ (unbox index) len))))
          (set-box! index (+ (unbox index) 1))
          (works? (cast s Sexp) '(-100 0 456 -1 0 1 -2 2 -3 3) '(-1 0 1 -1 0 1 -1 1 -1 1))) progs)
