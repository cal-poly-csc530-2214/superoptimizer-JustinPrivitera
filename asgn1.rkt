#lang typed/racket

(require typed/rackunit)

;; can I do program synthesis with a very simple language???

;; here is a parser, typechecker, and interpreter

;; arguments later

(define-type expr (U binopE ifE Natural Boolean))
(struct binopE ([op : Symbol] [left : expr] [right : expr])#:transparent)
(struct ifE ([cond : expr] [then : expr] [else : expr])#:transparent)

(define-type Value (U Natural Boolean))

(define-type type (U numT boolT))
(struct numT ()#:transparent)
(struct boolT ()#:transparent)

(define (op-lookup [op : Symbol]) : (-> Value Value Value)
  (match op
    ['+ (lambda ([left : Value] [right : Value]) : Value (+ (cast left Natural) (cast right Natural)))]
    ['* (lambda ([left : Value] [right : Value]) : Value (* (cast left Natural) (cast right Natural)))]
    ['< (lambda ([left : Value] [right : Value]) : Value (< (cast left Natural) (cast right Natural)))]
    ['<= (lambda ([left : Value] [right : Value]) : Value (<= (cast left Natural) (cast right Natural)))]
    ['> (lambda ([left : Value] [right : Value]) : Value (> (cast left Natural) (cast right Natural)))]
    ['>= (lambda ([left : Value] [right : Value]) : Value (>= (cast left Natural) (cast right Natural)))]
    ['== equal?]
    ['and (lambda ([left : Value] [right : Value]) : Value (and left right))]
    ['or (lambda ([left : Value] [right : Value]) : Value (or left right))]))

(define (top-interp [s : Sexp]) : Value
  (define e (parse s))
  (type-check e)
  (interp e))

(define (parse [s : Sexp]) : expr
  (match s
    [(list (? symbol? operand) left right)
     (binopE
      (if (member operand (list '+ '* 'and 'or '< '<= '> '>= '==))
          operand
          (error 'parse "bad operand in '~a'" operand))
      (parse left)
      (parse right))]
    [(? natural? n) n]
    [(? boolean? b) b]
    [(list 'if cond then else)
     (ifE (parse cond) (parse then) (parse else))]))

(define (type-check [e : expr]) : type
  (match e
    [(? natural? n) (numT)]
    [(? boolean? b) (boolT)]
    [(binopE op left right)
     (if (member op (list '+ '*))
         (if (and (equal? (type-check left) (numT)) (equal? (type-check right) (numT)))
             (numT)
             (error 'type-check "expression failed to typecheck: ~a" e))
         (if (member op (list 'and 'or))
             (if (and (equal? (type-check left) (boolT)) (equal? (type-check right) (boolT)))
                 (boolT)
                 (error 'type-check "expression failed to typecheck: ~a" e))
             (if (and (equal? (type-check left) (numT)) (equal? (type-check right) (numT)))
                 (boolT)
                 (error 'type-check "expression failed to typecheck: ~a" e))))]
    [(ifE cond then else)
     (define ret-type (type-check then))
     (if (equal? (type-check cond) (boolT))
         (if (equal? ret-type (type-check else))
             ret-type
             (error 'type-check "expression failed to typecheck: ~a" e))
         (error 'type-check "expression failed to typecheck: ~a" e))]))

(define (interp [e : expr]) : Value
  (match e
    [(? natural? n) n]
    [(? boolean? b) b]
    [(binopE op left right)
     ((op-lookup op) (interp left) (interp right))]
    [(ifE cond then else)
     (if (interp cond)
         (interp then)
         (interp else))]))

(check-equal? (top-interp '(+ 2 3)) 5)
(check-equal? (top-interp '(* 2 3)) 6)
(check-equal? (top-interp '(<= 2 3)) #t)
(check-equal? (top-interp '(<= 3 3)) #t)
(check-equal? (top-interp '(<= 4 3)) #f)
(check-equal? (top-interp '(< 4 3)) #f)
(check-equal? (top-interp '(> 4 3)) #t)
(check-equal? (top-interp '(and (>= 1 1) (and (<= 1 1) (== 1 1)))) #t)
(check-equal? (top-interp '(and #t #f)) #f)
(check-equal? (top-interp '(if (and #t #f) 5 3)) 3)
(check-equal? (top-interp '(if (or #t #f) 5 3)) 5)
(check-equal? (top-interp '(if (or #f #t) 5 3)) 5)
(check-exn (regexp (regexp-quote "bad operand in '^'"))
           (lambda () (top-interp '(^ 2 3))))
(check-exn (regexp (regexp-quote "expression failed to typecheck: #(struct:binopE + #t 3)"))
           (lambda () (top-interp '(+ #t 3))))
(check-exn (regexp (regexp-quote "expression failed to typecheck: #(struct:binopE and #t 3)"))
           (lambda () (top-interp '(and #t 3))))
(check-exn (regexp (regexp-quote "expression failed to typecheck: #(struct:binopE <= #t 3)"))
           (lambda () (top-interp '(<= #t 3))))
(check-exn (regexp (regexp-quote "expression failed to typecheck: #(struct:ifE 3 5 3)"))
           (lambda () (top-interp '(if 3 5 3))))
(check-exn (regexp (regexp-quote "expression failed to typecheck: #(struct:ifE #t #t 3)"))
           (lambda () (top-interp '(if #t #t 3))))
