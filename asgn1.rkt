#lang typed/racket

(require typed/rackunit)

;; can I do program synthesis with a very simple language???

;; here is a parser, typechecker, and interpreter

;; arguments later

(define-type expr (U binopE ifE Integer Boolean Symbol))
(struct binopE ([op : Symbol] [left : expr] [right : expr])#:transparent)
(struct ifE ([cond : expr] [then : expr] [else : expr])#:transparent)

(define-type Value (U Integer Boolean))

(define-type type (U numT boolT))
(struct numT ()#:transparent)
(struct boolT ()#:transparent)

(define (op-lookup [op : Symbol]) : (-> Value Value Value)
  (match op
    ['+ (lambda ([left : Value] [right : Value]) : Value (+ (cast left Integer) (cast right Integer)))]
    ['* (lambda ([left : Value] [right : Value]) : Value (* (cast left Integer) (cast right Integer)))]
    ['< (lambda ([left : Value] [right : Value]) : Value (< (cast left Integer) (cast right Integer)))]
    ['<= (lambda ([left : Value] [right : Value]) : Value (<= (cast left Integer) (cast right Integer)))]
    ['> (lambda ([left : Value] [right : Value]) : Value (> (cast left Integer) (cast right Integer)))]
    ['>= (lambda ([left : Value] [right : Value]) : Value (>= (cast left Integer) (cast right Integer)))]
    ['== equal?]
    ['and (lambda ([left : Value] [right : Value]) : Value (and left right))]
    ['or (lambda ([left : Value] [right : Value]) : Value (or left right))]))

(define (top-interp [s : Sexp] [arg : Integer]) : Value
  (define e (parse s))
  (type-check e)
  (interp e arg))

(define (parse [s : Sexp]) : expr
  (match s
    [(list (? symbol? operand) left right)
     (binopE
      (if (member operand (list '+ '* 'and 'or '< '<= '> '>= '==))
          operand
          (error 'parse "bad operand in '~a'" operand))
      (parse left)
      (parse right))]
    [(? integer? n) (cast n Integer)]
    [(? boolean? b) b]
    [(? symbol? s) s]
    [(list 'if cond then else)
     (ifE (parse cond) (parse then) (parse else))]))

(define (type-check [e : expr]) : type
  (match e
    [(? integer? n) (numT)]
    [(? boolean? b) (boolT)]
    [(? symbol? s) (numT)] ;; for now args can only be numbers!!!
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

(define (interp [e : expr] [arg : Integer]) : Value
  (match e
    [(? integer? n) n]
    [(? boolean? b) b]
    [(? symbol? s) arg]
    [(binopE op left right)
     ((op-lookup op) (interp left arg) (interp right arg))]
    [(ifE cond then else)
     (if (interp cond arg)
         (interp then arg)
         (interp else arg))]))

(provide (all-defined-out))

(check-equal? (top-interp '(+ 2 3) 0) 5)
(check-equal? (top-interp '(* 2 3) 0) 6)
(check-equal? (top-interp '(<= 2 3) 0) #t)
(check-equal? (top-interp '(<= 3 3) 0) #t)
(check-equal? (top-interp '(<= 4 3) 0) #f)
(check-equal? (top-interp '(< 4 3) 0) #f)
(check-equal? (top-interp '(> 4 3) 0) #t)
(check-equal? (top-interp '(and (>= 1 1) (and (<= 1 1) (== 1 1))) 0) #t)
(check-equal? (top-interp '(and #t #f) 0) #f)
(check-equal? (top-interp '(if (and #t #f) 5 3) 0) 3)
(check-equal? (top-interp '(if (or #t #f) 5 3) 0) 5)
(check-equal? (top-interp '(if (or #f #t) 5 3) 0) 5)
(check-equal? (top-interp '(+ 1 a) 2) 3)
(check-equal? (top-interp '(and (>= 1 x) (and (<= 1 y) (== 1 z))) 2) #f)
(check-exn (regexp (regexp-quote "bad operand in '^'"))
           (lambda () (top-interp '(^ 2 3) 0)))
(check-exn (regexp (regexp-quote "expression failed to typecheck: #(struct:binopE + #t 3)"))
           (lambda () (top-interp '(+ #t 3) 0)))
(check-exn (regexp (regexp-quote "expression failed to typecheck: #(struct:binopE and #t 3)"))
           (lambda () (top-interp '(and #t 3) 0)))
(check-exn (regexp (regexp-quote "expression failed to typecheck: #(struct:binopE <= #t 3)"))
           (lambda () (top-interp '(<= #t 3) 0)))
(check-exn (regexp (regexp-quote "expression failed to typecheck: #(struct:ifE 3 5 3)"))
           (lambda () (top-interp '(if 3 5 3) 0)))
(check-exn (regexp (regexp-quote "expression failed to typecheck: #(struct:ifE #t #t 3)"))
           (lambda () (top-interp '(if #t #t 3) 0)))
