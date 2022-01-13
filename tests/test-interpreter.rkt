#lang racket

(require rackunit
         rackunit/text-ui
         "../syntax.rkt"
         "../interp.rkt")

;; testing the expression evaluation algorithm


(define eval-expr-tests
  (test-suite
   "Tests for eval-expr"
   (test-case
       "Evaluating a value"
     (let ([env (hash-set (make-immutable-hash '()) "a" (value 2))]
           [e   (value 3)])
       (check-equal? (eval-expr env e) (cons env e))))
   (test-case
       "Evaluating a variable in environment"
     (let ([env (hash-set (make-immutable-hash '()) "a" (value 2))]
           [e   (var "a")])
       (check-equal? (eval-expr env e) (cons env (value 2)))))
   (test-case
       "Evaluating a variable not defined"
     (let* ([env (hash-set (make-immutable-hash '()) "a" (value 2))]
            [e   (var "b")]
            [nenv (hash-set env "b" (value 0))])
       (check-equal? (eval-expr env e) (cons nenv (value 0)))))
   (test-case
       "Adding two values"
     (let ([env (make-immutable-hash '())]
           [e1 (value 2)]
           [e2 (value 3)])
       (check-equal? (eval-expr env (add e1 e2))
                     (cons env (value 5)))))
   (test-case
       "Adding a variable (defined) and a value"
     (let ([env (hash-set (make-immutable-hash '()) "a" (value 3))]
           [e1  (var "a")]
           [e2  (value 4)])
       (check-equal? (eval-expr env (add e1 e2))
                     (cons env (value 7)))))
   (test-case
       "Adding a variable (undefined) and a value"
     (let* ([env (hash-set (make-immutable-hash '()) "a" (value 3))]
            [e1  (var "b")]
            [e2  (value 4)]
            [nenv (hash-set env "b" (value 0))])
       (check-equal? (eval-expr env (add e1 e2)) (cons nenv (value 4)))))

   (test-case
         "Subtracting two values"
       (let ([env (make-immutable-hash '())]
             [e1 (value 3)]
             [e2 (value 1)])
         (check-equal? (eval-expr env (minus e1 e2))
                       (cons env (value 2)))))
   (test-case
         "Subtracting a variable (defined) and a value"
       (let ([env (hash-set (make-immutable-hash '()) "a" (value 3))]
             [e1  (var "a")]
             [e2  (value 1)])
         (check-equal? (eval-expr env (minus e1 e2))
                       (cons env (value 2)))))
   (test-case
         "Subtracting a variable (undefined) and a value"
       (let* ([env (hash-set (make-immutable-hash '()) "a" (value 3))]
              [e1  (var "b")]
              [e2  (value 4)]
              [nenv (hash-set env "b" (value 0))])
         (check-equal? (eval-expr env (minus e1 e2)) (cons nenv (value (- 4))))))

   (test-case
       "Multiplying two values"
     (let ([env (make-immutable-hash '())]
           [e1 (value 3)]
           [e2 (value 1)])
       (check-equal? (eval-expr env (mult e1 e2))
                     (cons env (value 3)))))
   (test-case
       "Multiplying a variable (defined) and a value"
     (let ([env (hash-set (make-immutable-hash '()) "a" (value 3))]
           [e1  (var "a")]
           [e2  (value 1)])
       (check-equal? (eval-expr env (mult e1 e2))
                     (cons env (value 3)))))
   (test-case
       "Multiplying a variable (undefined) and a value"
     (let* ([env (hash-set (make-immutable-hash '()) "a" (value 3))]
            [e1  (var "b")]
            [e2  (value 4)]
            [nenv (hash-set env "b" (value 0))])
       (check-equal? (eval-expr env (mult e1 e2)) (cons nenv (value 0)))))

   (test-case
       "Dividing two values"
     (let ([env (make-immutable-hash '())]
           [e1 (value 3)]
           [e2 (value 1)])
       (check-equal? (eval-expr env (divv e1 e2))
                     (cons env (value 3)))))
   (test-case
       "Dividing a variable (defined) and a value"
     (let ([env (hash-set (make-immutable-hash '()) "a" (value 3))]
           [e1  (var "a")]
           [e2  (value 1)])
       (check-equal? (eval-expr env (divv e1 e2))
                     (cons env (value 3)))))
   (test-case
       "Dividing a variable (undefined) and a value"
     (let* ([env (hash-set (make-immutable-hash '()) "a" (value 3))]
            [e1  (var "b")]
            [e2  (value 4)]
            [nenv (hash-set env "b" (value 0))])
       (check-equal? (eval-expr env (divv e1 e2)) (cons nenv (value 0)))))

   (test-case
       "Testing the ordering of two values"
     (let ([env (make-immutable-hash '())]
           [e1 (value 3)]
           [e2 (value 1)])
       (check-equal? (eval-expr env (lt e1 e2))
                     (cons env (value #f)))))
   (test-case
       "Testing the ordering of a variable (defined) and a value"
     (let ([env (hash-set (make-immutable-hash '()) "a" (value 3))]
           [e1  (var "a")]
           [e2  (value 1)])
       (check-equal? (eval-expr env (lt e1 e2))
                     (cons env (value #f)))))
   (test-case
       "Testing the ordering of a variable (undefined) and a value"
     (let* ([env (hash-set (make-immutable-hash '()) "a" (value 3))]
            [e1  (var "b")]
            [e2  (value 4)]
            [nenv (hash-set env "b" (value 0))])
       (check-equal? (eval-expr env (lt e1 e2)) (cons nenv (value #t)))))

     (test-case
         "Testing the equality of two values"
       (let ([env (make-immutable-hash '())]
             [e1 (value 1)]
             [e2 (value 1)])
         (check-equal? (eval-expr env (eeq e1 e2))
                       (cons env (value #t)))))
     (test-case
         "Testing the equality of a variable (defined) and a value"
       (let ([env (hash-set (make-immutable-hash '()) "a" (value 3))]
             [e1  (var "a")]
             [e2  (value 1)])
         (check-equal? (eval-expr env (eeq e1 e2))
                       (cons env (value #f)))))
     (test-case
         "Testing the equality of a variable (undefined) and a value"
       (let* ([env (hash-set (make-immutable-hash '()) "a" (value 3))]
              [e1  (var "b")]
              [e2  (value 4)]
              [nenv (hash-set env "b" (value 0))])
         (check-equal? (eval-expr env (eeq e1 e2)) (cons nenv (value #f)))))

     (test-case
         "Conjunction of two values"
       (let ([env (make-immutable-hash '())]
             [e1 (value #t)]
             [e2 (value #f)])
         (check-equal? (eval-expr env (eand e1 e2))
                       (cons env (value #f)))))
     (test-case
         "Conjunction of a variable (defined) and a value"
       (let ([env (hash-set (make-immutable-hash '()) "a" (value #t))]
             [e1  (var "a")]
             [e2  (value #t)])
         (check-equal? (eval-expr env (eeq e1 e2))
                       (cons env (value #t)))))
   ))


(run-tests eval-expr-tests)
 
