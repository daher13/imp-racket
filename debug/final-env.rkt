#lang racket

(module reader racket
    (require "../parser.rkt"
             "../interp.rkt"
             "../syntax.rkt")

  (provide (rename-out [imp-read read]
                       [imp-read-syntax read-syntax]))

  (define (imp-read in)
    (syntax->datum
     (imp-read-syntax #f in)))

  (define (imp-read-syntax path port)
    (datum->syntax
     #f
     `(module imp-mod racket
        ,@(print-env (imp-interp (parse port))))))

  (define (print-env env)
    (define (print-var p)
      (printf "~a -> ~v\n"
              (car p)
              (value-value (cdr p))))
    (begin
      (displayln "Final configuration of program environment:")
      (map print-var (hash->list env)))))
