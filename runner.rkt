#lang racket

(require "lexer.rkt")
(require "parser.rkt")
(require "passes.rkt")

(define passes
  (list (cons "coalesce conditions" coalesce-cond)))

(define passify
  (λ (p current-passes)
    (if (null? current-passes)
        (begin (displayln "Final Program: ")
               p)
        (let* [(current-pass (car current-passes))
               (pass-res ((cdr current-pass) p))]
          (begin
            (displayln (format "Pass Result (~a): " (car current-pass)))
            (pretty-print pass-res)
            (displayln "")
            (passify pass-res (cdr current-passes)))))))

(let [(lexer-res (lex "cond"))]
  (displayln "Lexical Analysis: ")
  (pretty-print lexer-res)
  (displayln "")
  (let [(parser-res (parse lexer-res))]
    (displayln "Parser: ")
    (pretty-print parser-res)
    (displayln "")
    (pretty-print (passify parser-res passes))))
