#lang racket

(require "structs.rkt")
(require "utilities.rkt")

(define ops
  (list
   '+
   '-
   '*
   '=
   '>
   '<
   '>=
   '<=
   '==
   '!=
   '!
   '|(|
   '|)|
   '|{|
   '|}|
   '|[|
   '|]|
   '|,|))

(define char-ops (map (λ (sym-op) (string-ref (symbol->string sym-op) 0)) ops))

(define list-op?
  (λ (loc) (op? (string->symbol (list->string loc)))))

(define op?
  (λ (op) (memv op ops)))

(define word-char?
  (λ (c)
    (not (member c char-ops))))

(define chars?
  (λ (loc)
    (= (length (filter char? loc)) (length loc))))

(define chars-op?
  (λ (op)
    (if (null? op) #f
        (if (chars? op) (memv (string->symbol (list->string op)) ops) #f))))

(define value->lexeme
  (λ (s)
    (match s
      [n #:when (number? n) (Number n)]
      [b #:when (boolean? b) (Boolean (if b #t #f))]
      ['\; (Semicolon)]
      [(? op?) (Symbol s)]
      [v #:when (symbol? v)
         (match v
           [(or 'true 'false) (Boolean (eqv? v 'true))]
           ['let (Keyword 'Let)]
           ['while (Keyword 'While)]
           ['if (Keyword 'If)]
           ['else (Keyword 'Else)]
           [_ (Identifier v)])]
      [_ (error "invalid char found")])))

(define char->number
  (λ (c) (- (char->integer c) (char->integer #\0))))

(define word?
  (λ (loc)
    (cond
      [(list? loc)
       (let* [(str (list->string loc))
              (sym (string->symbol str))]
         (cond
           [(ormap op? loc) #f]
           [(memv sym ops) #f]
           [(= (string-length str) 0) #t]
           [else #t]))]
      [else #f])))

(define acc->sym
  (λ (acc)
    (match acc
      ['|;| acc]
      [(? number?) acc]
      [(or (? list-op?) (? word?)) (string->symbol (apply string acc))])))

(define extend-acc
  (λ (acc item)
    (append (list-if-needed acc) (if (null? item) null (list (acc->sym item))))))

(define file->symbols
  (λ (p acc alt-acc)
    (let [(r (read-char p))]
      (match* (r alt-acc)
        [(_ _) #:when (eof-object? r) (extend-acc acc alt-acc)]
        [(#\; _) (file->symbols p (extend-acc (extend-acc acc alt-acc) '|;|) null)]
        [(#\space _) (file->symbols p (extend-acc acc alt-acc) null)]
        [(#\newline _) (file->symbols p acc alt-acc)]
        [((? char-numeric?) (? number?)) (file->symbols p acc (+ (* alt-acc 10) (char->number r)))]
        [((? char-numeric?) _) (file->symbols p (extend-acc acc alt-acc) (char->number r))]
        [((? word-char?) (? word?)) (file->symbols p acc (append alt-acc (list r)))]
        [((? word-char?) _) (file->symbols p (extend-acc acc alt-acc) (list r))]
        [((? char?) _) #:when (chars-op? (append (list-if-needed alt-acc) (list r))) (file->symbols p acc (append alt-acc (list r)))]
        [((? char?) (? op?)) (file->symbols p acc (append alt-acc (list r)))]
        [((? char?) _) (file->symbols p (extend-acc acc alt-acc) (list r))]))))

(define lex
  (λ (file-name) (map value->lexeme (file->symbols (open-input-file file-name) null null))))

(provide lex)
