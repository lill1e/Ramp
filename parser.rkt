#lang racket

(require "structs.rkt")
(require "utilities.rkt")

(define parse-literal
  (λ (tokens)
    (match tokens
      [(? null?) (error "reached end of tokens without a match")]
      [`(,token . ,other-tokens) #:when (Number? token) (values token other-tokens)]
      [`(,token . ,other-tokens) #:when (Boolean? token) (values token other-tokens)]
      [`(,(Identifier ident) ,(Symbol '|[|) . ,other-tokens)
       (match/values
        (parse-comma-vals other-tokens null)
        [(exprs post-tokens) (values (Indexing (Identifier ident) (car exprs)) post-tokens)])]
      [`(,(Identifier ident) ,(Symbol '|(|) . ,other-tokens)
       (let-values
           [((pairs post-tokens) (parse-comma-vals other-tokens null))]
         (values (FunctionCall (Identifier ident) pairs) post-tokens))]
      [`(,token . ,other-tokens) #:when (Identifier? token) (values token other-tokens)]
      [`(,(Symbol '|(|) . ,other-tokens)
       (match/values
        (parse-expr other-tokens)
        [(expr post-tokens)
         (let [(rparen-tokens (consume-symbol post-tokens '|)|))]
           (values expr rparen-tokens))])]
      [`(,(Symbol '|[|) . ,other-tokens)
       (match/values
        (parse-comma-vals other-tokens null)
        [(exprs post-tokens) (values (Vector exprs) post-tokens)])])))

(define parse-comma-vals
  (λ (tokens acc)
    (match tokens
      [`(,(Symbol '|,|) . ,other-tokens) (parse-comma-vals other-tokens acc)]
      [`(,(or (Symbol '|]|) (Symbol '|)|)) . ,other-tokens) (values acc other-tokens)]
      [`(,val . ,other-tokens)
       (let-values [((v _) (parse-literal (list val)))]
         (parse-comma-vals other-tokens (append acc (list v))))])))

(define parse-args
  (λ (tokens acc)
    (match tokens
      [`(,(Symbol '|,|) . ,other-tokens) (parse-args other-tokens acc)]
      [`(,(or (Symbol '|]|) (Symbol '|)|)) . ,other-tokens) (values acc other-tokens)]
      [`(,(Identifier val) ,(Symbol ':) ,(Type t) . ,other-tokens) (parse-args other-tokens (append acc (list (cons val t))))])))

(define parse-unary
  (λ (tokens)
    (match tokens
      [(? null?) (error "reached end of tokens without a match")]
      [`(,(or (Symbol '!)
              (Symbol '-)) . ,other-tokens)
       (match/values
        (parse-unary other-tokens)
        [(expr post-tokens) (values (Unary (car tokens) expr) post-tokens)])]
      [_ (parse-literal tokens)])))

(define parse-mult
  (λ (tokens)
    (match/values
     (parse-unary tokens)
     [(lhs lhs-tokens) (make-binary lhs-tokens lhs parse-unary '(*))])))

(define parse-bin
  (λ (tokens)
    (match/values
     (parse-mult tokens)
     [(lhs lhs-tokens) (make-binary lhs-tokens lhs parse-mult '(+ -))])))

(define parse-cmp
  (λ (tokens)
    (match/values
     (parse-bin tokens)
     [(lhs lhs-tokens)
      (make-binary lhs-tokens lhs parse-bin '(> >= < <=))])))

(define parse-eq
  (λ (tokens)
    (match/values
     (parse-cmp tokens)
     [(lhs lhs-tokens)
      (make-binary lhs-tokens lhs parse-cmp '(== !=))])))

(define parse-expr
  (λ (tokens)
    (parse-eq tokens)))

(define consume
  (λ (tokens t)
    (match tokens
      [(? null?) (error "tried to consume an empty program")]
      [`(,token . ,other-tokens) #:when (t token) other-tokens]
      [_ (error "consumed wrong type")])))

(define consume-keyword
  (λ (tokens kw)
    (match tokens
      [(? null?) (error "tried to consume an empty program")]
      [`(,token . ,other-tokens) #:when (and (Keyword? token) (eqv? (Keyword-keyword token) kw)) other-tokens]
      [_ (error "consumed wrong type")])))

(define consume-symbol
  (λ (tokens kw)
    (match tokens
      [(? null?) (error "tried to consume an empty program")]
      [`(,token . ,other-tokens) #:when (and (Symbol? token) (eqv? (Symbol-sym token) kw)) other-tokens]
      [_ (error "consumed wrong type")])))

(define parse-identifier
  (λ (tokens)
    (match tokens
      [(? null?) (error "tried to consume an empty program")]
      [`(,(Identifier ident) . ,other-tokens) (values ident other-tokens)]
      [_ (error "consumed wrong type")])))

(define parse-type
  (λ (tokens)
    (match tokens
      [(? null? ) (error "tried to consume an empty program")]
      [`(,(Type t) . ,other-tokens) (values t other-tokens)]
      [_ (error "consumed wrong type")])))

(define parse-assignment
  (λ (tokens)
    (match/values
     (parse-expr tokens)
     [((Identifier ident) expr-tokens)
      (match (car expr-tokens)
        [(Symbol '=)
         (let [(eq-tokens (consume-symbol expr-tokens '=))]
           (match/values
            (parse-expr eq-tokens)
            [(rhs rhs-tokens) (values (SetBang ident rhs) rhs-tokens)]))]
        [_ (values (Identifier ident) expr-tokens)])]
     [(expr-node expr-tokens) (values expr-node expr-tokens)])))

(define list->begin
  (λ (exprs)
    (match exprs
      [(? null?) (Void)]
      [(list expr) expr]
      [_ (Begin (drop-right exprs 1) (car (take-right exprs 1)))])))

(define parse-block
  (λ (tokens exprs)
    (match tokens
      [`(,(Symbol '|}|) . ,curr-toks) (values (list->begin exprs) curr-toks)]
      [`(,(Keyword 'Let) . ,_) (parse-binding tokens)]
      [_ (match/values
          (parse-top tokens)
          [(expr new-toks) (parse-block new-toks (append exprs (list expr)))])])))

(define parse-stmt
  (λ (tokens)
    (match tokens
      [`(,(Symbol '|{|) . ,after-tokens)
       (parse-block after-tokens null) #;(parse-top after-tokens)]
      [`(,(Keyword 'If) . ,after-tokens)
       (let [(lp-tokens (consume-symbol after-tokens '|(|))]
         (match/values
          (parse-expr lp-tokens)
          [(cnd expr-tokens)
           (let [(rp-tokens (consume-symbol expr-tokens '|)|))]
             (match/values
              (parse-stmt rp-tokens) [(stmt stmt-tokens) (values (If cnd stmt) stmt-tokens)]))]))]
      [`(,(Keyword 'Else) . ,after-tokens)
       (match/values
        (parse-stmt after-tokens)
        [(stmt stmt-tokens) (values (Else stmt) stmt-tokens)])]
      [_ (let-values [((e other-tokens) (parse-assignment tokens))]
           (values e (consume other-tokens Semicolon?)))])))

(define parse-binding
  (λ (tokens)
    (let [(tokens (consume-keyword tokens 'Let))]
      (match/values
       (parse-identifier tokens)
       [(identifier post-tokens)
        (let [(post-tokens (consume-symbol post-tokens '=))]
          (match/values
           (parse-stmt post-tokens)
           [(rhs post-rhs-tokens)
            (match/values
             (parse-body-tk post-rhs-tokens null)
             [(body post-body-tokens)
              (values (Let identifier rhs body) post-body-tokens)])]))]))))

(define parse-while
  (λ (tokens)
    (let* [(tokens (consume-keyword tokens 'While))
           (tokens (consume-symbol tokens '|(|))]
      (match/values
       (parse-expr tokens)
       [(cond-expr cond-tokens)
        (let [(rparen-tokens (consume-symbol cond-tokens '|)|))]
          (match/values
           (parse-stmt rparen-tokens)
           [(stmt stmt-tokens) (values (WhileLoop cond-expr stmt) stmt-tokens)]))]))))

(define parse-function
  (λ (tokens)
    (let*-values [((tokens) (consume-keyword tokens 'Function))
                  ((ident tokens) (parse-identifier tokens))
                  ((tokens) (consume-symbol tokens '|(|))
                  ((arg-pairs tokens) (parse-args tokens null))
                  ((tokens) (consume-symbol tokens '->))
                  ((t tokens) (parse-type tokens))
                  ((body tokens) (parse-stmt tokens))]
      (values (Function ident arg-pairs t body) tokens))))

(define parse-top
  (λ (tokens)
    (match tokens
      [`(,(Keyword 'Let) . ,_) (parse-binding tokens)]
      [`(,(Keyword 'While) . ,_) (parse-while tokens)]
      [`(,(Keyword 'Function) . ,_) (parse-function tokens)]
      [_ (parse-stmt tokens)])))

(define parse-body-tk
  (λ (tokens stmts)
    (match tokens
      ['() (match stmts
             ['() (values (Void) tokens)]
             [`(,stmt) (values stmt tokens)]
             [_ (values (Begin (take stmts (- (length stmts) 1)) (car (drop stmts (- (length stmts) 1))))
                        tokens)])]
      [`(,(Symbol '|}|) . ,other-tokens) (match/values
                                          (parse-body-tk null stmts)
                                          [(fin _) (values fin other-tokens)]) ]
      [_ (match/values
          (parse-top tokens)
          [(stmt post-tokens) (parse-body-tk post-tokens (append stmts (list stmt)))])])))

(define parse-body
  (λ (tokens)
    (match/values (parse-body-tk tokens null)
                  [(body _) body])))

(define parse
  (λ (tokens)
    (parse-body tokens)))

(provide parse)
