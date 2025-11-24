#lang racket

(require "structs.rkt")

(define coalesce-stmts
  (λ (stmts)
    (match stmts
      [`(,(or (If cnd body)
              (Else (If cnd body))) . ,other-stmts)
       (match/values
        (coalesce-stmts other-stmts)
        [(other-conds post-stmts) (values (cons (cons cnd body) other-conds) post-stmts)])]
      [`(,(Else stmt) . ,other-stmts) (values (list stmt) other-stmts)]
      [_ (values null stmts)])))

(define coalesced->if
  (λ (stmts)
    (match stmts
      [(cons (cons cnd body) other-stmts) (IfStmt cnd body (coalesced->if other-stmts))]
      [(list (cons cnd body)) (IfStmt cnd body (Void))]
      [(list body) body]
      [_ null])))

(define coalesce-cond
  (λ (expr)
    (match expr
      [(or (Identifier _)
           (Number _)
           (Boolean _)
           (Void)) expr]
      [(Begin stmts final)
       (match (Begin (map coalesce-cond stmts) (coalesce-cond final))
         [(Begin stmts final)
          (let*-values [((all-stmts) (append stmts (list final)))
                        ((branches post-stmts) (coalesce-stmts all-stmts))
                        ((new-branches) (coalesced->if branches))]
            (cond [(null? new-branches) (Begin stmts final)]
                  [(null? post-stmts) (coalesced->if branches)]
                  [else (Begin (append (list (coalesced->if branches)) (drop-right post-stmts 1)) (take-right post-stmts 1))]))])]
      [(Let sym rhs body) (Let sym (coalesce-cond rhs) (coalesce-cond body))]
      [(SetBang sym rhs) (SetBang sym (coalesce-cond rhs))]
      [(Unary op child) (Unary op (coalesce-cond child))]
      [(Binary op lhs rhs) (Binary op (coalesce-cond lhs) (coalesce-cond rhs))]
      [(WhileLoop cnd body) (WhileLoop (coalesce-cond cnd) (coalesce-cond body))]
      [(If cnd body) (If (coalesce-cond cnd) (coalesce-cond body))]
      [(Else body) (Else (coalesce-cond body))])))

(provide coalesce-cond)
