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
           (Void)
           (Type _)) expr]
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
      [(Else body) (Else (coalesce-cond body))]
      [(Vector vals) (Vector (map coalesce-cond vals))]
      [(Indexing vec index) (Indexing (coalesce-cond vec) (coalesce-cond index))]
      [(Function name args rtype body) (Function name args rtype (coalesce-cond body))]
      [(FunctionCall fn args) (FunctionCall (coalesce-cond fn) (map coalesce-cond args))]
      [(Array len initial) (Array (coalesce-cond len) (coalesce-cond initial))])))

(define conform
  (λ (expr)
    (match expr
      [(Identifier ident) (Var ident)]
      [(Number n) (Int n)]
      [(Boolean b) (Bool b)]
      [(Void) (Void)]
      [(Begin stmts final) (Begin (map conform stmts) (conform final)) ]
      [(Let sym rhs body) (Let sym (conform rhs) (conform body))]
      [(SetBang sym rhs) (SetBang sym (conform rhs))]
      [(Unary op child) (Prim (Symbol-sym op) (list (conform child)))]
      [(Binary op lhs rhs) (Prim (Symbol-sym op) (list (conform lhs) (conform rhs)))]
      [(WhileLoop cnd body) (WhileLoop (conform cnd) (conform body))]
      [(IfStmt cnd conseq alt) (IfStmt (conform cnd) (conform conseq) (conform alt))]
      [(Vector vals) (Prim 'vector (map conform vals))]
      [(Indexing vec index) (Prim 'vector-ref (list (conform vec) (conform index)))]
      [(Type t)
       (Type (match t
               ['Number 'Integer]
               [_ t]))]
      [(Function name args rtype body) (Def name (map (λ (p) `[,(car p) : ,(cdr p)]) args) rtype null (conform body))]
      [(FunctionCall fn args) (Apply (conform fn) (map conform args))]
      [(Array len initial) (Array (conform len) (conform initial))])))

(provide coalesce-cond conform)
