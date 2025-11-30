#lang racket

;; Lexer
(define-struct Keyword (keyword) #:transparent)
(define-struct Identifier (ident) #:transparent)
(define-struct Symbol (sym) #:transparent)
(define-struct Semicolon () #:transparent)

;; Intermediate Representations
(define-struct If (cnd body) #:transparent)
(define-struct Else (body) #:transparent)

;; Parser Add-ons
(define-struct Begin (stmts final) #:transparent)
(define-struct Number (n) #:transparent)
(define-struct Boolean (b) #:transparent)
(define-struct Let (sym rhs body) #:transparent)
(define-struct Void () #:transparent)
(define-struct SetBang (sym rhs) #:transparent)
(define-struct Unary (op child) #:transparent)
(define-struct Binary (op lhs rhs) #:transparent)
(define-struct WhileLoop (cnd body) #:transparent)
(define-struct IfStmt (cnd conseq alt) #:transparent)
(define-struct Vector (vals) #:transparent)
(define-struct Array (len initial) #:transparent) ;; TODO: switch to java like syntax (e.g. new Number[5] => (Array 5 0))
(define-struct Indexing (vec index) #:transparent)
(define-struct Type (t) #:transparent)
(define-struct Function (name args rtype body) #:transparent)
(define-struct FunctionCall (fn args) #:transparent)

;; Conform Structs
(define-struct Var (v) #:transparent)
(define-struct Bool (b) #:transparent)
(define-struct Int (n) #:transparent)
(define-struct Prim (op ops) #:transparent)
(define-struct Def (name param* rty info body) #:transparent)
(define-struct Apply (fn args) #:transparent)

(provide (struct-out Keyword) (struct-out Identifier) (struct-out Symbol)
         (struct-out Begin) (struct-out Number) (struct-out Boolean)
         (struct-out Semicolon) (struct-out Let) (struct-out Void)
         (struct-out SetBang) (struct-out Unary) (struct-out Binary)
         (struct-out WhileLoop) (struct-out If) (struct-out Else)
         (struct-out IfStmt) (struct-out Vector) (struct-out Indexing)
         (struct-out Var) (struct-out Bool) (struct-out Int)
         (struct-out Prim) (struct-out Type) (struct-out Function)
         (struct-out Def) (struct-out FunctionCall) (struct-out Apply)
         (struct-out Array))
