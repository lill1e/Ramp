# Ramp
## Grammar
```
cmp        ::= == | != | and | or | > | >= | < | <=
Type       ::= "Number" | "Boolean" | "Void"
Number     ::= [0-9]+
Boolean    ::= True | False
Void       ::= void
Identifier ::= ([A-Z]|[a-z])(A-Za-z0-9_)*
Literal    ::= Number | Boolean | Identifier | Void
Unary      ::= !Unary | -Unary | #Unary | Literal
Bin        ::= Bin + Bin | Bin - Bin | Bin cmp Bin
Mult       ::= Mult * Mult | Bin
Expression ::= Mult
Stmt       ::= (Let)? Identifier = Expression
StmtAlt    ::= while Expression Statement
Block      ::= { Statement* }
Statement  ::= Stmt ; | Block
TopLevel   ::= Statement
			 | fn Identifier ((Identifier: Type)*) Statement
			 | while Expression Statement
			 | Let Identifier = Statement
Program    ::= Statement*
```
