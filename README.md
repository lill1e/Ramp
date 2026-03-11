# Ramp
## Grammar
```
cmp        ::= and | or | > | >= | < | <=
Type       ::= "Number" | "Boolean" | "Void"
Number     ::= [0-9]+
Boolean    ::= True | False
Void       ::= void
Identifier ::= ([A-Z]|[a-z])(A-Za-z0-9_)*
Literal    ::= Number | Boolean | Identifier
			 | Identifier[Literal]
			 | Identifier(Literal (, Literal)*)
			 | (Expression)
			 | [Literal (, Literal)*]
			 | Array(Literal (, Literal*))
			 | Void
Unary      ::= !Unary | -Unary | #Unary | Literal
Mult       ::= Mult * Mult | Unary
Bin        ::= Bin + Bin | Bin - Bin | Mult
BinCmp     ::= BinCmp cmp BinCmp | Bin
Eq         ::= Eq == Eq | Eq != Eq | BinCmp
Assignment ::= Identifier = Eq
Expression ::= Assignment
Stmt       ::= (Let)? Identifier = Expression
			 | if (Expression) Statement (else Statement)?
StmtAlt    ::= while Expression Statement
Block      ::= { Statement* }
Statement  ::= Stmt ; | Block
TopLevel   ::= Statement
			 | function Identifier ((Identifier: Type)*) -> Type Statement
			 | while Expression Statement
			 | Let Identifier = Statement
Program    ::= Statement*
```
