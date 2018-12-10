module Language where

-- The type of expressions
-- TODO: power (right associative)
-- TODO: relational operators (=, !=, >, >=, <, <=)
-- TODO: logical operators (&&, ||)
-- TODO: unnary operator for changing the sign (-)
-- TODO: unnary operator for logical negation (!)
data Expr = Cte Double
          | Var String
          | Bin Op Expr Expr
     deriving (Show)

data Op = Add
        | Sub
        | Mul
        | Div
        | Pow
        | Negative
        | Negation
        | And
        | Or
  deriving (Show)

-- The type of commands
data Cmd = Assign String Expr                       --pronto
         | Print Expr                               --pronto
         | Seq [Expr]
         | Read String Double --altera a memoria    --pronto
         | If Expr Cmd Cmd                          --pronto
         | While Expr [Cmd]
         deriving (Show)
