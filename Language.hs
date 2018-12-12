module Language where

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
        | GThen
        | LThen
        | LEqual
        | GEqual
        | Equal
        | Different
        | And
        | Or
        | Negation
  deriving (Show)

-- The type of commands
data Cmd = Assign String Expr                       --pronto
         | Print Expr                               --pronto
         | Seq [Cmd]
         | Read String Double --read from I/O       --pronto
         | If Expr Cmd Cmd                          --pronto
         | While Expr Cmd --while <expr> do <cmd> 
         deriving (Show)

-- End of Language.hs