module MyFunParser where

import Control.Applicative (some, (<|>))
import Parser
import Language

-- A parser for double
double :: Parser Double
double =
  do x <- integer
     y <- decimalPart
     return (fromIntegral x + y)            
  where
    decimalPart =
      do char '.'
         rest <- some digit
         return (read rest / 10 ^ (length rest))
      <|>
      return 0

-- A parser for constant expressions
cte :: Parser Expr
cte = Cte <$> token double

-- A parser for variable expressions
var :: Parser Expr
var = Var <$> token identifier

-- A parser for atomic expressions, that is, the simplest expressions,
-- with the highest precedence
atomic :: Parser Expr
atomic = cte <|>
         var <|>
         token (char '(') *> expr <* token (char ')')


-- A parser for a binary operator
binop :: String -> (a -> a -> a) -> Parser (a -> a -> a)
binop name function = token (string name) *> pure function


-- A parser for pow
pow :: Parser Expr
pow = chainl1 atomic (binop "^" (Bin Pow))


-- A parser for multiplication and division
mul :: Parser Expr
mul = chainl1 pow (binop "*" (Bin Mul) <|> binop "/" (Bin Div))


-- A parser for addition and subtraction
add :: Parser Expr
add = chainl1 mul (binop "+" (Bin Add) <|> binop "-" (Bin Sub))


-- A parser for negation of expression
negation :: Parser Expr
negation = chainl1 atomic (binop "!" (Bin Negation))


-- A parser for binary operators: && and ||
and1 :: Parser Expr
and1 = chainl1 negation (binop "&&" (Bin And) <|> binop "||" (Bin Or))


-- A parser for expressions
expr :: Parser Expr
expr = add <|> pow <|> mul <|> add <|> negation <|> and1  


-- A parser for assignment command
assign2 :: Parser Cmd
assign2 = Assign <$> token identifier <*
                     token (string ":=") <*>
                     expr
-- ... or alternatively
assign :: Parser Cmd
assign = do id <- token identifier
            token (string ":=")
            e <- expr
            return (Assign id e)

-- A parser for print command
printcmd :: Parser Cmd
printcmd = Print <$> (token (string "print") *> expr)
 

-- A parser for if-else command
ifcmd :: Parser Cmd
ifcmd = do token (string "if")
           token (char '(')
           e <- expr
           token (string ") then")
           c1 <- cmd
           token (string "else") 
           c2 <- cmd
           return (If e c1 c2)

-- A parser for while command
whilecmd :: Parser Cmd
whilecmd = do token (string "while")
              e <- expr
              return (While e lista)
           where
             lista = (:) <$> cmd <* token (string ";") 
              

-- (:) <$> letter <*> many (letter <|> digit <|> underscore)
-- A parser for read command
readcmd :: Parser Cmd
readcmd = do id <- token identifier
             token (string "<-")
             e <- token double 
             return (Read id e)

{-
seqcmd :: Parser Cmd
seqcmd = do command <- expr
            token (string "; ")
            return ( (:) <$> (Seq command) <*> seqcmd)

-}
-- A parser for command
cmd :: Parser Cmd
cmd = assign <|> printcmd  <|> ifcmd  <|> readcmd <|> whilecmd -- <|> seqcmd <|> whilecmd 


{-

whilecmd :: Parser Cmd
whilecmd = While <$> token (string "while(") *> expr <* token (char ')') 

readcmd :: Parser Cmd
readcmd = Read <$> token identifier <*
                   token (string "read ") <*>
                   expr 

ifcmd :: Parser Cmd
ifcmd = If <$> (token (string "if") *> expr)  

-}