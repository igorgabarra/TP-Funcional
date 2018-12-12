module Interpreter where

import Language
-- A type to represent the memory, that is, that association of
-- variable names with values
type Memory = [(String, Double)]

-- initial memory
initialMemory :: Memory
initialMemory = [("x", 15)]

-- Expression evaluation
eval :: Memory -> Expr -> Double
eval _ (Cte x) = x
eval m (Var v) =
  case lookup v m of
    Just x -> x
    Nothing -> 0
eval m (Bin op x y) =
  case op of
    Add      -> vx + vy
    Sub      -> vx - vy
    Mul      -> vx * vy
    Div      -> vx / vy
    Pow      -> vx ** vy
    Negative -> (-1)* vx
    GThen    -> if    vx > vy then 1 else 0
    LThen    -> if    vx < vy then 1 else 0
    GEqual   -> if   vx >= vy then 1 else 0
    LEqual   -> if   vx <= vy then 1 else 0
    Equal    -> if   vx == vy then 1 else 0
    Different-> if   vx /= vy then 1 else 0
    And      -> if lvx && lvy then 1 else 0
    Or       -> if lvx || lvy then 1 else 0
    Negation -> if ( vx == 0 || vy == 0 ) then 1 else 0
  where
    vx = eval m x
    vy = eval m y
    lvx = if vx == 1 then True else False
    lvy = if vy == 1 then True else False

-- Command execution
execute :: Memory -> Cmd -> IO Memory
-- Execute for Assign structure
execute m (Assign v e)    = return ((v, eval m e) : m)
-- Execute for Print structure
execute m (Print e)       = do print (eval m e)
                               return m
-- Execute for Seq structure, recursive
execute m (Seq lista)     = sequencia m lista
                              where
                               sequencia m [] = return m
                               sequencia m (x:xs) = do newMemory <- execute m x 
                                                       sequencia newMemory xs
-- Execute for Read structure
execute m (Read str x)    = return ((str, x) : m)
-- Execute for If structure, recursive
execute m (If e c1 c2)    = case eval m e of
                                 0         -> execute m c2
                                 otherwise -> execute m c1
-- Execute for While structure, recursive
execute m (While e c1)    = repeticao m e c1
                              where
                               repeticao m e c1 = case eval m e of
                                                    0         -> return m
                                                    otherwise -> do newMemory <- execute m c1
                                                                    repeticao newMemory e c1

