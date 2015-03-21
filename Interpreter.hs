{- Assignment 2 - A Racket Interpreter

This module is the main program for the interpreter.
All of your work should go into this file.

We have provided a skeleton interpreter which can be run
successfully on sample.rkt; it is your job to extend this
program to handle the full range of Paddle.

In the space below, please list your group member(s):
John Collier, g3collie
-}

module Interpreter (main) where

import BaseParser (BaseExpr(LiteralInt, LiteralBool, Atom, Compound), parseFile)
import Data.List
import System.Environment (getArgs)

-- |Run interpreter on an input file,
--  either from commandline or user input.
--  You should not need to change this function.
main :: IO ()
main =
    getArgs >>= \args ->
    if length args > 0
    then
        parseFile (head args) >>= \baseTree ->
        putStr (interpretPaddle baseTree)
    else
        putStrLn "Enter the name of a file: " >>
        getLine >>= \file ->
        parseFile file >>= \baseTree ->
        putStr (interpretPaddle baseTree)

-- Helper for outputting lists
printList [] init = init
printList [x] init = init ++ show x ++ ")"
printList (x:xs) init = printList xs (init ++ show x ++ " ")

getMaybe (Just val) = val

-- Helper function that searches for an identifier and returns its corresponding value
getIdentifier :: BaseExpr -> [(BaseExpr, Expr)] -> Expr
getIdentifier ident exprs = (getMaybe (lookup ident exprs))

-- |Take the output of the base parser and interpret it,
--  first constructing the AST, then evaluating it,
--  and finally returning string representations of the results.
--  You will need to make this function more robust against errors.
interpretPaddle :: Maybe [BaseExpr] -> String
interpretPaddle (Just exprs) =
    let ast = map parseExpr exprs
        vals = map evaluate ast
    in
        -- String representations of each value, joined with newlines
        unlines (map show vals)

-- An expression data type
data Expr = Number Integer |
            Boolean Bool |
            If Expr Expr Expr |
            Not Expr |
            List [Expr] |
            Cond [[Expr]] Expr |
            BinaryExpr BaseExpr Expr Expr
            
instance Show Expr where
    show (Number x) = show x
    show (Boolean True) = "#t"
    show (Boolean False) = "#f"
    show (List []) = "'()"
    show (List vals) = (printList vals "(")

    -- Note: the following definition is not necessary for this assignment,
    -- but you may find it helpful to define string representations of all
    -- expression forms.
    show (If e1 e2 e3) =
        "(if " ++ show e1 ++ " " ++ show e2 ++ " " ++ show e3 ++ ")"

-- |Take a base tree produced by the starter code,
--  and transform it into a proper AST.
parseExpr :: BaseExpr -> Expr
parseExpr (LiteralInt n) = Number n
parseExpr (LiteralBool b) = Boolean b

-- Parse a 'list' expression
parseExpr (Atom "list") =
    List []
parseExpr (Compound ((Atom "list"):vals)) =
    List (map parseExpr vals)
parseExpr (Compound [Atom "if", b, x, y]) =
    If (parseExpr b) (parseExpr x) (parseExpr y)
 

-- Parse any binary expression (an expression with two parameters)
parseExpr (Compound [(Atom opStr), x, y]) =
    BinaryExpr (Atom opStr) (parseExpr x) (parseExpr y)
-- Parse a 'not' expression
parseExpr (Compound [Atom "not", x]) =
    Not (parseExpr x)




-- |Evaluate an AST by simplifying it into
--  a number, boolean, list, or function value.
evaluate :: Expr -> Expr
evaluate (Number n) = Number n
evaluate (Boolean b) = Boolean b

-- If Statement expression. 
-- Rewritten to allow subexpression for all three parameters
evaluate (If (Boolean cond) x y) =
    case cond of
        True -> (evaluate x)
        False -> (evaluate y)
evaluate (If cond x y) =
    (evaluate (If (evaluate cond) x y))

-- Evaluate some kind of binary expression
evaluate (BinaryExpr (Atom opStr) (Number x) (Number y)) =
    case opStr of
        "+" -> (Number (x + y))
        "*" -> (Number (x * y))
        "equal?" -> (Boolean (x == y))
evaluate (BinaryExpr (Atom opStr) (Boolean x) (Boolean y)) =
   case opStr of
       "and" -> (Boolean (x && y))
       "or" -> (Boolean (x || y))
       "equal?" -> (Boolean (x == y))
evaluate (BinaryExpr (Atom opStr) x y) = 
    (evaluate (BinaryExpr (Atom opStr) (evaluate x) (evaluate y)))

-- Evaluate a 'not' expression
evaluate (Not (Boolean x)) = (Boolean (not x))
evaluate (Not x) = (evaluate (Not (evaluate x)))

-- Evaluate a 'list'
evaluate (List vals) = (List (map evaluate vals))


