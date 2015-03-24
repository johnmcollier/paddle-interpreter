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



-- |Take the output of the base parser and interpret it,
--  first constructing the AST, then evaluating it,
--  and finally returning string representations of the results.
--  You will need to make this function more robust against errors.
interpretPaddle :: Maybe [BaseExpr] -> String
interpretPaddle (Just exprs) =
    let ast = map parseExpr exprs 
        vals = evalExpr ast []
    in
        -- String representations of each value, joined with newlines
        unlines (map myShow (findErrors (filter isNotNull vals) []))

evalExpr :: [Expr] -> [([Char], Expr)] -> [Expr]
evalExpr asts idens = 
	if ((length asts) == 1)
		then let (_, output) = (evaluate idens (head asts))
		in output:[]
		else let (n_idens, output) = (evaluate idens (head asts))
		in output:(evalExpr (tail asts) n_idens)

-- An expression data type
data Expr = Number Integer |
            Boolean Bool |
            If Expr Expr Expr |
            Not Expr |
            List [Expr] |
            BinaryExpr BaseExpr Expr Expr |
            Identifier [Char] Expr |
            Function [Char] Expr |
            FunctionApp Expr [Expr] |
            Error [Char] |
            Cond [Expr] |
            Null
            
instance Show Expr where
    show (Number x) = show x
    show (Boolean True) = "#t"
    show (Boolean False) = "#f"
    show (List []) = "'()"
    show (List vals) = (printList vals "(")
    show (Identifier _ expr) = show expr
    show (Function _ _) = "#<procedure>"
    show (Null) = ""
    show (Error errType) = errType
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



parseExpr (Compound [Atom "else", expr]) =
   parseExpr expr
-- Parse a 'list' expression
parseExpr (Atom "list") =
    List []
parseExpr (Compound ((Atom "list"):vals)) =
    List (map parseExpr vals)
parseExpr (Compound [Atom "if", b, x, y]) =
    If (parseExpr b) (parseExpr x) (parseExpr y)
parseExpr (Compound [Atom "define", Atom name, expr]) =
    Identifier name (parseExpr expr)
parseExpr (Compound ((Atom "let"):exprs)) = 
    Number 1
parseExpr (Compound ((Atom "let*"):exprs)) = 
    Number 1
-- Parse a 'not' expression
parseExpr (Compound [Atom "not", x]) =
    Not (parseExpr x)
parseExpr (Atom identName) =
	(Identifier identName Null)

parseExpr (Compound [x, expr]) = 
    let output = parseExpr x
    in case output of
           Boolean True -> parseExpr expr
           _ -> Null
parseExpr (Compound ((Atom "cond"):exprs)) =
    Cond (map parseExpr exprs)
-- Parse any binary expression (an expression with two parameters)
parseExpr (Compound [(Atom opStr), x, y]) =
    BinaryExpr (Atom opStr) (parseExpr x) (parseExpr y)
    

-- |Evaluate an AST by simplifying it into
--  a number, boolean, list, or function value.
evaluate :: [([Char], Expr)] -> Expr -> ([([Char], Expr)], Expr)
evaluate idents (Number n) = (idents, (Number n))
evaluate idents (Boolean b) = (idents, (Boolean b))

-- If Statement expression. 
-- Rewritten to allow subexpression for all three parameters
evaluate ident (If (Boolean cond) x y) =
    case cond of
        True -> (evaluate ident x)
        False -> (evaluate ident y)
evaluate ident (If cond x y) =
	let (_, output) = (evaluate ident cond)
	in (evaluate ident (If output x y))

-- Evaluate some kind of binary expression
evaluate ident (BinaryExpr (Atom opStr) (Number x) (Number y)) =
    case opStr of
        "+" -> (ident, (Number (x + y)))
        "*" -> (ident, (Number (x * y)))
        "equal?" -> (ident, (Boolean (x == y)))
evaluate ident (BinaryExpr (Atom opStr) (Boolean x) (Boolean y)) =
   case opStr of
       "and" -> (ident, (Boolean (x && y)))
       "or" -> (ident, (Boolean (x || y)))
       "equal?" -> (ident, (Boolean (x == y)))
evaluate ident (BinaryExpr (Atom opStr) x y) =
	let (identX, outputX) = (evaluate ident x)
	in let (identY, outputY) = (evaluate ident y)
	in (evaluate ident (BinaryExpr (Atom opStr) outputX outputY))

-- Evaluate a 'not' expression
evaluate ident (Not (Boolean x)) = (ident, (Boolean (not x)))
evaluate ident (Not x) = 
	let (_, output) = (evaluate ident x)
	in (evaluate ident (Not output))

-- Evaluate a 'list'
evaluate ident (List vals) = 
	(ident, (List (map (\x -> let (_, output) = (evaluate ident x) in output) vals)))

evaluate ident (Identifier name Null) =
	(ident, (getIdentifier name ident))
evaluate ident (Identifier name expr) =
	((name, expr):ident, Null)

-- Evaluate an 'Error'
evaluate ident (Error errorType) =
	(ident, (Error errorType))

-- Evaluate a 'Cond'
evaluate ident (Cond exprs) = 
        let (_, output) = (evaluate ident (head (filter isNotNull exprs)))
	in (ident, output)
----- HELPER FUNCTIONS -----------
-- Helper for outputting lists
printList [] init = init
printList [x] init = init ++ show x ++ ")"
printList (x:xs) init = printList xs (init ++ show x ++ " ")

getMaybe (Just val) = val
getMaybe (Nothing) = (Error "Syntax error")

-- Helper function that searches for an identifier and returns its corresponding value
getIdentifier :: [Char] -> [([Char], Expr)] -> Expr
getIdentifier ident expr = (getMaybe (lookup ident expr))

-- Inserts a new identifier into the identifier list and returns the updated list
putIdentifier :: ([Char], Expr) -> [([Char], Expr)] -> [([Char], Expr)]
putIdentifier (name, expr) idents = (name, expr):idents

isEmpty :: [([Char], Expr)] -> Bool
isEmpty idents = null idents

-- Helper function that returns True iff an Expr is a 'Null' type
isNotNull :: Expr -> Bool
isNotNull expr = case expr of
                  Null -> False
                  _ -> True 

isError :: Expr -> Bool
isError expr = case expr of
                   (Error _) -> True
                   _ -> False

-- Helper function that checks a list of expressions for an error
-- It returns the list up to and including the error, otherwise the
-- entire list is returned
findErrors :: [Expr] -> [Expr] -> [Expr]
findErrors [] acc = acc
findErrors exprs acc =
    if (isError (head exprs))
        then acc++[(head exprs)]
        else findErrors (tail exprs) (acc++[(head exprs)])

-- Function that adds the little apostrophe in front of lists
showList :: [Char] -> [Char]
showList listOutput = "'"++listOutput

-- Custom show function, used to append values to show output (mainly for lists)
myShow :: Expr -> [Char]
myShow expr = case expr of
                  (List _) -> "'"++(show expr)
                  _ -> (show expr)
