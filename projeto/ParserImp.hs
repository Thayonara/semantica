module ParserImp where

import Control.Applicative ((<*))
import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import LexerImp
import qualified Text.Parsec.Token as Token


data Expression = IntExpr  AritExpression 
          | BoolExpr   BoolExpression
          | VarExpr    String
          deriving (Show)


data Command = Skip
          | Assign String Expression
          | If BoolExpression Command Command
          | While BoolExpression Command
          | Print Expression
          | Chain [Command]
          | Declaration [String]
          deriving (Show)

data BinaryOpArit = Add 
            | Mult
            | Div
            | Sub
            deriving (Show)

data BinaryOpBool = And 
            | Or 
            deriving (Show)

data BinOpRel = Less
            | Greater
            | Equal
            deriving (Show)

data AritExpression = IntConst Integer
           | IVar String
           | Neg AritExpression
           | ABinary BinaryOpArit AritExpression AritExpression
           deriving (Show)

data BoolExpression = BoolConst Bool
           | BVar String
           | Not BoolExpression
           | BBinary BinaryOpBool BoolExpression BoolExpression
           | RBinary BinOpRel AritExpression AritExpression
           deriving (Show)


command :: Parser Command
command = parens command <|> chainCommand

statement' :: Parser Command
statement' =  ifCommand 
          <|> whileCommand 
          <|> skipCommand 
          <|> assignCommand 
          <|> printCommand
          <|> declarationCommand

chainCommand :: Parser Command
chainCommand = do
        list <- sepEndBy1 statement' semi
        return $ if length list == 1 then head list else Chain list

ifCommand :: Parser Command
ifCommand = do
        reserved "if"
        cond <- bExpr
        reserved "then"
        command1 <- command
        reserved "else"
        command2 <- command
        return $ If cond command1 command2


whileCommand :: Parser Command
whileCommand = do
        reserved "while"
        cond <- bExpr
        reserved "do"
        comd <- command
        return $ While cond comd

assignCommand :: Parser Command
assignCommand = do
        var <- identifier
        reservedOp ":="
        expresion <- anyExpresion
        return $ Assign var expresion

skipCommand :: Parser Command
skipCommand = reserved "skip" >> return Skip

declarationCommand :: Parser Command
declarationCommand = do
        reserved "var"
        vars <- sepBy1 identifier comma
        return $ Declaration vars

printCommand :: Parser Command
printCommand = do
        reserved "print" 
        value <- parens anyExpresion
        return $ Print value

blockEnd :: Parser String
blockEnd = semi <|> (whiteSpace >> string ")")

anyExpresion :: Parser Expression
anyExpresion =  try (liftM VarExpr (identifier <* lookAhead blockEnd))
       <|> try (liftM IntExpr    aExpr    <* lookAhead blockEnd) 
       <|> try (liftM BoolExpr   bExpr    <* lookAhead blockEnd)


bExpr :: Parser BoolExpression
bExpr = buildExpressionParser bOperators bTerm

aExpr :: Parser AritExpression
aExpr = buildExpressionParser aOperators aTerm

rExpr :: Parser BoolExpression
rExpr = do
        a1 <- aExpr
        op <- relation
        a2 <- aExpr
        return $ RBinary op a1 a2

relation :: Parser BinOpRel
relation =  (reservedOp "<" >> return Less)
        <|> (reservedOp ">" >> return Greater)
        <|> (reservedOp "=" >> return Equal)
aOperators = [ [ Prefix (reservedOp "-" >> return Neg) ]
             , [ Infix  (reservedOp "*" >> return (ABinary Mult)) AssocLeft
             ,   Infix  (reservedOp "/" >> return (ABinary Div)) AssocLeft ]
             , [ Infix  (reservedOp "+" >> return (ABinary Add)) AssocLeft 
             ,   Infix  (reservedOp "-" >> return (ABinary Sub)) AssocLeft ]
             ]

bOperators = [ [ Prefix (reservedOp "not" >> return Not) ]
             , [ Infix  (reservedOp "and" >> return (BBinary And)) AssocLeft ]
             , [ Infix  (reservedOp "or" >> return (BBinary Or)) AssocLeft ]
             ]

aTerm :: Parser AritExpression
aTerm = parens aExpr <|> liftM IVar identifier <|> liftM IntConst integer

bTerm :: Parser BoolExpression
bTerm =   parens bExpr 
     <|> (reserved "true"  >> return (BoolConst True))
     <|> (reserved "false" >> return (BoolConst False))
     <|> try rExpr
     <|> liftM BVar identifier
     
---------------------------------------

parseSource :: String -> Command
parseSource code = 
        case parse whileParser "" code of
            Left e -> error $ show e
            Right r -> r
    where
        whileParser = whiteSpace >> command


