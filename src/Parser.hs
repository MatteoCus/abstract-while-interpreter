{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Parser (parseString, parseFile) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Functor.Identity
import Stm (Stm (..))
import Exp (AExp (..), Comparison (..))
import IntervalDomain (Infinitable(..))

languageDef :: GenLanguageDef String u Data.Functor.Identity.Identity
languageDef =
   emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "if"
                                      , "then"
                                      , "else"
                                      , "fi"
                                      , "while"
                                      , "do"
                                      , "done"
                                      , "skip"
                                      ]
            , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                      , "=" , "<", ">", "<=", ">=", "<>"
                                      ]
            }

-- Lexer definition, based on reserved names, operators, identifiers naming rules, comments' rules
lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDef

-- Helper parsers
identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

-- Main parser
mainParser :: Parser Stm
mainParser = whiteSpace >> statement

statement :: Parser Stm
statement = do
            stm <- sepBy1 statement' semi
            if null stm
              then return Skip
              else if length stm == 1
                then return $ head stm
                else return $ Concat stm

statement' :: Parser Stm
statement' = whileParser
            <|> ifParser
            <|> skipParser
            <|> assignParser
            <?> "statement"

constantRange :: Parser AExp
constantRange = do
    reserved "["
    low <- integer
    reserved ","
    up <- integer
    reserved "]"
    return $ ConstantRange (Regular low) (Regular up)

constantRangeSingle :: Parser AExp
constantRangeSingle = do
    val <- integer
    return $ ConstantRange (Regular val) (Regular val)

ifParser :: Parser Stm
ifParser = do
                reserved "if"
                cond <- rExpression <?> "arithmetic expression relation"
                reserved "then"
                stm1 <- statement <?> "statement after 'then'"
                reserved "else"
                stm2 <- statement <?> "statement after 'else'"
                reserved "fi" <?> "fi keyword"
                return $ If cond stm1 stm2

whileParser :: Parser Stm
whileParser = do
                reserved "while"
                cond <- rExpression <?> "arithmetic expression relation"
                reserved "do" <?> "do keyword"
                stm <- statement <?> "statement after 'do'"
                reserved "done" <?> "done keyword"
                return $ While cond stm

skipParser :: Parser Stm
skipParser = do
                reserved "skip"
                return Skip

assignParser :: Parser Stm
assignParser = do
                variable <- identifier
                reserved ":=" <?> "assignment operator (:=)"
                Assign variable <$> aExpParser

aExpParser :: Parser AExp
aExpParser = buildExpressionParser aOperators aTerm

aOperators = [ [Prefix (reservedOp "-"   >> return Neg)          ]
              , [Infix  (reservedOp "*"   >> return Mul) AssocLeft,
                 Infix  (reservedOp "/"   >> return Div) AssocLeft]
              , [Infix  (reservedOp "+"   >> return Sum) AssocLeft,
                 Infix  (reservedOp "-"   >> return Sub) AssocLeft]
             ]

aTerm =  fmap Var identifier
     <|> constantRange
     <|> constantRangeSingle
     <?> "arithmetic expression"

rExpression =
  do
    aexp <- aExpParser
    op <- relation
    aexp2 <- aExpParser
    case aexp2
     of (ConstantRange 0 0) -> return $ op aexp (ConstantRange 0 0)
        _ -> error "comparison must be against 0"


relation =   (reservedOp "=" >> return Equal)
         <|> (reservedOp "<" >> return Smaller)
         <|> (reservedOp "<=" >> return SmallerOrEqual)
         <|> (reservedOp ">" >> return Greater)
         <|> (reservedOp ">=" >> return GreaterOrEqual)
         <|> (reservedOp "<>" >> return Different)
         <?> "relational operator"

parseString :: String -> Either String Stm
parseString str =
  case parse (mainParser <* spaces <* eof) "" str of
    Left e  -> Left (show e)
    Right r -> Right r

parseFile :: String -> IO (Either String Stm)
parseFile file =
  do program  <- readFile file
     case parse (mainParser <* spaces <* eof) "" program of
       Left e  -> return $ Left  (show e)
       Right r -> return $ Right r