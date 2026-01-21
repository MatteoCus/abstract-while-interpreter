{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Parser (parseString, parseFile) where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Functor.Identity
import Stm (Stm (..))
import Exp (AExp (..), BExp (..))

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
                                      , "while"
                                      , "do"
                                      , "skip"
                                      ]
            , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                      , "<=", "=", "(", ")"
                                      ]
            }

-- Lexer definition, based on reserved names, operators, identifiers naming rules, comments' rules
lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDef

-- Helper parsers
identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                     --   parens p
                                     -- takes care of the parenthesis and
                                     -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace


-- Main parser
mainParser :: Parser [Stm]
mainParser = whiteSpace >> statement

statement :: Parser [Stm]
statement = sepBy1 statement' semi

statement' :: Parser Stm
statement' =    concatParser
            <|> whileParser
            <|> ifParser
            <|> skipParser
            <|> assignParser
            <?> "statement"

concatParser :: Parser Stm
concatParser = do
                  reservedOp "("
                  stms <- statement <?> "statement after '('"
                  reservedOp ")"
                  case stms
                     of [s] -> return s
                        _ -> return $ Concat stms
                  

ifParser :: Parser Stm
ifParser = do
                reserved "if"
                cond <- rExpression <?> "arithmetic expression relation"
                reserved "then"
                stm1 <- statement' <?> "statement after 'then'"
                reserved "else"
                stm2 <- statement' <?> "statement after 'else'"
                return $ If cond stm1 stm2

whileParser :: Parser Stm
whileParser = do
                reserved "while"
                cond <- rExpression <?> "arithmetic expression relation"
                reserved "do" <?> "do keyword"
                stm <- statement' <?> "statement after 'do'"
                return $ While cond stm

skipParser :: Parser Stm
skipParser = do 
                reserved "skip"
                return Skip

assignParser :: Parser Stm
assignParser = do
                variable <- identifier
                reserved ":=" <?> "assignment operator (:=)"
                value <- aExpParser
                return $ Assign variable value

aExpParser :: Parser AExp
aExpParser = buildExpressionParser aOperators aTerm

aOperators = [ [Prefix (reservedOp "-"   >> return (Neg))          ]
              , [Infix  (reservedOp "*"   >> return (Mul)) AssocLeft,
                 Infix  (reservedOp "/"   >> return (Div)) AssocLeft]
              , [Infix  (reservedOp "+"   >> return (Sum)) AssocLeft,
                 Infix  (reservedOp "-"   >> return (Sub)) AssocLeft]
             ]

aTerm =  parens aExpParser
     <|> liftM Var identifier
     <|> liftM AConstant integer
     <?> "arithmetic expression"

rExpression =
  do a1 <- aExpParser
     op <- relation
     a2 <- aExpParser
     return $ op a1 a2

relation =   (reservedOp "=" >> return Equal)
         <|> (reservedOp "<=" >> return SmallerOrEqual)
         <?> "relational operator (= or <=)"

parseString :: String -> (Either String [Stm])
parseString str =
  case parse (mainParser <* spaces <* eof) "" str of
    Left e  -> Left (show e)
    Right r -> Right r

parseFile :: String -> IO (Either String [Stm])
parseFile file =
  do program  <- readFile file
     case parse (mainParser <* spaces <* eof) "" program of
       Left e  -> return $ Left  (show e) 
       Right r -> return $ Right r