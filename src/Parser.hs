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
import RuntimeConfiguration (RuntimeConfig (intervalBounds))

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
                                      , "=" , "<", ">", "<=", ">=", "<>", "[", "]", ","
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
mainParser :: RuntimeConfig -> Parser Stm
mainParser config = whiteSpace >> statement config

statement :: RuntimeConfig -> Parser Stm
statement config = do
            stm <- sepBy1 (statement' config) semi
            if null stm
              then return Skip
              else if length stm == 1
                then return $ head stm
                else return $ Concat stm

statement' :: RuntimeConfig -> Parser Stm
statement' config = whileParser config
            <|> ifParser config
            <|> skipParser
            <|> assignParser config
            <?> "statement"

constantRange :: RuntimeConfig -> Parser AExp
constantRange config = do
    reservedOp "["
    low <- integer
    reservedOp ","
    up <- integer
    reservedOp "]"
    if low > up
      then error $ "Invalid interval: [" ++ show low ++ ", " ++ show up ++ "]"                                                   -- Lower bound greater than the upper bound
      else 
        if ((m > n) && (low /= up)) || ((m <= n) && ((low < m) || (up > n)))                                                     -- Not suitable wrt the analysis configuration
        then error $ "The instantiated domain doesn't allow the provided interval: [" ++ show low ++ ", " ++ show up ++ "]"
        else return $ ConstantRange (Regular low) (Regular up)
      where (m,n) = intervalBounds config

constantRangeSingle :: Parser AExp
constantRangeSingle = do
    val <- integer
    return $ ConstantRange (Regular val) (Regular val)

ifParser :: RuntimeConfig -> Parser Stm
ifParser config = do
                reserved "if"
                cond <- rExpression config <?> "arithmetic expression relation"
                reserved "then"
                stm1 <- statement config <?> "statement after 'then'"
                reserved "else"
                stm2 <- statement config <?> "statement after 'else'"
                reserved "fi" <?> "fi keyword"
                return $ If cond stm1 stm2

whileParser :: RuntimeConfig -> Parser Stm
whileParser config= do
                reserved "while"
                cond <- rExpression config <?> "arithmetic expression relation"
                reserved "do" <?> "do keyword"
                stm <- statement config <?> "statement after 'do'"
                reserved "done" <?> "done keyword"
                return $ While cond stm

skipParser :: Parser Stm
skipParser = do
                reserved "skip"
                return Skip

assignParser :: RuntimeConfig -> Parser Stm
assignParser config = do
                variable <- identifier
                reserved ":=" <?> "assignment operator (:=)"
                Assign variable <$> aExpParser config

aExpParser :: RuntimeConfig -> Parser AExp
aExpParser config = buildExpressionParser aOperators (aTerm config)

aOperators = [ [Prefix (reservedOp "-"   >> return Neg)          ]
              , [Infix  (reservedOp "*"   >> return Mul) AssocLeft,
                 Infix  (reservedOp "/"   >> return Div) AssocLeft]
              , [Infix  (reservedOp "+"   >> return Sum) AssocLeft,
                 Infix  (reservedOp "-"   >> return Sub) AssocLeft]
             ]

aTerm config =  fmap Var identifier
     <|> constantRange config
     <|> constantRangeSingle
     <?> "arithmetic expression"

rExpression config =
  do
    aexp <- aExpParser config
    op <- relation
    aexp2 <- aExpParser config
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

parseString :: String -> RuntimeConfig -> Either String Stm
parseString str config =
  case parse (mainParser  config <* spaces <* eof) "" str of
    Left e  -> Left (show e)
    Right r -> Right r

parseFile :: String -> RuntimeConfig -> IO (Either String Stm)
parseFile file config =
  do program  <- readFile file
     case parse (mainParser config <* spaces <* eof) "" program of
       Left e  -> return $ Left  (show e)
       Right r -> return $ Right r