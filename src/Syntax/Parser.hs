{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Syntax.Parser
  ( parse
  ) where

import           Data.Functor                   ( ($>) )
import           Syntax.Core
import           Text.Parsec             hiding ( parse
                                                , space
                                                , spaces
                                                )

space :: Parsec String u Char
space = char ' '

eol :: Parsec String u String
eol =
  try (string "\r\n")
    <|> try (string "\n\r")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

anySpace :: Parsec String u String
anySpace = (: []) <$> space <|> eol

spaces :: Parsec String u ()
spaces = skipMany space

anySpaces :: Parsec String u ()
anySpaces = skipMany anySpace

keywords :: [String]
keywords = ["if", "then", "else", "let", "true", "false", "match", "case"]

-- Atoms

identifierChar :: Parsec String u Char
identifierChar = alphaNum <|> oneOf "'_"

identifier :: Parsec String u String
identifier = spaces *> identifier' <* spaces
  where
    identifier' = (:) <$> letter <*> many identifierChar >>= failOnKeyword
    failOnKeyword x | x `elem` keywords = fail ""
                    | otherwise         = return x

underscore :: Parsec String u Expr
underscore = spaces *> underscore' <* spaces
  where underscore' = string "_" *> notFollowedBy identifierChar $> Underscore

boolean :: Parsec String u Bool
boolean = spaces *> (true <|> false) <* spaces
  where
    true  = string "true" *> notFollowedBy identifierChar $> True
    false = string "false" *> notFollowedBy identifierChar $> False

number :: Parsec String u Integer
number = spaces *> (positive <|> negative) <* spaces
  where
    positive = read <$> many1 digit
    negative = negate <$> (char '-' *> positive)

block :: Parsec String u Expr
block = Block <$> (spaces *> braces body <* spaces)
  where
    braces p = char '{' *> p <* char '}'
    body = many expr

parens :: Parsec String u Expr
parens = spaces *> char '(' *> expr <* char ')' <* spaces

atom :: Parsec String u Expr
atom = foldl1 (<|>) atoms
  where
    atoms =
      try
        <$> [ underscore
            , BoolLiteral <$> boolean
            , NumberLiteral <$> number
            , Identifier <$> identifier
            , block
            , parens
            ]

-- Factors

call :: Parsec String u Expr
call = do
  f <- Identifier <$> identifier
  many atom >>= \case
    []   -> return f
    args -> return $ Call f args

factor :: Parsec String u Expr
factor = foldl1 (<|>) factors
  where factors = try <$> [BoolLiteral <$> boolean, call, atom]

-- Terms

binop :: Parsec String u Expr
binop = factor `chainl1` op
  where op = BinOp <$> (spaces *> many1 (oneOf "+-*/=<>|&") <* spaces)

term :: Parsec String u Expr
term = foldl1 (<|>) terms where terms = try <$> [binop, factor]

-- Expressions

ifExpr :: Parsec String u Expr
ifExpr = do
  cond  <- string "if" *> term <* anySpaces
  then' <- string "then" *> term <* anySpaces
  else' <- string "else" *> term
  return $ If cond then' else'

matchExpr :: Parsec String u Expr
matchExpr = do
  value <- string "match" *> term <* spaces
  cases <- spaces *> braces body <* spaces
  return $ Match value cases
  where
    braces p = char '{' *> p <* char '}'
    body = many caseExpr
    caseExpr =
      (,)
        <$> (anySpaces *> string "case" *> atom)
        <*> (string "=>" *> spaces *> expr)

letBinding :: Parsec String u Expr
letBinding = Let <$> (let' *> identifier) <*> args' <* char '=' <*> expr
  where
    let'  = spaces *> string "let" <* spaces
    args' = many identifier

expr :: Parsec String u Expr
expr =
  anySpaces
    *> (try letBinding <|> try matchExpr <|> try ifExpr <|> try term)
    <* anySpaces

parse :: String -> Either String AST
parse = mapLeft show . runParser (Expr <$> expr <* eof) () ""
  where mapLeft f = either (Left . f) Right
