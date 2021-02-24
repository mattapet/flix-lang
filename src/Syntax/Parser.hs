{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

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
keywords = ["if", "then", "else", "let", "true", "false"]

-- Atoms

identifier :: Parsec String u String
identifier = spaces *> identifier' <* spaces
  where
    identifier' = (:) <$> letter <*> many rest >>= failOnKeyword
    failOnKeyword x | x `elem` keywords = fail ""
                    | otherwise         = return x
    rest = alphaNum <|> oneOf "'_"


boolean :: Parsec String u Bool
boolean = spaces *> (true <|> false) <* spaces
  where
    true  = string "true" $> True
    false = string "false" $> False

number :: Parsec String u Integer
number = spaces *> (positive <|> negative) <* spaces
  where
    positive = read <$> many1 digit
    negative = negate <$> (char '-' *> positive)

block :: Parsec String u AST
block = Block <$> (spaces *> braces body <* spaces)
  where
    braces p = char '{' *> p <* char '}'
    body = many expr

parens :: Parsec String u AST
parens = spaces *> char '(' *> expr <* char ')' <* spaces

atom :: Parsec String u AST
atom = foldl1 (<|>) atoms
  where
    atoms =
      try
        <$> [ BoolLiteral <$> boolean
            , NumberLiteral <$> number
            , Identifier <$> identifier
            , block
            , parens
            ]

-- Factors

call :: Parsec String u AST
call = do
  f <- Identifier <$> identifier
  many atom >>= \case
    []   -> return f
    args -> return $ Call f args

factor :: Parsec String u AST
factor = foldl1 (<|>) factors
  where factors = try <$> [BoolLiteral <$> boolean, call, atom]

-- Terms

binop :: Parsec String u AST
binop = factor `chainl1` op
  where op = BinOp <$> (spaces *> many1 (oneOf "+-*/=<>|&") <* spaces)

term :: Parsec String u AST
term = foldl1 (<|>) terms where terms = try <$> [binop, factor]

-- Expressions

ifExpr :: Parsec String u AST
ifExpr = do
  cond  <- string "if" *> term <* anySpaces
  then' <- string "then" *> term <* anySpaces
  else' <- string "else" *> term
  return $ If cond then' else'

letBinding :: Parsec String u AST
letBinding = Let <$> (let' *> identifier) <*> args' <* char '=' <*> expr
  where
    let'  = spaces *> string "let" <* spaces
    args' = many identifier

expr :: Parsec String u AST
expr = anySpaces *> (try letBinding <|> try ifExpr <|> try term) <* anySpaces

parse :: String -> Either String AST
parse = mapLeft show . runParser (expr <* eof) () ""
  where mapLeft f = either (Left . f) Right
