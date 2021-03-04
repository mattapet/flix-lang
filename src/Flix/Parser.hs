{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Flix.Parser
  ( parse
  ) where

import           Control.Applicative            ( liftA2
                                                , liftA3
                                                )
import           Control.Monad                  ( guard )
import           Control.Monad.Extra            ( bind2 )
import           Data.Char                      ( isUpper )
import           Data.Functor                   ( ($>) )
import           Flix.Syntax
import           Text.Parsec             hiding ( parse
                                                , space
                                                , spaces
                                                )

parse :: String -> Either String AST
parse = mapLeft show . runParser (ast <* eof) () ""
  where mapLeft f = either (Left . f) Right

ast :: Parsec String u AST
ast = (Decl <$> try decl) <|> (Expr <$> try expr)

-- Atoms

identifierChar :: Parsec String u Char
identifierChar = alphaNum <|> oneOf "'_"

operatorChar :: Parsec String u Char
operatorChar = oneOf "+-*/=<>|&:.$%!"

identifier :: Parsec String u String
identifier = spaces *> identifier' <* spaces
  where
    identifier' = liftA2 (:) letter (many identifierChar) >>= failOnKeyword
    failOnKeyword x | x `elem` keywords = fail "Unexpected keyword identifier"
                    | otherwise         = return x

operator :: Parsec String u String
operator = spaces *> operator' <* spaces
  where
    operator' = many1 operatorChar >>= failOnEquals
    failOnEquals x | x == "="  = fail "Unexpected '=' operator"
                   | otherwise = return x

underscore :: Parsec String u Expr
underscore = spaces *> underscore' <* spaces
  where underscore' = string "_" *> notFollowedBy identifierChar $> Underscore

boolean :: Parsec String u Bool
boolean = spaces *> (true <|> false) <* spaces
  where
    true  = string "true" *> notFollowedBy identifierChar $> True
    false = string "false" *> notFollowedBy identifierChar $> False

charLiteral :: Parsec String u Char
charLiteral = char '\'' *> character <* char '\''
  where
    character = nonEscape <|> escape
    nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"
    escape    = char '\\' *> oneOf "\\'0nrvtbf"

stringLiteral :: Parsec String u String
stringLiteral = char '"' *> many character <* char '"'
  where
    character = nonEscape <|> escape
    nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"
    escape    = char '\\' *> oneOf "\\\"0nrvtbf"

number :: Parsec String u Integer
number = spaces *> (positive <|> negative) <* spaces
  where
    positive = read <$> many1 digit
    negative = negate <$> (char '-' *> positive)

identifierOrConstructor :: Parsec String u Expr
identifierOrConstructor = wrap <$> identifier
  where
    wrap id' | isUpper $ head id' = Constructor id'
             | otherwise          = Identifier id'

list :: Parsec String u [Expr]
list = brackets body
  where
    brackets p = spaces *> char '[' *> p <* char ']' <* spaces
    body = expr `sepBy` char ','

lambda :: Parsec String u Expr
lambda = spaces *> braces body <* spaces
  where
    braces p = char '{' *> p <* char '}'
    body = liftA2 Lambda args expr
    args = anySpaces *> many1 identifier <* spaces <* string "=>" <* spaces

block :: Parsec String u Expr
block = Block <$> (spaces *> braces body <* spaces)
  where
    braces p = char '{' *> p <* char '}'
    body = many expr

parens :: Parsec String u Expr
parens = spaces *> char '(' *> expr' <* char ')' <* spaces
  where
    expr' = try (OperatorCapture <$> operator) <|> tuple
    tuple = expr `sepBy` char ',' >>= \case
      [x] -> return x
      xs  -> return $ Tuple xs


atom :: Parsec String u Expr
atom = foldl1 (<|>) atoms
  where
    atoms =
      try
        <$> [ underscore
            , BoolLiteral <$> boolean
            , NumberLiteral <$> number
            , CharLiteral <$> charLiteral
            , StringLiteral <$> stringLiteral
            , identifierOrConstructor
            , ListLiteral <$> list
            , lambda
            , block
            , parens
            ]

-- Factors

call :: Parsec String u Expr
call = liftA2 unpack_call_or_ident atom (many atom)
  where
    unpack_call_or_ident callee []    = callee
    unpack_call_or_ident callee args' = Call callee args'

factor :: Parsec String u Expr
factor = foldl1 (<|>) factors
  where factors = try <$> [BoolLiteral <$> boolean, call, atom]

-- Terms

binop :: Parsec String u Expr
binop = bind2 go_next factor (optionMaybe operator')
  where
    operator' = operator <|> escaped identifier
    escaped p = spaces *> char '`' *> p <* char '`' <* spaces

    go_next x Nothing = return x
    go_next x (Just op)
      | is_rightAssociative op = BinOp op x <$> binop
      | otherwise = do
        y   <- factor
        op' <- optionMaybe operator'
        go_next (BinOp op x y) op'
    is_rightAssociative = (`elem` ":.") . last


term :: Parsec String u Expr
term = foldl1 (<|>) terms where terms = try <$> [binop, factor]

-- Expressions

ifExpr :: Parsec String u Expr
ifExpr = liftA3 If cond then' else'
  where
    cond  = string "if" *> term <* anySpaces
    then' = string "then" *> term <* anySpaces
    else' = string "else" *> term

matchExpr :: Parsec String u Expr
matchExpr = liftA2 Match value cases
  where
    value = string "match" *> term <* spaces
    cases = spaces *> braces body <* spaces

    braces p = char '{' *> p <* char '}'
    body     = many caseExpr

    caseExpr = liftA2 (,)
                      (anySpaces *> string "case" *> atom)
                      (string "=>" *> spaces *> expr)

letMatch :: Parsec String u Expr
letMatch = do
  name         <- identifier
  (args, body) <- matchCase
  cases        <- many $ try (string name *> matchCase)
  return $ LetMatch name ((args, body) : cases)
  where
    args'     = many atom <* char '=' <* notFollowedBy operatorChar
    matchCase = liftA2 (,) args' expr

letOperator :: Parsec String u Expr
letOperator = liftA3 Let name args body
  where
    name = parens' operator
    args = many identifier <* char '=' <* notFollowedBy operatorChar
    body = expr
    parens' p = spaces *> char '(' *> p <* char ')' <* spaces

let' :: Parsec String u Expr
let' = letKw *> (letOperator <|> letMatch)
  where
    letKw = spaces *> string "let" <* notFollowedBy identifierChar <* spaces

def :: Parsec String u Expr
def = do
  (name, case') <- liftA2 (,) go_decl go_matchCase
  cases         <- many $ try (go_nextDecl name *> go_matchCase)
  return $ Def name (case' : cases)
  where
    go_decl = defKw *> (identifier <|> parens' operator)
    go_nextDecl name = go_decl >>= guard . (name ==)

    go_args = many atom <* char '=' <* notFollowedBy operatorChar
    go_matchCase = liftA2 (,) go_args expr

    defKw = spaces *> string "def" <* notFollowedBy identifierChar <* spaces
    parens' p = spaces *> char '(' *> p <* char ')' <* spaces

expr :: Parsec String u Expr
expr =
  anySpaces
    *> (try def <|> try let' <|> try matchExpr <|> try ifExpr <|> try term)
    <* anySpaces

-- Declarations

moduleDecl :: Parsec String u Decl
moduleDecl = liftA2 Module (moduleKw *> identifier) (many ast)
  where moduleKw = string "module" <* notFollowedBy identifierChar <* space

record :: Parsec String u Decl
record = liftA2 Record (recordKw *> name) (many identifier)
  where
    recordKw = string "record" <* notFollowedBy identifierChar <* space
    name     = identifier <|> parens' operator
    parens' p = spaces *> char '(' *> p <* char ')' <* spaces

decl :: Parsec String u Decl
decl = anySpaces *> (try moduleDecl <|> try record) <* anySpaces


-- Primitive parsers

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
keywords =
  [ "if"
  , "then"
  , "else"
  , "let"
  , "true"
  , "false"
  , "match"
  , "case"
  , "module"
  , "record"
  ]
