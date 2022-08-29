{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Arrow
import Data.Maybe (maybeToList)
import qualified Lexer

data Expression
  = Literal Lexer.Token
  | Name Lexer.Token
  | Application [Expression]
  | SpecialForm [Expression]
  | Program [Expression] -- this is different from `List`... in theory >_>
  | Empty
  deriving (Show, Eq)

newtype AST = AST {getExpressions :: [Expression]}

data ParseError
  = Expected Lexer.Token Lexer.Token
  deriving (Eq)

instance Show ParseError where
  show (Expected expected actual) =
    "ParseError: Expected " ++ show expected ++ " got " ++ show actual

newtype Parser a = Parser {runParser :: [Lexer.Token] -> [(a, [Lexer.Token])]}

satisfies :: (Lexer.Token -> Bool) -> Parser Lexer.Token
satisfies p =
  Parser $ \case
    t : ts | p t -> [(t, ts)]
    _ -> []

token :: Lexer.Token -> Parser Lexer.Token
token = satisfies . (==)

pluck :: (Lexer.Token -> Maybe a) -> Parser a
pluck f =
  Parser $ \case
    t : ts -> maybeToList $ fmap (,ts) (f t)
    _ -> []

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser where
  pure a = Parser $ \ts -> [(a, ts)]
  Parser lp <*> Parser rp =
    Parser $ \ts -> do
      (f, rest) <- lp ts
      (a, s) <- rp rest
      return (f a, s)

instance Alternative Parser where
  empty = Parser (const [])
  Parser lp <|> Parser rp =
    Parser $ \ts -> lp ts ++ rp ts

parser :: [Lexer.Token] -> Either ParseError AST
parser (t : ts) = Left $ Expected Lexer.None t
parser [] = Right $ AST []
