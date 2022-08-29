{-# LANGUAGE LambdaCase #-}

module Lexer (lexer, Lexer (..), Token (..), LexerError (..)) where

import Control.Applicative (Alternative (many, some), Applicative (liftA2))
import Control.Arrow (Arrow (first))
import Data.CaseInsensitive (mk)
import Data.Char
import Data.List (foldl1')
import GHC.Base (Alternative (empty, (<|>)))

data Token
  = LParen
  | RParen
  | LCurly
  | RCurly
  | LBracket
  | RBracket
  | Quote
  | Hash
  | Dot
  | Name String
  | StringLiteral String
  | NumberLiteral Double
  | None
  deriving (Show, Eq)

data PreprocessorToken
  = Comment String
  | Newline
  | Blankspace String
  | SourceToken Token
  deriving (Show, Eq)

data LexerError
  = Unexpected Char
  | UnexpectedEOF
  deriving (Eq, Show)

data Positioned a = Positioned

newtype Lexer a = Lexer {runLexer :: String -> Either LexerError (a, String)}

unexpected :: String -> LexerError
unexpected [] = UnexpectedEOF
unexpected (c : _) = Unexpected c

satisfies :: (Char -> Bool) -> Lexer Char
satisfies p =
  Lexer $ \case
    c : cs | p c -> Right (c, cs)
    rest -> Left $ unexpected rest

{-# INLINEABLE optionalLexer #-}
optionalLexer :: a -> Lexer a -> Lexer a
optionalLexer z l = pure z <|> l

{-# INLINEABLE (<?>) #-}

infixl 5 <?>

(<?>) :: a -> Lexer a -> Lexer a
(<?>) = optionalLexer

{-# INLINEABLE notChar #-}
notChar :: Char -> Lexer Char
notChar c = satisfies (/= c)

{-# INLINEABLE char #-}
char :: Char -> Lexer Char
char c = satisfies (== c)

{-# INLINEABLE ichar #-}
ichar :: Char -> Lexer Char
ichar c = satisfies $ (== mk c) . mk

{-# INLINEABLE digit #-}
digit :: Lexer Char
digit = satisfies isDigit

{-# INLINEABLE alpha #-}
alpha :: Lexer Char
alpha = satisfies isAlpha

{-# INLINEABLE alnum #-}
alnum :: Lexer Char
alnum = satisfies isAlphaNum

{-# INLINEABLE string #-}
string :: String -> Lexer String
string = traverse char

{-# INLINEABLE istring #-}
istring :: String -> Lexer String
istring = traverse ichar

both :: Lexer a -> Lexer a -> Lexer a
both (Lexer ll) (Lexer lr) =
  Lexer $ \input -> case (ll input, lr input) of
    (l@(Right _), Right _) -> l
    (l@(Left _), _) -> l
    (_, r@(Left _)) -> r

instance Functor Lexer where
  fmap f (Lexer lf) = Lexer (fmap (first f) . lf)

instance Applicative Lexer where
  pure a = Lexer $ \input -> Right (a, input)
  Lexer ll <*> Lexer lr =
    Lexer $ \input -> do
      (f, rest) <- ll input
      (a, rest') <- lr rest
      return (f a, rest')

instance Alternative Lexer where
  empty = Lexer $ Left . unexpected
  Lexer ll <|> Lexer lr =
    Lexer $ \input -> case (ll input, lr input) of
      (res, Left _) -> res
      (Left _, res) -> res
      (l@(Right (_, restL)), r@(Right (_, restR))) ->
        if length restL <= length restR then l else r

one :: Applicative f => f a -> f [a]
one a = (:) <$> a <*> pure []

oneOf :: Alternative f => [f a] -> f a
oneOf = foldl1' (<|>)

-- singleFollowedBy :: Lexer Char -> Lexer Char -> Lexer String
-- followedBy l1 l2 = liftA2 (:) l1 (many l2)

followedBySome :: Lexer a -> Lexer a -> Lexer [a]
followedBySome l1 l2 = liftA2 (:) l1 (some l2)

{-# INLINEABLE followedByConcat #-}
followedByConcat :: Lexer [a] -> Lexer [a] -> Lexer [a]
followedByConcat ll lr = (++) <$> ll <*> lr

{-# INLINEABLE followedBy #-}
followedBy :: Lexer a -> Lexer [a] -> Lexer [a]
followedBy ll lr = (:) <$> ll <*> lr

maybeFollowedBy :: Lexer [a] -> Lexer [a] -> Lexer [a]
maybeFollowedBy ll lr = followedByConcat ll ([] <?> lr)

manyFollowedBy :: Lexer [a] -> Lexer a -> Lexer [a]
manyFollowedBy ll lr = followedByConcat ll (many lr)

manyFollowedBySome :: Lexer [a] -> Lexer a -> Lexer [a]
manyFollowedBySome ll lr = followedByConcat ll (some lr)

infixr 5 >?

{-# INLINEABLE (>?) #-}
(>?) :: Lexer [a] -> Lexer [a] -> Lexer [a]
(>?) = maybeFollowedBy

infixl 6 >*

{-# INLINEABLE (>*) #-}
(>*) :: Lexer [a] -> Lexer a -> Lexer [a]
(>*) = manyFollowedBy

infixl 6 >+

{-# INLINEABLE (>+) #-}
(>+) :: Lexer [a] -> Lexer a -> Lexer [a]
(>+) = manyFollowedBySome

lexNumber :: Lexer Double
lexNumber = read <$> (("" <?> string "-") `followedByConcat` (some digit >? (char '.' `followedBySome` digit)) >? ((istring "e" >? string "-") >+ digit))

lexString :: Lexer String
lexString = char '"' *> strLexer <* char '"'
  where
    strLexer :: Lexer String
    strLexer = many (satisfies ((&&) <$> (/= '"') <*> (/= '\\'))) >? escSeq >? strLexer

    escSeq :: Lexer String
    escSeq =
      char '\\'
        *> Lexer
          ( \case
              '\\' : cs -> Right ("\\", cs)
              'n' : cs -> Right ("\n", cs)
              'r' : cs -> Right ("\r", cs)
              't' : cs -> Right ("\t", cs)
              '0' : cs -> Right ("\0", cs)
              '\n' : _ -> Left $ Unexpected '\n'
              c : cs -> Right ([c], cs)
              [] -> Left UnexpectedEOF
          )

lexName :: Lexer String
lexName = one begNameChar >* nameChar >* char '\''
  where
    specialChars :: [Lexer Char]
    specialChars = fmap char "!@#$%^&*-_=+:<>?/"

    begNameChar :: Lexer Char
    begNameChar = oneOf $ alpha : specialChars

    nameChar :: Lexer Char
    nameChar = oneOf $ alnum : specialChars

with :: Functor f => b -> f a -> f b
with a = fmap (const a)

numberLit :: Lexer PreprocessorToken
numberLit = SourceToken . NumberLiteral <$> lexNumber

stringLit :: Lexer PreprocessorToken
stringLit = SourceToken . StringLiteral <$> lexString

name :: Lexer PreprocessorToken
name = SourceToken . Name <$> lexName

operator :: Lexer PreprocessorToken
operator =
  SourceToken
    <$> oneOf
      [ LParen `with` char '(',
        RParen `with` char ')',
        LCurly `with` char '{',
        RCurly `with` char '}',
        LBracket `with` char '[',
        RBracket `with` char ']',
        Quote `with` char '\'',
        Hash `with` char '#',
        Dot `with` char '.'
      ]

-- whitespace :: Lexer Token
-- whitespace = Lexer $ \case
-- c : cs | isSpace c -> Right (Whitespace c, cs)
-- s -> Left $ unexpected s

whitespace :: Lexer PreprocessorToken
whitespace = space <|> newline <|> comment
  where
    comment = Comment <$> char ';' `followedBy` many (notChar '\n')
    newline = Newline `with` char '\n'
    space = Blankspace <$> some (satisfies ((&&) <$> isSpace <*> (/= '\n')))

lexer :: Lexer [PreprocessorToken]
lexer = Lexer $ \input -> case runLexer (many lexer') input of
  v@(Right (_, "")) -> v
  (Right (_, rest)) -> runLexer (one lexer') rest
  v@(Left _) -> v
  where
    lexer' = whitespace <|> operator <|> numberLit <|> stringLit <|> name
