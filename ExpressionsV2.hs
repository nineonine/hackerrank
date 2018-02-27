module ExpressionsV2 where

import Control.Monad
import Control.Applicative

data UnaryOp = UnaryPlus | UnaryMinus deriving (Show, Eq)

data Expression = Plus !Expression !Expression
                | Sub  !Expression !Expression
                | Mult !Expression !Expression
                | Div  !Expression !Expression
                | Unary UnaryOp !Expression
                | Brackets !Expression
                | Number !Double
                deriving ( Show, Eq)



-- data Expression = Term T `BinOp` Expression
--                 | Term T

-- data T = Factor `BinOp` T
--        | Factor Fact

-- data Fact = Number Integer
--           | Un UnaryOp Factor
--           | Brackets Expression

-- class Evaluable a where
--  eval ::

newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Functor Parser where
    fmap f (Parser cs) = Parser ( \s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
    pure = return
    (Parser cs1) <*> (Parser cs2) = Parser ( \s -> [(f a, s) | (f, s1) <- cs1 s, (a, s) <- cs2 s1] )

instance Monad Parser where
    return a = Parser ( \s -> [(a, s)] )
    p >>= f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

instance MonadPlus Parser where
    mzero     = Parser ( const [] )
    mplus p q = Parser ( \s -> parse p s ++ parse q s)

instance Alternative Parser where
    empty     = mzero
    (<|>) p q = Parser $ \s ->
        case parse p s of
            [] -> parse q s
            parsed -> parsed

some :: Alternative f => f a -> f [a]
some v = some_v
  where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

-- | Zero or more.
many :: Alternative f => f a -> f [a]
many v = many_v
  where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

item :: Parser Char
item = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
  if p c then return c else Parser (const [])
