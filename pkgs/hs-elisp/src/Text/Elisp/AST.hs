{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
-- |

module Text.Elisp.AST where

import Data.Text (Text)
import qualified Data.Text as Text

import Data.String (IsString(fromString))

data Literal = LInt !Int
             | LFloat !Double
             | LChar !Char
             | LString !Text
             deriving (Show)

newtype Raw = Raw Text

data Expr l = Nil
            | Cons { _car:: !(Expr l), _cdr:: !(Expr l) }
            | Symb !Text
            | Lit !l
            | Vect ![Expr l]
            | Quote !(Expr l)
            deriving (Show, Functor)

instance IsString Literal where
  fromString = LString . Text.pack

instance IsString (Expr Literal) where
  fromString = Lit . fromString

list :: Foldable f => f (Expr l) -> Expr l
list = foldr Cons Nil

int :: Integral a => a -> Expr Literal
int = Lit . LInt . fromIntegral

float :: Real a => a -> Expr Literal
float = Lit . LFloat . fromRational . toRational

char :: Char -> Expr Literal
char = Lit . LChar

string :: Text -> Expr Literal
string = Lit . LString

funcall :: Text -> [Expr l] -> Expr l
funcall name args = list $ Symb name : args

data Expr' l = Nil'
             | Symb' !Text
             | Lit' !l
             | Vect' ![Expr l]
             | Quote' !(Expr l)

unfoldCons :: Expr l -> ([Expr l], Expr' l)
unfoldCons (Cons car cdr) =
  let (init', last') = unfoldCons cdr
  in (car:init', last');
unfoldCons Nil = ([], Nil')
unfoldCons (Symb name) = ([], Symb' name)
unfoldCons (Lit val) = ([], Lit' val)
unfoldCons (Vect val) = ([], Vect' val)
unfoldCons (Quote val) = ([], Quote' val)
