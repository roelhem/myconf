{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module Text.Elisp where

import Text.Elisp.AST

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Foldable (Foldable(toList))

class ToElisp a where
  elisp :: a -> Expr Literal
  default elisp :: (ToElispLiteral a) => a -> Expr Literal
  elisp = Lit . toElispLiteral

class ToElisp a => ToElispLiteral a where
  toElispLiteral :: a -> Literal

instance ToElisp (Expr Literal) where
  elisp = id

instance ToElisp Literal
instance ToElispLiteral Literal where
  toElispLiteral = id

instance ToElisp Text
instance ToElispLiteral Text where
  toElispLiteral = LString

instance ToElisp a => ToElisp [a] where
  elisp = list'

instance ToElisp Integer
instance ToElispLiteral Integer where
  toElispLiteral = LInt . fromInteger

instance ToElisp Int
instance ToElispLiteral Int where
  toElispLiteral = LInt . fromIntegral

instance ToElisp Char
instance ToElispLiteral Char where
  toElispLiteral = LChar

instance ToElispLiteral [Char] where
  toElispLiteral = LString . Text.pack

instance ToElisp Bool where
  elisp True = Symb "t"
  elisp False = Nil

instance ToElisp a => ToElisp (Maybe a) where
  elisp Nothing = Nil
  elisp (Just x) = elisp x

instance (ToElisp a, ToElisp b) => ToElisp (Either a b) where
  elisp (Left x) = elisp x
  elisp (Right x) = elisp x

nil :: Expr l
nil = Nil

cons :: (ToElisp a, ToElisp b) => a -> b -> Expr Literal
cons a b = Cons (elisp a) (elisp b)

list' :: (Foldable f, ToElisp a) => f a -> Expr Literal
list' = foldr (cons . elisp) Nil

vect :: (Foldable f, ToElisp a) => f a -> Expr Literal
vect = vect . fmap elisp . toList

quote :: ToElisp a => a -> Expr Literal
quote = quote .elisp
