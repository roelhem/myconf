{-# LANGUAGE TypeOperators #-}
-- |

module Text.Elisp.Pretty where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import qualified Text.Elisp.AST as Elisp
import qualified Text.Megaparsec.Byte.Lexer as L

import Data.Text (Text)
import qualified Data.Text as Text


type Parser s = Parsec Void s

parens :: (Stream s, Token s ~ Char) => Parser s a -> Parser s a
parens = between (char '(') (char ')')

brackets :: (Stream s, Token s ~ Char) => Parser s a -> Parser s a
brackets = between (char '[') (char ']')
