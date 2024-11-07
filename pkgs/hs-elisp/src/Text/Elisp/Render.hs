{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Text.Elisp.Render where

import Text.Elisp.AST

import Data.Char (isAlphaNum, isDigit)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy.Builder (Builder, fromString, fromText, singleton)

class ElispExpressable a where
    renderElisp :: a -> Builder

instance ElispExpressable Raw where
    renderElisp (Raw repr) = fromText repr

instance ElispExpressable Literal where
    renderElisp (LInt val) = rInt val
    renderElisp (LFloat val) = rFloat val
    renderElisp (LString val) = rString val
    renderElisp (LChar val) = fromString ['?', val]

instance (ElispExpressable a) => ElispExpressable (Expr a) where
    renderElisp :: ElispExpressable a => Expr a -> Builder
    renderElisp val = case last' of
        Nil' -> rList init''
        Symb' name -> rListCons init'' $ rSymbol name
        Lit' x -> rListCons init'' $ renderElisp x
        Vect' exprs -> rListCons init'' $ rVect (renderElisp <$> exprs)
        Quote' expr -> singleton '\'' <> renderElisp expr
      where
        (init', last') = unfoldCons val
        init'' = renderElisp <$> init'

instance (ElispExpressable a, ElispExpressable b) => ElispExpressable (Either a b) where
    renderElisp = either renderElisp renderElisp

rSymbol :: Text -> Builder
rSymbol name
    | isIntLike = singleton '\\' <> fromText name
    | otherwise = Text.foldr (\c -> (escapeChar c <>)) mempty name
  where
    escapeChar c
        | isAlphaNum c || c `elem` ("-+=*/_~!@$%^&:<>{}" :: String) = singleton c
        | otherwise = fromString ['\\', c]
    isIntLike = fromMaybe False $ do
        (c, cs) <- Text.uncons name
        pure $ (isDigit c || c `elem` ("-+" :: String))
                && not (Text.null cs)
                && Text.all isDigit cs

rString :: Text -> Builder
rString val = singleton '"' <> escaped <> singleton '"'
  where
    escapedChar c
        | c `elem` ['\\', '"'] = fromString ['\\', c]
        | otherwise = singleton c
    escaped = Text.foldr (\c -> (escapedChar c <>)) mempty val

rInt :: Int -> Builder
rInt = fromString . show
rFloat :: Double -> Builder
rFloat = fromString . show

rList :: [Builder] -> Builder
rList [] = "nil"
rList xs =
    singleton '('
        <> mconcat (intersperse (singleton ' ') xs)
        <> singleton ')'

rListCons :: [Builder] -> Builder -> Builder
rListCons [] x = x
rListCons xs x = singleton '('
                 <> mconcat (intersperse (singleton ' ') xs)
                 <> " . "
                 <> x
                 <> singleton ')'

rVect :: [Builder] -> Builder
rVect xs =
    singleton '['
        <> mconcat (intersperse (singleton ' ') xs)
        <> singleton ']'

rCons :: Builder -> Builder -> Builder
rCons car' cdr' = mconcat [singleton '(', car', " . ", cdr', singleton ')']
