-- |

module Data.Emacs.Encode (ToEmacs(..)) where

import Emacs.Module
import Emacs.Module.Assert
import Data.Text (Text)
import Data.Int (Int64)


class ToEmacs a where
  make :: WithCallStack => MonadEmacs m v => a -> m s (v s)

instance ToEmacs Int where
  make = makeInt

instance ToEmacs Int64 where
  make = makeWideInteger

instance ToEmacs Text where
  make = makeText


instance ToEmacs a => ToEmacs [a] where
  make xs = do
    vals <- traverse make xs
    makeList vals
