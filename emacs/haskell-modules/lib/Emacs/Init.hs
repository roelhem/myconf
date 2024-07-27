-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Emacs.Init (initialise) where

import Foreign
import Foreign.C
import Prelude
import Data.Emacs.Module.Runtime (Runtime)
import Data.Emacs.Module.Runtime qualified as Runtime
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Monad (runEmacsM, EmacsM)
import Data.Emacs.Module.SymbolName (mkSymbolName, mkSymbolNameString)
import System.Info (compilerVersion, compilerName)
import Data.Version (Version(versionBranch))

foreign export ccall initialise :: Ptr Runtime -> IO CBool

true, false :: CBool
true = CBool 1
false = CBool 0

initialise :: WithCallStack => WithCallStack => Ptr Runtime -> IO CBool
initialise runtime = do
  runtime' <- Runtime.validateRuntime runtime
  case runtime' of
    Nothing -> pure false
    Just runtime'' ->
      Runtime.withEnvironment runtime'' $ \env -> do
        res <- reportAllErrorsToEmacs env (pure True) $ runEmacsM env initialise'
        pure $ if res then true else false

initialise' :: WithCallStack => EmacsM s Bool
initialise' = do
  bindFunction "hs+compiler-info" =<<
    makeFunction compilerInfo "Retrieves the version of the compiler that build the `my-emacs-native` module."
  provide $ mkSymbolName "my-haskell-native-emacs-extensions"
  pure True


compilerInfo :: WithCallStack => EmacsFunction 'Z 'Z 'False EmacsM Value s
compilerInfo Stop = do
  cSymb <- intern (mkSymbolNameString compilerName)
  vs <- traverse makeInt $ versionBranch compilerVersion
  makeList (cSymb: vs)
