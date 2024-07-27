{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- | An emacs renderer for prettyprinter

module Data.Emacs.Properties.Pretty where

import Data.Emacs.Encode
import Prettyprinter
import Emacs.Module (MonadEmacs)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as TLB
import Control.Monad.State (StateT)
import Control.Monad.Trans.Writer (WriterT)
import Data.STRef
import Control.Monad.ST


type AnnPos ann = ((Int, Int), ann)

data Alg ann m = Alg { incrPos :: !(Int -> m ())
                     , pushAnn :: !(ann -> m ())
                     , popAnn  :: !(m (AnnPos ann))
                     }


runAlg :: Monad m => Alg ann m -> SimpleDocStream ann -> m [AnnPos ann]
runAlg Alg{..} = go
  where go = \case
          SFail -> error "Encountered SFail"
          SEmpty -> pure []
          SChar _ rest -> do
            incrPos 1
            go rest
          SText l _ rest -> do
            incrPos l
            go rest
          SLine l rest -> do
            incrPos (l + 1)
            go rest
          SAnnPush ann rest -> do
            pushAnn ann
            go rest
          SAnnPop rest -> do
            pos <- popAnn
            (pos :) <$> go rest

runAnnPosST :: SimpleDocStream ann -> [AnnPos ann]
runAnnPosST docStream = runST do
  pos <- newSTRef 0
  stack <- newSTRef []
  let incrPos i = modifySTRef' pos (+ i)
  let readPos = readSTRef pos
  let pushAnn a = do
        i <- readPos
        modifySTRef stack ((i, a) :)
  let popAnn = do
        as <- readSTRef stack
        case as of
          [] -> error "Stack is empty :("
          ((prevPos, a):stail) -> do
              currPos <- readPos
              writeSTRef stack stail
              pure ((prevPos, currPos), a)

  runAlg (Alg{..}) docStream

renderLazyText :: SimpleDocStream ann -> LT.Text
renderLazyText = TLB.toLazyText . go
  where go = \case
          SFail -> undefined
          SEmpty -> mempty
          SChar c rest -> TLB.singleton c <> go rest
          SText _l t rest -> TLB.fromText t <> go rest
          SLine i rest -> TLB.singleton '\n' <> (TLB.fromText (T.replicate i (T.singleton ' ')) <> go rest)
          SAnnPush _ rest -> go rest
          SAnnPop rest -> go rest

-- renderStringWithProperties :: MonadEmacs m v => SimpleDocStream (v s) -> m v
-- (v s)
