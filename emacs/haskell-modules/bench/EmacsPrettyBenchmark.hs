{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |

module Main where

import Criterion
import Criterion.Main (defaultMain)
import System.Random
import Control.DeepSeq
import Data.Emacs.Properties.Pretty
import Prettyprinter

instance NFData a => NFData (SimpleDocStream a)

exFlat :: Int -> Doc (Int, Int)
exFlat i | i > 0 = (nest 4 $ vsep
           [ "Hallo hallo daar!" <+> "We gaan testen met" <+> pretty i <+> "niveaus zonder nesting."
           , "Text:  " <+> annotate (i, 1) "Dit is annotated Text"
           , "Char:  " <+> annotate (i, 2) (pretty 'c')
           , "Empty: " <+> annotate (i, 3) mempty
           ]) <> line <> exFlat (i - 1)
         | otherwise = mempty

exNested :: Int -> Doc (Int, Int)
exNested i | i > 0 = nest 4 $ vsep
                     [ "Hallo hallo daar!" <+> "We gaan testen met" <+> pretty i <+> "niveaus en nesting."
                     , annotate (i, 1) (exNested (i - 1))
                     ]
           | otherwise = mempty

exDenseFlat :: Int -> Doc Int
exDenseFlat i = mconcat [annotate k (pretty ' ') | k <- [0..i]]

exDenseNested :: Int -> Doc Int
exDenseNested = go
  where go i | i > 0 = annotate i (pretty ' ' <> go (i - 1))
             | otherwise = mempty

type AlgFn ann = (String, SimpleDocStream ann -> [AnnPos ann])


algos :: [AlgFn ann]
algos = [ ("ST", runAnnPosST) ]


ns :: [Int]
ns = [10 ^ n | n <- [4..10]]

type DatFn ann = (Int -> Doc ann)

docs :: DatFn ann -> [(Int, Doc ann)]
docs datFn = [(n, datFn n) | n <- ns]

sDocs' :: NFData ann => DatFn ann -> [(String, [(Int, SimpleDocStream ann)])]
sDocs' datFn = force [(layName, [(n, lay doc) | (n, doc) <- docs datFn]) | (layName, lay) <- layouts]

layouts :: [(String, Doc a -> SimpleDocStream a)]
layouts = [ -- ("Pretty", layoutPretty defaultLayoutOptions)
          ("compact", layoutCompact)
          ]

bdat :: NFData a => String -> (Int -> Doc a) -> Benchmark
bdat name datFn = bgroup name $ do
    (algName, alg) <- algos
    pure $ bgroup algName $ do
      (layName, laySDocs) <- sDocs
      pure $ bgroup layName $ do
        (n, doc) <- laySDocs
        pure $ bench (show n) $ nf alg doc
  where sDocs = sDocs' datFn

main :: IO ()
main = defaultMain
    [-- bdat "Flat" exFlat
    -- , bdat "DenseFlat" exDenseFlat
    -- , bdat "Nested" exNested
     bdat "DenseNested" exDenseNested
    ]
