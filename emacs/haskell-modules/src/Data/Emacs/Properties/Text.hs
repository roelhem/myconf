{-# LANGUAGE DuplicateRecordFields #-}
-- |

module Data.Emacs.Properties.Text where
import Data.Emacs.Module.SymbolName (SymbolName)
import Data.Text (Text)
import Data.Void (Void)
import Data.List.NonEmpty (NonEmpty)

type FontFamily = Text
type FontFoundry = Text
type FaceAttrV = Maybe

data FontWidth = FWthUltraCondensed
               | FWthExtraCondensed
               | FWthCondensed
               | FWthSemiCondensed
               | FWthNormal
               | FWthRegular
               | FWthMedium
               | FWthSemiExpanded
               | FWthExpanded
               | FWthExtraExpanded
               | FWthUltraExpanded
               deriving (Show, Eq, Ord, Enum, Bounded)

data FontHeight f = FHAbsolute !Int
                  | FHRelative !Double
                  | FHFunction !f

data FontWeight = FWghtUltraBold
                | FWghtExtraBold
                | FWghtBold
                | FWghtSemiBold
                | FWghtNormalWeight
                | FWghtSemiLight
                | FWghtLight
                | FWghtExtraLight
                | FWghtUrltraLight
                deriving (Show, Eq, Ord, Enum, Bounded)

data FontSlant = FSlItalic
               | FSlOblique
               | FSlNormal
               | FSlReverseOblique
               | FSlReverseItalic
               deriving (Show, Eq, Ord, Enum, Bounded)


type EmacsColor = Text

data FaceLineStyle = FLnLine
                   | FLnWave
                   deriving (Show, Eq, Ord, Enum, Bounded)

data FaceBoxStyle = FBoxReleasedBtn
                  | FBoxFlatBtn
                  | FBoxPressedBtn
                  deriving (Show, Eq, Ord, Enum, Bounded)

data FaceUnderlinePosition = FUlnBaseline
                           | FUlnDescent !Int
                           deriving (Show, Eq, Ord)

data FaceBoxOptions = FaceBoxOptions
  { color     :: !(Maybe EmacsColor)
  , lineWidth :: !(Int, Int)
  , style     :: !FaceBoxStyle
  } deriving (Show, Eq)

data FaceUnderlineOptions = FaceUnderlineOptions
  { color    :: !(Maybe EmacsColor)
  , style    :: !FaceLineStyle
  , position :: !FaceUnderlinePosition
  } deriving (Show, Eq)

data FaceLine o = FLnNone
                | FLnFgColor
                | FLnColor !EmacsColor
                | FLnOpts !o
                deriving (Show, Eq)


data FaceAttributes f = FaceAttributes
  { family :: f FontFamily
  , foundry :: f FontFoundry
  , width :: f FontWidth
  , height :: f (FontHeight Void)
  , weight :: f FontWeight
  , slant :: f FontSlant
  , foreground :: f EmacsColor
  , distantForeground :: f EmacsColor
  , background :: f EmacsColor
  , underline :: f (FaceLine FaceUnderlineOptions)
  , overline :: f (FaceLine Void)
  , strikeThrough :: f (FaceLine Void)
  , box :: f (FaceLine FaceBoxOptions)
  , inverseVideo :: f Bool
  , stipple :: f Text
  , inherit :: f (NonEmpty SymbolName)
  , extend :: f Bool
  }

data FaceRef = FName !(Either Text SymbolName)
             | FAnnonymous !(FaceAttributes Maybe)

data TextProperties f = TypeProperties
  { category :: f SymbolName
  , face :: f [FaceRef]
  , fontLockFace :: f [FaceRef]
  , mouseFace :: f [FaceRef]
  , cursorFace :: f [FaceRef]
  }
