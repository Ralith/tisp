module Tisp.Value (Literal(..), Var(..), Neutral(..), Value(..), ValueVal(..), Name(..), nameBase) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Data.String (IsString, fromString)
import Data.Ratio

import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty, (<>), (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import Tisp.Tokenize (SourceLoc(..), Symbol)

data Name = NUser Symbol | NMachine Symbol Word64
  deriving (Eq, Ord, Show)

nameBase :: Name -> Symbol
nameBase (NUser s) = s
nameBase (NMachine s _) = s

instance IsString Name where
  fromString = NUser . T.pack

instance Pretty Name where
  pretty (NUser s) = PP.text (T.unpack s)
  pretty (NMachine s n) = PP.text (T.unpack s) <> PP.char '#' <> PP.integer (fromIntegral n)

data Literal = LitNum Rational
             | LitText Text
             | LitForeign Text
             | LitUniverse Integer
  deriving (Eq, Show)

instance Pretty Literal where
  pretty (LitNum r) = if denominator r == 1
                        then PP.integer (numerator r)
                        else PP.parens $ PP.text "rational" <+> PP.integer (numerator r) <+> PP.integer (denominator r)
  pretty (LitText t) = PP.text (show t)
  pretty (LitForeign f) = PP.parens $ PP.text "foreign" <+> PP.text (T.unpack f)
  pretty (LitUniverse n) = PP.parens $ PP.text "Type" <+> PP.integer n

data Var = Local Int | Global Symbol
  deriving (Show, Ord, Eq)

data Neutral a = NVar Var | NApp (Neutral a) (Value a)

data Value a = Value a (ValueVal a)

data ValueVal a = VLiteral Literal
                | VNeutral (Neutral a)
                | VData Symbol [Value a]
                | VLambda Symbol (Value a) (Value a -> Value a)
                | VPi Symbol (Value a) (Value a -> Value a)
                | VError SourceLoc Text
