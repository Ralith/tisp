module Tisp.Value (Literal(..), Value(..), Name(..)) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Data.String (IsString, fromString)

import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty, (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import Tisp.Tokenize (SourceLoc(..), SourceRange(..), Symbol)

data Name = NUser Symbol | NMachine Symbol Word64
  deriving (Eq, Ord, Show)

instance IsString Name where
  fromString = NUser . T.pack

instance Pretty Name where
  pretty (NUser s) = PP.text (T.unpack s)
  pretty (NMachine s n) = PP.text (T.unpack s) <> PP.char '#' <> PP.integer (fromIntegral n)

data Literal = Num Rational
             | Foreign Text
  deriving (Eq, Show)

instance Pretty Literal where
  pretty (Num r) = PP.rational r

data Value = VLiteral Literal
           | VFunc (Value -> Value)
           | VError SourceRange SourceLoc Text
