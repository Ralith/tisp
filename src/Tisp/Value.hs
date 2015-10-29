module Tisp.Value (Literal(..), Value(..)) where

import Data.Text (Text)
import qualified Data.Text as T

import Tisp.Tokenize (SourceLoc(..), SourceRange(..))

data Literal = Num Rational
             | Foreign Text
  deriving (Eq, Show)

data Value = VLiteral Literal
           | VFunc (Value -> Value)
           | VError SourceRange SourceLoc Text