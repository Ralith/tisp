module Tisp.Primitives where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Tisp.Tokenize (Symbol)
import Tisp.Value

primitives :: Map Symbol Value
primitives = M.fromList $
  [("+", VFunc (\(VLiteral (Num x)) -> VFunc (\(VLiteral (Num y)) -> VLiteral (Num (x + y)))))]
