module Tisp.Primitives where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Tisp.Tokenize (Symbol)
import Tisp.Value

values :: Map Symbol Value
values = M.fromList $
  [("+", VFunc (\(VLiteral (LitNum x)) -> VFunc (\(VLiteral (LitNum y)) -> VLiteral (LitNum (x + y)))))]
