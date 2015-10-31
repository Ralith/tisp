module Tisp.Primitives where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Tisp.Tokenize (Symbol)
import Tisp.Value

-- types :: Map Symbol L.Type
-- types = M.fromList $
--   [ ("Bool", L.i1)
--   , ("I8" , L.i8)
--   , ("I16", L.i16)
--   , ("I32", L.i32)
--   , ("I64", L.i64)
--   , ("Float32", L.float)
--   , ("Float64", L.double)
--   ]

values :: Map Symbol Value
values = M.fromList $
  [("+", VFunc (\(VLiteral (Num x)) -> VFunc (\(VLiteral (Num y)) -> VLiteral (Num (x + y)))))]
