module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map as M

import Tisp.Parse
import Tisp.Tokenize
import Tisp.AST
import Tisp.Expr

parseExpr :: Text -> Typed
parseExpr src =
  case parse (tokenize src) of
    Just (tree, _, []) -> infer . fromAST primTys . fromTree $ tree
    _ -> undefined

tests :: [Test.Framework.Test]
tests = hUnitTestToTests $ TestList
  [ "type rational" ~: rational ~=? exprTy (parseExpr "42")
  , "type monomorphic identity lambda" ~: (fn rational rational) ~=? exprTy (parseExpr "(lambda (x) (the (Ratio Integer) x))")
  , "type wildcard case" ~: rational ~=? exprTy (parseExpr "(case 42 (x x))")
  , "type matching lambda" ~: (fn rational rational) ~=? exprTy (parseExpr "(lambda (x) (case x (0 1) (1 42) (x 7)))")
  ]

main :: IO ()
main = defaultMain tests
