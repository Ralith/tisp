module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Text (Text)

import Tisp.Parse
import Tisp.Tokenize
import Tisp.AST
import Tisp.Expr
import Tisp.Type

parseExpr :: Text -> Typed
parseExpr src =
  case parse (tokenize src) of
    Just (tree, _, []) -> infer . fromAST . fromTree $ tree
    _ -> undefined

tests :: [Test.Framework.Test]
tests = hUnitTestToTests $ TestList
  [ "type rational" ~: rational ~=? exprTy (parseExpr "42")
  , "type monomorphic identity lambda" ~: (fn rational rational) ~=? exprTy (parseExpr "(lambda ((x Rational)) x)")
  , "type wildcard case" ~: rational ~=? exprTy (parseExpr "(case 42 (x x))")
  , "type matching lambda" ~: (fn rational rational) ~=? exprTy (parseExpr "(lambda ((x Rational)) (case x (0 1) (1 42) (x 7)))")
  ]

main :: IO ()
main = defaultMain tests
