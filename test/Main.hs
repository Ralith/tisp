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

parseExpr :: Text -> Maybe Typed
parseExpr text =
  case parse (tokenize text) of
    Just (tree, _, []) ->
      case infer . fromAST . fromTree $ tree of
        Right t -> Just t
        Left _ -> Nothing
    _ -> Nothing

tests :: [Test.Framework.Test]
tests = hUnitTestToTests $ TestList
  [ "type rational" ~: Just rational ~=? exprTy <$> parseExpr "42"
  , "type monomorphic identity lambda" ~: Just (fn rational rational) ~=? exprTy <$> parseExpr "(lambda ((x Rational)) x)"
  ]

main :: IO ()
main = defaultMain tests
