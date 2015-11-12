module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Text (Text)
import qualified Data.Text as T

import Text.PrettyPrint.ANSI.Leijen (displayS, renderPretty, pretty, (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Tisp.Parse
import Tisp.Tokenize
import Tisp.AST (equiv, fromTree)
import Tisp.Value (Literal(..))
import Tisp.Expr

parseExpr :: Text -> Expr SourceRange
parseExpr src =
  case parse (tokenize src) of
    Just (tree, _, []) -> fromAST . fromTree $ tree
    _ -> error "couldn't parse"

reduced :: Text -> Text -> Assertion
reduced n x =
  let x' = normalize emptyEnv (parseExpr x)
  in assertBool ("expected: " ++ T.unpack n ++ "\n"
                  ++ displayS (renderPretty 0.6 80 (PP.text "but got: " <+> pretty (toAST x'))) "")
                (parseExpr n `equiv` x')

tests :: [Test.Framework.Test]
tests = hUnitTestToTests $ TestList
  [ "parse rational" ~: Literal (LitNum 42) ~=? exprVal (parseExpr "42")
  , "normalize trivial application" ~: reduced "42" "((lambda ((x Rational)) x) 42)"
  , "normalize with variable" ~:
     reduced "(lambda ((y Rational)) y)"
             "(lambda ((y Rational)) ((lambda ((x Rational)) x) y))"
  , "normalize binary" ~: reduced "42" "((lambda ((x Rational) (y Rational)) y) 12 42) "
  , "normalize const" ~:
     reduced "(lambda ((z Rational) (y Rational)) z)"
             "(lambda ((z Rational) (y Rational)) \
             \ ((lambda ((x (Function Rational Rational))) \
             \   (x y))\
             \ (lambda ((x Rational)) z)))"
  ]

main :: IO ()
main = defaultMain tests
