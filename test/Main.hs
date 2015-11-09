module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Text (Text)
import Control.Lens

import Tisp.Parse
import Tisp.Tokenize
import Tisp.AST (fromTree)
import Tisp.Value (Literal(..))
import Tisp.Expr

parseExpr :: Text -> Expr SourceRange
parseExpr src =
  case parse (tokenize src) of
    Just (tree, _, []) -> fromAST . fromTree $ tree
    _ -> error "couldn't parse"

forget :: Expr a -> Expr ()
forget (Expr _ val) = Expr () $
  case val of
    Lambda n t x -> Lambda n (forget t) (forget x)
    Pi n t x -> Pi n (forget t) (forget x)
    App f x -> App (forget f) (forget x)
    Case x cs -> Case (forget x) (map (_2 %~ forget) cs)
    Var v -> Var v
    Literal l -> Literal l
    ExprError l m -> ExprError l m

parseExpr' :: Text -> ExprVal ()
parseExpr' = exprVal . forget . parseExpr

tests :: [Test.Framework.Test]
tests = hUnitTestToTests $ TestList
  [ "parse rational" ~: Literal (LitNum 42) ~=? exprVal (parseExpr "42")
  , "normalize trivial application" ~: Literal (LitNum 42) ~=? exprVal (normalize (parseExpr "((lambda ((x Rational)) x) 42)"))
  , "normalize with variable" ~:
     parseExpr' "(lambda ((y Rational)) y)" ~=?
     exprVal (normalize (parseExpr "(lambda ((y Rational)) ((lambda ((x Rational)) x) y))"))
  ]

main :: IO ()
main = defaultMain tests
