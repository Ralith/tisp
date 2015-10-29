module Tisp.AST (AST(..), ASTVal(..), Definition(..), Record, build, buildRecord) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Either (partitionEithers)
import Data.Ratio

import Text.PrettyPrint.ANSI.Leijen (Doc, Pretty, pretty, (<>), (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Tisp.Parse (Tree(..), TreeVal(..))
import Tisp.Tokenize (Symbol, Atom(..), SourceRange(..), SourceLoc(..))
import Tisp.Value (Literal(..))

data AST = AST SourceRange ASTVal
  deriving (Show)

data ASTVal = ASTError SourceLoc Text
            | Abs [(Symbol, AST)] AST
            | App AST [AST]
            | Literal Literal
            | Ref Symbol
            | Case AST [(Symbol, AST)]
  deriving (Show)

data Definition = Definition Symbol AST AST

type Record = Map Symbol Definition

instance Pretty AST where
  pretty (AST _ v) = pretty v

parens :: Doc -> Doc
parens = PP.parens . PP.align

instance Pretty ASTVal where
  pretty (ASTError l t) = PP.angles $ pretty l <> PP.space <> PP.text (T.unpack t)
  pretty (Abs args x) = parens $ PP.char 'λ' <+> PP.parens (PP.fillSep $ map (PP.text . T.unpack . fst) args) <+> pretty x
  pretty (App f xs) = parens $ PP.fillSep (pretty f : map pretty xs)
  pretty (Literal (Num n)) = if denominator n == 1 then PP.integer (numerator n) else (PP.char '#' <> (PP.angles $ PP.rational n))
  pretty (Ref v) = PP.text . T.unpack $ v
  pretty (Case x cs) = parens $ PP.text "case" <+> pretty x <+> (PP.fillSep (map (\(ctor, expr) -> PP.text (T.unpack ctor) <+> PP.text ": " <+> pretty expr) cs))

buildRecord :: [Tree] -> (Record, [(SourceRange, Text)])
buildRecord forms = (M.fromList defs, errs)
  where
    (errs, defs) = partitionEithers (map recordEntry forms)

recordEntry :: Tree -> Either (SourceRange, Text) (Symbol, Definition)
recordEntry (Tree _ (Branch [(Tree _ (Leaf (Symbol "def"))), (Tree _ (Leaf (Symbol name))), ty, value])) =
  Right (name, Definition name (build ty) (build value))
recordEntry (Tree r _) = Left (r, "illegal definition")

build :: Tree -> AST
build (Tree r@(SourceRange treeStart _) v) = AST r $ helper v
  where
    helper :: TreeVal -> ASTVal
    helper (TreeError l t) = ASTError l t
    helper (Leaf (Symbol x)) = Ref x
    helper (Leaf (Number x)) = Literal (Num x)
    helper (Branch (Tree _ (Leaf (Symbol "lambda")) : Tree _ (Branch args) : body : [])) =
      case lambdaArgs args of
        Right argSyms -> Abs argSyms (build body)
        Left (l, e) -> ASTError l e
    helper (Branch ((Tree _ (Leaf (Symbol "case"))) : value : cases)) =
      case mapM buildCase cases of
        Right cs -> Case (build value) cs
        Left (l, e) -> ASTError l e
    helper (Branch []) = ASTError treeStart "illegal application (at least one function and one argument must be supplied)"
    helper (Branch [(Tree _ x)]) = helper x
    helper (Branch (f:xs)) = App (build f) (map build xs)

lambdaArgs :: [Tree] -> Either (SourceLoc, Text) [(Symbol, AST)]
lambdaArgs (Tree _ ((Branch [Tree _ (Leaf (Symbol s)), ty])):xs) = lambdaArgs xs >>= (\x -> return ((s, build ty):x))
lambdaArgs [] = return []
lambdaArgs ((Tree (SourceRange startLoc _) _):_) = Left (startLoc, "illegal argument name (should be (symbol type))")

buildCase :: Tree -> Either (SourceLoc, Text) (Symbol, AST)
buildCase (Tree _ (Branch [Tree _ (Leaf (Symbol name)), function])) = Right (name, build function)
buildCase (Tree (SourceRange startLoc _) _) = Left (startLoc, "illegal case (should be (constructor lambda)")
