module Tisp.AST (AST(..), ASTVal(..), Pattern(..), Definition(..), Record, fromTree, buildRecord) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Either (partitionEithers)

import Text.PrettyPrint.ANSI.Leijen (Doc, Pretty, pretty, (<>), (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

import Tisp.Parse (Tree(..), TreeVal(..), symbolList)
import Tisp.Tokenize (Symbol, Atom(..), SourceRange(..), SourceLoc(..))
import Tisp.Value (Literal(..))

data AST = AST SourceRange ASTVal
  deriving (Show)

data Pattern = PLit Literal | PData Symbol | PAny
  deriving (Show)

data ASTVal = ASTError SourceLoc Text
            | Abs [Symbol] AST
            | App AST [AST]
            | Literal Literal
            | Ref Symbol
            | Case AST [(Pattern, [Symbol], AST)]
            | The Tree AST
  deriving (Show)

data Definition = Definition Symbol AST AST

type Record = Map Symbol Definition

instance Pretty AST where
  pretty (AST _ v) = pretty v

parens :: Doc -> Doc
parens = PP.parens . PP.align

instance Pretty ASTVal where
  pretty (ASTError l t) = PP.angles $ pretty l <> PP.space <> PP.text (T.unpack t)
  pretty (Abs args x) = parens $ PP.char 'λ' <+> PP.parens (PP.fillSep $ map (PP.text . T.unpack) args) <+> pretty x
  pretty (App f xs) = parens $ PP.fillSep (pretty f : map pretty xs)
  pretty (Literal l) = pretty l
  pretty (The ty expr) = parens $ PP.text "the" <+> pretty ty <+> pretty expr
  pretty (Ref v) = PP.text . T.unpack $ v
  pretty (Case x cs) = parens $ PP.text "case" <+> pretty x <+>
                       (PP.fillSep (map (\(pat, vars, expr) -> (case pat of
                                                                 PData sym -> parens (PP.text (T.unpack sym) <+> PP.fillSep (map (PP.text . T.unpack) vars))
                                                                 PLit l -> pretty l
                                                                 PAny -> PP.text (T.unpack (vars !! 0))
                                                               ) <+> pretty expr) cs))

buildRecord :: [Tree] -> (Record, [(SourceRange, Text)])
buildRecord forms = (M.fromList defs, errs)
  where
    (errs, defs) = partitionEithers (map recordEntry forms)

recordEntry :: Tree -> Either (SourceRange, Text) (Symbol, Definition)
recordEntry (Tree _ (Branch [(Tree _ (Leaf (Symbol "def"))), (Tree _ (Leaf (Symbol name))), ty, value])) =
  Right (name, Definition name (fromTree ty) (fromTree value))
recordEntry (Tree r _) = Left (r, "illegal definition")

fromTree :: Tree -> AST
fromTree (Tree r@(SourceRange treeStart _) v) = AST r $ helper v
  where
    helper :: TreeVal -> ASTVal
    helper (TreeError l t) = ASTError l t
    helper (Leaf (Symbol x)) = Ref x
    helper (Leaf (Number x)) = Literal (LitNum x)
    helper (Leaf (AText x)) = Literal (LitText x)
    helper (Branch (Tree _ (Leaf (Symbol "lambda")) : Tree _ (Branch args) : body : [])) =
      case symbolList args of
        Right argSyms -> Abs argSyms (fromTree body)
        Left l -> ASTError l "expected symbol"
    helper (Branch ((Tree _ (Leaf (Symbol "case"))) : value : cases)) =
      case mapM buildCase cases of
        Right cs -> Case (fromTree value) cs
        Left (l, m) -> ASTError l m
    helper (Branch []) = ASTError treeStart "illegal application (at least one function and one argument must be supplied)"
    helper (Branch [(Tree _ x)]) = helper x
    helper (Branch (Tree _ (Leaf (Symbol "the")) : xs)) =
      case xs of
        [ty, expr] -> The ty (fromTree expr)
        _ -> ASTError treeStart "malformed type declaration (should be (the type expr))"
    helper (Branch (f:xs)) = App (fromTree f) (map fromTree xs)

atomLit :: Atom -> Maybe Literal
atomLit (Number n) = Just $ LitNum n
atomLit (AText t) = Just $ LitText t
atomLit (Symbol _) = Nothing

buildCase :: Tree -> Either (SourceLoc, Text) (Pattern, [Symbol], AST)
buildCase (Tree _ (Branch [ Tree _ (Leaf (Symbol sym)), expr])) = Right (PAny, [sym], fromTree expr)
buildCase (Tree _ (Branch [ Tree _ (Leaf a), expr])) = Right (PLit (fromJust . atomLit $ a), [], fromTree expr)
buildCase (Tree _ (Branch [ Tree _ (Branch ((Tree _ (Leaf (Symbol name))) : syms))
                          , expr])) = do
  syms' <- getSyms syms
  Right (PData name, syms', fromTree expr)
buildCase (Tree (SourceRange startLoc _) _) = Left (startLoc, "illegal case (should be ((constructor vars*) expr) or (literal expr) or (var expr))")

getSyms :: [Tree] -> Either (SourceLoc, Text) [Symbol]
getSyms [] = Right []
getSyms (Tree _ (Leaf (Symbol s)) : xs) = do
  rest <- getSyms xs
  pure (s:rest)
getSyms (Tree (SourceRange startLoc _) _ : _) = Left (startLoc, "expected name")
