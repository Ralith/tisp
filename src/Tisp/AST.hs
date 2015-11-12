module Tisp.AST (AST(..), ASTVal(..), astVal, Equiv, equiv, Pattern(..), Definition(..), Record, fromTree, buildRecord, arity) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Either (partitionEithers)

import Text.PrettyPrint.ANSI.Leijen (Doc, Pretty, pretty, (<>), (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Control.Lens
import Data.Ratio

import Tisp.Parse (Tree(..), TreeVal(..))
import Tisp.Tokenize (Symbol, Atom(..), SourceRange(..), SourceLoc(..), start)
import Tisp.Value (Literal(..))

data AST = AST SourceRange ASTVal
  deriving (Show)

data Pattern = PLit Literal | PData Symbol [Symbol] | PAny Symbol
  deriving (Eq, Show)

class Equiv a where
  -- Equal without regard to naming
  equiv :: a -> a -> Bool

instance Equiv Pattern where
  equiv (PLit x) (PLit y) = x == y
  equiv (PData x _) (PData y _) = x == y
  equiv (PAny _) (PAny _) = True
  equiv _ _ = False

arity :: Pattern -> Int
arity (PLit _) = 0
arity (PData _ xs) = length xs
arity (PAny _) = 1

data ASTVal = ASTError SourceLoc Text
            | Lambda [(Symbol, AST)] AST
            | Pi [(Symbol, AST)] AST
            | App AST [AST]
            | Literal Literal
            | Var Symbol
            | Case AST [(Pattern, AST)]
  deriving (Show)

astVal :: AST -> ASTVal
astVal (AST _ v) = v

data Definition = Definition Symbol AST AST

type Record = Map Symbol Definition

instance Pretty AST where
  pretty (AST _ v) = pretty v

parens :: Doc -> Doc
parens = PP.parens . PP.align

instance Pretty ASTVal where
  pretty (ASTError l t) = PP.angles $ pretty l <> PP.space <> PP.text (T.unpack t)
  pretty (Lambda args x) =
    parens $ PP.text "lambda"
    <+> (parens . PP.fillSep $ map (\(n, t) -> parens $ (PP.text . T.unpack $ n) <+> pretty t) args)
    <+> pretty x
  pretty (Pi args x) =
    parens $ PP.text "Pi"
    <+> (parens . PP.fillSep $ map (\(n, t) -> parens $ (PP.text . T.unpack $ n) <+> pretty t) args)
    <+> pretty x
  pretty (App f xs) = parens $ PP.fillSep (pretty f : map pretty xs)
  pretty (Literal l) = pretty l
  pretty (Var v) = PP.text . T.unpack $ v
  pretty (Case x cs) = parens $ PP.text "case" <+> pretty x <+>
                       (PP.fillSep (map (\(pat, expr) -> (case pat of
                                                           PData sym vars ->
                                                             parens (PP.text (T.unpack sym)
                                                                     <+> PP.fillSep (map (PP.text . T.unpack) vars))
                                                           PLit l -> pretty l
                                                           PAny var -> PP.text (T.unpack var)
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
    helper (Leaf (Symbol x)) = Var x
    helper (Leaf (Number x)) = Literal (LitNum x)
    helper (Leaf (AText x)) = Literal (LitText x)
    helper (Branch [Tree _ (Leaf (Symbol "Type")), Tree _ (Leaf (Number x))])
      | denominator x == 1 && x >= 0 = Literal (LitUniverse (numerator x))
      | otherwise = ASTError treeStart "illegal type level (must be a natural number)"
    helper (Branch (Tree _ (Leaf (Symbol "Type")) : _)) =
      ASTError treeStart "malformed type (should be (Type <natural>))"
    helper (Branch (Tree _ (Leaf (Symbol "Pi")) : Tree _ (Branch args) : body : [])) =
      case argList args of
        Right args' -> Pi args' (fromTree body)
        Left l -> ASTError l "invalid argument declaration: expected (symbol type)"
    helper (Branch (Tree _ (Leaf (Symbol "lambda")) : Tree _ (Branch args) : body : [])) =
      case argList args of
        Right args' -> Lambda args' (fromTree body)
        Left l -> ASTError l "invalid argument declaration: expected (symbol type)"
    helper (Branch ((Tree _ (Leaf (Symbol "case"))) : value : cases)) =
      case mapM buildCase cases of
        Right cs -> Case (fromTree value) cs
        Left (l, m) -> ASTError l m
    helper (Branch []) = ASTError treeStart "illegal application (at least one function and one argument must be supplied)"
    helper (Branch [(Tree _ x)]) = helper x
    helper (Branch (f:xs)) = App (fromTree f) (map fromTree xs)

argList :: [Tree] -> Either SourceLoc [(Symbol, AST)]
argList [] = pure []
argList (Tree _ (Branch [(Tree _ (Leaf (Symbol sym))), ty]) : rest) = ((sym, fromTree ty) :) <$> argList rest
argList (Tree l _ : _) = Left (l ^. start)

atomLit :: Atom -> Maybe Literal
atomLit (Number n) = Just $ LitNum n
atomLit (AText t) = Just $ LitText t
atomLit (Symbol _) = Nothing

buildCase :: Tree -> Either (SourceLoc, Text) (Pattern, AST)
buildCase (Tree _ (Branch [ Tree _ (Leaf (Symbol sym)), expr])) = Right (PAny sym, fromTree expr)
buildCase (Tree _ (Branch [ Tree _ (Leaf a), expr])) = Right (PLit (fromJust . atomLit $ a), fromTree expr)
buildCase (Tree _ (Branch [ Tree _ (Branch ((Tree _ (Leaf (Symbol name))) : syms)), expr])) = do
  syms' <- getSyms syms
  Right (PData name syms', fromTree expr)
buildCase (Tree (SourceRange startLoc _) _) = Left (startLoc, "illegal case (should be ((constructor vars*) expr) or (literal expr) or (var expr))")

getSyms :: [Tree] -> Either (SourceLoc, Text) [Symbol]
getSyms [] = Right []
getSyms (Tree _ (Leaf (Symbol s)) : xs) = do
  rest <- getSyms xs
  pure (s:rest)
getSyms (Tree (SourceRange startLoc _) _ : _) = Left (startLoc, "expected name")
