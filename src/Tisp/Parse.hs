module Tisp.Parse (Tree(..), TreeVal(..), parse) where

import Data.Text (Text)
import qualified Data.Text as T

import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty, (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import Tisp.Tokenize

data Tree = Tree SourceRange TreeVal
  deriving (Show)

data TreeVal = TreeError SourceLoc Text | Leaf Atom | Branch [Tree]
  deriving (Show)

instance Pretty Tree where
  pretty (Tree _ v) = pretty v

instance Pretty TreeVal where
  pretty (TreeError l t) = PP.angles $ pretty l <> PP.space <> PP.text (T.unpack t)
  pretty (Leaf x) = pretty x
  pretty (Branch xs) = (PP.parens . PP.align $ PP.fillSep (map pretty xs))

parse :: [Token] -> Maybe (Tree, SourceLoc, [Token])
parse [] = Nothing
parse (Token r@(SourceRange _ endLoc) (TokError loc msg) : ts) = Just (Tree r $ TreeError loc msg, endLoc, ts)
parse (Token r@(SourceRange _ endLoc) (Atom x) : ts) = Just (Tree r $ Leaf x, endLoc, ts)
parse (Token r@(SourceRange startLoc endLoc) RParen : ts) = Just (Tree r $ TreeError startLoc "unmatched right parenthesis", endLoc, ts)
parse (Token (SourceRange startLoc afterOpen) LParen : ts) = Just (Tree (SourceRange startLoc endLoc) $ Branch xs, endLoc, unparsed)
  where
    (xs, endLoc, unparsed) = parseBranch (SourceRange startLoc afterOpen) ts

parseBranch :: SourceRange -> [Token] -> ([Tree], SourceLoc, [Token])
parseBranch _ ((Token (SourceRange _ endLoc) RParen):xs) = ([], endLoc, xs)
parseBranch r@(SourceRange _ endLoc) [] = ([Tree r $ TreeError endLoc "missing right parenthesis"], endLoc, [])
parseBranch (SourceRange startLoc _) xs = (t:ts, endLoc', unparsed')
  where
    Just (t, endLoc, unparsed) = parse xs -- Safe because xs is guaranteed non-empty by prior case
    (ts, endLoc', unparsed') = parseBranch (SourceRange startLoc endLoc) unparsed
