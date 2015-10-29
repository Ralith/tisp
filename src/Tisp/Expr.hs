module Tisp.Expr (eval, fromAST, emptyEnv) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Lens
import Data.Maybe (fromMaybe)

import Tisp.Tokenize
import Tisp.Value
import Tisp.AST (AST)
import qualified Tisp.AST as A
import Tisp.Primitives

data Var = Local Int | Global Symbol
  deriving (Show, Ord, Eq)

data Expr = Expr SourceRange ExprVal
  deriving (Eq, Show)

data ExprVal = Var Var
             | Lambda Expr
             | App Expr Expr
             | Literal Literal
             | ExprError SourceLoc Text
  deriving (Eq, Show)

data Environment = Environment { _locals :: [Value], _globals :: Map Symbol Value }
makeLenses ''Environment

emptyEnv :: Environment
emptyEnv = Environment [] primitives

eval :: Environment -> Expr -> Value
eval env (Expr range exprVal) =
  case exprVal of
    ExprError loc msg -> VError range loc msg
    Var (Local i) -> fromMaybe (VError range (range ^. start) "internal error: unbound local") (env ^? locals . ix i)
    Var (Global s) -> fromMaybe (VError range (range ^. start) (T.concat ["unbound global: ", s])) (env ^. globals . at s)
    Lambda e -> VFunc (\x -> eval (locals %~ (x:) $ env) e)
    App f x ->
      case eval env f of
        VFunc f' ->
          case eval env x of
            VError r l m -> VError r l m
            x' -> f' x'
        VError r l m -> VError r l m
        VLiteral _ -> VError range (range ^. start) "applying non-function"
    Literal l -> VLiteral l

fromAST :: AST -> Expr
fromAST = fromAST' M.empty

-- Fixme: better data structure than Map
fromAST' :: Map Symbol Int -> AST -> Expr
fromAST' env (A.AST range astVal) =
  case astVal of
    A.ASTError loc msg -> Expr range $ ExprError loc msg
    A.Abs [] _ -> Expr range $ ExprError (range ^. start) "nullary abstraction"
    A.Abs args body -> fromAbs env args body
    A.App _ [] -> Expr range $ ExprError (range ^. start) "nullary application"
    A.App f xs@(_:_) -> foldr (\x f -> Expr range $ App f (fromAST' env x)) (fromAST' env f) xs
    A.Literal l -> Expr range $ Literal l
    A.Ref s -> Expr range $
      case M.lookup s env of
        Nothing -> Var (Global s)
        Just index -> Var (Local index)
  where
    fromAbs :: Map Symbol Int -> [(Symbol, AST)] -> AST -> Expr
    fromAbs env [] body = fromAST' env body
    fromAbs env ((x, _):xs) body = Expr range (Lambda (fromAbs (M.insert x 0 (M.map succ env)) xs body))
