module Tisp.Expr (eval, fromAST, defaultEnv) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Word
import Control.Monad

import Tisp.Tokenize
import Tisp.Value
import Tisp.AST (AST)
import qualified Tisp.AST as A
import qualified Tisp.Primitives as Prims
import Tisp.Type

data Var = Local Int | Global Symbol
  deriving (Show, Ord, Eq)

data Expr a = Expr a (ExprVal a)

deriving instance Show a => Show (Expr a)

data ExprVal a = Var Var
               | Lambda Text Type (Expr a) -- arg name, arg type, body
               | App (Expr a) (Expr a)
               | Literal Literal
               | ExprError SourceLoc Text

deriving instance Show a => Show (ExprVal a)

data Environment = Environment { _locals :: [Value], _globalValues :: Map Symbol Value, _globalTypes :: Map Symbol TypeDefinition }
makeLenses ''Environment

defaultEnv :: Environment
defaultEnv = Environment [] Prims.values M.empty

type Untyped = Expr SourceRange

instance FromSource Untyped where
  sourceRange f (Expr r v) = fmap (\r' -> Expr r' v) (f r)

eval :: Environment -> Untyped -> Value
eval env (Expr range exprVal) =
  case exprVal of
    ExprError loc msg -> VError range loc msg
    Var (Local i) -> fromMaybe (VError range (range ^. start) "internal error: unbound local") (env ^? locals . ix i)
    Var (Global s) -> fromMaybe (VError range (range ^. start) (T.concat ["unbound global: ", s])) (env ^. globalValues . at s)
    Lambda _ _ e -> VFunc (\x -> eval (locals %~ (x:) $ env) e)
    App f x ->
      case eval env f of
        VFunc f' ->
          case eval env x of
            VError r l m -> VError r l m
            x' -> f' x'
        VError r l m -> VError r l m
        VLiteral _ -> VError range (range ^. start) "applying non-function"
    Literal l -> VLiteral l

fromAST :: AST -> Untyped
fromAST = fromAST' M.empty

-- Fixme: better data structure than Map
fromAST' :: Map Symbol Int -> AST -> Untyped
fromAST' env (A.AST range astVal) =
  case astVal of
    A.ASTError loc msg -> Expr range $ ExprError loc msg
    A.Abs [] _ -> Expr range $ ExprError (range ^. start) "nullary abstraction"
    A.Abs args body -> fromAbs env args body
    A.App _ [] -> Expr range $ ExprError (range ^. start) "nullary application"
    A.App f xs -> foldl (\f' x -> Expr range $ App f' (fromAST' env x)) (fromAST' env f) xs
    A.Literal l -> Expr range $ Literal l
    A.Ref s -> Expr range $
      case M.lookup s env of
        Nothing -> Var (Global s)
        Just i -> Var (Local i)
  where
    fromAbs :: Map Symbol Int -> [(Symbol, AST)] -> AST -> Untyped
    fromAbs env' [] body = fromAST' env' body
    fromAbs env' ((x, ty):xs) body =
      case typeFromAST ty of
        Right ty' -> Expr range (Lambda x ty' (fromAbs (M.insert x 0 (M.map succ env')) xs body))
        Left (loc, err) -> Expr range $ ExprError loc err

typeFromAST :: AST -> Either (SourceLoc, Text) Type
typeFromAST (A.AST range astVal) =
  case astVal of
    A.Ref s -> Right $ TyCon (TypeConstructor s Star)
    A.App _ [] -> Left (range ^. start, "nullary type application")
    A.App t xs -> do
      base <- typeFromAST t
      foldM (\t' x -> TyApp t' <$> typeFromAST x) base xs
    _ -> Left (range ^. start, "illegal type specifier")

data TypedLabel = TypedLabel SourceRange Type

type Typed = Expr TypedLabel

instance FromSource Typed where
  sourceRange f (Expr (TypedLabel r t) v) = fmap (\r' -> Expr (TypedLabel r' t) v) (f r)

tag :: Untyped -> Typed
tag (Expr range exprVal) =
  case exprVal of
    Var (Local i) -> undefined
