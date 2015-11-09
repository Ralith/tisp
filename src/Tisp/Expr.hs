module Tisp.Expr (fromAST, Expr(..), ExprVal(..), errors, eval, reify, normalize) where

import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Foldable
import Data.Monoid

import Tisp.Tokenize
import Tisp.Value
import Tisp.AST (AST, Pattern(..))
import qualified Tisp.AST as A

data Expr a = Expr { exprLabel :: a, exprVal :: (ExprVal a) }

deriving instance Eq a => Eq (Expr a)
deriving instance Show a => Show (Expr a)

data ExprVal a = Var Var
               | Lambda Symbol (Expr a) (Expr a)
               | Pi Symbol (Expr a) (Expr a)
               | App (Expr a) (Expr a)
               | Case (Expr a) [(Pattern, Expr a)]
               | Literal Literal
               | ExprError SourceLoc Text

deriving instance Eq a => Eq (ExprVal a)
deriving instance Show a => Show (ExprVal a)

instance Plated (Expr a) where
  plate _ e@(Expr _ (Var _)) = pure e
  plate f (Expr l (Lambda n t e)) = Expr l <$> (Lambda n <$> f t <*> f e)
  plate f (Expr l (Pi n t e)) = Expr l <$> (Pi n <$> f t <*> f e)
  plate f (Expr l (App g x)) = Expr l <$> (App <$> f g <*> f x)
  plate f (Expr l (Case e cs)) = Expr l <$> (Case <$> f e <*> traverseOf (traverse._2) f cs)
  plate _ e@(Expr _ (Literal _)) = pure e
  plate _ e@(Expr _ (ExprError _ _)) = pure e

data Subst a = Shift Int | Dot a (Subst a)

instance Functor Subst where
  fmap _ (Shift s) = Shift s
  fmap f (Dot x s) = Dot (f x) (fmap f s)

class CanSubst a where
  subst :: Subst a -> a -> a

instance CanSubst a => Monoid (Subst a) where
  mempty = Shift 0

  mappend s (Shift 0) = s
  mappend (Dot _ s) (Shift m) = s <> (Shift (pred m))
  mappend (Shift n) (Shift m) = Shift (n + m)
  mappend s (Dot e t) = Dot (subst s e) (s <> t)

shift :: CanSubst a => Subst a -> Subst a
shift = mappend (Shift 1)

instance CanSubst (Expr a) where
  subst s e@(Expr label e') =
    case (s, e') of
      (Shift m, Var (Local k)) -> Expr label $ Var (Local (k+m))
      (Dot x _, Var (Local 0)) -> x
      (Dot _ s', Var (Local k)) -> subst s' (Expr label (Var (Local (pred k))))
      (_, Var (Global _)) -> e
      (_, Literal _) -> e
      (_, ExprError _ _) -> e
      (_, Pi n t x) -> Expr label $ Pi n (subst s t) (subst (shift s) x)
      (_, Lambda n t x) -> Expr label $ Lambda n (subst s t) (subst (shift s) x)
      (_, App f x) -> Expr label $ App (subst s f) (subst s x)
      (_, Case x cs) -> Expr label $
        Case (subst s x)
             (map (\(pat, c) ->
                     (pat
                     ,case pat of
                        PLit _ -> subst s c
                        PAny _ -> subst (shift s) c
                        PData _ vs -> subst (foldl' (\s' _ -> shift s') s vs) c))
                  cs)

errors :: Expr a -> [(a, SourceLoc, Text)]
errors e = [(label, loc, msg) | (Expr label (ExprError loc msg)) <- universe e]

eval :: HasRange a => Expr a -> Value
eval = eval' []

eval' :: HasRange a => [Value] -> Expr a -> Value
eval' env (Expr label exprVal) =
  case exprVal of
    ExprError l m -> VError (label ^. sourceRange) l m
    Var v@(Local i) -> fromMaybe (VNeutral (NVar v)) (env ^? ix i)
    Var v@(Global _) -> VNeutral (NVar v)
    Literal l -> VLiteral l
    Lambda n t v -> VLambda n (eval' env t) (\x -> eval' (x:env) v)
    Pi n t v -> VPi n (eval' env t) (\x -> eval' (x:env) v)
    App f x ->
      let x' = eval' env x in
      case eval' env f of
        VLambda _ _ f' -> f' x'
        VNeutral n -> VNeutral (NApp n x')
        _ -> VError (label ^. sourceRange) (label ^. sourceRange.start) "applied non-function"
    Case x cs ->
      case eval' env x of
        v@(VData _ _) -> findClause cs v
        v@(VLiteral _) -> findClause cs v
        _ -> VError (exprLabel x ^. sourceRange) (exprLabel x ^. sourceRange.start) "case on non-data"
  where
    findClause :: HasRange a => [(Pattern, Expr a)] -> Value -> Value
    findClause [] _ = VError (label ^. sourceRange) (label ^. sourceRange.start) "no matching case for value"
    findClause ((PAny _, v):_) x = eval' (x:env) v
    findClause ((PLit l, v):vs) x@(VLiteral l') =
      if l == l'
         then eval' env v
         else findClause vs x
    findClause ((PData n syms, v):vs) x@(VData n' vals) =
      if n == n'
         then if length syms /= length vals
                 then error "eval': impossible"
                 else eval' (vals ++ env) v
         else findClause vs x
    findClause _ _ = VError (label ^. sourceRange) (label ^. sourceRange.start) "pattern type mismatch"

reify :: Value -> Expr ()
reify (VLiteral l) = Expr () $ Literal l
reify (VNeutral n) = helper n
  where
    helper :: Neutral -> Expr ()
    helper (NVar v) = Expr () $ Var v
    helper (NApp f v) = Expr () $ App (helper f) (reify v)
reify (VData s vs) = foldl' (\f x -> Expr () $ App f (reify x))
                            (Expr () $ (Var (Global s)))
                            vs
reify (VLambda n t f) = Expr () $ Lambda n (reify t) (reify (f (VNeutral (NVar (Local 0)))))
reify (VPi n t f) = Expr () $ Pi n (reify t) (reify (f (VNeutral (NVar (Local 0)))))
reify (VError _ l m) = Expr () $ ExprError l m

normalize :: HasRange a => Expr a -> Expr ()
normalize = reify . eval

type Untyped = Expr SourceRange

fromAST :: AST -> Untyped
fromAST = fromAST' M.empty

-- Could this use a better data structure than Map?
fromAST' :: Map Symbol Int -> AST -> Untyped
fromAST' locals (A.AST range astVal) =
  case astVal of
    A.ASTError loc msg -> Expr range $ ExprError loc msg
    A.Lambda [] _ -> Expr range $ ExprError (range ^. start) "nullary lambda"
    A.Lambda args body -> fromAbs Lambda locals args body
    A.Pi [] _ -> Expr range $ ExprError (range ^. start) "nullary Pi"
    A.Pi args body -> fromAbs Pi locals args body
    A.App _ [] -> Expr range $ ExprError (range ^. start) "nullary application"
    A.App f xs -> foldl (\f' x -> Expr range $ App f' (fromAST' locals x)) (fromAST' locals f) xs
    A.Literal l -> Expr range $ Literal l
    A.Var s -> Expr range $
      case M.lookup s locals of
        Nothing -> Var (Global s)
        Just i -> Var (Local i)
    A.Case expr cases -> Expr range $ Case (fromAST' locals expr) (map caseFromAST cases)
  where
    fromAbs :: (Symbol -> Untyped -> Untyped -> ExprVal SourceRange) -> Map Symbol Int -> [(Symbol, AST)] -> AST -> Untyped
    fromAbs _    locals' []           body = fromAST' locals' body
    fromAbs ctor locals' ((x, ty):xs) body = Expr range (ctor x (fromAST' locals' ty) (fromAbs ctor (bind x locals') xs body))

    caseFromAST :: (Pattern, AST) -> (Pattern, Untyped)
    caseFromAST (p@(PLit _), expr) = (p, fromAST' locals expr)
    caseFromAST (p@(PAny sym), expr) = (p, fromAST' (bind sym locals) expr)
    caseFromAST (p@(PData _ vars), expr) = (p, fromAST' (foldr bind locals vars) expr)

    bind :: Symbol -> Map Symbol Int -> Map Symbol Int
    bind s ls = M.insert s 0 (M.map succ ls)
