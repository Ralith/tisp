module Tisp.Expr (fromAST, toAST, Expr(..), ExprVal(..), errors, NormalizeMode(..), normalize, Subst(..), subst) where

import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Foldable
import Data.Monoid

import Tisp.Tokenize
import Tisp.Value
import Tisp.AST (AST, Equiv, equiv, Pattern(..))
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
  deriving (Eq, Show)

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

instance CanSubst (Expr a) where
  subst s e@(Expr label e') =
    case (s, e') of
      (Shift m, Var (Local k)) -> Expr label $ Var (Local (k+m))
      (Dot x _, Var (Local 0)) -> x
      (Dot _ s', Var (Local k)) -> subst s' (Expr label (Var (Local (pred k))))
      (_, Var (Global _)) -> e
      (_, Literal _) -> e
      (_, ExprError _ _) -> e
      (_, Pi n t x) -> Expr label $ Pi n (subst s t) (subst (Dot (Expr label (Var (Local 0))) (Shift 1 <> s)) x)
      (_, Lambda n t x) -> Expr label $ Lambda n (subst s t) (subst (Dot (Expr label (Var (Local 0))) (Shift 1 <> s)) x)
      (_, App f x) -> Expr label $ App (subst s f) (subst s x)
      (_, Case x cs) -> Expr label $
        Case (subst s x)
             (map (\(pat, c) ->
                     (pat
                     ,case pat of
                        PLit _ -> subst s c
                        PAny _ -> subst (Shift 1 <> s) c
                        PData _ vs -> subst (foldl' (\s' _ -> Shift 1 <> s') s vs) c))
                  cs)

shift :: CanSubst a => Int -> a -> a
shift k e = subst (Shift k) e

singleton :: a -> Subst a
singleton x = Dot x (Shift 0)

errors :: Expr a -> [(a, SourceLoc, Text)]
errors e = [(label, loc, msg) | (Expr label (ExprError loc msg)) <- universe e]

instance Equiv (Expr a) where
  -- Alpha equivalence
  -- Assumes case branches are sorted.
  equiv a b =
    case (exprVal a, exprVal b) of
      (Var x, Var y) -> x == y
      (Lambda _ t1 x1, Lambda _ t2 x2) -> equiv t1 t2 && equiv x1 x2
      (Pi _ t1 x1, Pi _ t2 x2) -> equiv t1 t2 && equiv x1 x2
      (App f1 x1, App f2 x2) -> equiv f1 f2 && equiv x1 x2
      (Case x1 cs1, Case x2 cs2) -> equiv x1 x2 && all (\((p1, c1), (p2, c2)) -> equiv p1 p2 && equiv c1 c2) (zip cs1 cs2)
      (Literal x, Literal y) -> x == y
      (ExprError _ _, ExprError _ _) -> True
      _ -> False

data NormalizeMode = Weak | Strong

data NormalizeEnv a = NormalizeEnv (Map Symbol (Expr a, Expr a)) [(Symbol, Expr a)]

envLookup :: Int -> NormalizeEnv a -> Maybe (Symbol, Expr a)
envLookup k (NormalizeEnv _ xs) = (_2 %~ shift k) <$> xs ^? ix k

envGlobals :: Lens' (NormalizeEnv a) (Map Symbol (Expr a, Expr a))
envGlobals f (NormalizeEnv gs ls) = fmap (\gs' -> NormalizeEnv gs' ls) (f gs)

envBind :: Symbol -> Expr a -> NormalizeEnv a -> NormalizeEnv a
envBind n ty (NormalizeEnv gs e) = NormalizeEnv gs ((n,ty):e)

normalize :: NormalizeMode -> Expr a -> Expr a
normalize m = normalize' m (NormalizeEnv M.empty [])

normalize' :: NormalizeMode -> NormalizeEnv a -> Expr a -> Expr a
normalize' mode env (Expr l e) =
  case e of
    Var (Local _) -> Expr l e
    Var (Global g) -> fromMaybe (Expr l e) (view _2 <$> env ^. envGlobals.at g)
    Lambda n t x -> Expr l $
      case mode of
        Weak -> e
        Strong -> let normTy = normalize' mode env t
                  in Lambda n normTy (normalize' mode (envBind n normTy env) x)
    Pi n t x -> Expr l $
      case mode of
        Weak -> e
        Strong -> let normTy = normalize' mode env t
                  in Pi n normTy (normalize' mode (envBind n normTy env) x)
    App f x ->
      let x' = case mode of { Weak -> x; Strong -> normalize' mode env x; } in
      case normalize' mode env f of
        Expr _ (Lambda _ _ body) -> normalize' mode env (subst (singleton x') body)
        f' -> Expr l $ App f' x'
    Literal _ -> Expr l e
    ExprError _ _ -> Expr l e

type Untyped = Expr SourceRange

toAST :: HasRange a => Expr a -> AST
toAST = toAST' []

fromAST :: AST -> Untyped
fromAST = fromAST' M.empty

toAST' :: HasRange a => [Symbol] -> Expr a -> AST
toAST' env (Expr label exprVal) =
  A.AST (label ^. sourceRange) $
  case exprVal of
    ExprError loc msg -> A.ASTError loc msg
    Lambda n t x -> uncurry A.Lambda (flattenLambda env n t x)
    Pi n t x -> uncurry A.Pi (flattenPi env n t x)
    App f x -> uncurry A.App (flattenApp env [] f x)
    Literal l -> A.Literal l
    Var (Local i) -> A.Var (env !! i)
    Var (Global s) -> A.Var s

flattenLambda :: HasRange a => [Symbol] -> Symbol -> Expr a -> Expr a -> ([(Symbol, AST)], AST)
flattenLambda env n t (Expr _ (Lambda n' t' x)) = flattenLambda (n:env) n' t' x & _1 %~ ((n, toAST' env t):)
flattenLambda env n t x = ([(n, toAST' env t)], toAST' (n:env) x)

flattenPi :: HasRange a => [Symbol] -> Symbol -> Expr a -> Expr a -> ([(Symbol, AST)], AST)
flattenPi env n t (Expr _ (Pi n' t' x)) = flattenPi (n:env) n' t' x & _1 %~ ((n, toAST' env t):)
flattenPi env n t x = ([(n, toAST' env t)], toAST' (n:env) x)

flattenApp :: HasRange a => [Symbol] ->[AST] -> Expr a -> Expr a -> (AST, [AST])
flattenApp env accum (Expr _ (App f x')) x = flattenApp env (toAST' env x : accum) f x'
flattenApp env accum f x = (toAST' env f, toAST' env x : accum)

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
