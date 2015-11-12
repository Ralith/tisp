module Tisp.Expr (fromAST, toAST, Expr(..), ExprVal(..), emptyEnv, errors, normalize, Subst(..), subst, eval, infer) where

import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Foldable
import Data.Monoid
import Control.Monad.Reader
import Control.Monad.Except

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
               | Data Symbol [Expr a]
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
  plate f (Expr l (Data s xs)) = Expr l <$> (Data s <$> traverse f xs)
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
                        PData _ vs -> subst (foldl' (\s' _ -> Dot (Expr label (Var (Local 0))) (Shift 1 <> s')) s vs) c))
                  cs)
      (_, Data c xs) -> Expr label $ Data c (map (subst s) xs)

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

data Constructor a = Constructor { _ctorResult :: Expr a, _ctorArgs :: [(Symbol, Expr a)] }
makeLenses ''Constructor

data Env a = Env { _envGlobals :: Map Symbol (Expr a, Expr a)
                 , _envCtors :: Map Symbol (Constructor a)
                 , _envLocals :: [(Symbol, Expr a, Maybe (Expr a))]
                 }
makeLenses ''Env

emptyEnv :: Env a
emptyEnv = Env M.empty M.empty []

lookupTy :: Var -> Reader (Env a) (Expr a)
lookupTy (Local i) = do
  ls <- view envLocals
  pure $ shift (succ i) (view _2 (ls !! i))
lookupTy (Global g) = do
  gs <- view envGlobals
  case M.lookup g gs  of
    Just x -> pure . fst $ x
    Nothing -> error "undefined global"

lookupVal :: Var -> Reader (Env a) (Maybe (Expr a))
lookupVal (Local i) = do
  ls <- view envLocals
  pure $ shift (succ i) <$> (view _3 (ls !! i))
lookupVal (Global g) = do
  gs <- view envGlobals
  case M.lookup g gs  of
    Just x -> pure . Just . snd $ x
    Nothing -> pure $ Nothing

normalize :: Env a -> Expr a -> Expr a
normalize e x = runReader (normalize' x) e

patternMatch :: Expr a -> Pattern -> Bool
patternMatch _ (PAny _) = True
patternMatch (Expr _ (Data s' _)) (PData s _) = s == s'
patternMatch (Expr _ (Literal l')) (PLit l) = l == l'
patternMatch _ _ = False

normalize' :: Expr a -> Reader (Env a) (Expr a)
normalize' (Expr l e) =
  case e of
    Var v -> fromMaybe (Expr l e) <$> lookupVal v
    Lambda n t x -> do
      t' <- normalize' t
      x' <- local (envLocals %~ ((n, t', Nothing):)) $ normalize' x
      pure . Expr l $ Lambda n t' x'
    Pi n t x -> do
      t' <- normalize' t
      x' <- local (envLocals %~ ((n, t', Nothing):)) $ normalize' x
      pure . Expr l $ Pi n t' x'
    App f x -> do
      x' <- normalize' x
      f' <- normalize' f
      case f' of
        Expr _ (Lambda _ _ body) -> normalize' (subst (singleton x') body)
        _ -> pure . Expr l $ App f' x'
    Case x cs -> do
      x' <- normalize' x
      case find (patternMatch x' . fst) cs of
        Just (_, c) ->
          case exprVal x' of
            Literal _ -> normalize' (subst (singleton x') c)
            Data _ xs -> normalize' (subst (foldr Dot (Shift 0) xs) c)
            _ -> error "normalize': impossible"
        Nothing -> pure $ Expr l e
    Data c xs -> Expr l . Data c <$> (mapM normalize' xs)
    Literal _ -> pure $ Expr l e
    ExprError _ _ -> pure $ Expr l e

eval :: HasRange a => Expr a -> Value a
eval = eval' []

eval' :: HasRange a => [Value a] -> Expr a -> Value a
eval' env (Expr label exprVal) =
  case exprVal of
    ExprError l m -> Value label $ VError l m
    Var v@(Local i) -> fromMaybe (Value label (VNeutral (NVar v))) (env ^? ix i)
    Var v@(Global _) -> Value label $ VNeutral (NVar v)
    Literal l -> Value label $ VLiteral l
    Lambda n t v -> Value label $ VLambda n (eval' env t) (\x -> eval' (x:env) v)
    Pi n t v -> Value label $ VPi n (eval' env t) (\x -> eval' (x:env) v)
    Data s xs -> Value label $ VData s (map (eval' env) xs)
    App f x ->
      let x' = eval' env x in
      case eval' env f of
        Value _ (VLambda _ _ f') -> f' x'
        Value _ (VNeutral n) -> Value label $ VNeutral (NApp n x')
        _ -> Value label $ VError (label ^. sourceRange.start) "applied non-function"
    Case x cs ->
      case eval' env x of
        v@(Value _ (VData _ _)) -> findClause label env cs v
        v@(Value _ (VLiteral _)) -> findClause label env cs v
        _ -> Value label $ VError (exprLabel x ^. sourceRange.start) "case on non-data"
  where
    findClause :: HasRange a => a -> [Value a] -> [(Pattern, Expr a)] -> Value a -> Value a
    findClause l _ [] _ = Value l $ VError (l ^. sourceRange.start) "no matching case for value"
    findClause _ e ((PAny _, v):_) x = eval' (x:e) v
    findClause label' e ((PLit l, v):vs) x@(Value _ (VLiteral l')) =
      if l == l'
         then eval' e v
         else findClause label' e vs x
    findClause label' e ((PData n syms, v):vs) x@(Value _ (VData n' vals)) =
      if n == n'
         then if length syms /= length vals
                 then error "eval': impossible"
                 else eval' (reverse vals ++ e) v
         else findClause label' e vs x
    findClause l _ _ _ = Value l $ VError (l ^. sourceRange.start) "pattern type mismatch"

-- BROKEN: Every variable ends up being 0
-- reify :: Value a -> Expr a
-- reify (Value label (VLiteral l)) = Expr label $ Literal l
-- reify (Value label (VNeutral n)) = helper label n
--   where
--     helper :: a -> Neutral a -> Expr a
--     helper l (NVar v) = Expr l $ Var v
--     helper l (NApp f v) = Expr l $ App (helper l f) (reify v)
-- reify (Value l (VData s vs)) = Expr l $ Data s (map reify vs)
-- reify (Value l (VLambda n t f)) = Expr l $ Lambda n (reify t) (reify (f (Value l (VNeutral (NVar (Local 0))))))
-- reify (Value l (VPi n t f)) = Expr l $ Pi n (reify t) (reify (f (Value l (VNeutral (NVar (Local 0))))))
-- reify (Value l (VError loc m)) = Expr l $ ExprError loc m

type Untyped = Expr SourceRange

toAST :: HasRange a => Expr a -> AST
toAST = toAST' []

fromAST :: AST -> Untyped
fromAST = fromAST' M.empty

toAST' :: HasRange a => [Symbol] -> Expr a -> AST
toAST' env (Expr label exprVal) =
  let range = label ^. sourceRange in
  A.AST range $
  case exprVal of
    ExprError loc msg -> A.ASTError loc msg
    Lambda n t x -> uncurry A.Lambda (flattenLambda env n t x)
    Pi n t x -> uncurry A.Pi (flattenPi env n t x)
    App f x -> uncurry A.App (flattenApp env [] f x)
    Literal l -> A.Literal l
    Var (Local i) -> A.Var (env !! i)
    Var (Global s) -> A.Var s
    Case x cs -> A.Case (toAST' env x) (cs & traverse._2 %~ toAST' env)
    Data s args -> A.App (A.AST range (A.Var s)) (map (toAST' env) args)

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

literalTy :: Literal -> ExprVal a
literalTy (LitNum _) = Var (Global "Rational")
literalTy (LitUniverse i) = Literal (LitUniverse (succ i))
literalTy (LitText _) = Var (Global "Text")
literalTy (LitForeign _) = Var (Global "Foreign")

infer :: HasRange a => Env a -> Expr a -> Expr a
infer e x = runReader (infer' x) e

infer' :: HasRange a => Expr a -> Reader (Env a) (Expr a)
infer' (Expr label exprVal) =
  case exprVal of
    Var v -> lookupTy v
    Literal x -> pure $ Expr label (literalTy x)
    Pi n t x -> do
      u <- runExceptT $ do
        u1 <- inferUniverse t
        u2 <- local (envLocals %~ ((n,t,Nothing):)) $ inferUniverse x
        pure (max u1 u2)
      case u of
        Left (l, m) -> pure $ Expr label $ ExprError l m
        Right u' -> pure $ Expr label . Literal $ LitUniverse u'
    Lambda n t x -> do
      u <- runExceptT $ inferUniverse t
      case u of
        Left (l, m) -> pure . Expr label $ ExprError l m
        Right _ -> do
          xty <- local (envLocals %~ ((n,t,Nothing):)) $ infer' x
          pure . Expr label $ Pi n t xty
    App f x -> do
      fty <- inferPi f
      xty <- normalize' =<< infer' x
      case fty of
        Left (l, m) -> pure . Expr label $ ExprError l m
        Right (_, argty, retty) -> do
          argty' <- normalize' argty
          pure $ if equiv xty argty'
                    then subst (singleton x) retty
                    else Expr label $ ExprError (label ^. sourceRange.start) "argument type mismatch"
    Case _ [] -> pure . Expr label $ Var (Global "Void")
    Case x cs -> undefined -- TODO: Bind variables to correct types in case bodies
      -- xty <- normalize' =<< infer' x
      -- ptys <- map (fromMaybe xty) <$> mapM (inferPattern . fst) cs
      -- if any (not . equiv xty) ptys
      --    then pure . Expr label $ ExprError (label ^. sourceRange.start) "pattern/interrogand type mismatch"
      --    else if any (not . equiv (head ctys)) (tail ctys)
      --            then pure . Expr label $ ExprError (label ^. sourceRange.start) "result type mismatch"
      --            else head ctys
    Data s xs -> do
      cts <- view envCtors
      case M.lookup s cts of
        Nothing -> pure . Expr label $ ExprError (label ^. sourceRange.start) "undefined data constructor"
        Just ct -> pure $ subst (foldr Dot (Shift 0) xs) (ct ^. ctorResult)
    ExprError _ _ -> pure . Expr label $ Var (Global "Void")

inferUniverse :: HasRange a => Expr a -> ExceptT (SourceLoc, Text) (Reader (Env a)) Integer
inferUniverse ty = do
  ty' <- lift $ normalize' =<< infer' ty
  case ty' of
    Expr _ (Literal (LitUniverse i)) -> pure $ i
    Expr l _ -> throwError (l ^. sourceRange.start, "expected a type")

inferPi :: HasRange a => Expr a -> Reader (Env a) (Either (SourceLoc, Text) (Symbol, Expr a, Expr a))
inferPi f = do
  ty <- normalize' =<< infer' f
  case ty of
    Expr _ (Pi n t x) -> pure $ Right (n, t, x)
    Expr l _ -> pure $ Left (l ^. sourceRange.start, "expected a function")

data PatternTy a = PTData Symbol | PTLit (ExprVal a) | PTAny
  deriving (Eq)

-- instance Equiv (PatternTy a) where
--   equiv PTAny _ = True
--   equiv _ PTAny = True
--   equiv (PTLit x) (PTLit y) = x == y
--   equiv (PTData x) (PTData y) = x == y

-- inferPattern :: Pattern -> PatternTy a
-- inferPattern (PAny _) = PTAny
-- inferPattern (PLit l) = PTLit (literalTy l)
-- inferPattern (PData s _) = PTData s
