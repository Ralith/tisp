module Tisp.Expr (eval, fromAST, defaultEnv, infer, exprTy, Expr(..), ExprVal(..), Typed, TIError(..), errors) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Word
import Data.Either (partitionEithers)
import Data.Foldable
import Control.Monad

import Tisp.Tokenize
import Tisp.Value
import Tisp.AST (AST, Pattern(..))
import qualified Tisp.AST as A
import qualified Tisp.Primitives as Prims
import Tisp.Type

import Control.Monad.State
import Control.Monad.Except

data Var = Local Int | Global Symbol
  deriving (Show, Ord, Eq)

data Expr a = Expr a (ExprVal a)

deriving instance Show a => Show (Expr a)

data ExprVal a = Var Var
               | Abs Text (Expr a) -- arg name, arg type, body
               | App (Expr a) (Expr a)
               | Case (Expr a) [(Pattern, Expr a)]
               | Literal Literal
               | The Type (Expr a)
               | ExprError SourceLoc Text

deriving instance Show a => Show (ExprVal a)

errors :: Expr a -> [(a, SourceLoc, Text)]
errors (Expr label exprVal) =
  case exprVal of
    ExprError loc msg -> [(label, loc, msg)]
    Abs _ x -> errors x
    App f x -> errors f ++ errors x
    Case x cs -> errors x ++ concatMap (errors . snd) cs
    The _ x -> errors x
    _ -> []

data EvalEnv = EvalEnv { _evalLocals :: [Value], _evalGlobals :: Map Symbol Value}
makeLenses ''EvalEnv

defaultEnv :: EvalEnv
defaultEnv = EvalEnv [] Prims.values

type Untyped = Expr SourceRange

eval :: HasRange a => EvalEnv -> Expr a -> Value
eval env (Expr label exprVal) =
  let range = label ^. sourceRange in
  case exprVal of
    ExprError loc msg -> VError range loc msg
    Var (Local i) -> fromMaybe (VError range (range ^. start) "internal error: unbound local") (env ^? evalLocals . ix i)
    Var (Global s) -> fromMaybe (VError range (range ^. start) (T.concat ["unbound global: ", s])) (env ^. evalGlobals . at s)
    Abs _ e -> VFunc (\x -> eval (evalLocals %~ (x:) $ env) e)
    App f x ->
      case eval env f of
        VFunc f' ->
          case eval env x of
            VError r l m -> VError r l m
            x' -> f' x'
        VError r l m -> VError r l m
        _ -> VError range (range ^. start) "applying non-function"
    Literal l -> VLiteral l
    The _ x -> eval env x
    Case val@(Expr valLabel _) cases ->
      case eval env val of
        VData ctor args ->
          case find (\(PData x, _) -> x == ctor) cases of
            Nothing -> VError range (valLabel ^. sourceRange . start) "missing case for value"
            Just (_, expr) -> eval (evalLocals %~ (args ++) $ env) expr
        VLiteral l ->
          case find (\(PLit l', _) -> l == l') cases of
            Nothing -> VError range (valLabel ^. sourceRange . start) "missing case for value"
            Just (_, expr) -> eval (evalLocals %~ (VLiteral l :) $ env) expr
        _ -> VError range (valLabel ^. sourceRange . start) "case on unmatchable value"

fromAST :: AST -> Untyped
fromAST = fromAST' M.empty M.empty

-- Could this use a better data structure than Map?
fromAST' :: Map Symbol Int -> Map Symbol Type -> AST -> Untyped
fromAST' locals types (A.AST range astVal) =
  case astVal of
    A.ASTError loc msg -> Expr range $ ExprError loc msg
    A.Abs [] _ -> Expr range $ ExprError (range ^. start) "nullary abstraction"
    A.Abs args body -> fromAbs locals args body
    A.App _ [] -> Expr range $ ExprError (range ^. start) "nullary application"
    A.App (A.AST range' (A.Ref "the")) [ty, x] ->
      Expr range $ App (Expr range (App (Expr range' (Var (Global "the"))) (typeExprFromAST ty))) (fromAST' locals types x)
    A.App f xs -> foldl (\f' x -> Expr range $ App f' (fromAST' locals types x)) (fromAST' locals types f) xs
    A.Literal l -> Expr range $ Literal l
    A.Ref s -> Expr range $
      case M.lookup s locals of
        Nothing -> Var (Global s)
        Just i -> Var (Local i)
    A.Case expr cases -> Expr range $
      case partitionEithers (map caseFromAST cases) of
        ([], branches) -> Case (fromAST' locals types expr) branches
        (((loc, msg) :_), _) -> ExprError loc msg
    A.The ty expr -> Expr range $
      case parseType types ty of
        Left (loc, msg) -> ExprError loc msg
        Right ty' -> The ty' (fromAST' locals types expr)
  where
    fromAbs :: Map Symbol Int -> [Symbol] -> AST -> Untyped
    fromAbs locals' [] body = fromAST' locals' types body
    fromAbs locals' (x:xs) body = Expr range (Abs x (fromAbs (bind x locals') xs body))

    caseFromAST :: (Pattern, [Symbol], AST) -> Either (SourceLoc, Text) (Pattern, Untyped)
    caseFromAST (PLit lit, [], expr) = Right (PLit lit, fromAST' locals types expr)
    caseFromAST (PAny, [sym], expr) = Right (PAny, fromAST' (bind sym locals) types expr)
    caseFromAST (PData sym, vars, expr) = Right (PData sym, fromAST' (foldr bind locals vars) types expr)
    caseFromAST _ = error "caseFromAST: impossible"

    bind :: Symbol -> Map Symbol Int -> Map Symbol Int
    bind s ls = M.insert s 0 (M.map succ ls)

typeExprFromAST :: AST -> Untyped
typeExprFromAST (A.AST range astVal) =
  case astVal of
    A.Ref s -> Expr range $ Var (Global s)
    A.App t xs -> foldl (\t' x -> Expr range $ App t' (typeExprFromAST x)) (typeExprFromAST t) xs
    _ -> Expr range $ ExprError (range ^. start) "malformed type specifier"

data TIState = TIState { _tiSubst :: Subst, _tiNameCount :: Word64 }
makeLenses ''TIState

data TIError = UnificationError Text Type Type Type Type
             | GenericError SourceLoc Text
  deriving Show

newtype TI a = TI (StateT TIState (Except TIError) a)
  deriving (Functor, Applicative, Monad, MonadState TIState, MonadError TIError)

runTI :: TI a -> Either TIError (a, Subst)
runTI (TI x) = case runExcept (runStateT x (TIState mempty 0)) of
                 Left err -> Left err
                 Right (y, st) -> Right (y, st ^. tiSubst)

freshVar :: Kind -> TI TypeVariable
freshVar = freshVar' "t"

freshVar' :: Symbol -> Kind -> TI TypeVariable
freshVar' n k = do
  x <- use tiNameCount
  tiNameCount %= succ
  pure (TypeVariable (NMachine n x) k)

unify :: Type -> Type -> TI ()
unify t1 t2 = do
  s <- use tiSubst
  case unifier (apply s t1) (apply s t2) of
    Left (msg, t1', t2') -> throwError (UnificationError msg t1 t2 t1' t2')
    Right u -> tiSubst %= mappend u

data TypedLabel = TypedLabel SourceRange Type
  deriving Show

type Typed = Expr TypedLabel

instance Types TypedLabel where
  apply s (TypedLabel r ty) = TypedLabel r (apply s ty)
  typeVars (TypedLabel _ ty) = typeVars ty

instance Types a => Types (Expr a) where
  apply s (Expr label exprVal) =
    Expr (apply s label)
         (case exprVal of
            Abs name body -> Abs name (apply s body)
            App f x -> App (apply s f) (apply s x)
            x -> x)

  typeVars (Expr label exprVal) =
    S.union (typeVars label)
            (case exprVal of
               Abs _ body -> typeVars body
               App f x -> S.union (typeVars f) (typeVars x)
               _ -> S.empty)

exprTy :: Typed -> Type
exprTy (Expr (TypedLabel _ t) _) = t

instance HasRange TypedLabel where
  sourceRange f (TypedLabel r t) = fmap (\r' -> TypedLabel r' t) (f r)

-- Freshen type variables
instantiate :: Type -> TI Type
instantiate t = fst <$> helper M.empty t
  where
    helper :: Map Name TypeVariable -> Type -> TI (Type, Map Name TypeVariable)
    helper _ con@(TyCon _) = pure (con, M.empty)
    helper vs (TyVar (TypeVariable n k)) = do
      v <- case M.lookup n vs of
           Nothing -> freshVar' (nameBase n) k
           Just v -> pure v
      pure (TyVar v, M.insert n v vs)
    helper vs (TyApp ty x) = do
      (ty', vs') <- helper vs ty
      (x', vs'') <- helper vs' x
      pure (TyApp ty' x', vs'')

infer' :: Map Symbol Type -> Map Symbol (Either (DataConstructor, Type) Type) -> [Type] -> Untyped -> TI Typed
infer' types globals locals (Expr range exprVal) =
  (case exprVal of
     Var (Local i) -> ex (locals !! i) (Var (Local i))
     Var (Global n) -> do
       ty <- case M.lookup n globals of
               Nothing -> throwError $ GenericError (range ^. start) "variable not in scope"
               Just (Right t) -> pure t
               Just (Left (DataConstructor args, t)) ->
                 instantiate (foldr fn t args)
       ex ty $ Var (Global n)
     Abs name body -> do
       argTy <- TyVar <$> freshVar Star
       body'@(Expr (TypedLabel _ bodyTy) _) <- infer' types globals (argTy : locals) body
       ex (fn argTy bodyTy) (Abs name body')
     App f x -> do
       f' <- infer' types globals locals f
       x' <- infer' types globals locals x
       retty <- TyVar <$> freshVar Star
       unify (fn (exprTy x') retty) (exprTy f')
       ex retty (App f' x')
     The t x -> do
       x' <- infer' types globals locals x
       unify t (exprTy x')
       pure x'
     Literal l -> ex (literalTy l) (Literal l)
     Case x cases -> do
       x' <- infer' types globals locals x
       retty <- TyVar <$> freshVar Star
       cases' <- forM cases $ \(pat, body) -> do
         locals' <- case pat of
                      PLit lit -> do
                        unify (exprTy x') (literalTy lit)
                        pure locals
                      PAny -> pure $ exprTy x' : locals
                      PData name ->
                        case M.lookup name globals of
                          Just (Left (DataConstructor tys, ctorTy)) -> do
                            unify (exprTy x') ctorTy
                            pure $ tys ++ locals
                          Just (Right _) -> throwError $ GenericError (range ^. start) (T.concat ["not a data constructor: ", name])
                          Nothing -> throwError $ GenericError (range ^. start) (T.concat ["data constructor not in scope: ", name])
         body' <- infer' types globals locals' body
         unify retty (exprTy body')
         pure (pat, body')
       ex retty $ Case x' cases'
     ExprError l t -> do
       ty <- TyVar <$> freshVar Star
       ex ty (ExprError l t))
  `catchError`
  (\err -> do
     ty <- TyVar <$> freshVar Star
     let (loc, msg) = case err of
                        UnificationError m t1 t2 spec1 spec2 -> (range ^. start, m)
                        GenericError l m -> (l, m)
     ex ty $ ExprError loc msg)
  where
    ex :: Type -> ExprVal TypedLabel -> TI Typed
    ex t = pure . Expr (TypedLabel range t)

infer :: Untyped -> Typed
infer x =
  case runTI (infer' M.empty M.empty [] x)  of
    Left _ -> error "infer: impossible"
    Right (t, s) -> apply s t
