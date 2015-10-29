module Tisp.TT where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe

import Control.Monad
import Control.Lens

import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty, (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Tisp.Tokenize

data Var = Local Integer | Global Symbol
  deriving (Show, Ord, Eq)

instance Pretty Var where
  pretty (Local x) = PP.text "local-" <> PP.integer (toInteger x)
  pretty (Global g) = PP.text (T.unpack g)

-- (x, t, e) meaning variable named x having type t is bound in e
type Abstraction a = (Text, Expr a, Expr a)

data Expr a = Expr a (ExprVal a)
  deriving (Eq)

deriving instance Show a => Show (Expr a)

data ExprVal a = Var Var
               | Pi (Abstraction a)
               | Lambda (Abstraction a)
               | App (Expr a) (Expr a)
               | Literal Literal
               | Subst (Substitution a) (Expr a)
  deriving (Eq)

deriving instance Show a => Show (ExprVal a)

data LiteralTy = TyNat | TyF32 | TyF64
  deriving (Show, Eq)

data Literal = Bits Word Integer
             | F32 Float
             | F64 Double
             | Nat Integer
             | Succ -- Nat -> Nat
             | LiteralTy LiteralTy
             | TyBits -- Nat -> Type 0
             | TyUniverse -- (n : Nat) -> Type n
  deriving (Show, Eq)

data Substitution a = Shift Integer | Dot (Expr a) (Substitution a)
  deriving (Show, Eq)

data Binding a = Binding { _bindingTy :: Expr a, _bindingDef :: Maybe (Expr a) }
makeLenses ''Binding

data Env a = Env { _locals :: [Binding a], _globals :: Map Symbol (Binding a) }
makeLenses ''Env

type Label = Maybe SourceRange
type Expr' = Expr Label
type Env' = Env Label
type Substitution' = Substitution Label

type Err = ([SourceRange], Text)

compose :: Substitution a -> Substitution a -> Substitution a
compose s (Shift 0) = s
compose (Dot _ s) (Shift m) = compose s (Shift (m - 1))
compose (Shift m) (Shift n) = Shift (m + n)
compose s (Dot e@(Expr x _) t) = Dot (Expr x (Subst s e)) (compose s t)

instance Monoid (Substitution a) where
  mempty = Shift 0
  mappend = compose

ex :: ExprVal Label -> Expr'
ex v = Expr Nothing v

bits :: Word -> Integer -> Expr'
bits n x = ex . Literal . Bits n $ x

natTy :: Expr'
natTy = ex . Literal . LiteralTy $ TyNat

nat :: Integer -> Expr'
nat = ex . Literal . Nat

bitsTy :: Expr' -> Expr'
bitsTy n = ex $ App (ex . Literal $ TyBits) n

univ :: Expr' -> Expr'
univ = ex . App (ex . Literal $ TyUniverse)

globalApp :: Symbol -> Expr' -> Expr'
globalApp name arg = ex $ App (ex . Var . Global $ name) arg

literalTy :: Literal -> Expr'
literalTy (Bits n _) = bitsTy (nat . fromIntegral $ n)
literalTy (F32 _) = ex . Literal . LiteralTy $ TyF32
literalTy (F64 _) = ex . Literal . LiteralTy $ TyF64
literalTy (Nat _) = ex . Literal . LiteralTy $ TyNat
literalTy Succ = ex . Pi $ ("x",  natTy, natTy)
literalTy (LiteralTy _) = univ (ex . Literal . Nat $ 0)
literalTy TyBits =
  ex $ Pi ("width", ex . Literal . LiteralTy $ TyNat, univ (ex . Literal . Nat $ 0))
literalTy TyUniverse =
  ex $ Pi ( "k"
          , ex . Literal . LiteralTy $ TyNat
          , univ (ex $ App (ex . Literal $ Succ) (ex . Var . Local $ 0)))

local :: Env a -> Expr a -> Env a
local (Env ls gs) l = Env (Binding l Nothing : ls) gs

defineLocal :: Env a -> Expr a -> Env a
defineLocal (Env (l@(Binding _ Nothing):ls) gs) binding = Env ((l & bindingDef . _Just .~ binding) : ls) gs
defineLocal (Env (_:_) _) _ = error "TT.defineLocal: applied to an already-defined local"
defineLocal _ _ = error "TT.defineLocal: applied to an environment with no locals"

subst :: Substitution a -> Expr a -> Expr a
subst s e@(Expr x e') =
  case (s, e') of
    (Shift m, Var (Local k)) -> Expr x $ Var (Local (k+m))
    (Dot expr _, Var (Local 0)) -> subst mempty expr
    (Dot _ s', Var (Local k)) -> subst s' (Expr x (Var (Local (k - 2))))
    (_, Var (Global _)) -> e
    (_, Subst t expr) -> subst s (subst t expr)
    (_, Literal _) -> e
    (_, Pi a) -> Expr x $ Pi (substAbstraction s a)
    (_, Lambda a) -> Expr x $ Lambda (substAbstraction s a)
    (_, App e1@(Expr e1x _) e2@(Expr e2x _)) -> Expr x $ App (Expr e1x (Subst s e1)) (Expr e2x (Subst s e2))

substAbstraction :: Substitution a -> Abstraction a -> Abstraction a
substAbstraction s (x, e1@(Expr e1x _), e2@(Expr e2x _)) = (x, Expr e1x $ Subst s e1, Expr e2x $ Subst (Dot (Expr e1x $ Var (Local 0)) (compose (Shift 1) s)) e2)

equal :: Env' -> Expr' -> Expr' -> Either Err Bool
equal ctx x y = do
  x' <- normalize ctx x
  y' <- normalize ctx y
  pure $ x' == y'

inferType :: Env' -> Expr' -> Either Err Expr'
inferType ctx (Expr exprLoc v) = inf v
  where
    inf :: ExprVal Label -> Either Err (Expr Label)
    inf (Subst s e) = inferType ctx (subst s e)
    inf (Var (Local x)) = case ctx ^? locals . ix (fromIntegral x) . bindingTy of
                            Nothing -> Left (maybeToList exprLoc, "free variable")
                            Just ty -> pure ty
    inf (Var (Global name)) = case M.lookup name (ctx ^. globals) of
                                Nothing -> Left (maybeToList exprLoc, T.concat ["undefined variable ", name])
                                Just (Binding ty _) -> pure ty
    inf (Literal l) = pure . literalTy $ l
    inf (Pi (_, t1, t2)) = do
      k1 <- inferUniverse ctx t1
      k2 <- inferUniverse (local ctx t1) t2
      pure . univ . ex . Literal . Nat $ max k1 k2
    inf (Lambda (_, t, e)) = do
      ty <- inferType (local ctx t) e
      pure $ Expr Nothing (Pi ("", t, ty))
    inf (App f@(Expr funcLoc _) x@(Expr argLoc _)) = do
      (_, s, t) <- inferPi ctx f
      te <- inferType ctx x
      wellTyped <- equal ctx s te
      unless wellTyped $ Left ( concatMap maybeToList [funcLoc, argLoc]
                              , "function applied to argument of inapprorpriate type"
                              )
      pure . ex $ Subst (Dot x mempty) t

inferUniverse :: Env' -> Expr' -> Either Err Integer
inferUniverse ctx e@(Expr r _) = do
  ty <- inferType ctx e
  nrm <- normalize ctx ty
  case nrm of
    Expr _ (App (Expr _ (Literal TyUniverse)) (Expr _ (Literal (Nat k)))) -> pure k
    Expr _ (Literal (LiteralTy _)) -> pure 0
    _ -> Left ( maybeToList r, "type expected")

inferPi :: Env' -> Expr' -> Either Err (Abstraction Label)
inferPi ctx e@(Expr r _) = do
  ty <- inferType ctx e
  nrm <- normalize ctx ty
  case nrm of
    Expr _ (Pi a) -> Right a
    _ -> Left ( maybeToList r, "function expected")

normalize :: Env' -> Expr' -> Either Err Expr'
normalize ctx (Expr exprLoc v) =
  case v of
    Var (Local x) ->
      case ctx ^? locals . ix (fromIntegral x) . bindingDef of
        Nothing -> Left (maybeToList exprLoc, "free variable")
        Just Nothing -> pure . Expr exprLoc $ Var (Local x)
        Just (Just e) -> normalize ctx e
    Var (Global x) ->
      case ctx ^? globals . ix x . bindingDef of
        Nothing -> Left (maybeToList exprLoc, "undeclared global")
        Just Nothing -> pure . Expr exprLoc $ Var (Global x)
        Just (Just e) -> normalize ctx e
    App f x -> do
      x' <- normalize ctx x
      f' <- normalize ctx f
      case (f', x') of
        (Expr _ (Literal Succ), (Expr _ (Literal (Nat n)))) -> pure . Expr exprLoc . Literal . Nat $ (n+1)
        (Expr _ (Lambda (_, _, fBody)), _) -> normalize (defineLocal ctx x') fBody
        (other, _) -> pure . Expr exprLoc $ App other x'
    x@(Literal _) -> pure . Expr exprLoc $ x
    Pi a -> Expr exprLoc . Pi <$> normalizeAbs ctx a
    Lambda a -> Expr exprLoc . Lambda <$> normalizeAbs ctx a
    Subst s e -> normalize ctx (subst s e)

normalizeAbs :: Env' -> Abstraction Label -> Either Err (Abstraction Label)
normalizeAbs ctx (x, t, e) = do
  t' <- normalize ctx t
  e' <- normalize (local ctx t') e
  pure $ (x, t', e')
