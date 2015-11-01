module Tisp.Type where

import Control.Lens
import Data.Word
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

import Tisp.Tokenize
import Tisp.Value

data Kind = Star | KFun Kind Kind
  deriving (Eq, Ord, Show)

data TypeConstructor = TypeConstructor Symbol Kind
                     | TyBits Word32
                     | TyFloat Word32
  deriving (Eq, Show)

function :: Type
function = TyCon $ TypeConstructor "Function" (KFun Star (KFun Star Star))

data TypeVariable = TypeVariable Name Kind
  deriving (Eq, Ord, Show)

data Type = TyCon TypeConstructor
          | TyApp Type Type
          | TyVar TypeVariable
  deriving (Eq, Show)

class HasKind t where
  kind :: t -> Kind

data DataConstructor = DataConstructor { _ctorTag :: Symbol, _ctorArgs :: [TypeVariable] -> [Type] }
makeLenses ''DataConstructor

data TypeDefinition = TypeDefinition Kind [DataConstructor]

instance HasKind TypeDefinition where
  kind (TypeDefinition k _) = k

listDef :: TypeDefinition
listDef = TypeDefinition (KFun Star Star)
                         [ DataConstructor "nil" (\[_] -> [])
                         , DataConstructor "cons" (\[t] -> [ TyVar t
                                                           , TyApp (TyCon (TypeConstructor "List" (KFun Star Star))) (TyVar t)])
                         ]

fn :: Type -> Type -> Type
fn arg ret = TyApp (TyApp function arg) ret

instance HasKind TypeConstructor where
  kind (TypeConstructor _ k) = k
  kind _ = Star

instance HasKind TypeVariable where
  kind (TypeVariable _ k) = k

instance HasKind Type where
  kind (TyCon c) = kind c
  kind (TyVar v) = kind v
  kind (TyApp t _) =
    case kind t of
      KFun _ k -> k
      _ -> error "HasKind Type kind: impossible"

newtype Subst = Subst (Map TypeVariable Type)

singleton :: TypeVariable -> Type -> Subst
singleton v t = Subst (M.singleton v t)

class Types t where
  apply :: Subst -> t -> t
  typeVars :: t -> Set TypeVariable

instance Types Type where
  apply _ t@(TyCon _) = t
  apply s (TyApp c x) = TyApp (apply s c) (apply s x)
  apply (Subst s) t@(TyVar v) = case M.lookup v s of
                                  Just t' -> t'
                                  Nothing -> t

  typeVars (TyVar v) = S.singleton v
  typeVars (TyApp c x) = typeVars c `S.union` typeVars x
  typeVars (TyCon _) = S.empty

instance Types a => Types [a] where
  apply s = map (apply s)
  typeVars = S.unions . map typeVars

instance Monoid Subst where
  mempty = Subst M.empty
  mappend l@(Subst l') (Subst r') = Subst $ M.union (M.map (apply l) r') l'

merge :: Subst -> Subst -> Maybe Subst
merge l@(Subst l') r@(Subst r') =
   if all (\v -> apply l (TyVar v) == apply r (TyVar v)) (S.toList $ M.keysSet l' `S.intersection` M.keysSet r')
     then Just . Subst $ l' `M.union` r'
     else Nothing

unifier :: Type -> Type -> Either (Text, Type, Type) Subst
unifier (TyApp c1 x1) (TyApp c2 x2) =
  do s1 <- unifier c1 c2
     s2 <- unifier (apply s1 x1) (apply s1 x2)
     pure (s2 `mappend` s1)
unifier t1@(TyVar v) t2 =
  case varBind v t2 of
    Left msg -> Left (msg, t1, t2)
    Right u -> Right u
unifier t1 t2@(TyVar v) =
  case varBind v t1 of
    Left msg -> Left (msg, t1, t2)
    Right u -> Right u
unifier (TyCon c1) (TyCon c2)
  | c1 == c2 = pure mempty
unifier t1 t2 = Left ("cannot unify", t1, t2)

varBind :: TypeVariable -> Type -> Either Text Subst
varBind v t
  | t == TyVar v            = Right mempty
  | v `S.member` typeVars t = Left "recursive type"
  | kind v /= kind t        = Left "kind mismatch"
  | otherwise               = Right (singleton v t)

rational :: Type
rational = TyCon $ TypeConstructor "Rational" Star

pointer :: Type
pointer = TyCon $ TypeConstructor "Pointer" Star

literalTy :: Literal -> Type
literalTy (Num _) = rational
literalTy (Foreign _) = pointer
