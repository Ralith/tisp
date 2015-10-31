module Tisp.Type where

import Control.Lens

import Tisp.Tokenize

data Kind = Star | KFun Kind Kind
  deriving (Eq, Show)

data TypeConstructor = TypeConstructor Symbol Kind
  deriving (Eq, Show)

function :: Type
function = TyCon $ TypeConstructor "function" (KFun Star (KFun Star Star))

data TypeVariable = TypeVariable Symbol Kind
  deriving (Eq, Show)

data Type = TyCon TypeConstructor
          | TyApp Type Type
          | TyVar TypeVariable
  deriving (Eq, Show)

data SumCase = SumCase { _caseTag :: Symbol, _caseFields :: [Type] }
makeLenses ''SumCase

type TypeDefinition = [SumCase]

fn :: Type -> Type -> Type
fn arg ret = TyApp (TyApp function arg) ret

class HasKind t where
  kind :: t -> Kind

instance HasKind TypeConstructor where
  kind (TypeConstructor _ k) = k

instance HasKind TypeVariable where
  kind (TypeVariable _ k) = k

instance HasKind Type where
  kind (TyCon c) = kind c
  kind (TyVar v) = kind v
  kind (TyApp t _) =
    case kind t of
      KFun _ k -> k
