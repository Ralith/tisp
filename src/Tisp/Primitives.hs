module Tisp.Primitives where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Word (Word64)

import Control.Lens

import Tisp.Tokenize (Symbol)
import Tisp.TT

type VAbstraction = (Value, Value -> Value)

data Value = VLiteral Literal | VPi VAbstraction | VLambda VAbstraction | VNeutral VNeutral | VLApp Literal Value

data VNeutral = VVar Var | VApp VNeutral Value

data Definition a = D { _defTy :: Expr a, _defVal :: Value }
makeLenses ''Definition

reify :: Value -> Expr'
reify (VLiteral l) = ex $ Literal l
reify (VPi a) = ex . Pi $ (reifyAbs a)
reify (VLambda a) = ex . Lambda $ (reifyAbs a)
reify (VNeutral (VVar x)) = ex . Var $ x
reify (VNeutral (VApp f x)) = ex $ App (reify (VNeutral f)) (reify x)
reify (VLApp l v) = ex $ App (ex . Literal $ l) (reify v)

reifyAbs :: VAbstraction -> Abstraction Label
reifyAbs (t, f) = (reify t, reify (f (VNeutral (VVar (Local 0)))))

primitives :: Map Symbol (Definition Label)
primitives = M.fromList
  [ ("nat+", D (ex . Pi $ (natTy, ex . Pi $ (natTy, natTy)))
               (VLambda (VLiteral . LiteralTy $ TyNat, \(VLiteral (Nat x)) -> VLambda (VLiteral . LiteralTy $ TyNat, \(VLiteral (Nat y)) -> VLiteral . Nat $ x + y))))
  , ("unsigned+", D (ex $ Pi ( natTy
                             , ex $ Pi ( bitsTy (ex . Var . Local $ 0)
                                       , ex $ Pi ( bitsTy (ex . Var . Local $ 1)
                                                 , bitsTy (ex . Var . Local $ 2)))))
                    (VLambda (VLiteral . LiteralTy $ TyNat, \bits ->
                     VLambda (VLApp TyBits bits, \(VLiteral (Bits n x)) ->
                      VLambda (VLApp TyBits bits, \(VLiteral (Bits _ y)) -> VLiteral $ Bits n ((x + y) `mod` (2^n)))))))
  ]
