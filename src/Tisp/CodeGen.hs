module Tisp.CodeGen (codegen, CodeGen) where

import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

import Control.Monad.Reader hiding (void)

import LLVM.General.AST
import LLVM.General.AST.DataLayout
import LLVM.General.AST.Type
import LLVM.General.AST.Global
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.CallingConvention as CallingConvention
import qualified LLVM.General.AST.Linkage as Linkage
import qualified LLVM.General.AST.Attribute as A

import Tisp.LLVMMonad
import Tisp.AST as AST
import Tisp.Tokenize (Symbol)
import Tisp.LLVMTypes (typeOf)

newtype CGS = CGS [Operand]

newtype CodeGen a = CodeGen (ReaderT CGS FunctionGen a)
  deriving (Functor, Applicative, Monad, MonadReader CGS)

instance MonadModuleGen CodeGen where
  getMGS = CodeGen (lift getMGS)
  putMGS = CodeGen . lift . putMGS
  askTarget = CodeGen (lift askTarget)

instance MonadFunctionGen CodeGen where
  getFGS = CodeGen (lift getFGS)
  putFGS = CodeGen . lift . putFGS
  tellFGO = CodeGen . lift . tellFGO

runCodeGen :: CodeGen a -> FunctionGen a
runCodeGen (CodeGen cg) = runReaderT cg (CGS [])

defaultTarget :: Target
defaultTarget = Target "x86_64-unknown-linux-gnu" (defaultDataLayout { endianness = Just LittleEndian })

codegen :: Record -> Module
codegen defs = runModuleGen "" (sequence (map (codegen' . snd) (M.toList defs))) defaultTarget

codegen' :: AST.Definition -> ModuleGen ()
codegen' (AST.Definition name tyExpr value) = do
  let ty = cgType tyExpr
  runFunctionGen (T.unpack name) ty (tyArgs ty) False functionDefaults (runCodeGen . funcgen value)
  pure ()

funcgen :: AST -> [Operand] -> CodeGen ()
funcgen x args = do
  startBlock (Name "entry")
  local (\(CGS vars) -> CGS (args ++ vars)) $ cgExpr x
  pure ()

cgType :: AST -> Type
cgType (AST _ (Ref (AST.Global "unit"))) = void
cgType (AST _ (Ref (AST.Global "i32"))) = i32
cgType (AST _ (App (AST _ (Ref (AST.Global "func"))) (ret:args))) = FunctionType (cgType ret) (map cgType args) False
cgType x = error $ "unrecognized type " ++ show x

tyArgs :: Type -> [(String, Type, [A.ParameterAttribute])]
tyArgs (FunctionType _ args False) = zip3 (repeat "") args (repeat [])
tyArgs _ = []

cgExpr :: AST -> CodeGen Operand
cgExpr (AST _ v) = cg v
  where
    cg (App (AST _ (Ref (AST.Global f))) xs) = fromJust <$> (call' f =<< mapM cgExpr xs)
    cg (Ref v) = fromJust <$> lookupVar v
    cg (Num n) = pure . ConstantOperand $ C.Int 32 (round n)
    cg (ASTError _ _) = call intrinsicTrap []
    cg (Abs args value) = do
      func <- runFunctionGen "lambda" i32 (map (\(name, ty) -> (T.unpack name, cgType ty, [])) args) False functionDefaults (runCodeGen . funcgen value)
      pure . ConstantOperand . C.GlobalReference (typeOf func) $ (name func)

intrinsicTrap :: Global
intrinsicTrap = functionDefaults { returnType = void, name = Name "llvm.trap", parameters = ([], False) }

call :: Global -> [Operand] -> CodeGen Operand
call fn@(Function {..}) args = inst $ Call { isTailCall = False
                                           , callingConvention = callingConvention
                                           , returnAttributes = []
                                           , function = Right (ConstantOperand (C.GlobalReference (typeOf fn) name))
                                           , arguments = map (\op -> (op, [])) args
                                           , functionAttributes = []
                                           , metadata = []
                                           }
call _ _ = error "call: Tried to call non-function"

call' :: Symbol -> [Operand] -> CodeGen (Maybe Operand)
call' f xs = do
  let name = (Name (T.unpack f))
  global <- findGlobal name
  case global of
    Nothing -> pure Nothing
    Just g -> Just <$> call g xs

lookupVar :: Var -> CodeGen (Maybe Operand)
lookupVar (AST.Local i) = do
  CGS vars <- ask
  if (fromIntegral $ length vars) <= i
     then pure Nothing
     else pure . Just $ vars !! (fromIntegral i)
lookupVar (AST.Global name) = do
  let name' = Name $ T.unpack name
  result <- findGlobal name'
  case result of
    Just value -> pure $ Just (ConstantOperand . C.GlobalReference (typeOf value) $ name')
    Nothing -> pure Nothing
